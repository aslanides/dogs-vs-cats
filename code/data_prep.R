source('code/include.R', echo = T)

# File paths
RAW_PATH <- 'data/raw/'
PROC_PATH <- 'data/processed/'

# File lists
train_files <- list.files('data/raw/train/')
test_files <- list.files('data/raw/test/')


# Pre-process names
train_data <- data_frame(raw_name = train_files) %>%
  tbl_df() %>%
  mutate(id = str_pad(str_extract(raw_name, '\\d+'), 5, pad = '0'),
         class = ifelse(str_detect(raw_name, 'dog'), 1, 0)) %>%
  arrange(class, id)

test_data <- data_frame(raw_name = test_files) %>%
  tbl_df() %>%
  mutate(id = str_pad(str_extract(raw_name, '\\d+'), 5, pad = '0'),
         class = NA) %>%
  arrange(id)


# Register a cluster and convert image data from character vector to integer matrix
cl <- makeCluster(4)
registerDoSNOW(cl)


# Pad and resize images
convert_img <- function(x, data_class) {
  RAW_PATH <- 'data/raw/'
  PROC_PATH <- 'data/processed/'
  hist.eq <- function(im) as.cimg(ecdf(im)(im),dim=dim(im))
  
  foreach(i = iter(x, by='row'), .combine = rbind, .packages = c('imager', 'magrittr', 'stringr')) %dopar% {
    read_path  <- str_c(RAW_PATH, data_class, '/', i$raw_name)
    write_path <- str_c(PROC_PATH, data_class, '/', i$class, '_', i$id, '.jpg')

    img <- load.image(read_path) %>%
      grayscale() %>%
      hist.eq()
    
    # Pad images with black bar to set 1:1 aspect ratio
    img_x <- dim(img)[1]
    img_y <- dim(img)[2]
    if(img_x > img_y) {
      img %<>% pad(img_x - img_y, "y", 1)
    } else {
      img %<>% pad(img_y - img_x, "x", 1)
    }
    
    img %<>% resize(64, 64) %>%
      renorm()
    
    save.image(img, write_path)
    
    return(c(i[[1]], i[[2]], i[[3]], floor(as.data.frame(img)$value)))
  }
}


train_df <- convert_img(train_data, 'train') %>%
  as.data.frame()
#test_df <- convert_img(test_data, 'test')

stopCluster(cl)

write_csv(train_df, 'data/train.csv', col_names = F)
#write_csv(test_df, 'data/train.csv', col_names = F)


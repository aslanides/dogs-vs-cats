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
         class = ifelse(str_detect(raw_name, 'dog'), 'dog', 'cat')) %>%
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
  
  foreach(i = iter(x, by='row'), .combine = rbind, .packages = c('magick', 'magrittr', 'stringr')) %dopar% {
    read_path  <- str_c(RAW_PATH, data_class, '/', i$raw_name)
    write_path <- str_c(PROC_PATH, data_class, '/', i$class, i$id, '.jpg')

    img <- image_read(read_path)
    
    img_x <- image_info(img)$width
    img_y <- image_info(img)$height
    
    img <- image_scale(img, str_c(2 * img_x, 'x', 2 * img_y))
    
    img_fill_x <- max(0, img_y - img_x)
    img_fill_y <- max(0, img_x - img_y)

    img_scaled <-
      image_border(img, "grey", str_c(img_fill_x, 'x', img_fill_y)) %>%
      image_scale("64x64")

    image_write(img_scaled, write_path, format = 'jpg')

    img_data <- image_write(img_scaled, format = 'rgba') %>% as.integer()
    
    return(c(as.vector(i), img_data))
  }
}

train_df <- convert_img(train_data[1:10,], 'train') %>%
  as.data.frame()
#test_df <- convert_img(test_data, 'test')

stopCluster(cl)

write_csv(train_df, 'data/train.csv')
write_csv(test_df, 'data/train.csv')


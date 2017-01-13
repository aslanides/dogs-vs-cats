source('code/include.R', echo = T)

load('data/cln_data.Rd')


# Visualise data
vis_img <- function(x) {
  x <- matrix(rev(x), nrow = 96, ncol = 96)
  image(1:96, 1:96, x, col = gray((0:255)/255), axes = F, xlab = '', ylab = '')
}

par(mfrow = c(4,4))
for(i in 1:16) {
  vis_img(im_train[i,])
  
  kp <- train[i, ]
  points(x = 96 - kp$left_eye_center_x, y = 96 - kp$left_eye_center_y, col = 'red')
  points(x = 96 - kp$right_eye_center_x, y = 96 - kp$right_eye_center_y, col = 'green')
  points(x = 96 - kp$mouth_center_bottom_lip_x, y = 96 - kp$mouth_center_bottom_lip_y, col = 'blue')
}


par(mfrow = c(1, 1))
vis_img(im_train[1,])

for(i in 1:nrow(train)) {
  kp <- train[i, ]
  points(x = 96 - kp$left_eye_center_x, y = 96 - kp$left_eye_center_y, col = 'red')
}

outlier <- which(train$left_eye_center_x == min(train$left_eye_center_x, na.rm = T))

vis_img(im_train[outlier,])
kp <- train[outlier, ]
points(x = 96 - kp$left_eye_center_x, y = 96 - kp$left_eye_center_y, col = 'red')

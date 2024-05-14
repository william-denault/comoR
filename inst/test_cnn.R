library(raster)
library(keras)
library(tensorflow)
library(abind)
# Load the image
devtools::load_all(".")

#generate a dummy file
#library(tiff)
#library(png)
# Set up the plot parameters
#png("temp_image.png", width = 300, height = 300)  # Using png to draw the image
#plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))

# Draw some shapes
#rect(1, 1, 3, 3, col="red")
#rect(4, 4, 6, 6, col="blue")
#rect(7, 7, 9, 9, col="green")

# Add some text
#text(5, 5, "Example", cex=1.5)

# Finish the drawing
#dev.off()

# Convert PNG to TIFF
#png_image <- readPNG("temp_image.png")
#writeTIFF(png_image, "example_image.tif")
raster_image <-raster("C:/Document/Serieux/Travail/Package/comoR/data/example_image.tif")

# Function to crop image into smaller tiles
crop_image <- function(r, size) {
  nr <- nrow(r) / size
  nc <- ncol(r) / size
  cropped_images <- list()
  for (i in 1:nr) {
    for (j in 1:nc) {
      extent_obj <- extent(r, (j - 1) * size + 1, j * size, (i - 1) * size + 1, i * size)
      cropped_images[[length(cropped_images) + 1]] <- crop(r, extent_obj)
    }
  }
  return(cropped_images)
}

# Assuming we want 32x32 tiles
# Assuming we want 32x32 tiles
tiles <- crop_image(raster_image, 32)
set.seed(123)
labels <- lapply(tiles, function(x) runif(3))  # For example, 3 classes
# Convert rasters to arrays
image_arrays <- lapply(tiles, function(tile) {
  array <- as.array(tile)
  aperm(array, c(2, 1, 3))  # Adjust dimensions if needed
})

# Stack all images into a single array
image_stack <- abind(image_arrays, along = 4)

x_train <- abind::abind(lapply(image_arrays, function(x) aperm(x, c(2, 1, 3))), along = 4)
x_train <- aperm(x_train, c(4, 1, 2, 3))  # Rearrange to [samples, height, width, channels]
cat("Corrected shape of input images:", dim(x_train), "\n")

set.seed(123)

Y <- matrix(rnorm( length(tiles)*100), nrow=length(tiles), ncol=100)


max_class=10
# Define the model
param_nnet.x  <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(32, 32, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = max_class, activation = 'softmax')

param_como.x  = list(max_class=max_class,mnreg_type="keras",
                     prior="mix_exp" ,
                     epoch     =10,
                     batch_size= 20)
param_como.y  = list(max_class=10,mnreg_type="constant_mnreg",
                     prior="mix_exp"  )
type_noise='column_wise'
init_type="flashier_NMF"
maxit=10
tol=1e-3
library(flashier)
library(comoR)
Y=Y



X_l=x_train
X_f= matrix(rnorm(2*100),   nrow=100)
mnreg_type.x="keras"
mnreg_type.y="constant_mnreg"
K=3
type_noise    = type_noise
init_type     = init_type
param_como.x  =  param_como.x
param_como.y  =  param_como.y
maxit_como    = 1
param_nnet.x  = param_nnet.x
check_l_prior=NULL
cEBMF.obj <- init_cEBMF (Y,
                         X_l,
                         X_f,
                         mnreg_type.x  = mnreg_type.x,
                         mnreg_type.y  = mnreg_type.y,
                         K             = K,
                         type_noise    = type_noise,
                         init_type     = init_type,
                         param_como.x  = param_como.x,
                         param_como.y  = param_como.y,
                         maxit_como    = maxit_como,
                         param_nnet.x  = param_nnet.x

)

cEBMF.obj <- cEBMF (Y=Y,
                    X_l,
                    X_f,
                    mnreg_type.x="keras",
                    mnreg_type.y="constant_mnreg",
                    K=3,
                    type_noise    = type_noise,
                    init_type     = init_type,
                    param_como.x  =  param_como.x,
                    param_como.y  =  param_como.y,
                    maxit_como    = 1,
                    param_nnet.x  = param_nnet.x )
cEBMF.obj$Y_fit

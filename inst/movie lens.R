
library(comoR)
library(tensorflow)
library(keras)
# library(RhpcBLASctl)
# blas_set_num_threads(32)
# install.packages("devtools")
# devtools::install_github("YangLabHKUST/mfair")
library(mfair)
library(Matrix)
library(reshape2)
library(ggplot2)
library(scales)
library(truncnorm)
#set.seed(20230514)
# Create MFAIR object
Y <- t(ml100k$rating)
X <- ml100k$genre
rating_matrix=Y

library(dplyr)
df= ml100k$user
# Create dummy variables for the qualitative columns (Gender and Occupation)
df_transformed <- df %>%
  mutate(Gender_Male = ifelse(Gender == "M", 1, 0) ) %>%
  select(-Gender) %>%
  bind_cols(model.matrix(~Occupation - 1, data = df))

# Remove the original Occupation column
df_transformed <- df_transformed %>%
  select(-Occupation)

# View the transformed data frame
print(df_transformed)
X_f = as.matrix(df_transformed)


library(mfair)
n_repeat=10
trainning_ratio =c( 0.5, 0.7,0.9)



maxit=100
X=X
#define comoR object

library(softImpute)
l2_reg=0.2
Y <-as.matrix(Y)
X_l =X

X_f

o=1
l=1
rating_matrix[rating_matrix == 0] <- NA
train_id=sample(1: nrow(rating_matrix   ), replace=FALSE,
                size= floor(trainning_ratio[o]*nrow(rating_matrix   )))
test_id = (1: nrow(rating_matrix   ))[-train_id]


train_mat =  rating_matrix

Y[test_id,]=0

param_nnet.x =keras_model_sequential() %>%
  layer_dense(units = 64,
              activation = 'relu',
              input_shape = c(ncol(X_l))) %>%
  layer_dense(units = 64,
              activation = 'relu',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 64,
              activation = 'relu',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dense(units = 64,
              activation = 'relu',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dense(units = 64,
              activation = 'relu',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dense(units = 11,
              activation = 'softmax')



param_nnet.y =keras_model_sequential() %>%
  layer_dense(units = 64,
              activation = 'relu',
              input_shape = c(ncol(X_f))) %>%
  layer_dense(units = 64,
              activation = 'relu',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 64,
              activation = 'relu',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dense(units = 64,
              activation = 'relu',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dense(units = 64,
              activation = 'relu',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dense(units = 10,
              activation = 'softmax')

mnreg_type="keras"
K=3
type_noise='column_wise'
init_type="udv_si_svd"#"flashier_NMF"

tol=1e-3

param_como2 = list()
param_susie =  list(L=5)
maxit_como  = 2

param_como.x  = list(max_class=11,mnreg_type="keras",
                     prior="mix_unif" ,
                     epoch     =100,
                     batch_size= 200 )
param_como.y  = list(max_class=11,mnreg_type="keras",
                     prior="mix_unif"  ,
                     epoch     =100,
                     batch_size= 200  )




mnreg_type="keras"
K=3
type_noise='column_wise'
init_type="udv_si_svd"#"flashier_NMF"

tol=1e-3

param_como2 = list()
param_susie =  list(L=5)
maxit_como  = 2

param_como.x  = list(max_class=11,mnreg_type="keras",
                     prior="mix_unif" ,
                     epoch     =20,
                     batch_size= 150)

param_como.y  = list(max_class=10,mnreg_type="constant_mnreg",
                     prior="mix_exp"  )

#intercept <-   fit_default$L_pm[,1]%*% t(fit_default$F_pm[,1])

cEBMF.obj <- comoR:::init_cEBMF (Y   ,# removed estimated intercept
                                 X_l =as.matrix(X_l),
                                 X_f =matrix(runif(prod(dim(X_f))), ncol=ncol(X_f)),# as.matrix(X_f),
                                 mnreg_type.x="keras",
                                 mnreg_type.y="constant_mnreg",
                                 K=7,
                                 type_noise    = type_noise,
                                 init_type     = init_type,
                                 param_como.x  =  param_como.x,
                                 param_como.y  =  param_como.y,
                                 maxit_como    = 2,
                                 param_nnet.x  = param_nnet.x,
                                 param_como2   = param_como2,
                                 param_susie   = param_susie,
                                 check_l_prior = TRUE )


maxit=5
for (o in 1:maxit) {#5 is good
  cEBMF.obj <- comoR:::cEBMF_iter  (cEBMF.obj)
  cEBMF.obj <- comoR:::out_prep.cEBMF(cEBMF.obj)

}

fit_cebmf =cEBMF.obj$Y_fit+1

mfairObject <- createMFAIR(Y, X, K_max = 20)
mfairObject <- fitGreedy(mfairObject )
Y_hat <- predict(mfairObject)
fit_mfai=Y_hat+1
residual_mfai = Y_hat+1 - rating_matrix

residual_cebmf = fit_cebmf - rating_matrix
sqrt( mean(residual_cebmf[test_id,]^2   , na.rm = TRUE) )

sqrt( mean(  residual_mfai[test_id,] ^2 , na.rm = TRUE) )

plot(fit_mfai[1:100,] ,  rating_matrix[1:100,] )
points(fit_cebmf[1:100,]  ,  rating_matrix[1:100,], col="lightgreen")

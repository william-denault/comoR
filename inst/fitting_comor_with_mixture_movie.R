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
library(truncnorm)
library(scales)
set.seed(20230514)
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

o=1
l=1
rating_matrix[rating_matrix == 0] <- NA


for ( l in 1:n_repeat){
  train_id=sample(1: nrow(rating_matrix   ), replace=FALSE,
                  size= floor(trainning_ratio[o]*nrow(rating_matrix   )))
  test_id = (1: nrow(rating_matrix   ))[-train_id]


  train_mat =  rating_matrix
  train_mat[test_id]=NA

  cebnm_L <- function( x,s,g_init=FALSE,fix_g=TRUE, output){

    if (length(x) == 3){ ### just to satisfy check of custom function
      return (ebnm_flat(x))
    }

    print(paste( "NA" ,sum(is.na(x))))
    print(paste( "length" ,length(x)))
    Z <- matrix( 1, nrow=length(x), ncol=1)
    param_como = list(max_class= 11,
                      mnreg_type="keras")
    data <- comoR:::como_prep_data (betahat=x,
                                    se=s, X=as.matrix(X),
                                    Z =Z )

    # you need to retreive the actual number of mixture component in the model
    num_classes <- 11

    #define the nnet paramet using Keras syntax
    param_nnet =keras_model_sequential() %>%
      layer_dense(units = 64,
                  activation = 'relu',
                  input_shape = c(ncol(X))) %>%
      layer_dropout(rate = 0.2)%>%
      layer_dense(units = 64,
                  activation = 'relu' ) %>%
      layer_dropout(rate = 0.2)%>%
      layer_dense(units = 64,
                  activation = 'relu' ) %>%
      layer_dropout(rate = 0.2)%>%
      layer_dense(units = num_classes,
                  activation = 'softmax')

    # run comoR
    fit  <- rlang::exec( "data_initialize_como", !!! param_como ,
                         data= data,
                         prior="mix_unif",
                         # prior= "mix_exp",
                         param_nnet= param_nnet) # initialize the model from the data
    fit <- comoR:::fit.como (  fit, data, max_iter = 10 )

#browser()
    est <- comoR:::post_mean_sd (fit,data)



    g <-   list( c(0,0.1),
                  c(0.1,0.9),
                  c(0.9,0,11),
                  c(1.1,1.9),
                  c(1.9,2.1),
                  c(2.1,2.9),
                  c(2.9,3.1),
                  c(3.1,3.9),
                  c(3.9,3.1),
                  c(4.1,4.9),
                  c(5.9,5.1) )
    print(data$betahat[which(is.na(est$mean))])

    print(data$betahat[which(is.na(est$sd))])
   print( sum(is.na(est$mean)))
   print( sum(is.na(est$sd^2)))

    out <- list( data= data.frame(x=data$betahat,
                                  s=data$se),
                 posterior = data.frame(mean= est$mean,
                                        second_moment=(est$sd^2+est$mean^2)
                 ) ,
                 fitted_g = g,
                 log_likelihood=sum( comoR:::compute_data_loglikelihood(fit, data) * (fit$post_assignment))

    )

    return( out)

  }

  cebnm_F <- function( x,s,g_init=FALSE,fix_g=TRUE, output){

    if (length(x) == 3){ ### just to satisfy check of custom function
      return (ebnm_flat(x))
    }

    print(paste( "NA" ,sum(is.na(x))))
    print(paste( "length" ,length(x)))
    Z <- matrix( 1, nrow=length(x), ncol=1)
    param_como = list(max_class= 10,
                      mnreg_type="keras")
    data <- comoR:::como_prep_data (betahat=x,
                                    se=s, X=as.matrix(X_f),
                                    Z =Z )

    # you need to retreive the actual number of mixture component in the model
    num_classes <- 11

    #define the nnet paramet using Keras syntax
    param_nnet =keras_model_sequential() %>%
      layer_dense(units = 64,
                  activation = 'relu',
                  input_shape = c(ncol(X_f))) %>%
      layer_dropout(rate = 0.2)%>%
      layer_dense(units = 32,
                  activation = 'relu' ) %>%
      layer_dropout(rate = 0.2)%>%
      layer_dense(units = 16,
                  activation = 'relu' ) %>%
      layer_dropout(rate = 0.2)%>%
      layer_dense(units = num_classes,
                  activation = 'softmax')

    # run comoR
    fit  <- rlang::exec( "data_initialize_como", !!! param_como ,
                         data= data,
                         prior="mix_unif",
                         param_nnet= param_nnet) # initialize the model from the data
    fit <- comoR:::fit.como (  fit, data, max_iter = 10 )


    est <- comoR:::post_mean_sd (fit,data)



    g <-  g= list( c(0,0.1),
                   c(0.1,0.9),
                   c(0.9,0,1),
                   c(1.1,1.9),
                   c(1.9,2.1),
                   c(2.1,2.9),
                   c(2.9,3.1),
                   c(3.1,3.9),
                   c(3.9,3.1),
                   c(4.1,4.9),
                   c(5.9,5.1) )

    out <- list( data= data.frame(x=data$betahat,
                                  s=data$se),
                 posterior = data.frame(mean= abs(est$mean),
                                        second_moment=(est$sd^2+est$mean^2)
                 ) ,
                 fitted_g = g,
                 log_likelihood=sum( comoR:::compute_data_loglikelihood(fit, data) * (fit$post_assignment))

    )

    return( out)

  }
  library(flashier)


  library(flashier)

  train_mat =  rating_matrix
  train_mat[test_id]=NA



  fit_custom <- flash_init (train_mat ) %>%

    #flash_factors_init() %>%
    flash_set_verbose(0) %>%
    flash_greedy( ebnm_fn = c(cebnm_L,
                              ebnm_unimodal_nonnegative
    )
    )%>%
    flash_backfit()

  res_flash = flash(  train_mat ) %>%
    flash_set_verbose(0) %>%
    flash_greedy(
    ) %>%
    flash_backfit()






  mfairObject <- createMFAIR(Y, X, K_max = 20)
  mfairObject <- fitGreedy(mfairObject )
  if (dim (mfairObject@W)[2]==0){
    Y_hat <- 0*Y
  }else{
    Y_hat <- predict(mfairObject)
  }
  fit_mfai=Y_hat
  res_cmf <- cmfrec::CMF(X= as.matrix(train_mat), U=as.matrix(X))
  fit_cmf = t ( res_cmf$matrices$A)%*% res_cmf$matrices$B
  fit_flash = fitted(res_flash)
  fit_cebmf =  fitted(fit_custom)




  residual_flash = fit_flash -as.matrix(rating_matrix)
  residual_cebmf = fit_cebmf - as.matrix(rating_matrix)

  residual_mfai  = fit_mfai-as.matrix(rating_matrix)
  residual_CMF = fit_cmf - as.matrix(rating_matrix)



  rmse_flash=  sqrt( mean( (residual_flash[test_id, ])^2, na.rm = TRUE))
  rmse_cebmf = sqrt( mean((residual_cebmf[test_id, ])^2, na.rm = TRUE) )
  rmse_mfai  = sqrt( mean(( residual_mfai[test_id, ])^2, na.rm = TRUE) )
  rmse_CMF  = sqrt( mean(( residual_CMF[test_id, ])^2, na.rm = TRUE) )
  rmse= c(rmse_flash,rmse_cebmf, rmse_mfai,rmse_CMF)
  rmse
  names(rmse) = c("EBMF", "cEBMF", "MFAI", "CMF")
  iter=l
  out  = list(rmse=rmse,
              ratio= trainning_ratio [o], iter=l)

  save(out, file = paste0( "C:/Document/Serieux/Travail/Data_analysis_and_papers/redo_plot_ICLR/movie_lens_analysis/" ,
                           100*trainning_ratio [o],"_", l,".RData"))
}





rm(list=ls())
library(keras)
library(comoR)
library(nnet)
devtools::load_all(".")
set.seed(1)
y <-runif(2000)
X = cbind(y)
xtrue = 0*y
for (i in 1:length(xtrue)){

  if ( (    y[i] <.5 & y[i] >.2 ) ){
    xtrue[i] = 0
  }else{
    xtrue[i]= rnorm(1)
  }


}
plot (y,xtrue)
x = xtrue + rnorm(length(xtrue), sd=0.2)
plot (y,x)
s= rep(0.2,length(x))
Z <- matrix( 1, nrow=length(x), ncol=1)


#start by defining the como parameter
param_como = list(max_class= 10,
                  mnreg_type="keras")
data <- comoR:::prep_data_como2 (betahat=x,
                                 se=s, X=X,
                                 Z =Z )

# you need to retreive the actual number of mixture component in the model
num_classes <- length( autoselect_scales(data$betahat, data$se,10))

#define the nnet paramet using Keras syntax
param_nnet =keras_model_sequential() %>%
  layer_dense(units = 64,
              activation = 'relu',
              input_shape = c(1)) %>%
  layer_dense(units = 64,
              activation = 'relu' ) %>%
  layer_dense(units = 64,
              activation = 'relu' ) %>%
  layer_dense(units = num_classes,
              activation = 'softmax')

# run comoR
fit_como  <- rlang::exec( "data_initialize_como", !!! param_como ,
                          data= data,
                          param_nnet= param_nnet) # initialize the model from the data
fit_como <- comoR:::fit.como ( fit_como, data, max_iter = 5 )
fit =fit_como










#check performance.


plot(fit_como$post_assignment[,1],y ,   col =ifelse(y<0.5, 1,2))
est <- comoR:::post_mean_sd (fit,data)

g <- ashr::normalmix(rep(1/length(fit$f_list),length(fit$f_list)),
                     rep( 0, length(fit$f_list)),
                     do.call(c, lapply( 1: length(fit$f_list) ,
                                        function(k) {sqrt(fit$f_list [[k]]$var) } )
                     )
)

par(mfrow=c(1,2))
plot(est$mean, xtrue ,   col =ifelse(xtrue==0, 1,2))



lol <- ashr::ash(x, s )

plot(lol$result$PosteriorMean, xtrue,  col =ifelse(xtrue==0, 3,4))
cor (lol$result$PosteriorMean, xtrue)


rmse = function(x,y){
  sqrt(mean (x-y)^2)
}
rmse(lol$result$PosteriorMean, xtrue)

rmse(est$mean, xtrue )

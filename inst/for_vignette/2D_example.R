rm(list=ls())
library(keras)
library(comoR)
library(nnet)
devtools::load_all(".")

set.seed(1)

axis1   <-runif(10000)
axis2   <-runif(10000)
X  = cbind(axis1,axis2)
plot (axis1,axis2)

set.seed(1)

xtrue <- rep(0,length(axis2))

for (i in 1:length(axis2)){

  if( (axis1[i] <.5 & axis2 [i] <.5 )|(axis1[i] >.5 & axis2 [i] >.5 ))    {
    xtrue [i] <- rnorm(1)

  }


}



df_plot <- data.frame(x=axis1,
                      y=axis2,
                      mixture= factor( ifelse(xtrue==0, "H0", "H1") )
)


P1 <- ggplot(df_plot, aes( x,y, col= mixture))+
  geom_point()
P1
plot (axis1,axis2,  col = ifelse(xtrue==0, 1, 2) )

x = xtrue + rnorm(length(xtrue), sd=1)


s = rep(1, length(x))
Z <- matrix(1, nrow = length(x), ncol = 1)
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
              input_shape = c(2)) %>%

  layer_dense(units = 64,
              activation = 'relu' ) %>%
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
df = data.frame(x=axis1, y=axis2, est_prior_H0=fit_como$post_assignment[,1])
library(ggplot2)
P2 <- ggplot(df , aes(x,y, col= est_prior_H0))+geom_point()



est <- comoR:::post_mean_sd (fit,data)


g <- ashr::normalmix(rep(1/length(fit$f_list),length(fit$f_list)),
                     rep( 0, length(fit$f_list)),
                     do.call(c, lapply( 1: length(fit$f_list) ,
                                        function(k) {sqrt(fit$f_list [[k]]$var) } )
                     )
)

par(mfrow=c(1,2))
plot(est$mean, xtrue ,   col =ifelse(xtrue==0, 1,2))
abline(a=0,b=1)


lol <- ashr::ash(x, s )
plot(lol$result$PosteriorMean, xtrue,  col =ifelse(xtrue==0, 3,4))
abline(a=0,b=1)
cor (lol$result$PosteriorMean, xtrue)
par(mfrow=c(1,1))

rmse = function(x,y){
  sqrt(mean (x-y)^2)
}
rmse(lol$result$PosteriorMean, xtrue)

rmse(est$mean, xtrue )



library(gridExtra)
grid.arrange(P1,P2, ncol=2)
cor (est$mean, xtrue)
cor (lol$result$PosteriorMean, xtrue)

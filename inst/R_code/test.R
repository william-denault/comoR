rm(list=ls())
library(keras)
library(comoR)
library(nnet)
library(tensorflow)

set.seed(1)
y1 <-runif(2000, max=2)

l1 = 0*y1
for (i in 1:length(l1)){

  if ( (    y1[i] <.5 & y1[i] >.2 ) | (y1[i] >1.5 & y1[i] <1.8) ){
    l1[i] = 0
  }else{
    l1[i]= rnorm(1,sd=0.5+ 2*abs(sin( pi*y1[i])))
  }


}


y2 <-runif(2000, max=2)

l2 = 0*y2
for (i in 1:length(  l2)){

  if ( (    y2[i] <.5 & y2[i] >.2 ) | (y2[i] >1.5 & y2[i] <1.8) ){
   l2[i] = 0
  }else{
    l2[i]= rnorm(1,sd=0.5+ 2*abs(sin( pi*y2[i])))
  }


}


X = cbind(y1,y2)
X_l =X

f1 <- rnorm(2000, sd=1)
f1  <- f1*sample(c(0,1), size =length(f1), prob = c( 0.5, 0.5), replace =TRUE)
 f2 <- rnorm(2000, sd=2)
#f2  <- f2*sample(c(0,1), size =length(f2), prob = c( 0.5, 0.5), replace =TRUE)


Z = l1%*%t(f1) + l2%*%t(f2)
Yobs = Z + matrix(rnorm(length(l1)*length(f1), sd=3), nrow=length(l1), ncol=length(f1))


cebnm_L <- function( x,s,g_init=FALSE,fix_g=TRUE, output){

  if (length(x) == 3){ ### just to satisfy check of custom function
    return (ebnm_flat(x))
  }
  Z <- matrix( 1, nrow=length(x), ncol=1)
  param_como = list(max_class= 10,
                    mnreg_type="keras",
                    epoch = 10)
  data <- comoR:::como_prep_data (betahat=x,
                                  se=s, X=X_l,
                                  Z =Z )

  # you need to retreive the actual number of mixture component in the model
  num_classes <- length( autoselect_scales_mix_norm(data$betahat, data$se,10))

  #define the nnet paramet using Keras syntax
  param_nnet =keras_model_sequential() %>%
    layer_dense(units = 10,
                activation = 'relu',
                input_shape = c(ncol(X_l))) %>%
    layer_dense(units = num_classes,
                activation = 'softmax')

  # run comoR
  fit  <- rlang::exec( "data_initialize_como", !!! param_como ,
                       data= data,
                       param_nnet= param_nnet) # initialize the model from the data
  fit <- comoR:::fit.como (  fit, data, max_iter = 40)


  est <- comoR:::post_mean_sd (fit,data)



  g <- ashr::normalmix(rep(1/length(fit$f_list),length(fit$f_list)),
                       rep( 0, length(fit$f_list)),
                       do.call(c, lapply( 1: length(fit$f_list) ,
                                          function(k) {sqrt(fit$f_list [[k]]$var) } )
                       )
  )

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




library(flashier)
fit_custom <- flash_init(Yobs ) %>%

  flash_set_verbose(0) %>%
  flash_greedy(
    Kmax = 2,
    ebnm_fn = c(cebnm_L, ebnm_point_exponential)
  )

f <- flashier::flash(Yobs)

fit_default  <- f
cor (c(fitted(fit_default )) ,c(Z))
cor (c(fitted(fit_custom )) ,c(Z))
rmse = function(x,y){
  sqrt(mean ((x-y)^2))
}
rmse (c(fitted(fit_default )) ,c(Z))
rmse (c(fitted(fit_custom )) ,c(Z))
plot(f$L_pm[,1], fit_custom$L_pm[,1])
 plot(y2, f$L_pm[,1])
plot(y1, f$L_pm[,2])

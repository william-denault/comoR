

rm(list=ls())
library(keras)
library(comoR)
library(nnet)
library(tensorflow)
devtools::load_all(".")
set.seed(1)
y <-runif(2000,min=-0.5, max=2.5)
X = cbind(y)
xtrue = 0*y
for (i in 1:length(xtrue)){

  if ( (    y[i] <.5 & y[i] >.0 ) | (y[i] >1.5 & y[i] <2) ){
    xtrue[i] = 0
  }else{
    xtrue[i]= rnorm(1,sd=0.5+ 2*abs(sin( pi*y[i])))
  }


}
plot (y,xtrue, main="true underlying effect")
x = xtrue + rnorm(length(xtrue), sd=1)
plot (y,x, main="observed estimates")
s= rep(1,length(x))
Z <- matrix( 1, nrow=length(x), ncol=1)

param_unif_prior=  list( list(min= 0, max= 0.1),
                                           list(min= 0.1, max= 0.9),
                                           list(min= 0.9, max= 1.1),
                                           list(min= 1.1, max= 1.9),
                                           list(min= 1.9, max= 2.1),
                                           list(min= 2.1, max= 2.9),
                                           list(min= 2.9, max= 3.1),
                                           list(min= 3.1, max= 3.9),
                                           list(min= 3.9, max= 4.1),
                                           list(min= 4.1, max= 4.9),
                                           list(min= 4.9, max= 5.1))
g=param_unif_prior

num_classes <- length(g)
#start by defining the como parameter using mixture of exponential priors and a neural net regressor
param_como = list(max_class= num_classes,
                  mnreg_type="keras",
                  prior =  "mix_unif"
)
data <- comoR:::como_prep_data (betahat=x,
                                se=s, X=X,
                                Z =Z )





#define the nnet paramet using Keras syntax
param_nnet =keras_model_sequential() %>%
  layer_dense(units = 64,
              activation = 'relu',
              input_shape = c(1)) %>%
  layer_dense(units = 64,
              activation = 'relu' ) %>%
  layer_dense(units = 32,
              activation = 'relu' ) %>%
  layer_dense(units = 16,
              activation = 'relu' ) %>%
  layer_dense(units = num_classes,#important to have the same number of units as the number of classes
              activation = 'softmax')

fit_como  <- rlang::exec( "data_initialize_como", !!! param_como ,
                          data= data,
                          param_nnet= param_nnet) # initialize the model from the data
fit =fit_como

#fit$data_loglik <- compute_data_loglikelihood(fit, data)
#hist(fit$data_loglik)

convolved_logpdf.unif(fit$f_list[[10]], data$betahat, data$se)
fit$f_list
K <- fit$K

# pre-compute data likelihood, if we haven't already
# TODO: use digest::digest to hash the scales and recompute if scales change?
if(is.null(fit$data_loglik)){
  fit$data_loglik <- compute_data_loglikelihood(fit, data)
}



do.call( rbind,
         lapply(1: length(data$betahat) ,
         function( i) {unlist(
                                      lapply( 1:length( fit$f_list)  ,
                                             function( j) convolved_logpdf.unif( fit$f_list[[j]],
                                                                                 data$betahat[i],
                                                                                 data$se[i] )
                                            )
                                    )

                              }
)
)

t1 =

t2 = do.call( rbind,
              lapply(1: length(data$betahat) ,
                     function( i) {unlist(
                       lapply( 1:length( fit$f_list)  ,
                               function( i) convolved_logpdf_unif( fit$f_list[[i]],
                                                                   data$betahat[j],
                                                                   data$se[i] )
                       )
                     )

                     }
              )
)



data_loglik <- do.call(cbind,

                       purrr::map(
                         fit$f_list, ~ convolved_logpdf(.x,  data$betahat,  data$se)
                       ))



# updates posterior assignments probabilities
# these are the response variable for mn_regression
if (update_assignment) {
  fit$post_assignment <- compute_posterior_assignment(fit, data)
  if (sum(is.na(fit$post_assignment) )> 0) {
    fit$post_assignment <-  matrix (1/K, #some problem happens with keras sometime I dont know why...
                                    nrow=length(data$betahat),
                                    ncol = K)
  }


}

if (update_logreg) {
  fit$mnreg <- update_prior(fit$mnreg,
                            resps=   fit$post_assignment,
                            loglik=  fit$data_loglik,
                            data=data)
}

if (track_elbo){
  fit$elbo <- c(fit$elbo, compute_elbo(fit, data))
}
return(fit)

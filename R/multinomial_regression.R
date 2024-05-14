# interface for covariate moderated prior weights

#' Compute the prior probability of assignment to each mixture component
#' @export
compute_log_prior_assignment <- function(x, ...){
  UseMethod("compute_log_prior_assignment", x)
}


#' Update the multinomial regression
#' @export
update_prior <- function(x, ...){
  UseMethod("update_prior", x)
}

#' Compute ELBO, but if there is not ELBO
#' @export
compute_elbo <- function(x, ...){
  UseMethod("compute_elbo", x)
}

#' Default no ELBO
#' @export
compute_elbo.default <- function(mnreg, ...){
  return(0)
}


# Constant prior------------
# Prior is the same for all observations
initialize_constant_mnreg <- function(K,n){
  logpi <-  matrix (log(1/K), nrow=n, ncol = K)

  mnreg <- list(logpi=logpi, K=K)
  class(mnreg) <- 'constant_mnreg'
  return(mnreg)
}

#' @param mnreg multinomial regression object
#' @param data a list at least containing covariates `X`
#' @export
compute_log_prior_assignment.constant_mnreg <- function(mnreg, data){
  n <- nrow(data$X)

  logpi <-  mnreg$logpi
  return(logpi)
}

#' @param mnreg multinomial regression object
#' @param resp responsibilities, posterior assignment probability
#'     of each observation to each class
#' @param data a list at least containing covariates `X`
#' @export
update_prior.constant_mnreg <- function(mnreg, resps,loglik, data){

 # browser()
  new_log_pis <- log(  mixsqp::mixsqp(loglik,
                                              log=TRUE,control = list(verbose=FALSE))$x)
  new_log_pis <- .clamp(new_log_pis, 1e4, -1e4)

  mnreg$logpi <- matrix( new_log_pis, byrow = TRUE,
                         nrow=nrow(data$X),
                         ncol=length(new_log_pis))
  return(mnreg)
}


#' @param mnreg multinomial regression object
#' @param resp responsibilities, posterior assignment probability
#'     of each observation to each class
#' @param data a list at least containing covariates `X`
#' @export
compute_elbo.constant_mnreg <- function(mnreg, resps, data){
  # E[log p(y | X, \theta)] - KL[q(theta) | p (theta)]
  # in this case theta is a point estimate so just compute
  # E[log p(y | pi)] where expectations are over q(y)
  logpi <- compute_log_prior_assignment(mnreg, data)

  ll <- sum(resps * exp(logpi))
  return(ll)
}


#Multinomial/ NNET model------------
initialize_mnreg  <- function(mnreg_type,K, n,  p,param_nnet=list( size=1, decay=1)){


  tt <-rlang::exec( "nnet",
                    !!! param_nnet ,
                    y = matrix (1/K, nrow=n, ncol = K),
                    x =matrix (rnorm(n*p), nrow=n, ncol = p),
                    softmax=TRUE  ,
                    trace=FALSE )

  logpi <- log(tt$fitted.values )
  coef  <- tt
  mnreg  <- list(logpi=logpi,
                 K=K,
                 coef=coef,
                 param_nnet=param_nnet)
  class(mnreg) <- 'mult_reg'

  return(mnreg)
}
update_prior.mult_reg<- function(mnreg, resps, loglik, data   ){
  X  = as.matrix(data$X)
  tt <-rlang::exec( "nnet",
                    !!!mnreg$param_nnet ,
                    y = resps,
                    x = X,
                    softmax=TRUE  ,
                    trace=FALSE )
  mnreg$logpi <- log(tt$fitted.values)
  mnreg$coef  <- tt

  return(mnreg)
}


#' @rdname compute_log_prior_assignment
#'
#' @method compute_log_prior_assignment
#'
#' @export compute_log_prior_assignment.mult_reg
#' @export
#' @keywords internal

compute_log_prior_assignment.mult_reg <- function(mnreg, data){
  # in case of multinomial cbind( 1,exp(tt))/(1+apply(exp(tt),1,sum))
  logpi <- mnreg$logpi
  return(logpi)
}

## Keras object
initialize_nnet_keras <-function (mnreg_type,K ,n , param_nnet,epoch, verbose=0, batch_size=100,  weights=NULL ){
  logpi <-  matrix (log(1/K), nrow=n, ncol = K)

  mnreg <- list(logpi=logpi,
                K=K,
                param_nnet=  keras::clone_model(param_nnet),
                model= NULL,
                epoch=epoch,
                verbose=verbose,
                batch_size=  batch_size)
  if(!is.null(weights)){

    mnreg$model=  keras::clone_model(param_nnet)
    set_weights(mnreg$model,  weights)

  }

  class(mnreg)="keras_obj"
  return(mnreg)

}
#' @rdname compute_log_prior_assignment
#'
#' @method compute_log_prior_assignment
#'
#' @export compute_log_prior_assignment.keras_obj
#' @export
#' @keywords internal

compute_log_prior_assignment.keras_obj <- function(mnreg, data){
 # in case of multinomial cbind( 1,exp(tt))/(1+apply(exp(tt),1,sum))
  logpi <- mnreg$logpi
  return(logpi)
}

#' @rdname update_prior
#'
#' @method update_prior
#'
#' @export update_prior.keras_obj
#' @importFrom keras clone_model
#' @export
#' @keywords internal
update_prior.keras_obj<- function(mnreg, resps,loglik, data   ){
  #X  = as.matrix(data$X)

  X  = data$X

  model1 <- keras::clone_model(mnreg$param_nnet )
  if (!is.null(mnreg$model)){
    set_weights(model1, get_weights(mnreg$model))
  }

  batch_size = mnreg$batch_size
  model1 <-  model1 %>% compile(
    loss = custom_loss,
    optimizer= 'adam',#optimizer_adam(learning_rate = 1e-4, beta_1 = 0.9),
    metrics = c('accuracy')
  )

  x_train=X
  y_train= loglik
  candidate_batch_size =divisors(nrow(y_train))

  idx = which.min(abs( divisors(nrow(y_train))-batch_size))
  custom_batch_size <- candidate_batch_size[idx]

  history <-model1 %>% fit(
    x_train, y_train,
    epochs = mnreg$epoch,
    batch_size = custom_batch_size#,
      #verbose  = mnreg$verbose
  )


  pred <- predict(model1, x_train)
  pred[pred<1e-32]<- 1e-32#avoid numerical issue
  mnreg$logpi <- log(pred)
  mnreg$model <- model1

  return(mnreg)
}


custom_loss <- function(L, pi) {
  return(-sum(tf$reduce_logsumexp(L + log(tf$maximum(pi, 1e-5)), axis=as.integer(1))))
  #out <- out -tf$math$maximum(0,out) + tf$math$sqrt(out -tf$math$minimum(out,0)+1 )+1# doing the same thing as below
  # but tensorflow do not like logical operation in cus tom loss

  return(out)
}




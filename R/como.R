# Implements covariate moderated ASH "MOre COmponents COvariate MOderated"

#' @title Function implementation the como mode
#' @details Function implementation the como mode
#'
#' @param data an object of class data_como  see \link{\code{set_data_como}}
#' @param modeltype of model currently supported (normal and beta )
#' @param maxiter numeric, maximum numerous of iteration set to 100 by defaults
#' @param tol tolerance in term of change in ELBO value for stopping criterion
#' @param upper, logical, set to FALSE by default. Specific to beta distribution.
#'  If true use a to set of mixture for fitting both end of the of the distribution as in the ZAP paper by Leung and Sunn
#' @parma nullweight  numeric value for penalizing likelihood at point mass 0/null component (should be larger than  1, 1 corresponds to no penalty , 2 corresponds to considering 1 individual "being null" and so on)
#' (usefull in small sample size)
#' @param verbose logical, set to FALSE by default
#' @param track_elbo logical, set to TRUE by default
#' @param set_weights logical, set to FALSE by default. If TRUE use the weight oif the neural net as starting point for the next iteration
#' @export
#' @example
#' #Simulate data under the como model
#' sim  <- sim_twococomo()
#' #preparing the data
#' data <- set_data_como(betahat = sim$betahat,
#'                                se = sim$se ,
#'                                 X = sim$X)
#' #fit como model
#' fit <- fit.como(data, maxiter=20)
#' plot(fit$elbo)
#' .monotone(fit$elbo)
#'
#' #get posterior quantities
#' est<- post_mean_sd.como (fit)
#' head(est)
#'  plot( est$mean, data$betahat)
#'
#' #comparison with ash
#'
#' t_ash <- ash(sim $betahat, sim $se, mixcompdist = "normal")
#' post_mean_ash <- t_ash$result$PosteriorMean
#' plot(est$mean, post_mean_ash)
#' # TODO make a more convincing example
#'
#'  sim  <- logisticsusie:::sim_como_beta(n=100)
#'#preparing the data
#'data <- set_data_como(p = sim$p,
#'                          X = sim$X)
#'
#' fit <- fit.como(data, maxiter=20)

initialize_como <- function(
                            scales,
                            n,
                            p,
                            p2,
                            mu0=0,
                            var0=1,
                            nullweight=0,
                            mnreg_type='constant',
                            epoch=10,
                            batch_size=100,
                            param_nnet =list( size=1, decay=1),
                            prior = c("mix_norm", "mix_exp", "mix_unif"),
                            g=NULL,#prior coded under ashr type mixture form
                            verbose_keras=0,
                            weights =NULL
                            ){
  # initialize multinomial susie-- but could be any multinomial regression
  K      <- length(scales)
  prior  <- match.arg(prior)


  if(mnreg_type == 'constant_mnreg'){
    mnreg <- initialize_constant_mnreg( K          = K,
                                        n          = n)

  }
  if( mnreg_type== "mult_reg"){

    mnreg <- initialize_mnreg (mnreg_type = mnreg_type,
                               K          = K,
                               n          = n,
                               p          = p,
                               param_nnet = param_nnet)

  }
  if( mnreg_type== "keras"){

    mnreg <- initialize_nnet_keras (mnreg_type = mnreg_type,
                               K          = K,
                               n          = n,
                               param_nnet =param_nnet ,
                               epoch      = epoch ,
                               verbose    = verbose_keras,
                               batch_size = batch_size,
                               weights=  weights)

  }

  #mn_reg <- logisticsusie:::initialize_sbmn_susie(K, n, p, p2, L, mu0, var0)

  # initialize_scales
  if (prior == "mix_norm") {
    f_list <- purrr::map(scales, ~ normal_component(mu = 0, var = .x^2))
  }
  if (prior == "mix_exp") {
    f_list <- purrr::map(scales, ~ exp_component(mu = 0, scale = .x))
  }
  if (prior == "mix_unif") {
    f_list <- list( list(min= 0, max= 0.1),
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
    g=  list( c(0,0.1),
              c(0.1,0.9),
              c(0.9,.11),
              c(1.1,1.9),
              c(1.9,2.1),
              c(2.1,2.9),
              c(2.9,3.1),
              c(3.1,3.9),
              c(3.9,3.1),
              c(4.1,4.9),
              c(4.9,5.1) )
    K= length(g)
  }

  fit <- list(
    mnreg = mnreg, # multinomial regression function. takes X, returns pi
    f_list = f_list, # component distributions
    nullweight = nullweight, # penalty promoting the first component,
    K = K,
    elbo = -Inf,
    prior =prior,
    g     = g
  )
  class(fit) <- c('como')
  return(fit)
}

#' Use data to autoselect scales
#' @title data_initialize_como
#'  @param data description
#'  @param scales description
#'  @param mu0 description
#'  @param var0 description
#'  @param nullweight description
#'
#'  @param mnreg_type description
#'  @param param_nnet description
#' @export
data_initialize_como <- function(data,
                                 prior = c("mix_norm", "mix_exp", "mix_unif"),
                                 max_class=10,
                                 scales=NULL,
                                 mu0=0,
                                 var0=1,
                                 nullweight=0,
                                 mnreg_type='mnreg_constant',
                                 param_nnet =list( size=1, decay=1),
                                 epoch=10,
                                 verbose_keras=0,
                                 batch_size=100,
                                 weights  = NULL ) {
  como_check_data(data)


  prior  <- match.arg(prior)

  if(is.null(scales)){
    if(prior== "mix_norm"){
      scales <- autoselect_scales_mix_norm(data$betahat, data$se, max_class=max_class)
    }
    if(prior== "mix_exp"){

      scales <- autoselect_scales_mix_exp(data$betahat, data$se,max_class= max_class)
    }

    if (prior == "mix_unif") {
      f_list <-list( list(min= 0, max= 0.1),
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
      scales=  list( c(0,0.1),
                c(0.1,0.9),
                c(0.9,.11),
                c(1.1,1.9),
                c(1.9,2.1),
                c(2.1,2.9),
                c(2.9,3.1),
                c(3.1,3.9),
                c(3.9,3.1),
                c(4.1,4.9),
                c(4.9,5.1) )
      K= length(g)
    }

  }

  K  <- length(scales) # K <= max_class
  p  <- ncol(data$X)
  if(inherits(data$X,'matrix')){

    n  <- nrow(data$X)
  }else{

    n  <- nrow(data$Z)
  }
  p2 <- ncol(data$Z)

  if(prior== "mix_exp"){
    g <- ebnm::gammamix(pi=rep(1/length(scales ),
                               length(scales )),
                        shape= rep(1,
                                   length(scales )),
                        scale =scales,
                        shift =rep(0,
                                   length(scales) )
    )
  }

  if(prior== "mix_norm"){
    g <- ashr::normalmix(pi   = rep(1/length(scales ),
                                length(scales )),
                         mean = rep(0,
                                   length(scales) ),
                         sd   = scales

    )
  }
print(n)
  fit <- initialize_como(scales=scales,
                         n=n,
                         p=p,
                         p2=p2,
                         mu0=mu0,
                         var0=var0,
                         nullweight,
                         mnreg_type=mnreg_type,
                         param_nnet=param_nnet,
                         prior     = prior,
                         g = g,
                         epoch=epoch,
                         verbose_keras=verbose_keras,
                         batch_size=batch_size,
                         weights= weights)
  return(fit)
}

#' @export
update_model.como <- function(fit, data, update_assignment = T, update_logreg=T, fit_prior_variance=F, track_elbo=T){
  K <- fit$K

  # pre-compute data likelihood, if we haven't already
  # TODO: use digest::digest to hash the scales and recompute if scales change?
  if(is.null(fit$data_loglik)){
    fit$data_loglik <- compute_data_loglikelihood(fit, data)
  }

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
}

#' @export
compute_elbo.como <- function(fit, data) {
  # E[log p(beta | y)] -- expected data likelihood
  ll <- sum(fit$post_assignment * fit$data_loglik)

  # Entropy term # E[-log q(y)]
  assignment_entropy <- sum(apply(fit$post_assignment, 1, logisticsusie:::categorical_entropy))

  # E[log p(y | X, theta)] - KL[q(theta) || p(theta)] SuSiE ELBO
  elbo <- compute_elbo(fit$mnreg, fit$post_assignment, data)

  # put it all together
  elbo <- ll - assignment_entropy + elbo
  return(elbo)
}



#' @export
fit.como <- function(x, ...){

  return(fit_model(x, ...))
}


get_KL.como <- function(fit,data){
  ll <- sum(fit$post_assignment * fit$data_loglik)

  # Entropy term # E[-log q(y)]
  assignment_entropy <- sum(apply(fit$post_assignment, 1, logisticsusie:::categorical_entropy))

  # E[log p(y | X, theta)] - KL[q(theta) || p(theta)] SuSiE ELBO

  # put it all together
  KL <- -ll + assignment_entropy
  return(KL)
}




set_data_como <- function(betahat, se, X, ...) {



  if (!missing(betahat)) {
    if (!is.numeric(betahat)) {
      stop("betahat should be numercial vector")
    }
    if (!is.numeric(se)) {
      stop("se should be numercial vector")
    }

    dat <- list(
      betahat = betahat,
      se = se,
      X = X
    )
    class(dat) <- c("normal", "data_como")
  }



  return(dat)
}

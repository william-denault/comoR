# same interface as `multinomial_regression`
# implements 'compute_log_prior_assignment', 'update_prior', and 'compute_elbo'

#' Compute the prior probability of assignment to each mixture component
#' @export
compute_prior_log_odds <- function(x, ...){
  UseMethod("compute_prior_log_odds", x)
}


# Constant prior------------
# Prior is the same for all observations
initialize_constant_logreg <- function(pi0=0.5){
  logreg <- list(pi0=pi0)
  class(logreg) <- 'constant_logreg'
  return(logreg)
}

#' @param logreg logistic regression object
#' @param data a list at least containing covariates `X`
#' @export
compute_prior_log_odds.constant_logreg <- function(logreg, data){
  n <- nrow(data$X)
  pi0 <- logreg$pi0
  logit <- rep(log((1 - pi0)/pi0), n)
  return(logit)
}

#' @param logreg logistic regression object
#' @param resp responsibilities, posterior assignment probability
#'     of each observation to each class
#' @param data a list at least containing covariates `X`
#' @export
update_prior.constant_logreg <- function(logreg, resps, data){
  pi1 <- mean(resps)
  logreg$pi0 <- 1 - pi1
  return(logreg)
}


#' @param logreg logistic regression object
#' @param resp responsibilities, posterior assignment probability
#'     of each observation to each class
#' @param data a list at least containing covariates `X`
#' @export
compute_elbo.constant_logreg <- function(logreg, qz, data){
  # E[log p(z | X, \theta)] - KL[q(theta) | p (theta)]
  # in this case theta is a point estimate so just compute
  # E[log p(z | pi)] where expectations are over q(z)
  n <- length(qz)
  prior_log_odds <- compute_prior_log_odds(logreg, data)
  log_pi1 <- log(sigmoid(prior_log_odds))
  log_pi0 <- log(sigmoid(-prior_log_odds))

  elbo <- sum((1 - qz) * log_pi0 + qz * log_pi1)
  return(elbo)
}


# Transformed susie------------

#' Make a susie object that predicts 0
init_null_susie <- function(n){
  X <- matrix(rep(1., n), nrow = n)
  y <- rnorm(n) * 1e-5
  susie_fit <- susieR::susie(X, y, L=1, scaled_prior_variance = 0, estimate_prior_variance = F)
  return(susie_fit)
}

#' Initialize linear SuSiE logistic regression object
#'
#' dots are captured in a list and passed as arguments to susieR::susie
#' @param n number of observations
initialize_linear_susie <- function(n, ...){
  susie_args = list(...)
  logreg <- list(
    susie = init_null_susie(n),
    susie_args = susie_args
  )
  class(logreg) <- 'linear_susie'
  return(logreg)
}

#' @param logreg logistic regression object
#' @param data a list at least containing covariates `X`
#' @export
compute_prior_log_odds.linear_susie <- function(logreg, data){
  # prior log odds are just the susie predictions
  prior_log_odds <- susieR::predict.susie(logreg$susie)
  return(prior_log_odds)
}

#' @param logreg logistic regression object
#' @param resp responsibilities, posterior assignment probability
#'     of each observation to each class
#' @param data a list at least containing covariates `X`
#' @export
update_prior.linear_susie <- function(logreg, qz, data){
  # transform probabilities to log-odds scale
  # clip so that we don't get Inf, -Inf
  qz_clipped <- pmin(pmax(qz, 1e-10), 1 - 1e-10)
  y <- logodds(qz_clipped)

  # update susie fit using new `y`
  args <- c(list(X = as.matrix(data$X), y = y), logreg$susie_args)
  logreg$susie <- rlang::exec(susieR::susie, !!!args)
  return(logreg)
}


#' Compute "ELBO"
#' @export
compute_elbo.linear_susie <- function(logreg, qz, data){
  n <- length(qz)
  prior_log_odds <- compute_prior_log_odds(logreg, data)
  log_pi1 <- log(sigmoid(prior_log_odds))
  log_pi0 <- log(sigmoid(-prior_log_odds))
  elbo <- sum((1 - qz) * log_pi0 + qz * log_pi1)
  elbo <- elbo + tail(logreg$susie$elbo, 1)
  return(elbo)
}


# Logistic GIBSS------------

#' @export
null_model <- function(n){
  model <- list(n=n)
  class(model) <- 'null_model'
  return(model)
}

#' @export
predict.null_model <- function(x, ...){
  return(rep(0, x$n))
}

#' Initialize linear SuSiE logistic regression object
#'
#' dots are captured in a list and passed as arguments to susieR::susie
#' @param n number of observations
initialize_logistic_ibss <- function(n, ...){
  susie_args = list(...)
  logreg <- list(
    logistic_ibss = null_model(n),
    susie_args = susie_args
  )
  class(logreg) <- 'logistic_ibss'
  return(logreg)
}

#' @export
compute_prior_log_odds.logistic_ibss <- function(logreg, data){
  # prior log odds are just the susie predictions
  prior_log_odds <- predict(logreg$logistic_ibss, data$X)
  return(prior_log_odds)
}

#' @param logreg logistic regression object
#' @param resp responsibilities, posterior assignment probability
#'     of each observation to each class
#' @param data a list at least containing covariates `X`
#' @export
update_prior.logistic_ibss <- function(logreg, qz, data){
  # update susie fit using new `y`
  args <- c(list(X = as.matrix(data$X), y = qz), logreg$susie_args)
  logreg$logistic_ibss <- rlang::exec(logisticsusie::generalized_ibss, !!!args)
  return(logreg)
}


#' Compute "ELBO"
#' @export
compute_elbo.logistic_ibss <- function(logreg, qz, data){
  n <- length(qz)
  prior_log_odds <- compute_prior_log_odds(logreg, data)
  log_pi1 <- log(sigmoid(prior_log_odds))
  log_pi0 <- log(sigmoid(-prior_log_odds))
  elbo <- sum((1 - qz) * log_pi0 + qz * log_pi1)
  elbo <- elbo #+ tail(logreg$susie$elbo, 1)
  return(elbo)
}


reg_log = function(X,y,threshold = 1e-10, max_iter = 100, intercept=TRUE)

{
  if (intercept){
    X <- cbind(rep(1,nrow(X)),X)
  }


  calc_p = function(X,beta)
  {
    beta = as.vector(beta)
    return(exp(X%*%beta) / (1+ exp(X%*%beta)))
  }
  beta = rep(0,ncol(X))

  diff = 10000

  iter_count = 0


  while(diff > threshold )
  {
    #calculate probabilities using current estimate of beta
    p = as.vector(calc_p(X,beta))
    W =  diag(p*(1-p))

    #calculate the change in beta
    beta_change = solve(t(X)%*%W%*%X) %*% t(X)%*%(y - p)

    #update beta
    beta = beta + beta_change

    diff = sum(beta_change^2)

    #see if we've hit the maximum number of iterations
    iter_count = iter_count + 1
    if(iter_count > max_iter) {
      stop("Not converging")
    }
  }

  names(beta)[1] <- "Intercept"
  out <- list(coef     = beta,
              fitted_p = calc_p(X,beta))# first coef is intercept
              return(out)
}

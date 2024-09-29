# Generics -----


### likelihood computation ----
#' Compute Data Log Likelihood
#' Compute p(betahat_i| z=k, se_i) i = 1..., n, k=1... K
#' @title Compute data likelihood
#' @description Compute data likelihood
#' @param fit an como fit object
#' @return n x K matrix of  log likelihood of each data point for each component
compute_data_loglikelihood <- function(fit,...){
  UseMethod("compute_data_loglikelihood")
}


#' @export
compute_data_loglikelihood.default <- function(fit, data){
print("hello")
  if (fit$prior=="mix_unif" ){
    print("yo")
    data_loglik <-   do.call( rbind,
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
  }else{
    data_loglik <- do.call(cbind,
                           purrr::map(
                             fit$f_list, ~ convolved_logpdf(.x,  data$betahat,  data$se)
                           )
    )
  }


  return(data_loglik)
}

###Convoled logpdf -----


#' Convolved logpdf
#'
#' Generic function for `component_distribution` objects, computes the loglikelihood of an observation from
#' the component distribution, corrupted by gaussian measurement error
#'
#' @param betahat observations
#' @param se standard errors of observations
#' @return the convolved log density log p(betahat | se) = log \int N(betahat | beta, se) p(beta) dbeta
#' @export
convolved_logpdf <- function(x, ...) {
  UseMethod("convolved_logpdf", x)
}

#' Update Params
#'
#' Generic function for updating the parameters of a `component_distribution`
#'
#' @param betahat observations
#' @param se standard errors of observations
#' @param weights weights to weigh each observation by when optimizing
#' @return a new `component_distribution` object with update parameters
#' @export
update_params <- function(x, ...) {
  UseMethod("update_params", x)
}

# Defaults ----

#' @export
convolved_logpdf.component_distribution <- function(dist, betahat, se) {
  # TODO: compute convolved pdf by numerical integration?
  # Will probably require logpdf/pdf be implemented for each component
  stop("Generic convolved logpdf not implimented yet")
}

#' @export
update_params.component_distribution <- function(dist, betahat, se) {
  # TODO: compute convolved pdf by numerical integration?
  # Will probably require logpdf/pdf be implemented for each component
  stop("Generic update not implimented")
}

# Point mass Component----

#' @export
point_component <- function(mu = 0) {
  f <- list(mu = mu)
  class(f) <- c("point", "component_distribution")
  return(f)
}

#' @export
is.point <- function(x) {
  inherits(x, "point")
}

#' Just a normal distribution with mean centered at the point mass
#' @export
convolved_logpdf.point <- function(dist, betahat, se) {
  # return(dnorm(betahat, sd=se, log=T))
  sd <- se
  logp <- dnorm(betahat, mean = dist$mu, sd = sd, log = TRUE)
  logp <- .clamp(logp, 1e4, -1e4)
  return(logp)
}

# Normal Component----

#' @export
normal_component <- function(mu = 0, var = 1) {
  f <- list(mu = mu, var = var)
  class(f) <- c("normal", "component_distribution")
  return(f)
}

#' @export
is.normal <- function(x) {
  inherits(x, "normal")
}

#' Normal distribution with variance given by sum of component dist. and noise
#' @export
convolved_logpdf.normal <- function(dist, betahat, se) {
  sd <- sqrt(se^2 + dist$var)
  logp <- dnorm(betahat, mean = dist$mu, sd = sd, log = TRUE)
  logp <- .clamp(logp, 1e4, -1e4)
  return(logp)
}

#' Note: only updates the variance parameter, these components are assumed
#' to be mean 0
#' @export
update_params.normal <- function(dist, betahat, se, weights) {
  # TODO: right now it's just a grid search, but we can impliment EM update easily
  var.init <- dist$var
  grid_var <- 2^(seq(-1, 1, by = 0.1) + log2(var.init))
  grid_dist <- purrr::map(grid_var, ~ normal_component(mu = dist$mu, var = .x))
  ll <- purrr::map_dbl(grid_dist, ~ sum(convolved_logpdf.normal(.x, betahat, se) * weights, na.rm = T))
  return(grid_dist[[which.max(ll)]])
}



# exponential Component----

exp_component <- function(mu = 0, scale = 1) {

  if( scale==0){
    f <- point_component(mu=mu)
    return(f)
  }
  f <- list(mu = mu, scale = scale)
  class(f) <- c("exp", "component_distribution")
  return(f)
}


#' Convolution between  an exponential and Normal distribution with variance given by sum of component dist. and noise
#' @export
convolved_logpdf.exp <- function(dist, betahat, se) {
  # essentially from the ebnm package
  s <- se
  scale= dist$scale
  a = 1/  scale
  mu <- dist$mu
  if(scale< 1e-6){
    return(dnorm(betahat, 0, se, log=T))
  } else{
    rate <- 1/scale
    return (log(rate) + 0.5*se^2 * rate^2 - betahat * rate + pnorm(betahat/se - se * rate, log.p=TRUE))
  }


  return(logp)
}


convolved_logpdf.unif <- function(dist, betahat, se) {
  a <- dist$min
  b <- dist$max
  s <- se

  # Define the CDF of the standard normal distribution
  normal_cdf <- function(x) {
    return(pnorm(x, mean = 0, sd = 1))
  }

  # Compute the marginal likelihood using the analytical solution
  marginal_likelihood <- (1 / (b - a)) * (normal_cdf((b - betahat) / s) - normal_cdf((a - betahat) / s))

  # Add a small constant to avoid log(0)
  logp <- log(marginal_likelihood + 1e-6)

  return(logp)
}

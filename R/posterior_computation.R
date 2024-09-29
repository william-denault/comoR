

#' Compute Posterior Assignment Probabilities
#' For each data point return posterior assignment probabilities
#' @param fit a como fit object
#' @return an n x K matrix of log posterior probabilities for each data point
compute_posterior_assignment <- function(fit, data, log = FALSE) {

  if (fit$prior == "mix_norm") {
    res <- compute_posterior_assignment_mix_norm(fit, data, log)
  }
  if(fit$prior== "mix_exp"){
    res <- compute_posterior_assignment_mix_exp(fit, data, log)
  }
  if(fit$prior== "mix_unif"){
    res <- compute_post_assignement_unif(fit, data)
  }
  return(res)
}


compute_posterior_assignment_mix_norm <-function(fit, data, log=FALSE){
  # TODO: generalize to other models
  data_loglik            <- fit$data_loglik
  assignment_loglik      <- compute_log_prior_assignment(fit$mnreg, data)
  assignment_loglik[, 1] <- assignment_loglik[, 1] + fit$nullweight
#browser()
  # normalize
  res <- do.call(
    rbind,
    apply(data_loglik + assignment_loglik, 1, function(x) x - logSumExp(x), simplify = F)
  )

  # exponentiate if log=FALSE
  if (!log) {
    res <- exp(res)
  }
  return(res)
}


temp_posterior_component_unif <- function(betahat, sigma, intervals, weights) {

  likelihood <- function(theta, y, sigma) {
    return((1 / (sqrt(2 * pi) * sigma)) * exp(-(y - theta)^2 / (2 * sigma^2)))
  }

  # Define the prior as a mixture of uniform components
  prior_mixture <- function(intervals, weights) {
    prior_vals <- weights / (intervals[, 2] - intervals[, 1])
    return(prior_vals)
  }
  n_components <- length(weights)
  posterior_probs <- numeric(n_components)
  likelihood_vals <- numeric(n_components)

  # Calculate the posterior probability for each component
  for (i in 1:n_components) {
    a <- intervals[i, 1]
    b <- intervals[i, 2]

    # Compute the likelihood for the current interval
    midpoint <- (a + b) / 2  # Use the midpoint of the interval as representative value for likelihood
    likelihood_vals[i] <- likelihood(midpoint, betahat, sigma)
  }
  if(length(which(is.na(likelihood_vals)))>0){
    likelihood_vals= 1e-8
  }
  # Get the prior probabilities for each component
  prior_probs <- prior_mixture(intervals, weights)

  # Calculate the unnormalized posterior for each component
  unnormalized_posteriors <- abs(likelihood_vals * prior_probs)
  unnormalized_posteriors= unnormalized_posteriors +1e-6
  # Normalize to get posterior probabilities
  posterior_probs <- unnormalized_posteriors / sum(unnormalized_posteriors)

  return(posterior_probs)
}

compute_post_assignement_unif= function(fit, data, log=FALSE){


  x <- data$betahat
  s <- data$se
  g= fit$g
  assignment  <- exp(compute_log_prior_assignment(fit$mnreg, data))
  intervals = do.call(rbind,  fit$g)
  assignment <- assignment / apply(assignment,1,sum)
  res <- do.call(rbind,   lapply( 1:length(x),function (i) {   temp_posterior_component_unif(betahat=x[i],
                                                                                             sigma=s[i],
                                                                                             intervals=intervals ,
                                                                                             weights=  assignment [i,]  )

  }

  ))

  if (log) {
    res = log(res)
  }

  return(res)
}



#posterior assignment for mix_exp single obs
#here x is a single observation and s  corresponding sd
#w is a vector of prior weights
# g is a gammamix object

wpost_exp <- function(x, s, w,g) {

  # assuming a[1 ]=0
  if (w[1] == 1) {

    return(c(1, rep(0,length(g$scale) - 1))  )
  }
  a <- 1/ g$scale[-1]

  lf <- dnorm(x, 0, s, log = TRUE)
  lg <- log(a) + s^2 * a^2 / 2 - a * x + pnorm(x / s - s * a, log.p = TRUE)



  log_prob = c(lf, lg)
  bmax = max(log_prob)
  log_prob = log_prob - bmax
  wpost <- w* exp( log_prob) / (sum(w *exp(log_prob)))

  #wpost <- w*c(exp(lf), exp( lg)) / (sum(w *c(exp(lf), exp( lg))))#underflow here





  return(wpost)
}

compute_posterior_assignment_mix_exp <- function (fit, data, log=FALSE){


  x <- data$betahat
  s <- data$se

  assignment  <- exp(compute_log_prior_assignment(fit$mnreg, data))

  assignment <- assignment / apply(assignment,1,sum)


  res <- do.call(rbind,
                  lapply(1:length(x),
                          function(i)
                            wpost_exp(x = x[i],
                                      s = s[i],
                                      w = assignment [i, ],
                                      g =  fit$g)
                             )
                 )
#apply(res,1,sum)
  if (log) {
    res = log(res)
  }

  return(res)

}



#' @title Compute individual posterior variance from marginal normal mean model
#' @description internal function to compute posterior mean and sds
t_ind_var.como <- function(fit, data, i) {

  do.call(
    c,
    lapply(
      1:length(fit$f_list),
      function(k) {
        1 / ((1 /  data$se[i]^2) + (1 / fit$f_list[[k]]$var))
      }
    )
  )
}


#' @title Compute individual posterior first and second moment
#' @description Compute individual posterior first and second moment
#'
#' # TODO: currently only for center prior
#' @param fit a como object
#' @param  t_ind_var output of t_ind_var (using same mocomo object!)
#' @param i individual of interest
#' @exemple
#' t_post_var <-   do.call(rbind,
#'                        lapply( 1:length(fit$data$betahat),
#'                                function(i)t_ind_var.como(fit, i)
#'                        )
#' )
#'
#'
#' post_beta <-     do.call(c, lapply( 1: length(fit$data$betahat), function(i)cal_ind_postmean(fit, t_post_var,i,) ))

cal_ind_moment12_mix_norm <- function(fit,data, t_post_var, i) {
  temp <- do.call(
    c,
    lapply(
      1:ncol(t_post_var),
      function(k) {
        (t_post_var[i, k] / (data$se[i]^2) )*
          (data$betahat[i])
      }
    )
  )

  ind_mean <- sum(fit$post_assignment[i, ] * temp)
  ind_second_moment <- sum(fit$post_assignment[i, ] * (t_post_var[i, ] + temp^2))

  ind_var <- ind_second_moment - ind_mean^2

  return(list(
    mean = ind_mean,
    sd = sqrt(ind_var)
  ))
}




#' @title Compute individual posterior mean and sd under como model
#' @description Compute individual posterior mean and sd under como model
#'
#' # TODO: allow using new observation from another data set (e.g. testing)
#' @param fit a como object
#' @export
#' @example
#' see \link{\code{fit.como}}
post_mean_sd.como <- function(fit,data) {


  if(fit$prior=="mix_norm"){
    out <- post_mean_sd_mix_norm(fit,data)
  }

  if(fit$prior=="mix_exp"){
    out <- post_mean_sd_mix_exp (fit,data)
  }

  if (fit$prior=="mix_unif" ){

    out <- post_mean_sd_mix_unif (fit,data)
  }

  return(out)
}



post_mean_sd_mix_norm <- function(fit,data) {
  t_post_var <-   do.call(rbind,
                         lapply( 1:length(data$betahat),
                                 function(i)t_ind_var.como(fit, data, i)
                         )
  )

  out <- do.call(rbind,
                 lapply( 1:length(data$betahat),
                         function(i)cal_ind_moment12_mix_norm(fit, data, t_post_var, i)
                 )
  )

  out <- data.frame(
    mean = do.call(c, out[, 1]),
    sd   = do.call(c, out[, 2])
  ) # could be skip for speed


  return(out)
}




post_mean_sd_mix_exp <- function(fit,data) {


  post_assignment <- compute_posterior_assignment_mix_exp (fit,
                                                           data,
                                                           log=FALSE)

  x  <- data$betahat
  s  <- data$se
  g  <- fit$g
  mu <- 0
  a  <- 1/ g$scale[-1]
  post <- list()
  post$mean  <- apply( post_assignment[,-1] *ashr:: my_etruncnorm(0,
                                                                  Inf,
                                                                  x - s^2 %*% t(a),
                                                                  s),
                       1,
                       sum)

  post$mean2 <- apply( post_assignment[,-1] *ashr:: my_e2truncnorm(0,
                                                                   Inf,
                                                                   x - s^2 %*% t(a),
                                                                   s),
                       1,
                       sum)
  post$mean2 <- pmax(post$mean2, post$mean^2)



  if (any(is.infinite(s))) {
    post$mean[is.infinite(s)]  <-  apply(
      post_assignment[is.infinite(s), -1] / a,
      1,
      sum)
    post$mean2[is.infinite(s)] <-  apply(
      2*post_assignment[is.infinite(s), -1] / a^2,
      1,
      sum)
  }
  post$sd <- sqrt(pmax(0, post$mean2 - post$mean^2))

  post$mean2 <- post$mean2 + mu^2 + 2 * mu * post$mean
  post$mean  <- post$mean + mu


  out <- data.frame(
    mean = post$mean,
    sd   = post$sd
  ) # could be skip for speed


  return(out)
}





post_mean_sd_mix_unif <- function(fit,data) {


  post_assignment <- compute_post_assignement_unif (fit,
                                                    data,
                                                    log=FALSE)

  x  <- data$betahat
  se  <- data$se
  g  <- fit$g
  intervals = do.call(rbind,g)
  post <- list()
  post_mean_mat=   do.call(rbind,
                           lapply(1:length(x), function(i){
                             do.call( c, lapply(1:nrow(intervals),
                                                function(j){
                                                  etruncnorm(a = g[[j]][1], b = g[[j]][2], mean = x[i] ,sd = se [i])
                                                }))
                           }


                           )
  )


  post_var_mat=   do.call(rbind,
                          lapply(1:length(x), function(i){
                            do.call( c, lapply(1:nrow(intervals),
                                               function(j){
                                                 vtruncnorm(a = g[[j]][1], b = g[[j]][2], mean = x[i] ,sd = se [i])
                                               }))
                          }


                          )
  )


  post$mean  <- apply( post_assignment  *post_mean_mat,
                       1,
                       sum)

  post$mean2 <- apply( post_assignment  *  post_var_mat,
                       1,
                       sum)



  post$sd <- sqrt(pmax(0, post$mean2 ))

  post$sd[which(is.na(post$sd))] =se[which(is.na(post$sd))]
  post$sd[which(is.infinite(post$sd) )] =se[which(is.infinite(post$sd))]

  out <- data.frame(
    mean = post$mean,
    sd   = post$sd
  ) # could be skip for speed


  return(out)
}



#' @title Compute individual fdr value  como model with centered normal mixture
#' @description Compute individual fdr value  como model with centered normal mixture
#'
#' @param fit a como object
#' @export
get_fdr.como <- function(fit,data) {

  if(fit$prior=="mix_exp"){
    stop("fdr not implemented for mix_exp")
  }

  tt1 <- fit$post_assignment[, 1] * dnorm( data$betahat, mean = 0, sd =  data$se)
  tt2 <- Reduce("+", lapply(
    2:ncol(fit$post_assignment),
    function(k) {
      fit$post_assignment[, k] * dnorm(data$betahat,
                                       mean = 0,
                                       sd = sqrt(data$se^2 + fit$f_list[[k]]$var)
      )
    }
  ))
  out <- tt1 / (tt1 + tt2)

  return(out)
}



param_unif_prior= list( c(0,0.1),
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

library(truncnorm)
#work here


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



posterior_unif <- function(  betahat, se, a, b) {
  library(truncnorm)
  postmean = etruncnorm(a = a, b = b, mean = betahat ,sd = se)
  postmean2 =vtruncnorm(a = a, b = b, mean = betahat ,sd = se) +postmean ^2
  return((p_y_given_theta * p_theta) / marginal_lik)
}


compute_post_assignement_unif =function(fit,data){


  x <- data$betahat
  s <- data$se

  assignment  <- exp(compute_log_prior_assignment(fit$mnreg, data))
  assignment <- assignment / apply(assignment,1,sum)

  intervals <- do.call(rbind,g)
  prior_mixture <- function(theta, intervals, weights) {
    prior_val <- 0
    for (i in 1:length(weights)) {
      a <- intervals[i, 1]
      b <- intervals[i, 2]
      prior_val <- prior_val + weights[i] * ifelse(theta >= a & theta <= b, 1 / (b - a), 0)
    }
    return(prior_val)
  }
  posterior <- function(theta, y, sigma, intervals, weights) {
    p_y_given_theta <- likelihood(theta, y, sigma)
    p_theta <- prior_mixture(theta, intervals, weights)
    return(p_y_given_theta * p_theta)
  }
  temp_post_assg=   function(y, sigma, intervals, weights, n_points = 1000) {
    theta_vals <- seq(min(intervals), max(intervals), length.out = n_points)
    post_vals <- sapply(theta_vals, posterior, y = y, sigma = sigma, intervals = intervals, weights = weights)

    # Normalize the posterior (since it's proportional to likelihood * prior)
    post_vals <- post_vals / sum(post_vals)

    return(data.frame(theta = theta_vals, posterior = post_vals))
  }
  res = temp_post_assg(y=betahat,
                       sigma=se,
                       intervals=intervals,
                       weights=assignment[i,])
  return(res)
}




g= list( c(0,0.1),
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
se=1
betahat=6
i=1# Compute the posterior probabilities for each component


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

  # Normalize to get posterior probabilities
  posterior_probs <- unnormalized_posteriors / sum(unnormalized_posteriors)

  return(posterior_probs)
}

compute_post_assignement_unif= function(fit, data, log=FALSE){

  intervals=fit$g
  x <- data$betahat
  s <- data$se

  assignment  <- exp(compute_log_prior_assignment(fit$mnreg, data))
  intervals = do.call(rbind, g)
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




post_mean_sd_mix_unif <- function(fit,data) {


  post_assignment <- compute_post_assignement_unif (fit,
                                                           data,
                                                           log=FALSE)

  x  <- data$betahat
  s  <- data$se
  g  <- fit$g
  post <- list()
  post_mean_mat=   do.call(rbind,
                           lapply(1:length(x), function(i){
                             do.call( c, lapply(1:nrow(intervals),
                                                function(j){
                               etruncnorm(a = g[[j]][1], b = g[[j]][2], mean = x[i] ,sd = s [i])
                             }))
                           }


                            )
                           )


  post_var_mat=   do.call(rbind,
                          lapply(1:length(x), function(i){
                            do.call( c, lapply(1:nrow(intervals),
                                               function(j){
                                                 vtruncnorm(a = g[[j]][1], b = g[[j]][2], mean = x[i] ,sd = s [i])
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



  post$sd <- sqrt(pmax(0, post$mean2 - post$mean^2))

  post$mean2 <- post$mean2 + mu^2 + 2 * mu * post$mean
  post$mean  <- post$mean + mu


  out <- data.frame(
    mean = post$mean,
    sd   = post$sd
  ) # could be skip for speed


  return(out)
}

g= list( c(0,0.1),
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
se=1
betahat=6
i=1
x= runif(100,1,7 )
x= rnorm(100, mean=-1, sd=3)
se= rep(1, 100)
s=se
assignment= matrix(1/length(g), ncol =length(g), nrow=100)
intervals <- do.call(rbind,g)
intervals = do.call(rbind, g)
assignment <- assignment / apply(assignment,1,sum)
res <- do.call(rbind,   lapply( 1:length(x),function (i) {   temp_posterior_component_unif(betahat=x[i],
                                                                                           sigma=s[i],
                                                                                           intervals=intervals ,
                                                                                           weights=  assignment [i,]  )

}

))
post_assignment=res


post <- list()
post_mean_mat=   do.call(rbind,
                         lapply(1:length(x), function(i){
                           do.call( c, lapply(1:nrow(intervals),
                                              function(j){
                                                etruncnorm(a = g[[j]][1], b = g[[j]][2], mean = x[i] ,sd = s [i])
                                              }))
                         }


                         )
)


post_var_mat=   do.call(rbind,
                        lapply(1:length(x), function(i){
                          do.call( c, lapply(1:nrow(intervals),
                                             function(j){
                                               vtruncnorm(a = g[[j]][1], b = g[[j]][2], mean = x[i] ,sd = s [i])
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
plot(post$mean,x)

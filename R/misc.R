# quick implementation of normal means problem where g is two component mixture
# with a point mass at zero and a zero-centered normal component
# we use this to test implementation of como2,
# which reduces to this model in the most simple case

simulate_point_normal_normal_means <- function(n, pi0, sd){
  z <- rbinom(n, 1, 1 - pi0)
  sehat <- rgamma(n, 1)
  betahat <- rnorm(n, sd = sehat)
  betahat[z == 1] <- betahat[z == 1] + rnorm(sum(z), sd=sd)
  sim <- list(
    betahat = betahat,
    sehat = sehat,
    z = z,
    pi0 = pi0,
    sd = sd
  )
  return(sim)
}

fit_point_normal_normal_means <- function(betahat, sehat, sd){
  f <- function(pi0){
    d0 <- dnorm(betahat, sd = sehat, log =T)
    d1 <- dnorm(betahat, sd = sqrt(sehat^2 + sd^2), log = T)
    ll <- sum(purrr::map2_dbl(d0, d1, ~ matrixStats::logSumExp(c(.x + log(pi0), .y + log(1-pi0)))))
    return(ll)
  }
  opt <- optimize(f, c(1e-10, 1-1e-10), maximum = T)
  return(opt)
}

em_point_normal_normal_means <- function(betahat, sehat, sd, pi0_init = 0.5, tol=1e-10){

  ll0 <- dnorm(betahat, mean=0, sd=sehat, log=T)
  ll1 <- dnorm(betahat, mean=0, sd = sqrt(sehat^2 + sd^2), log=T)

  compute_assignment <- function(pi0){
    prior_log_odds <- log(1-pi0) - log(pi0)
    posterior_log_odds <- (ll1 - ll0) + prior_log_odds
    return (1 / (1 + exp(-posterior_log_odds)))
  }

  # do EM until convergence
  diff <- tol + 1
  pi0 <- pi0_init
  while(diff > tol){
    pi0_new <- 1 - mean(compute_assignment(pi0))
    diff <- abs(pi0_new - pi0)
    pi0 <- pi0_new
  }

  q <- compute_assignment(pi0)
  Q <- sum((1 - q) * (ll0 + log(pi0)) + q * (ll1 + log(1 - pi0)))

  p <- 1 - q
  H <- -sum(q * log(q) + p * log(p), na.rm = T)


  res <- list(pi0 = pi0, Q = Q, H = H, ll = Q + H)
  return(res)
}



example_fit <- function(){
  sim <- logisticsusie::sim_ser()
  betahat <- rnorm(length(sim$y))
  betahat[sim$y==1] <- rnorm(sum(sim$y == 1), sd=5)
  se <- rep(1, length(sim$y))

  data <- prep_data_como2(betahat, se, sim$X, sim$Z)
  fit <- data_initialize_como(data, 5, scales = c(0, 1, 5, 10))
  fit <- fit_model(fit, data, max_iter = 10)

  assignments <- purrr::map_int(1:nrow(fit$post_assignment), ~which.max(fit$post_assignment[.x,]))
  table(sim$y, assignments)
  fit <- update_model(fit, data, fit_prior_variance=F)
  table(fit$logits > 0, sim$y)
}

example_fit2 <- function(){
  sim <- logisticsusie::sim_ser()
  betahat <- rnorm(length(sim$y))
  betahat[sim$y==1] <- rnorm(sum(sim$y == 1), sd=5)
  se <- rep(1, length(sim$y))

  data <- prep_data_como2(betahat, se, sim$X, sim$Z)

  como <- data_initialize_como(data, 2, scales = c(0, 10))
  como <- fit_model(como, data)
  plot(como$elbo)
  A <- compute_posterior_assignment(como, data, log=F)

  como2 <- data_initialize_como2(data, f1_params=list(mu=0, var = 10**2))
  como2 <- fit_model(como2, data)
  B <- compute_post_assignment(como2, data)
  plot(como2$elbo)

  exp(como$mnreg$logpi)[1]
  como2$logreg$pi0

  plot(B, A[,2])

  como2$logreg$pi0 <- exp(como$mnreg$logpi)[1]
  B <- compute_post_assignment(como2, data)

  compute_elbo(como2, data)

  como2 <- update_model(como2, data)


  compute_elbo(como2, data)
  como2 <- fit_model(como2, data, max_iter = 1000)

  assignments <- purrr::map_int(1:nrow(fit$post_assignment), ~which.max(fit$post_assignment[.x,]))
  table(sim$y, assignments)
  fit <- update_model(fit, data, fit_prior_variance=F)
  table(fit$logits > 0, sim$y)
}



example_fit2 <- function(){
  sim <- logisticsusie::sim_ser()
  betahat <- rnorm(length(sim$y))
  betahat[sim$y==1] <- rnorm(sum(sim$y == 1), sd=5)
  se <- rep(1, length(sim$y))

  f1 <- function(sd){
    ash_fit <- ashr::ash(betahat, se, mixcompdist='normal', mixsd = c(sd))
    list(loglik = ash_fit$loglik, pi0 = ash_fit$fitted_g$pi[1])
  }

  sds <- seq(1, 10, by=0.5)

  library(dplyr)
  res1 <- purrr::map_dfr(sds, f1) %>%
    mutate(sd = sds)
  with(res1, plot(sd, loglik))


  f2 <- function(sd){
    data <- prep_data_como2(betahat, se, sim$X, sim$Z)
    como <- data_initialize_como(data, 2, scales = c(0, sd))
    como <- fit_model(como, data)
    list(elbo = tail(como$elbo,1), pi0 = exp(como$mnreg$logpi)[1])
  }

  res2 <- purrr::map_dfr(sds, f2) %>% mutate(sd = sds)
  with(res2, plot(sd, elbo))

  res2 <- data.frame(do.call(rbind, res))
  plot(como$elbo)
  A <- compute_posterior_assignment(como, data, log=F)

  como2 <- data_initialize_como2(data, f1_params=list(mu=0, var = 10**2))
  como2 <- fit_model(como2, data)
  B <- compute_post_assignment(como2, data)
  plot(como2$elbo)

  exp(como$mnreg$logpi)[1]
  como2$logreg$pi0

  plot(B, A[,2])

  como2$logreg$pi0 <- exp(como$mnreg$logpi)[1]
  B <- compute_post_assignment(como2, data)

  compute_elbo(como2, data)

  como2 <- update_model(como2, data)


  compute_elbo(como2, data)
  como2 <- fit_model(como2, data, max_iter = 1000)

  assignments <- purrr::map_int(1:nrow(fit$post_assignment), ~which.max(fit$post_assignment[.x,]))
  table(sim$y, assignments)
  fit <- update_model(fit, data, fit_prior_variance=F)
  table(fit$logits > 0, sim$y)
}

example_fit_como <- function(){
  sim <- logisticsusie::sim_ser()
  betahat <- rnorm(length(sim$y))
  betahat[sim$y==1] <- rnorm(sum(sim$y == 1), sd=5)
  se <- rep(1, length(sim$y))

  data <- prep_data_como2(betahat, se, sim$X, sim$Z)
  fit <- data_initialize_como(data, 5, scales = c(0, 1, 5, 10))

  fit <- update_model(fit, data, fit_prior_variance=F)
  fit <- fit_model(fit, data)
  table(fit$logits > 0, sim$y)
}

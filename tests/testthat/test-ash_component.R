test_that("multiplication works", {
  betahat <-  rnorm(1000) * sqrt(rgamma(1000, 2))
  sebetahat<- rep(1, 1000)
  weights <- runif(1000)

  f1 <- ash_component()
  f1 <- update_params(f1, betahat, sebetahat, weights=rep(1, length(betahat)))

  fit0 <- ashr::ash(betahat, sebetahat, outputlevel=3)

  fit <- ashr::ash(betahat, sebetahat, weights = weights, mixcompdist='normal', optmethod='mixSQP', pointmass=F, outputlevel=3)
  lln <- log(fit$fit_details$matrix_lik) # normalized
  ll1 <- dnorm(betahat, 0, sd = sqrt(fit$fitted_g$sd[1]^2 + sebetahat^2), log=T)
  ll <- do.call(rbind, purrr::map(1:length(ll1), ~lln[.x,] + ll1[.x]))

  ll2 <- dnorm(betahat, 0, sd = sqrt(fit$fitted_g$sd[2]^2 + sebetahat^2), log=T)



  ll <- purrr::map2_dbl(betahat, sebetahat, ~ll_obs(.x, .y, fit$fitted_g$sd, fit$fitted_g$pi))
  sum(ll)
  lln[1, 1]
  ll1[1]
  ll <- lln[1, 1] - ll1

  lln[1, 2]
  ll2[1]
  plot(ll[,1] - ll1, ll[,2] - ll2); abline(0, 1)

  ashr::mixSQP(fit$fit_details$matrix_lik)
})
)

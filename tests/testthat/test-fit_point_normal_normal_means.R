f1 <- function(){
  set.seed(1)
  sim <- simulate_point_normal_normal_means(n=1000, pi0=0.9, sd=5)

  # two implementations of normal means
  a <- with(sim, fit_point_normal_normal_means(betahat, sehat, sd=5))
  b <- with(sim, em_point_normal_normal_means(betahat, sehat, sd=5))

  data <- with(sim, {
    n <- length(z)
    X <- matrix(rep(1, n), nrow = n)
    prep_data_como2(betahat, sehat, X, X)
  })
  como2 <- data_initialize_como2(data, f1_params=list(mu=0, var = 5**2))
  como2 <- fit_model(como2, data)

  expect_equal(a$objective, b$ll)
  expect_equal(tail(como2$elbo, 1), b$ll)
}

test_that("como2 with 'constant_logreg' is equivalent to point normal normal means", {
  f1()
})


como2_linear_susie <- function(){
  sim <- logisticsusie:::sim_ser()
  betahat <- rnorm(length(sim$y))
  betahat[sim$y==1] <- rnorm(sum(sim$y == 1), sd=2)
  se <- rep(1, length(sim$y))

  data <- prep_data_como2(betahat, se, sim$X, sim$Z)
  fit <- data_initialize_como2(data, f1_params = list(mu=0, var = 4),
                               logreg='linear_susie')
  fit <- fit_model(fit, data)
}


como2_linear_susie <- function(){
  sim <- logisticsusie::sim_ser()
  betahat <- rnorm(length(sim$y))
  betahat[sim$y==1] <- rnorm(sum(sim$y == 1), sd=2)
  se <- rep(1, length(sim$y))

  data <- prep_data_como2(betahat, se, sim$X, sim$Z)
  fit_init <- data_initialize_como2(data, f1_dist = 'ash', logreg='logistic_ibss')
  fit_fixedf1 <- fit_model(fit_init, data)
  plot(fit_fixedf1$elbo)

  # remember, the elbo computation for logreg is not meaningfull so.... fit$elbo not monotone
  fit <- fit_model(fit_fixedf1, data, estimate_f1=T)
  plot(tail(fit$elbo, 100))

  fit <- fit_model(fit, data, estimate_f1=T)
  fit <- fit_model(fit, data, estimate_f1=T)
  fit <- fit_model(fit, data, estimate_f1=T)

}

como2_linear_susie <- function(){
  sim <- logisticsusie:::sim_ser()
  gibss <- with(sim, logisticsusie:::generalized_ibss(X, y, L=5))

  betahat <- rnorm(length(sim$y))
  betahat[sim$y==1] <- rnorm(sum(sim$y == 1), sd=5)
  se <- rep(1, length(sim$y))

  data <- prep_data_como2(betahat, se, sim$X, sim$Z)
  fit <- data_initialize_como2(data, f1_params = list(mu=0, var = 25), logreg='logistic_ibss', logreg_params = list(L=1))
  fit <- fit_model(fit, data, max_iter = 10)
  plot(fit$elbo)
  fit$logreg$logistic_ibss$cs
}


william_example <- function(){
  set.seed(2)
  P=20
  N <- 5000
  beta0 <- 0
  beta1 <-0.41
  x1 <- rnorm(N)
  samp_prob <- 1/(1 +exp(-(beta0+beta1*x1)))

  mix <- c()
  obs <- c()
  se <- runif(N)

  for (i in 1:N){
    mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[i], samp_prob[i])))
    obs <- c(obs, ifelse( mix[i]==1, rnorm(1, sd=sqrt(2+se[i])),rnorm(1, sd=se[i]) ))
  }
  X <- cbind( x1, matrix(rnorm(P*N), ncol=P))

  boxplot(obs~mix)

  data <- prep_data_como2(obs, se, X, Z = matrix(rep(1, N), nrow=N))
  fit <- data_initialize_como2(data, logreg='logistic_ibss',logreg_params = list(L=5))

  fit <- update_model(fit, data, estimate_f1 = F)
  fit <- fit_model(fit, data, estimate_f1=T)

  res <- cFDR( betahat=obs,
               se=se,
               X = X,
               n_sim= 1000, nullweight = 2.3,
               outputlevel = 2 )

  res$cs

  #or via mococomo

}

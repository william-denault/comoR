# Generate X matrix ----
sim_X <- function(n = 1000, p = 50, length_scale = 50) {
  mix <- exp(-1 / length_scale * outer(seq(p), seq(p), "-")**2)
  X <- matrix(rnorm(n * p), nrow = n) %*% mix
  return(X)
}

sim_X_sparse <- function(n = 1000, p = 50, pi1 = 0.2, p_flip = 0.01) {
  X <- list()
  X[[1]] <- rbinom(n, 1, pi1)
  for (i in seq(2, p)) {
    x <- X[[i - 1]]
    on <- which(x == 1)
    off <- which(x == 0)

    flip <- rbinom(1, min(length(on), length(off)), p_flip)
    if (flip > 0) {
      x[sample(on, flip)] <- 0
      x[sample(off, flip)] <- 1
    }
    X[[i]] <- x
  }
  X <- Matrix::Matrix(do.call(cbind, X), sparse = T)
  return(X)
}



# Generate effects ----
sim_b_constant <- function(b = 1, p = 50, L = 3) {
  idx <- seq(L) * 10
  beta <- rep(b, L)
  return(list(beta = beta, idx = idx))
}

sim_y_ser <- function(X, beta0, beta, idx = NULL, N = 1) {
  n <- dim(X)[1]
  p <- dim(X)[2]

  if (is.null(idx)) {
    idx <- sample(p, 1)
  }

  logits <- beta0 + beta * X[, idx]
  p <- sigmoid(logits)
  y <- rbinom(n, N, p)

  data <- list(y = y, logits = logits, N = N, beta = beta, beta0 = beta0, idx = idx)
  return(data)
}

sim_y_susie <- function(X, beta0, beta, re_var = 0, idx = NULL, N = 1) {
  n <- dim(X)[1]
  p <- dim(X)[2]

  if (length(beta) >= p) {
    stop("length(beta) must be less than number of columns of X")
  }
  if (is.null(idx)) {
    idx <- sample(p, length(beta))
  }

  logits <- beta0 + rowSums(beta * X[, idx, drop = F]) + (rnorm(n) * sqrt(re_var))
  p <- sigmoid(logits)
  y <- rbinom(n, N, p)
  data <- list(y = y, logits = logits, N = N, beta = beta, beta0 = beta0, idx = idx)
  return(data)
}

sim_y_null <- function(X, beta0, re_var = 0, N = 1) {
  n <- dim(X)[1]
  p <- dim(X)[2]

  logits <- beta0 + (rnorm(n) * sqrt(re_var))
  p <- sigmoid(logits)
  y <- rbinom(n, N, p)
  data <- list(y = y, logits = logits, N = N, beta0 = beta0, idx = list())
  return(data)
}

#' @export
sim_ser <- function(n = 1000,
                    p = 50,
                    N = 1,
                    idx = 1,
                    beta = 1,
                    beta0 = -2,
                    fsimX = sim_X,
                    fsimX_control = list()) {
  X <- rlang::exec(fsimX, n = n, p = p, !!!fsimX_control)
  Z <- matrix(rep(1, n), nrow = n)

  logits <- beta0 + beta * X[, idx]
  p <- sigmoid(logits)
  y <- rbinom(n, N, p)

  data <- list(
    X = X, Z = Z, y = y, N = N, logits = logits
  )
  return(data)
}

#' simulate SER with 3 covariates
#' @export
sim_ser_with_covariates <- function(n = 1000, p = 50, N = 1, idx = 1) {
  X <- sim_X(n, p)
  Z <- matrix(rnorm(n * 3), nrow = n)

  p <- sigmoid(X[, idx] + rowSums(Z))
  y <- rbinom(n, N, p)

  data <- list(
    X = X, Z = Z, y = y, N = N
  )
  return(data)
}


sim_ser_with_random_effects <- function(n = 1000,
                                        p = 50,
                                        N = 1,
                                        idx = 1,
                                        beta = 1,
                                        beta0 = -2,
                                        re_var = 1,
                                        fsimX = sim_X,
                                        fsimX_control = list()) {
  X <- rlang::exec(fsimX, n = n, p = p, !!!fsimX_control)
  Z <- matrix(rep(1, n), nrow = n)

  logits <- beta0 + beta * X[, idx] + rnorm(n, sd = sqrt(re_var))
  p <- sigmoid(logits)
  y <- rbinom(n, N, p)

  data <- list(
    X = X, Z = Z, y = y, N = N, logits = logits
  )
  return(data)
}


#' @title  Simulate data under the logistic SuSiE model
#' @description  Simulate data under the logistic SuSiE model
#' @param n numeric sample size
#' @param p numeric number of observed covariates
#' @param L numeric number of causal covariates (should be lower than p)
#' @param N numeric number of draw in the bionmial considered (set to 1 as default)
#' @param beta numeric if of length 1 assume that ervery effect have same beta coefficietns
#' otherwise should be  of length L, if missing beta <- seq(L) * .2
#' @param alpha numeric intercept in the lostic regression (control sparsity)
#' @export

sim_susie <- function(n = 1000, p = 50, L = 3, N = 1, beta, alpha = 0, idx = NULL, length_scale = 50) {
  X <- sim_X(n, p, length_scale = length_scale)
  Z <- matrix(rep(1, n), nrow = n)
  if (missing(beta)) {
    beta <- seq(L) * .2
  }
  if (length(beta) == 1) {
    beta <- rep(beta, L)
  }

  if (is.null(idx)) {
    idx <- seq(L) * 10
  }
  logits <- alpha + Matrix::drop(X[, idx] %*% beta)
  p <- sigmoid(logits)
  y <- rbinom(n, N, p)

  data <- list(
    X = X,
    Z = Z,
    y = y,
    N = N,
    logits = logits,
    effect = beta,
    intercept = alpha,
    beta = beta,
    idx = idx
  )
  return(data)
}


#' @title  Simulate data under the twococomo SuSiE model
#' @description  Simulate data under the twococomo SuSiE model
#' @param n numeric sample size
#' @param p numeric number of observed covariates
#' @param L numeric number of causal covariates (should be lower than p)
#' @param N numeric number of draw in the bionmial considered (set to 1 as default)
#' @param beta numeric if of length 1 assume that ervery effect have same beta coefficietns
#' otherwise should be  of length L, if missing beta <- seq(L) * .2
#' @export
sim_twococomo <- function(n = 1000, p = 50, L = 3, N = 1, beta ) {
  if (missing(beta)) {
    sim <- sim_susie(n=n, p=p, L=L, N=N )
  } else {
    sim <- sim_susie(n=n, p=p, L=L, N=N , beta=beta  )
  }

  sim$beta <- rnorm(n) * sim$y
  sim$se <- 0.1 + rgamma(n, shape = 0.5)
  sim$betahat <- sim$beta + rnorm(n) * sim$se

  class(sim) <- c("normal", "data_como")
  return(sim)
}



sim_susie_sparse <- function(n = 1000, p = 50, L = 3, N = 1, pi1 = 0.2, transition = 0.8) {
  X <- sim_X_sparse(n, p, pi1, transition)
  Z <- matrix(rep(1, n), nrow = n)

  beta <- 3.
  logits <- -2 + beta * Matrix::rowSums(X[, 1:L])
  p <- sigmoid(logits)
  y <- rbinom(n, N, p)

  data <- list(
    X = X, Z = Z, y = y, N = N, logits = logits
  )
  return(data)
}

sim_twococomo_sparse <- function(n = 1000, p = 50, L = 3, N = 1) {
  sim <- sim_susie_sparse(n, p, L, N)
  sim$beta <- 10 * rnorm(n) * sim$y
  sim$se <- 0.1 + rgamma(n, shape = 0.5)
  sim$betahat <- sim$beta + rnorm(n) * sim$se

  class(sim) <- c("normal", "data_como")
  return(sim)
}


sim_mn_susie <- function(n = 1000, p = 50, L = 3, N = 1, K = 10) {
  X <- sim_X(n = n, p = p)
  Z <- matrix(rep(1, n), nrow = n)

  Beta <- matrix(rnorm(p * K), nrow = p) / 10
  logits <- X %*% Beta

  Y <- t(do.call(cbind, purrr::map(seq(n), ~ rmultinom(1, 50, softmax(logits[.x, ])))))

  data <- list(
    X = X, Z = Z, y = Y, logits = logits
  )
  return(data)
}

#' @export
sim_como <- function(n = 1000, p = 50, L = 3, N = 1, beta, alpha = 0) {
  if (missing(beta)) {
    sim <- sim_susie(n, p, L, N, alpha = alpha)
  } else {
    sim <- sim_susie(n, p, L, N, beta, alpha = alpha)
  }
  sim$scales <- cumprod(c(1, rep(sqrt(2), 5)))
  sim$beta <- rnorm(n) * sim$y * sample(sim$scales, size = n, replace = T)
  sim$se <- 0.1 + rgamma(n, shape = 0.5)
  sim$betahat <- sim$beta + rnorm(n) * sim$se
  class(sim) <- c("normal", "data_como")
  return(sim)
}

#' @title Simulate data under como model with beta distribution
#' @details Simulate data under como model with beta distribution
#' @export


sim_como_beta <- function(n = 1000, p = 50, L = 3, N = 1, beta) {
  if (missing(beta)) {
    sim <- sim_como(n = n, p = p, L = L, N = N)
  } else {
    sim <- sim_como(n = n, p = p, L = L, N = N, beta = beta)

  }

  sim$p <- 1 - pnorm(sim$betahat)
  return(sim)
}

#' @export
sim_ordinal_mod <- function(n = 1000,
                            p = 30,
                            p_act = 1,
                            se = 1,
                            n_class = 5,
                            beta_size = 1, # 1 moderatly informative , 1.5 strongly infrmative
                            alpha_start = 3.5, # 3.5, 2.5, 1.5 as start corresponds to sparse medium and dense signals
                            grid_s,
                            max_grid = 2, # represent signal power the larger the clearer signals
                            full_res = TRUE) {
  if (missing(grid_s)) {
    grid_s <- seq(0, max_grid, length.out = n_class)
  }
  if (missing(se)) {
    se <- runif(n)
  }
  if (length(se) == 1) {
    se <- rep(se, n)
  }


  beta <- sample(c(-beta_size, beta_size), size = p_act, replace = TRUE)



  alpha <- alpha_start + seq(0, 3, length.out = n_class)
  # res_summary= length(which(cebmn_fdr<0.05))/n
  # length(which(camt_lfdr<0.05))/n
  # length(which(ihw_fdr<0.05))/n

  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  # simulation under ordinal model
  if (length(beta) == 1) {
    pred <- sigmoid(matrix(alpha,
      byrow = TRUE,
      nrow = n,
      ncol = n_class
    ) + matrix(rep(X[, c(1:p_act)] * (beta), n_class),
      ncol = n_class
    ))
  } else {
    pred <- sigmoid(matrix(alpha,
      byrow = TRUE,
      nrow = n,
      ncol = n_class
    ) + matrix(rep(X[, c(1:p_act)] %*% (beta), n_class),
      ncol = n_class
    ))
  }



  ind_prob <- matrix(NA, ncol = n_class, nrow = n)
  for (i in 1:nrow(ind_prob)) {
    for (j in 1:ncol(ind_prob)) {
      if (j == 1) {
        ind_prob[i, j] <- pred[i, j]
      } else {
        ind_prob[i, j] <- pred[i, j] - pred[i, (j - 1)]
      }
    }
  }
  ind_prob <- ind_prob / apply(ind_prob, 1, sum)

  obs <- rep(NA, n)
  true_obs <- rep(NA, n)
  class <- rep(NA, n)
  for (i in 1:n)
  {
    class[i] <- sample(size = 1, 1:n_class, prob = ind_prob[i, ])
    true_obs[i] <- ifelse(class[i] == 1, 0, rnorm(1, 0, grid_s[class[i]]))
    obs[i] <- true_obs[i] + rnorm(1, sd = se[i])
  }
  out <- list(
    true_obs = true_obs,
    obs = obs,
    X = X,
    se = se,
    class = class,
    sim_param = c(n, p, p_act, se, n_class)
  )
  return(out)
}


#' @title  Simulate mixture of beta distribution
#' @description  Simulate mixture of beta distribution
#' @param n numeric sample size
#' @param par list of the component parameter
#' @param prob mixture proportion
#' @export

sim_mixture_beta <- function(n = 1000, par = list(alpha = c(1, 1), beta = c(1, 10)), prob = c(0.9, 0.1)) {
  temp <- c()
  for (i in 1:n) {
    tt <- sample(c(1, 2), prob = prob, size = 1)
    temp <- c(temp, rbeta(1,
      shape1 = par$alpha[tt],
      shape2 = par$beta[tt]
    ))
  }
  return(temp)
}




sim_cfactor <- function(N=2000, # number of row
                        L=100, #number of columns
                        K=2, #number of factor
                        P1=20 , # number of cov for row /loadings
                        P2=20, # number of cov for col /factors
                        beta0=-2,
                        beta1=2,
                        noise_level= 3 ){
  library(softImpute)
  library(susieR)
  library(mvtnorm)
  data(N3finemapping)
  attach(N3finemapping)
  library(comoR)


  L_l <-  sample (1:20,size=1)
  L_f <-  sample (1:20,size=1)





  X_l <-   rmvnorm(N,sigma=cov(N3finemapping$X[1:100,1:P1]))
  X_f <-    rmvnorm(L,sigma=cov(N3finemapping$X[1:100,1:P2]))


  true_pos_l <- sample( 1:P1, size=L_l, replace=FALSE)
  true_pos_f <- sample( 1:P2, size=L_f, replace=FALSE)




  true_l  <- list()
  true_f  <- list()

  for( k in 1:K){



    samp_prob <- 1/(1 +exp(-(beta0+beta1*X_l[,k])))
    lk <- c()
    mix <- c()
    for ( i in 1:N){
      mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[i], samp_prob[i])))
      lk <- c(lk, mix[i]*rnorm(1,sd=3))
    }


    samp_prob <- 1/(1 +exp(-(beta0+beta1*X_f[,k])))
    fk <- c()
    mix <- c()
    for ( j in 1:L){

      mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[j], samp_prob[j])))
      fk <- c(fk, mix[j]*rnorm(1,sd=3))
    }

    true_l[[k]] <- lk
    true_f[[k]] <- fk

  }


  Y_true <- Reduce("+", lapply( 1:K, function(k) true_l[[k]]%*%t(true_f[[k]])
  )
  )

  Y_obs <- Y_true+ matrix( rnorm(N*L, sd= noise_level), ncol=L)
  out <- list(Y_true  =  Y_true,
              Y_obs   =Y_obs,
              fm_loadings=  do.call(rbind,true_l ),
              fm_factor= do.call(rbind,true_f),
              X_l = X_l,
              X_f= X_f)
  return(out)
}


#### Simulation cEBMF----
#'@export
sim_func_cEBMF <- function( N=2000, # number of row
                            L=100, #number of columns
                            K=2, #number of factor
                            P1=10 , # number of cov for row /loadings
                            P2=10, # number of cov for col /factors
                            beta0=0,
                            beta1=3,
                            noise_level= 3,
                            max_iter_cEBMF=10,
                            max_iter_como=10,
                            max_class=10,
                            seed,
                            epoch=50
){


  library(softImpute)
  library(susieR)
  library(mvtnorm)
  data(N3finemapping)
  attach(N3finemapping)
  library(comoR)
  library(tensorflow)
  library(keras)
    if (missing(seed)){
       set.seed(rpois(1, 10))
      }else{
       set.seed(o)
      }









  X_l <-   rmvnorm(N,sigma=cov(N3finemapping$X[1:100,1:P1]))
  X_f <-    rmvnorm(L,sigma=cov(N3finemapping$X[1:100,1:P2]))


  true_pos_l <- 1:10
  true_pos_f <-  1:10




  true_l  <- list()
  true_f  <- list()

  for( k in 1:K){



    samp_prob <- 1/(1 +exp(-(beta0+beta1*X_l[,k])))
    lk <- c()
    mix <- c()
    for ( i in 1:N){
      mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[i], samp_prob[i])))
      lk <- c(lk, mix[i]*rnorm(1,sd=1))
    }


    samp_prob <- 1/(1 +exp(-(beta0+beta1*X_f[,k])))
    fk <- c()
    mix <- c()
    for ( j in 1:L){

      mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[j], samp_prob[j])))
      fk <- c(fk, mix[j]*rnorm(1,sd=1))
    }

    true_l[[k]] <- lk
    true_f[[k]] <- fk

  }


  Y_true <- Reduce("+", lapply( 1:K, function(k) true_l[[k]]%*%t(true_f[[k]])
  )
  )

  Y_obs <- Y_true+ matrix( rnorm(N*L, sd= noise_level), ncol=L)

  library(nnet)

  l2_reg = 0.001
  param_nnet.x =keras_model_sequential() %>%
    layer_dense(units = 64,
                activation = 'relu',
                input_shape = c(ncol(X_l))) %>%
    layer_dense(units = 64,
                activation = 'relu',
                kernel_regularizer = regularizer_l2(l2_reg)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 64,
                activation = 'relu',
                kernel_regularizer = regularizer_l2(l2_reg)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 64,
                activation = 'relu' ) %>%
    layer_dense(units = 10,
                activation = 'softmax')


  param_nnet.y =keras_model_sequential() %>%
    layer_dense(units = 64,
                activation = 'relu',
                input_shape = c(ncol(X_f))) %>%
    layer_dense(units = 10,
                activation = 'softmax')

  library(softImpute)
  res_nnet <-cEBMF  ( Y=Y_obs,
                      X_l,
                      X_f,

                      K=2,
                      type_noise='constant',
                      init_type="flashier",
                      maxit=10,
                      tol=1e-3 ,
                      mnreg_type.x="keras",
                      mnreg_type.y="keras",
                      param_como.x  = list(max_class=10,mnreg_type="keras",
                                           prior="mix_norm" ,
                                           epoch     =50,
                                           batch_size= 500),
                      param_como.y  = list(max_class=10,mnreg_type="keras",
                                           prior="mix_norm" ,
                                           epoch     =20,
                                           batch_size= 100),
                      param_nnet.x  =param_nnet.x ,
                      param_nnet.y  =param_nnet.y,

                      maxit_como  = 2)



  cebnm_L <- function( x,s,g_init=FALSE,fix_g=TRUE, output){

    if (length(x) == 3){ ### just to satisfy check of custom function
      return (ebnm_flat(x))
    }
    Z <- matrix( 1, nrow=length(x), ncol=1)
    param_como = list(max_class= 10,
                      mnreg_type="keras",
                      epoch = epoch)
    data <- comoR:::como_prep_data (betahat=x,
                                    se=s, X=X_l,
                                    Z =Z )

    # you need to retreive the actual number of mixture component in the model
    num_classes <- length( autoselect_scales_mix_norm(data$betahat, data$se,10))

    #define the nnet paramet using Keras syntax
    param_nnet =keras_model_sequential() %>%
      layer_dense(units = 10,
                  activation = 'relu',
                  input_shape = c(ncol(X_l))) %>%
      layer_dense(units = num_classes,
                  activation = 'softmax')

    # run comoR
    fit  <- rlang::exec( "data_initialize_como", !!! param_como ,
                         data= data,
                         param_nnet= param_nnet) # initialize the model from the data
    fit <- comoR:::fit.como (  fit, data, max_iter = max_iter_cEBMF)


    est <- comoR:::post_mean_sd (fit,data)



    g <- ashr::normalmix(rep(1/length(fit$f_list),length(fit$f_list)),
                         rep( 0, length(fit$f_list)),
                         do.call(c, lapply( 1: length(fit$f_list) ,
                                            function(k) {sqrt(fit$f_list [[k]]$var) } )
                         )
    )

    out <- list( data= data.frame(x=data$betahat,
                                  s=data$se),
                 posterior = data.frame(mean= est$mean,
                                        second_moment=(est$sd^2+est$mean^2)
                 ) ,
                 fitted_g = g,
                 log_likelihood=sum( comoR:::compute_data_loglikelihood(fit, data) * (fit$post_assignment))

    )

    return( out)

  }
  cebnm_F <- function( x,s,g_init,fix_g=TRUE, output){
    if (length(x) == 3){ ### just to satisfy check of custom function
      return (ebnm_flat(x))
    }
    Z <- matrix( 1, nrow=length(x), ncol=1)
    param_como = list(max_class= 10,
                      mnreg_type="keras",
                      epoch = epoch)
    data <- comoR:::como_prep_data (betahat=x,
                                    se=s, X=X_f,
                                    Z =Z )

    # you need to retreive the actual number of mixture component in the model
    num_classes <- length( autoselect_scales_mix_norm(data$betahat, data$se,10))

    #define the nnet paramet using Keras syntax
    param_nnet =keras_model_sequential() %>%
      layer_dense(units = 10,
                  activation = 'relu',
                  input_shape = c(ncol(X_f))) %>%
      layer_dense(units = num_classes,
                  activation = 'softmax')

    # run comoR
    fit  <- rlang::exec( "data_initialize_como", !!! param_como ,
                         data= data,
                         param_nnet= param_nnet) # initialize the model from the data
    fit <- comoR:::fit.como (  fit, data, max_iter = max_iter_cEBMF)


    est <- comoR:::post_mean_sd (fit,data)



    g <- ashr::normalmix(rep(1/length(fit$f_list),length(fit$f_list)),
                         rep( 0, length(fit$f_list)),
                         do.call(c, lapply( 1: length(fit$f_list) ,
                                            function(k) {sqrt(fit$f_list [[k]]$var) } )
                         )
    )

    out <- list( data= data.frame(x=data$betahat,
                                  s=data$se),
                 posterior = data.frame(mean= est$mean,
                                        second_moment=(est$sd^2+est$mean^2)
                 ) ,
                 fitted_g = g,
                 log_likelihood=sum( comoR:::compute_data_loglikelihood(fit, data) * (fit$post_assignment))

    )

    return( out)

  }

  library(flashier)

  f <- flashier::flash(Y_obs)
  fit_custom <- flash_init(Y_obs, var_type = 2) %>%
    flash_set_verbose(0) %>%
    flash_greedy(
      Kmax = K,
      ebnm_fn = c(cebnm_L, cebnm_F)
    )

  f <- flashier::flash(Y_obs)


  library(irlba)
  library(PMA)
  ssvd_res = ssvd(Y_obs, k=3)
  svd_res  = svd(Y_obs)

  rmse = function(mat1 , mat2){

    squared_diff <- (mat1-  mat2)^2

    # Compute mean of squared differences
    mean_squared_diff <- mean(squared_diff)

    # Compute RMSE
    rmse <- sqrt(mean_squared_diff)
    return (rmse)
  }




  cv.out <- PMD.cv(Y_obs, type="standard", sumabss=seq(0.1, 0.6, len=20))
  PMD_res <- PMD(Y_obs,
                 type="standard",
                 sumabs=cv.out$bestsumabs,
                 K=3, v=cv.out$v.init
  )



  Y_est_nnet <- Reduce("+", lapply( 1:res_nnet$K, function(k) res_nnet $loading[,k] %*%t(res_nnet $factor[,k] ) ))


  rmse_cEBMF_nnet0   <-  rmse(Y_true , Y_est_nnet )
  rmse_cEBMF_nnet   <-  rmse(Y_true ,fitted(fit_custom))
  rmse_flash        <-  rmse(Y_true ,fitted(f))

  rmse_PMD         <- rmse(Y_true, PMD_res$u%*%diag(PMD_res$d)%*%t(PMD_res$v))
  rmse_PMD0         <- rmse(Y_true, PMD_res0$u%*%diag(PMD_res0$d)%*%t(PMD_res0$v))

  rmse_svd         <- rmse(Y_true, svd_res$u%*%diag(svd_res$d)%*%t(svd_res$v))
  rmse_ssvd        <- rmse(Y_true, ssvd_res$u%*%ssvd_res$d%*%t(ssvd_res$v))
  rmse_out         <- c( rmse_cEBMF_nnet0,
                         rmse_cEBMF_nnet ,
                         rmse_flash  ,
                         rmse_PMD,
                         rmse_svd, rmse_ssvd)
  names( rmse_out  ) <- c("cEBMF0",
                          "cEBMF",
                          "EBMF",
                          "PMD", # "SVD",
                          "SVD", # "SSVD",
                          "SSVD")# "PMD" )

  tot_sum_square <-   sum(Y_true^2)
  par <-  c( N,
             L ,
             K ,
             P1 ,
             P2 ,
             beta0 ,
             beta1 ,
             noise_level ,
             max_iter_cEBMF ,
             max_iter_como
  )
 names( par)  <-  c( "N",
                     "L"  ,
                     "K" ,
                     "P1"  ,
                     "P2" ,
                     "beta0"  ,
                     "beta1" ,
                     "noise_level"  ,
                     "max_iter_cEBMF" ,
                     "max_iter_como"
 )

  out <- list(rmse      =  rmse_out ,
              par       = par,
              tot_sum_square =tot_sum_square
               )


  return( out)
}




#'@export
c_ash_sim <- function( N=1000,
                       beta0=-2,
                       beta1=4,
                       var_cov_effect=3,
                       effect_var=3,
                       noise_level=1,
                       nullweight=.1,
                       P=20,
                       se_type="constant",
                       df_se=2,
                       max_iter=5,
                       dist="normal",
                       extended=FALSE

){
  L <- sample(1:5, size=1)
  library(susieR)
  library(mvtnorm)
  data(N3finemapping)
  attach(N3finemapping)
  x1 <- rmvnorm(N,sigma=cov(N3finemapping$X[1:100,]))

  true_pos <- sample( 1:ncol(x1), L)
  lin_pred <- rep(0,N)


  for ( l in 1:L){
    lin_pred <- lin_pred+beta1*x1 [ ,true_pos[[l]]]
  }

  lin_pred <- lin_pred+beta0

  samp_prob <- 1/(1 +exp(-(lin_pred)))
  betahat <- c()
  betatrue <- c()
  mix <- c()






  if( dist=="spiky"){

    samp_fun <- function(){
      id <- sample(size=1, 1:4)
      if(id==1){
        out <-  rnorm( 1, sd=sqrt(2)*0.25)
      }
      if(id==2){
        out <-  rnorm( 1, sd=sqrt(2)*0.5)
      }
      if(id==3){
        out <-  rnorm( 1, sd=sqrt(2)*1)
      }
      if(id==4){
        out <-  rnorm( 1, sd=sqrt(2)*2)
      }
      return( out)
    }
    for ( i in 1:N){
      mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[i], samp_prob[i])))
      betatrue <- c( betatrue, mix[i] *samp_fun())
      betahat <- c( betahat ,   betatrue[i]+rnorm(1,sd=noise_level ) )
    }
    if(se_type=="constant"){
      se <- rep(1, length(betahat))

    }
    if( se_type=="random"){
      se <-   rchisq(N, df=df_se)
    }


    tt1 <-  ( 1- samp_prob)  *dnorm(betahat, sd=noise_level)
    tt2 <- list( )
    for ( i in 1: length(betahat)){
      tt2[[i]]<-   (   samp_prob[i])*0.25*

        dnorm(  betahat[i] ,
                mean = 0,sd= sqrt(noise_level^2+ c(0.25,0.5,1,2)^2))
    }

    tt2 <- as.matrix(do.call(rbind,tt2) )
    true_lfdr <-  tt1/ ( tt1+apply(tt2,1, sum))

  }

  if( dist=="near_normal"){
    samp_fun <- function(){
      id <- sample(size=1, 1:2, prob=c(2/3,1/3))
      if(id==1){
        out <-  rnorm( 1, sd=sqrt(2)*1)
      }
      if(id==2){
        out <-  rnorm( 1, sd=sqrt(2)*2)
      }

      return( out)
    }

    for ( i in 1:N){
      mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[i], samp_prob[i])))
      betatrue <- c( betatrue, mix[i] *samp_fun())
      betahat <- c( betahat ,   betatrue[i]+rnorm(1,sd=noise_level ) )
    }
    if(se_type=="constant"){
      se <- rep(1, length(betahat))

    }
    if( se_type=="random"){
      se <-   rchisq(N, df=df_se)
    }
    tt1 <-  ( 1- samp_prob)  *dnorm(betahat, sd=noise_level)
    tt2 <- list( )
    for ( i in 1: length(betahat)){
      tt2[[i]]<-   (  samp_prob[i])*( (2/3)* dnorm(  betahat[i] ,
                                                     mean = 0,sd= sqrt(noise_level^2+ 1^2))+
                                        (1/3)* dnorm(  betahat[i] ,
                                                       mean = 0,sd= sqrt(noise_level^2+ 2^2)))


    }

    tt2 <- as.matrix(do.call(rbind,tt2) )
    true_lfdr <-  tt1/ ( tt1+apply(tt2,1, sum))

  }

  if( dist=="normal"){
    for ( i in 1:N){
      mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[i], samp_prob[i])))
      betatrue <- c( betatrue, mix[i] *rnorm(1,sd=sqrt(2)*1))
      betahat <- c( betahat ,   betatrue[i]+rnorm(1,sd=noise_level ) )
    }
    if(se_type=="constant"){
      se <- rep(1, length(betahat))

    }
    if( se_type=="random"){
      se <-   rchisq(N, df=df_se)
    }

    tt1 <-  ( 1- samp_prob)  *dnorm(betahat, sd=noise_level)
    tt2 <- list( )
    for ( i in 1: length(betahat)){
      tt2[[i]]<-   (  samp_prob[i])*(   dnorm(  betahat[i] ,
                                                mean = 0,sd= sqrt(noise_level^2+ 1^2))
      )


    }

    tt2 <-  (do.call(c,tt2) )
    true_lfdr <-  tt1/ ( tt1+tt2)
  }

  if( dist=="flattop"){

    samp_fun <- function(){
      id <- sample(size=1, 1:7 )
      if(id==1){
        out <-  rnorm( 1,mean=-1.5, sd=5)
      }
      if(id==2){
        out <-  rnorm( 1,mean=-1, sd=5)
      }
      if(id==3){
        out <-  rnorm( 1,  mean=-.5, sd=5)
      }
      if(id==4){
        out <-  rnorm( 1,   sd=5)
      }
      if(id==5){
        out <-  rnorm( 1,  mean= .5, sd=5)
      }
      if(id==6){
        out <-  rnorm( 1, mean= 1, sd=5)
      }
      if(id==7){
        out <-  rnorm( 1,  mean= 1.5, sd=5)
      }
      return( out)
    }

    for ( i in 1:N){
      mix <-c(mix, sample(c(0,1), size=1,
                          prob = c(1- samp_prob[i], samp_prob[i])))
      betatrue <- c( betatrue, mix[i] *samp_fun())
      betahat <- c( betahat ,   betatrue[i]+rnorm(1,sd=noise_level ) )
    }
    if(se_type=="constant"){
      se <- rep(1, length(betahat))

    }
    if( se_type=="random"){
      se <-   rchisq(N, df=df_se)
    }
    tt1 <-  ( 1- samp_prob)  *dnorm(betahat, sd=noise_level)
    tt2 <- list( )
    for ( i in 1: length(betahat)){
      tt2[[i]]<-   (  samp_prob[i])*(1/7)*(dnorm(  betahat[i] ,
                                                   mean = c(-1.5,-1,-.5,0,.5,1,1.5),
                                                   sd= sqrt(noise_level^2+ 5^2)) )


    }

    tt2 <-  (do.call(rbind,tt2) )
    true_lfdr <-  tt1/ ( tt1+apply(tt2,1, sum))

  }

  if( dist=="skew"){



    samp_fun <- function(){
      id <- sample(size=1, 1:4, prob = c(1/4,1/4,1/3,1/6) )
      if(id==1){
        out <-  rnorm( 1,mean=-2, sd=sqrt(2)*2)
      }
      if(id==2){
        out <-  rnorm( 1,mean=-1, sd=sqrt(2)*1.5)
      }
      if(id==3){
        out <-  rnorm( 1,  mean=0, sd=sqrt(2)*1)
      }
      if(id==4){
        out <-  rnorm( 1,mean=1,   sd=sqrt(2)*1)
      }

      return( out)
    }

    for ( i in 1:N){
      mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[i], samp_prob[i])))
      betatrue <- c( betatrue, mix[i] *samp_fun())
      betahat <- c( betahat ,   betatrue[i]+rnorm(1,sd=noise_level ) )
    }
    if(se_type=="constant"){
      se <- rep(1, length(betahat))

    }
    if( se_type=="random"){
      se <-   rchisq(N, df=df_se)
    }
    tt1 <-  ( 1- samp_prob)  *dnorm(betahat, sd=noise_level)
    tt2 <- list( )
    for ( i in 1: length(betahat)){
      tt2[[i]]<-   (  samp_prob[i])*(  (1/4)* dnorm(betahat[i], mean=-2, sd= sqrt(noise_level^2+ 2^2))+
                                         (1/4)* dnorm(betahat[i], mean=-1, sd= sqrt(noise_level^2+ 1.5^2))+
                                         (1/3)* dnorm(betahat[i], mean=0, sd= sqrt(noise_level^2+ 1^2))+
                                         (1/6)* dnorm(betahat[i], mean=1, sd= sqrt(noise_level^2+ 1^2))


      )



    }

    tt2 <-  (do.call(c,tt2) )
    true_lfdr <-  tt1/ ( tt1+tt2)

  }

  if( dist=="big-normal"){
    for ( i in 1:N){
      mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[i], samp_prob[i])))
      betatrue <- c( betatrue, mix[i] *rnorm(1,sd=4))
      betahat <- c( betahat ,   betatrue[i]+rnorm(1,sd=noise_level ) )
    }
    if(se_type=="constant"){
      se <- rep(1, length(betahat))

    }
    if( se_type=="random"){
      se <-   rchisq(N, df=df_se)
    }

    tt1 <-  ( 1- samp_prob)  *dnorm(betahat, sd=noise_level)
    tt2 <- list( )
    for ( i in 1: length(betahat)){
      tt2[[i]]<-   (  samp_prob[i])*(   dnorm(  betahat[i] ,
                                                mean = 0,sd= sqrt(noise_level^2+ 4^2))
      )


    }

    tt2 <-  (do.call(c,tt2) )
    true_lfdr <-  tt1/ ( tt1+tt2)

  }
  if( dist=="bimodal"){
    samp_fun <- function(){
      id <- sample(size=1 ,1:2   )
      if(id==1){
        out <-  rnorm( 1,mean=-2, sd=1)
      }
      if(id==2){
        out <-  rnorm( 1,mean=2, sd=1)
      }


      return( out)
    }


    for ( i in 1:N){
      mix <-c(mix, sample(c(0,1), size=1, prob = c(1- samp_prob[i], samp_prob[i])))
      betatrue <- c( betatrue, mix[i] *samp_fun())
      betahat <- c( betahat ,   betatrue[i]+rnorm(1,sd=noise_level ) )
    }
    if(se_type=="constant"){
      se <- rep(1, length(betahat))

    }
    if( se_type=="random"){
      se <-   rchisq(N, df=df_se)
    }
    tt1 <-  ( 1- samp_prob)  *dnorm(betahat, sd=noise_level)
    tt2 <- list( )
    for ( i in 1: length(betahat)){
      tt2[[i]]<-   (  samp_prob[i])*( 0.5*dnorm(  betahat[i] , mean=-2, sd=sqrt(noise_level^2+ 1^2))  +
                                        0.5*dnorm(  betahat[i] , mean=2, sd=sqrt(noise_level^2+ 1^2))    )



    }

    tt2 <-  (do.call(c,tt2) )
    true_lfdr <-  tt1/ ( tt1+tt2)


  }
  X=x1
  res <- cFDR(betahat = betahat,se=se, X=X,max_iter=max_iter, nullweight = nullweight)
  tt <- ashr::ash(betahat, se)


  #Power
  if (length(which( tt$result$lfdr<0.05))==0){
    power_ash <- 0
    T1_ash <-  0
  }else{
    power_ash <-  sum( mix[which( tt$result$lfdr<0.05)])/sum( mix)
    T1_ash <-  length(which( mix[which( tt$result$lfdr<0.05)]==0))/length(which( tt$result$lfdr<0.05))

  }
  if (length(which(res$result$lfdr<0.05))==0){
    power_mco <- 0
    T1_mco <-  0
  }else{
    power_mco <- sum( mix[which(res$result$lfdr <0.05)])/sum( mix)
    T1_mco <-length(which( mix[which(res$result$lfdr<0.05)]==0))/length(which(res$result$lfdr<0.05))

  }
  #T1 error

  rmse_mco <-  sqrt(mean( (res$result$PosteriorMean -  betatrue)^2))

  rmse_ash <- sqrt(mean( (( tt$result$PosteriorMean -  betatrue)^2)))


  n_effect <-   do.call(c, #number of effect per CS

                        lapply(1:length(res$cs), function(k)
                          length(which(true_pos%in% res$cs[[k]]$cs))
                        )
  )




  #number of effect found
  nfalse_effect <- do.call(c, lapply(1:length(res$cs), function(k)
    ifelse( length(which(true_pos%in%res$cs[[k]]$cs ))==0, 1,0)
  )
  )

  size_set <- do.call(c, lapply(1:length(res$cs), function(k)
    res$cs[[k]]$size
  )
  )
  summary_out <- c( rmse_mco,
                    rmse_ash,
                    power_mco,
                    power_ash,
                    T1_mco,
                    T1_ash,
                    N ,
                    beta0  ,
                    beta1  ,
                    effect_var ,
                    noise_level ,
                    var_cov_effect ,
                    P ,
                    ifelse(se_type=="random",1,0) ,
                    df_se,
                    max_iter,
                    dist,
                    L

  )




  df_bf <- data.frame (nfalse_effect= nfalse_effect,
                       n_effect     = n_effect ,
                       purity       = res$est_purity[1:length( nfalse_effect)],
                       size_set     =  size_set [1:length( nfalse_effect)],
                       maxLBF       = res$est_max_bf[1:length( nfalse_effect)]

  )

  names ( summary_out ) <-  c("rmse_mco",
                              "rmse_ash",
                              "power_mco",
                              "power_ash",
                              "T1_mco",
                              "T1_ash",
                              "N" ,
                              "beta0"  ,
                              "beta1"  ,
                              "effect_var" ,
                              "noise_level" ,
                              "var_cov_effect" ,
                              "P" ,
                              "se_type"  ,
                              "df_se",
                              "max_iter",
                              "dist",
                              "L"
  )


  out <- list( summary= summary_out,
               df_bf =  df_bf)

  if( extended ){


    lfdr_est = data.frame(lfdr_ash  =tt$result$lfdr,
                          lfdr_cash =res$result$lfdr,
                          true_lfdr = true_lfdr)

    out <- list( summary  = summary_out,
                 df_bf    = df_bf,
                 lfdr_est =lfdr_est,
                 size_set = size_set
    )
  }
  return(out)


}



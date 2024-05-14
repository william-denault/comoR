library(comoR)
library(softImpute)
library(nnet)
rm(list=ls())
 devtools::load_all(".")

 N=200  # number of row
                            L=100  #number of columns
                            K=2  #number of factor
                            P1=20  # number of cov for row /loadings
                            P2=20  # number of cov for col /factors
                            beta0=-1
                            beta1=2
                            max_class=10
                            noise_level=  10
                            max_iter_cEBMF=20
                            max_iter_como=20



    set.seed(rpois(lambda = 100,n=1))

  X_l <-   matrix(rnorm(P1*N, sd=1), ncol=P1)
  X_f <-   matrix(rnorm(P2*L, sd=1), ncol=P2)

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


  res <- cEBMF  (Y=Y_obs,
                 X_l=X_l,
                 X_f=X_f,
                 reg_method="logistic_susie",
                 K=K, init_type = "udv_si",
                 param_como = list(max_class=max_class,mnreg="mult_reg"),
                 maxit_como =max_iter_como ,
                 maxit=3,
                 param_nnet= list(size=3, decay=1.2))



  cEBMF.obj <- init_cEBMF (Y=Y_obs,
                           X_l=X_l,
                           X_f=X_f,
                           K=K, init_type = "udv_si",
                           param_como = list(max_class=max_class,mnreg="mult_reg"),
                           maxit_como = max_iter_como )









  for ( o in 1:10){
    for ( k in 1:K){
      Rk <- cal_partial_residuals.cEBMF(cEBMF.obj,k)
      l_k <- cal_expected_loading( cEBMF.obj, Rk,k)
      t_data <- set_data_como(betahat = l_k$l_i_hat,
                              se      = l_k$s_i,
                              X       = cEBMF.obj$X_l )
      t_fit <-  rlang::exec( "data_initialize_como", !!!cEBMF.obj$param_como ,
                             data= t_data,
                             param_nnet=cEBMF.obj$param_nnet) # initialize the model from the data
      t_fit <- fit_model( t_fit, t_data, max_iter = max_iter_como )


      fitted_loading <- post_mean_sd.como (fit= t_fit, data=t_data )
      cEBMF.obj$loading[,k] <-  fitted_loading$mean
      cEBMF.obj$loading2[,k] <- fitted_loading$sd^2+ fitted_loading$mean^2


      #factor update
      f_k <- cal_expected_factor( cEBMF.obj, Rk,k)
      t_data <- set_data_como(betahat = f_k$f_j_hat,
                              se      = f_k$s_j,
                              X       = cEBMF.obj$X_f )
      t_fit <-  rlang::exec( "data_initialize_como", !!!cEBMF.obj$param_como ,
                             data= t_data,
                             param_nnet=cEBMF.obj$param_nnet)# initialize the model from the data
      t_fit <- fit_model( t_fit, t_data, max_iter = max_iter_como )


      fitted_factor <- post_mean_sd.como (fit= t_fit, data=t_data )
      cEBMF.obj$factor[,k] <-  fitted_factor $mean
      cEBMF.obj$factor2[,k] <-  fitted_factor$sd^2+ fitted_factor$mean^2

      cEBMF.obj<- update_tau.cEBMF (cEBMF.obj )




    }

  }

  Y_est <- Reduce("+", lapply(1:ncol(cEBMF.obj$factor), function(k)
    cEBMF.obj$loading[,k]%*%t(cEBMF.obj$factor[,k])
  )
  )
  f <- flashier::flash(Y_obs)
  plot( fitted(f), Y_true, xlim= c(-5,5), ylim= c(-5,5))
    points( Y_est, Y_true, col="green")
    points(res$Y_fit
           ,Y_true, col="blue")

  rmse_cEBMF   <- sqrt(mean( (Y_true-Y_est)^2))
  rmse_flash   <-  sqrt(mean( (Y_true- fitted(f))^2))
  rmse_cEBMF2  <-  sqrt(mean( (Y_true- res$Y_fit)^2))
  rmse         <- c(rmse_cEBMF2 ,rmse_cEBMF, rmse_flash)

  test_that("cEBMF outpertfomr flash", {

    expect_gte(rmse_flash ,rmse_cEBMF )
  })

  test_that("cEBMF outpertfomr flash", {

    expect_equal(sqrt(mean( (res$Y_fit-Y_est)^2)) ,0, tolerance = 0.004 )# current precision
  })


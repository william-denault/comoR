

#' @title Initialize cEBMF object
#' @description  Initialize cEBMF object
#' @param Y numerical matrix size NxP
#' @param X_l matrix of size NxJ containning covariates affecting the factors
#' @param X_f matrix of size PxT containning covariates affecting the factors
#' @param K numeric number of factors
#'  @param type_noise specify which kind of noise structure is expected, currently three choices. Whether noise constant accross column ('column_wise'), constant 'constant' or constant across rown 'row_wise'
#'  @param init_type specify initialisation method for loading and factor, methods includes: "udv", "udv_si_svd", "si" (softimpute), "ones" and "rand_udv"
#' @return a cEBMF object
#' @export
init_cEBMF <- function(Y, X_l,X_f,
                       mnreg_type.x="nnet",
                       mnreg_type.y="nnet",
                       K=1,
                       type_noise= "constant",
                       init_type="udv_si",
                       param_como.x,
                       param_como.y,
                       maxit_como,
                       param_nnet.x  =list( size=1, decay=1),
                       param_nnet.y =list( size=1, decay=1),
                       param_como2 = list(),
                       param_susie = list(L=5),
                       dynamic_cov   =NULL,
                       check_l_prior = NULL,
                       intercept=FALSE)
{


  if( missing(X_l))
  {
    X_l <- matrix(rnorm(nrow(Y)*10), nrow= nrow(Y))
  }
  if( missing(X_f))
  {
    X_f <- matrix(rnorm(ncol(Y)*10), nrow= ncol(Y))
  }

  if (isTRUE(check_l_prior)){
    check_l_prior<- list()
  }



  model_loading = list()
  model_factor  = list()
  if(mnreg_type.x=="keras"){
    for ( k in 1:K){

      model_loading[[k]] = keras::clone_model (param_nnet.x)


    }
  }
  if(mnreg_type.y=="keras"){
    for ( k in 1:K){

      model_factor[[k]] = keras::clone_model (param_nnet.y)

    }
  }


  ### flashier  -----

  if(init_type=="flashier"){

    init_res <- flash_init(Y) %>%
      flash_set_verbose(0) %>%
      flash_greedy(
        Kmax = K,
        ebnm_fn = c(ebnm_ash,  ebnm_ash),
        maxiter=50
      )
    if (is.null(init_res$L_pm)){
      init_type="rand_udv"

    }else{
      if(K==1){
        l <-  init_res$L_pm [,1]
        f <-  init_res$F_pm [,1]
      }else{
        if (ncol(init_res$L_pm) < K){
          l <- matrix(1, nrow=nrow(Y), ncol=K)
          f <- matrix(1, nrow=ncol(Y), ncol=K)
          l[,1:ncol(init_res$L_pm)] <- init_res$L_pm
          f[,1:ncol(init_res$F_pm)] <- init_res$F_pm
        } else{
          l <- init_res$L_pm
          f <- init_res$F_pm
        }


      }

      cEBMF.obj <- list(
        Y          = Y,
        Y_fit      = 0*Y,
        loading    = l , #first moment
        factor     = f,
        loading2   = l^2,#second moment
        factor2    = f^2,
        tau        = matrix(1, ncol = ncol(Y), nrow=nrow(Y)),
        X_l        = X_l,
        X_f        = X_f,
        K          = K,
        type_noise = type_noise,
        elbo       = -Inf,
        model_loading = model_loading, #nnet param for each loading
        KL_l          = list(),
        model_factor  = model_factor,  #susie param for each factor
        KL_f          = list(),
        param_como.x  =  param_como.x,
        param_como.y  =  param_como.y,
        maxit_como  = maxit_como,
        param_nnet.x  = param_nnet.x,
        param_nnet.y  = param_nnet.y,
        mnreg_type.x= mnreg_type.x,
        mnreg_type.y= mnreg_type.y,
        param_como2 = param_como2,
        param_susie = param_susie,
        dynamic_cov = dynamic_cov,
        check_l_prior = check_l_prior

      )
    }

  }

  ### flashier_SNMF -----
  if(init_type=="flashier_SNMF"){

    init_res <- flash_init(Y) %>%
      flash_set_verbose(0) %>%
      flash_greedy(
        Kmax = K,
        ebnm_fn = c(ebnm_point_exponential,  ebnm_ash),
        maxiter=50
      )
    if (is.null(init_res$L_pm)){
      init_type="rand_udv"

    }else{
      if(K==1){
        l <-  init_res$L_pm [,1]
        f <-  init_res$F_pm [,1]
      }else{
        if (ncol(init_res$L_pm) < K){
          l <- matrix(1, nrow=nrow(Y), ncol=K)
          f <- matrix(1, nrow=ncol(Y), ncol=K)
          l[,1:ncol(init_res$L_pm)] <- init_res$L_pm
          f[,1:ncol(init_res$F_pm)] <- init_res$F_pm
        } else{
          l <- init_res$L_pm
          f <- init_res$F_pm
        }


      }

      cEBMF.obj <- list(
        Y          = Y,
        Y_fit      = 0*Y,
        loading    = l , #first moment
        factor     = f,
        loading2   = l^2,#second moment
        factor2    = f^2,
        tau        = matrix(1, ncol = ncol(Y), nrow=nrow(Y)),
        X_l        = X_l,
        X_f        = X_f,
        K          = K,
        type_noise = type_noise,
        elbo       = -Inf,
        model_loading = model_loading, #nnet param for each loading
        KL_l          = list(),
        model_factor  = model_factor,  #susie param for each factor
        KL_f          = list(),
        param_como.x  =  param_como.x,
        param_como.y  =  param_como.y,
        maxit_como  = maxit_como,
        param_nnet.x  = param_nnet.x,
        param_nnet.y  = param_nnet.y,
        mnreg_type.x= mnreg_type.x,
        mnreg_type.y= mnreg_type.y,
        param_como2 = param_como2,
        param_susie = param_susie,
        dynamic_cov = dynamic_cov,
        check_l_prior = check_l_prior

      )
    }
  }
  ### flashier_NMF -----
  if(init_type=="flashier_NMF"){

    init_res <- flash_init(Y) %>%
      flash_set_verbose(0) %>%
      flash_greedy(
        Kmax = K,
        ebnm_fn = c(ebnm_point_exponential,  ebnm_point_exponential),
        maxiter=50
      )
    if (is.null(init_res$L_pm)){
      init_type="rand_udv"

    }else{
      if(K==1){
        l <-  init_res$L_pm [,1]
        f <-  init_res$F_pm [,1]
      }else{
        if (ncol(init_res$L_pm) < K){
          l <- matrix(1, nrow=nrow(Y), ncol=K)
          f <- matrix(1, nrow=ncol(Y), ncol=K)
          l[,1:ncol(init_res$L_pm)] <- init_res$L_pm
          f[,1:ncol(init_res$F_pm)] <- init_res$F_pm
        } else{
          l <- init_res$L_pm
          f <- init_res$F_pm
        }


      }

      cEBMF.obj <- list(
        Y          = Y,
        Y_fit      = 0*Y,
        loading    = l , #first moment
        factor     = f,
        loading2   = l^2,#second moment
        factor2    = f^2,
        tau        = matrix(1, ncol = ncol(Y), nrow=nrow(Y)),
        X_l        = X_l,
        X_f        = X_f,
        K          = K,
        type_noise = type_noise,
        elbo       = -Inf,
        model_loading = model_loading, #nnet param for each loading
        KL_l          = list(),
        model_factor  = model_factor,  #susie param for each factor
        KL_f          = list(),
        param_como.x  =  param_como.x,
        param_como.y  =  param_como.y,
        maxit_como  = maxit_como,
        param_nnet.x  = param_nnet.x,
        param_nnet.y  = param_nnet.y,
        mnreg_type.x= mnreg_type.x,
        mnreg_type.y= mnreg_type.y,
        param_como2 = param_como2,
        param_susie = param_susie,
        dynamic_cov = dynamic_cov,
        check_l_prior = check_l_prior

      )
    }
  }
  ### ones -----

  if (init_type=="ones"){
    cEBMF.obj <- list(
      Y          = Y,
      Y_fit      = 0*Y,
      loading    = matrix(1,nrow=nrow(Y), ncol=K), #first moment
      factor     = matrix(1,nrow=ncol(Y), ncol=K),
      loading2   = matrix(1,nrow=nrow(Y), ncol=K),#second moment
      factor2    = matrix( 1,nrow=ncol(Y), ncol=K),
      tau        = matrix(1, ncol = ncol(Y), nrow=nrow(Y)),
      X_l        = X_l,
      X_f        = X_f,
      K          = K,
      type_noise = type_noise,
      elbo       = -Inf,
      model_loading = model_loading, #nnet param for each loading
      KL_l          = list(),
      model_factor  = model_factor,  #susie param for each factor
      KL_f          = list(),
      param_como.x  =  param_como.x,
      param_como.y  =  param_como.y,
      maxit_como  = maxit_como,
      param_nnet.x  = param_nnet.x,
      param_nnet.y  = param_nnet.y,
      mnreg_type.x= mnreg_type.x,
      mnreg_type.y= mnreg_type.y,
      param_como2 = param_como2,
      param_susie = param_susie,
      dynamic_cov = dynamic_cov,
      check_l_prior = check_l_prior


    )
  }
  ### udv -----
  if(init_type=="udv"){

    s <- svd(Y, K,K)
    if(K==1){
      l <- (s$u[,1])*(s$d[1])
    }else{
      l <-s$u[,1:K]%*%diag(s$d[1:K])
    }
    f <- s$v[,1:K]


    cEBMF.obj <- list(
      Y          = Y,
      Y_fit      = 0*Y,
      loading    = l  , #first moment
      factor     = f,
      loading2   = l ^2,#second moment
      factor2    = f ^2,
      tau        = matrix(1, ncol = ncol(Y), nrow=nrow(Y)),
      X_l        = X_l,
      X_f        = X_f,
      K          = K,
      type_noise = type_noise,
      elbo       = -Inf,
      model_loading = model_loading, #nnet param for each loading
      KL_l          = list(),
      model_factor  = model_factor,  #susie param for each factor
      KL_f          = list(),
      param_como.x  =  param_como.x,
      param_como.y  =  param_como.y,
      maxit_como  = maxit_como,
      param_nnet.x  = param_nnet.x,
      param_nnet.y  = param_nnet.y,
      mnreg_type.x= mnreg_type.x,
      mnreg_type.y= mnreg_type.y,
      param_como2 = param_como2,
      param_susie = param_susie,
      dynamic_cov = dynamic_cov,
      check_l_prior = check_l_prior

    )
  }
  ### udv_si -----
  if(init_type=="udv_si"){
    suppressWarnings(
      s <- softImpute(Y, rank.max=K,type="als", lambda=0)
    )
    if(K==1){
      l <- (s$u )*(s$d[1])
      f <- s$v
    }else{
      l <-s$u[,1:K]%*%diag(s$d[1:K])
      f <- s$v[,1:K]
    }


    cEBMF.obj <- list(
      Y          = Y,
      Y_fit      = 0*Y,
      loading    = l , #first moment
      factor     = f,
      loading2   = l^2,#second moment
      factor2    = f^2,
      tau        = matrix(1, ncol = ncol(Y), nrow=nrow(Y)),
      X_l        = X_l,
      X_f        = X_f,
      K          = K,
      type_noise = type_noise,
      elbo       = -Inf,
      model_loading = model_loading, #nnet param for each loading
      KL_l          = list(),
      model_factor  = model_factor,  #susie param for each factor
      KL_f          = list(),
      param_como.x  =  param_como.x,
      param_como.y  =  param_como.y,
      maxit_como  = maxit_como,
      pparam_nnet.x  = param_nnet.x,
      param_nnet.y  = param_nnet.y,
      mnreg_type.x= mnreg_type.x,
      mnreg_type.y= mnreg_type.y,
      param_como2 = param_como2,
      param_susie = param_susie,
      dynamic_cov = dynamic_cov,
      check_l_prior = check_l_prior

    )
  }


  ### udv_si_svd -----
  if(init_type=="udv_si_svd"){
    suppressWarnings(
      s <- softImpute(Y, rank.max=K,type="svd", lambda=0)
    )
    if(K==1){
      l <- (s$u )*(s$d[1])
      f <- s$v
    }else{
      l <-s$u[,1:K]%*%diag(s$d[1:K])
      f <- s$v[,1:K]
    }

    cEBMF.obj <- list(
      Y          = Y,
      Y_fit      = 0*Y,
      loading    = l , #first moment
      factor     = f,
      loading2   = l^2,#second moment
      factor2    = f^2,
      tau        = matrix(1, ncol = ncol(Y), nrow=nrow(Y)),
      X_l        = X_l,
      X_f        = X_f,
      K          = K,
      type_noise = type_noise,
      elbo       = -Inf,
      model_loading = model_loading, #nnet param for each loading
      KL_l          = list(),
      model_factor  = model_factor,  #susie param for each factor
      KL_f          = list(),
      param_como.x  =  param_como.x,
      param_como.y  =  param_como.y,
      maxit_como  = maxit_como,
      param_nnet.x  = param_nnet.x,
      param_nnet.y  = param_nnet.y,
      mnreg_type.x= mnreg_type.x,
      mnreg_type.y= mnreg_type.y,
      param_como2 = param_como2,
      param_susie = param_susie,
      dynamic_cov = dynamic_cov,
      check_l_prior = check_l_prior

    )
  }

  ### rand_udv -----
  if(init_type=="rand_udv"){
    s <- svd( matrix(rnorm(prod(dim(Y))),
                     ncol= ncol(Y)
    ),
    K,K
    )


    if(K==1){
      l <- (s$u[,1])*(s$d[1])
    }else{
      l <-s$u[,1:K]%*%diag(s$d[1:K])
    }
    f <- s$v[,1:K]


    cEBMF.obj <- list(
      Y          = Y,
      Y_fit      = 0*Y,
      loading    = l  , #first moment
      factor     = f,
      loading2   = l^2,#second moment
      factor2    = f^2,
      tau        = matrix(1, ncol = ncol(Y), nrow=nrow(Y)),
      X_l        = X_l,
      X_f        = X_f,
      K          = K,
      type_noise = type_noise,
      elbo       = -Inf,
      model_loading = model_loading, #nnet param for each loading
      KL_l          = list(),
      model_factor  = model_factor,  #susie param for each factor
      KL_f          = list(),
      param_como.x  =  param_como.x,
      param_como.y  =  param_como.y,
      maxit_como  = maxit_como,
      param_nnet.x  = param_nnet.x,
      param_nnet.y  = param_nnet.y,
      mnreg_type.x= mnreg_type.x,
      mnreg_type.y= mnreg_type.y,
      param_como2 = param_como2,
      param_susie = param_susie,
      dynamic_cov = dynamic_cov,
      check_l_prior = check_l_prior
    )
  }
  class(cEBMF.obj) <- "cEBMF"
  #initialize tau using observed variance
  tau <- update_tau.cEBMF(cEBMF.obj)$tau
  return(cEBMF.obj)
}

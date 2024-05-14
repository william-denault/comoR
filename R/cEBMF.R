
#' @title covariate moderated Empirical Bayes Matrix Factorization
#' @description covariate moderated Empirical Bayes Matrix Factorization
#' @param Y numerical matrix size NxP
#' @param X_l matrix of size NxJ containning covariates affecting the factors
#' @param X_f matrix of size PxT containning covariates affecting the factors
#' @param mnreg_type specify underlying learner for within topic heterogeneity , two methods available 'nnet' or 'keras'. Default is nnet.
#' you can pass the nnet specification through the param_nnet argument and through the param_susie for logistic susie.
#' @param K numeric number of factors
#' @param type_noise specify which kind of noise structure is expected, currently three choices. Whether noise constant accross column ('column_wise'), constant 'constant' or constant across rown 'row_wise'
#' @param init_type specify the initialization method, the following methods are available 'udv_si', 'random', 'flashier', 'flashier_NMF', 'flashier_SNM', 'udv_si_svd',
#' 'udv','rand_udv'. Default is udv_si.
#' @param maxit maximum nuber of iterations
#' @param tol paramter for assessing convergence
#' @param param_como.x list of parameters for como object for X_l
#' @param param_como.y list of parameters for como object for Y
#' @param param_nnet.x list of parameters for nnet object for X_f
#' @param param_nnet.y list of parameters for nnet object for Y
#' @param param_como2 list of parameters for como object for Y
#' @param param_susie list of parameters for susie object for Y
#' @param maxit_como maximum number of iterations for como object

#' @return a cEBMF object
#'
#'
#' @export
#'

cEBMF <- function( Y,
                   X_l,
                   X_f,
                   mnreg_type.x="nnet",
                   mnreg_type.y="nnet",
                   K=1,
                   type_noise='constant',
                   init_type="udv_si",
                   maxit=100,
                   tol=1e-3 ,
                   param_como.x  = list(max_class=10,mnreg_type="mult_reg"),
                   param_como.y  = list(max_class=10,mnreg_type="mult_reg"),
                   param_nnet.x  =list( size=1, decay=1),
                   param_nnet.y  =list( size=1, decay=1),
                   param_como2 = list(),
                   param_susie =  list(L=5),
                   maxit_como  = 10,
                   dynamic_cov   = NULL,
                   check_l_prior = NULL ,# for visulation purposes
                   intercept= FALSE
                   ){



  cEBMF.obj <- init_cEBMF (Y,
                           X_l,
                           X_f,
                           mnreg_type.x  = mnreg_type.x,
                           mnreg_type.y  = mnreg_type.y,
                           K             = K,
                           type_noise    = type_noise,
                           init_type     = init_type,
                           param_como.x  = param_como.x,
                           param_como.y  = param_como.y,
                           maxit_como    = maxit_como,
                           param_nnet.x  = param_nnet.x,
                           param_nnet.y  = param_nnet.y,
                           param_como2   = param_como2,
                           param_susie   = param_susie,
                           dynamic_cov   = dynamic_cov,
                           check_l_prior = check_l_prior,
                           intercept     = intercept

  )### Need to carry info about como obj

 #weird problem not writing the keras nnet parameters here
  for (i in 1:maxit) {
    print(paste('iteration' ,i ))

    cEBMF.obj <- cEBMF_iter  (cEBMF.obj)
    if (.converged(cEBMF.obj)) {
      break
    }
  }
  # plot( cEBMF.obj$elbo)
  cEBMF.obj <- out_prep.cEBMF(cEBMF.obj)
  return(cEBMF.obj)
}

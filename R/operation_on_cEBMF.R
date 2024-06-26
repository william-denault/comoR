


#'@param cEBMF a cEBMF object
#' @export
cEBMF_iter <- function(cEBMF.obj){

  #browser()
  for( k in 1:cEBMF.obj$K){

    cEBMF.obj <- update_cEBMF(cEBMF.obj, k)

  }
  #cEBMF.obj$tau

  cEBMF.obj <- update_elbo.cEBMF(cEBMF.obj)
  cEBMF.obj <-  check_null_factor(cEBMF.obj)
  return(cEBMF.obj)

}


#'@param cEBMF a cEBMF object
#'@param k component of interest
#'@return list of two l_i_hat the estimate loading, s_i the estimated standard errors
#' @export
cal_expected_loading <- function( cEBMF.obj, Rk,k){

  if( cEBMF.obj$K==1){
    f       <- cEBMF.obj$factor
    f2      <- cEBMF.obj$factor2
  }else{
    f       <- cEBMF.obj$factor[,k]
    f2      <- cEBMF.obj$factor2[,k]
  }

  mat_up  <- sweep(Rk*cEBMF.obj$tau ,2,f, "*")
  mat_low <- sweep(cEBMF.obj$tau,2,  f2 , "*")
  deno    <- apply(mat_low,1,sum)+1e-32#to avoid some numerical issue


  l_i_hat <- apply(mat_up,1,sum)/deno
  s_i     <-  1/sqrt(deno)


  out <- list( l_i_hat = l_i_hat,
               s_i     = s_i)
  return( out)
}



#'@param cEBMF a cEBMF object
#'@param k  component of interest
#'@return list of two l_i_hat the estimate loading, s_i the estimated standard errors
#' @export
cal_expected_factor <- function( cEBMF.obj, Rk,k){


  if( cEBMF.obj$K==1){
    l       <- cEBMF.obj$loading
    l2      <- cEBMF.obj$loading2
  }else{
    l       <- cEBMF.obj$loading[,k]
    l2      <- cEBMF.obj$loading2[,k]
  }

  mat_up  <- Rk*cEBMF.obj$tau *l
  mat_low <- cEBMF.obj$tau *(l2)
  deno    <- apply(mat_low,2,sum)+1e-32#to avoid some numerical issue


  f_j_hat <- apply(mat_up,2,sum)/deno
  s_j     <-  1/sqrt(deno)


  out <- list( f_j_hat = f_j_hat,
               s_j     = s_j)
  return( out)
}
#' @export
cal_fitted_value.cEBMF <- function(cEBMF.obj)
{

  if(cEBMF.obj$K ==1 ){
    cEBMF.obj$Y_fit <- cEBMF.obj$loading %*%t(cEBMF.obj$factor)
  }else{
    cEBMF.obj$Y_fit <-   Reduce("+",
                                lapply( 1:cEBMF.obj$K,
                                        function(k)
                                          cEBMF.obj$loading[ ,k]%*%t(cEBMF.obj$factor[ ,k])
                                )
    )
  }


  return(cEBMF.obj)
}

#' @export
cal_expected_residuals.cEBMF <- function(cEBMF.obj)
{
  if(cEBMF.obj$K==1){
    prod_square_firstmom <- (cEBMF.obj$loading^2)%*%t(cEBMF.obj$factor^2)

    prod_sectmom         <- (cEBMF.obj$loading2)%*%t(cEBMF.obj$factor2)
  }else{
    prod_square_firstmom <-   Reduce("+",
                                     lapply( 1:cEBMF.obj$K,
                                             function(k)
                                               (cEBMF.obj$loading[ ,k]^2)%*%t(cEBMF.obj$factor[ ,k]^2)
                                     )
    )
    prod_sectmom <-   Reduce("+",
                             lapply( 1:cEBMF.obj$K,
                                     function(k)
                                       (cEBMF.obj$loading2[ ,k] )%*%t(cEBMF.obj$factor2[ ,k] )
                             )
    )
  }

  cEBMF.obj <- update_fitted_val(cEBMF.obj)

  R2  <- (cEBMF.obj$Y  -  cEBMF.obj$Y_fit)^2 -  prod_square_firstmom + prod_sectmom

  return(R2)

}


cal_partial_residuals.cEBMF <- function(cEBMF.obj,k)
{
  K <- cEBMF.obj$K
  if (K > 1){

    id_k <- (1:K)[ -k ]
    partial_pred <-   Reduce("+",
                             lapply( id_k,
                                     function(k)
                                       (cEBMF.obj$loading[ ,k])%*%t(cEBMF.obj$factor[ ,k])
                             )

    )
  }else{
    partial_pred <- 0*cEBMF.obj$Y
  }


  Rk  <-  cEBMF.obj$Y  -   partial_pred

  return(Rk)

}


check_null_factor <- function(cEBMF.obj){
  if ( cEBMF.obj$K > 1){
    for( k in 2:cEBMF.obj$K){
      if( all(abs(cEBMF.obj$factor[,k])<1e-6) | all(abs(cEBMF.obj$loading[,k])<1e-6) )   {
        cEBMF.obj$factor [,k] <- 0
        cEBMF.obj$loading[,k] <- 0
        print(paste( "removing factor", cEBMF.obj$K))
        cEBMF.obj$K  <- max(cEBMF.obj$K-1,1)
        if(cEBMF.obj$K==1){
          cEBMF.obj$factor  <-  cEBMF.obj$factor [,1 ]
          cEBMF.obj$loading <-  cEBMF.obj$loading[,1 ]
          cEBMF.obj$factor2 <-  cEBMF.obj$factor2 [,1 ]
          cEBMF.obj$loading2<-  cEBMF.obj$loading2[,1 ]
        }
      }
    }
  }
  return(cEBMF.obj)
}



# @title Get expected log likelihood under current fit.
#
# @inheritParams flash_get_objective
#' @export
e_loglik = function(cEBMF.obj) {
  R2    <- cal_expected_residuals.cEBMF(cEBMF.obj)
  tau   <- cEBMF.obj$tau
  e_log <- e_loglik_R2_tau(R2, tau )
  return(e_log)
}


# adapted from flashr
#' @export
e_loglik_R2_tau= function(R2, tau, data) {
  # tau can be either a scalar or a matrix:

  return(-0.5 * sum(log(2 * pi / tau) + tau * R2))
}




#' @export
get_objective.cEBMF = function(cEBMF.obj) {
  out <-  e_loglik  (cEBMF.obj) - sum(unlist(cEBMF.obj$KL_l)) - sum(unlist(cEBMF.obj$KL_f))

  return(out)
}


#' @title prepare output of cEMBF function
#' @description prepare output of cEMBF function
#' @param cEBMF.obj a cEBMF object
#' @return a cEBMF object
#' @export
out_prep.cEBMF <- function(cEBMF.obj)
{
  cEBMF.obj <- update_fitted_val(cEBMF.obj)
}

set_data_fit.cEBMF <- function(cEBMF.obj,l_k=NULL, f_k=NULL,k=NULL ){


  if( is.null(l_k)& is.null(f_k)){
    stop ("whether l_k or f_k need to be non NULL")
  }
  if( !is.null(l_k)& !is.null(f_k)){
    stop ("Only l_k or f_k can be specified  ")
  }

  if(!is.null(l_k)){

    if( cEBMF.obj$mnreg_type.x %in% c( "nnet",'constant_mnreg')){
      data <- comoR:::como_prep_data (betahat   = l_k$l_i_hat,
                                      se        = l_k$s_i,
                                      X         = X,
                                      Z         = Z )
      fit <-  rlang::exec( "data_initialize_como", !!! cEBMF.obj$param_como.x ,
                           data= data,
                           param_nnet=cEBMF.obj$param_nnet.x) # initialize the model from the data
      fit  <- comoR:::fit.como(  fit,  data, max_iter = cEBMF.obj$maxit_como )
    }
    if( cEBMF.obj$mnreg_type.x=="keras"){
      N <- nrow(cEBMF.obj$Y)
      Z <- matrix( 1, N, ncol=1)

      if( !is.null(cEBMF.obj$dynamic_cov)){
        X <- cbind(cEBMF.obj$X_l , cEBMF.obj$dynamic_cov[,-k])
      }else{
        X = cEBMF.obj$X_l
      }


      data <- comoR:::como_prep_data (betahat   = l_k$l_i_hat,
                                      se        = l_k$s_i,
                                      X         = X,
                                      Z         = Z )

      # run comoR
      fit  <- rlang::exec( "data_initialize_como", !!! cEBMF.obj$param_como.x ,
                           data= data,
                           param_nnet=cEBMF.obj$param_nnet.x,
                           weights= get_weights(cEBMF.obj$model_loading[[k]])) # initialize the model from the data
      fit <- comoR:::fit.como (  fit, data, max_iter = cEBMF.obj$maxit_como)



      cEBMF.obj$model_loading[[k]] =keras::clone_model(fit$mnreg$model)

      # est <- comoR:::post_mean_sd (fit,data)



      # initialize the model from the data
    }

  }
  if(!is.null(f_k)){

    if( cEBMF.obj$mnreg_type.y%in% c( "nnet",'constant_mnreg')){
      N <- ncol(cEBMF.obj$Y)
      Z <- matrix( 1, N, ncol=1)
      data <- comoR:::como_prep_data (betahat = f_k$f_j_hat,
                                      se      = f_k$s_j,
                                      X=cEBMF.obj$X_f,
                                      Z =Z )
      fit <-  rlang::exec( "data_initialize_como", !!! cEBMF.obj$param_como.y ,
                           data= data,
                           param_nnet=cEBMF.obj$param_nnet.y) # initialize the model from the data
      fit  <-  comoR:::fit.como(  fit,  data, max_iter = cEBMF.obj$maxit_como )




    }
    if( cEBMF.obj$mnreg_type.y=="keras"){
      N <- ncol(cEBMF.obj$Y)
      Z <- matrix( 1, N, ncol=1)
      data <- comoR:::como_prep_data (betahat = f_k$f_j_hat,
                                      se      = f_k$s_j,
                                      X=cEBMF.obj$X_f,
                                      Z =Z )

      # run comoR
      fit  <- rlang::exec( "data_initialize_como", !!! cEBMF.obj$param_como.y,
                           data= data,
                           param_nnet= cEBMF.obj$model_factor[[k]]) # initialize the model from the data
      fit <- comoR:::fit.como (  fit, data, max_iter =cEBMF.obj$maxit_como)


      # est <- comoR:::post_mean_sd (fit,data)



      # initialize the model from the data
    }

  }

  return( list( fit=fit,
                data=data))
}






#'@param cEBMF a cEBMF object
#'@param k the factor to be updated
#' @export
#'
update_cEBMF <-  function(cEBMF.obj, k)
{
  #Expected residuals
  #browser()
  print(paste("fitting factor ",k))
  Rk   <- cal_partial_residuals.cEBMF(cEBMF.obj,k)

  #loading update
  l_k    <- cal_expected_loading( cEBMF.obj, Rk,k)
  temp   <- set_data_fit.cEBMF(cEBMF.obj,l_k=l_k,k=k)
  t_data <- temp$data
   # initialize the model from the data
  t_fit <- temp$fit
  if(cEBMF.obj$mnreg_type.x=="keras"){
    set_weights(cEBMF.obj$model_loading[[k]], get_weights(temp$fit$mnreg$model))
  }else{
    cEBMF.obj$model_loading[[k]]  <- t_fit
  }

  if( !is.null(cEBMF.obj$check_l_prior)){
    cEBMF.obj$check_l_prior[[k]] <- t_fit$mnreg$logpi
  }


  fitted_loading                <- post_mean_sd (fit= t_fit, data=t_data )
  if( cEBMF.obj$K==1){
    cEBMF.obj$loading      <-  fitted_loading$mean
    cEBMF.obj$loading2     <- fitted_loading$sd^2+ fitted_loading$mean^2
  }else{
    cEBMF.obj$loading[,k]  <-  fitted_loading$mean
    cEBMF.obj$loading2[,k] <- fitted_loading$sd^2+ fitted_loading$mean^2+1e-32
  }

  if( !is.null(cEBMF.obj$dynamic_cov)){
    cEBMF.obj$dynamic_cov[,k] <- compute_posterior_assignment(t_fit,t_data)[,1]
  }


  #factor update
  f_k    <- cal_expected_factor( cEBMF.obj, Rk,k)
  temp   <- set_data_fit.cEBMF(cEBMF.obj,f_k=f_k,k=k)
  t_data <- temp$data
  t_fit <- temp$fit


  if(cEBMF.obj$mnreg_type.y=="keras"){
    set_weights(cEBMF.obj$model_factor[[k]], get_weights(temp$fit$mnreg$model))
  }else{
    cEBMF.obj$model_factor[[k]]  <- t_fit
  }

  fitted_factor               <- post_mean_sd (fit= t_fit, data=t_data )
  if( cEBMF.obj$K==1){
    cEBMF.obj$factor      <-  fitted_factor $mean
    cEBMF.obj$factor2     <-  fitted_factor$sd^2+ fitted_factor$mean^2
  }else{
    cEBMF.obj$factor[,k]  <-  fitted_factor $mean
    cEBMF.obj$factor2[,k] <-  fitted_factor$sd^2+ fitted_factor$mean^2
  }
  cEBMF.obj<- update_tau.cEBMF (cEBMF.obj )



  return(cEBMF.obj)
}
#'@param cEBMF a cEBMF object
#' @export
update_elbo.cEBMF <- function( cEBMF.obj){
  cEBMF.obj$elbo <- c(cEBMF.obj$elbo,
                      get_objective.cEBMF (cEBMF.obj)
  )
  return(cEBMF.obj)
}
#' @export
update_fitted_val <- function( cEBMF.obj)
{
  cEBMF.obj <- cal_fitted_value.cEBMF(cEBMF.obj)
  return(cEBMF.obj)
}





#'@title Estimate noise value
#'@'description Estimate noise value for different type of structure
#'@param cEBMF an CEBMF object
#' @export

update_tau.cEBMF <- function(cEBMF.obj )
{
  R2 <- cal_expected_residuals.cEBMF(cEBMF.obj)
  if(cEBMF.obj$type_noise== 'constant')
  {
    cEBMF.obj$tau <- matrix(1/mean(R2),
                            ncol=ncol(cEBMF.obj$Y),
                            nrow=nrow(cEBMF.obj$Y)
    )
  }
  if(cEBMF.obj$type_noise== 'row_wise' ){
    cEBMF.obj$tau <-  matrix(
      rep(
        1/apply(R2, 1, mean),
        each=ncol(cEBMF.obj$Y)
      ),
      ncol= ncol(cEBMF.obj$Y),
      byrow=TRUE
    )
  }
  if(cEBMF.obj$type_noise== 'column_wise' ){
    cEBMF.obj$tau <-  matrix(
      rep(
        1/apply(R2, 2, mean),
        each=nrow(cEBMF.obj$Y)
      ),
      ncol= ncol(cEBMF.obj$Y),
      byrow=FALSE
    )
  }
  return(cEBMF.obj)
}




#adapted form the flashier package
ldf_cEBMF <- function(object, type = "f") {
  type <- tolower(type)
  if (type == "2") {
    type <- "f"
  } else if (type == "1") {
    type <- "o"
  } else if (type == "m") {
    type <- "i"
  }



  ldf <- calc.normalized.loadings(object, type = type)

  ret <- list()
  ret$L <- ldf$normalized.loadings[[1]]
  ret$D <- ldf$scale.constants
  ret$F <- ldf$normalized.loadings[[2]]

  return(ret)
}

calc.normalized.loadings_cEBMF <- function(cEBMF.obj, for.pve = FALSE, type = "o") {
  ret <- list()


  loadings <- cEBMF.obj$loading
  if (type == "f") {
    norms <- lapply(1:ncol(loadings), function(i) {sqrt(sum (loadings[,i]^2))})
  } else if (type == "o") {
    norms <- lapply(1:ncol(loadings), function(i) {sum(abs(loadings[,i]))})



  } else if (type == "i") {

    norms <- lapply(1:ncol(loadings), function(i) {max(abs(loadings[,i]), )})
  } else {
    stop("Norm type not recognized.")
  }

  norms<- do.call(c, norms)
  # Zero factors are "normalized" to zero.
  if( length(which( norms < 1e-30))>0 ){
    norms[which(norms) < 1e-30] <- Inf
  }



  L <- loadings
  for (i in 1:ncol(L)) {
    L[, i] <- L[, i] / norms[i]
  }


  if(length(is.infinite(norms))>0){
    norms[which(is.infinite(norms))] <- 0
  }

  ret$scale.constants <- norms
  ret$normalized.loadings <- L
  # if (!for.pve)
  #   ret$loading.SDs <- SD

  return(ret)
}

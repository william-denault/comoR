
#' @export
compute_kl <- function(x, ...){
  UseMethod("compute_kl", x)
}

#' @export
compute_elbo <- function(x, ...){
  UseMethod("compute_elbo", x)
}

#' @export
update_model <- function(x, ...){
  UseMethod("update_model", x)
}






fit_model <- function(x, ...){
  UseMethod("fit_model", x)
}

#' @export
fit_model.default <- function(fit, data, ..., max_iter=100, tol=1e-5){
  tictoc::tic()
  for(i in 1:max_iter){
    fit <- update_model(fit, data, ...)

    # Check convergence, assumes fit is tracking elbo

    if (length (fit$elbo)>2) {
        if(abs(diff(tail(fit$elbo, 2))) < tol){
         message('converged')
         break
          }
       }
    }


  timer <- tictoc::toc()
  fit$elapsed_time <- with(timer, toc - tic)
  return(fit)
}


#' @title Compute post first and second moment from como and como2 object
#
#' @export
post_mean_sd <- function (fit,data,...)
  UseMethod("post_mean_sd")



#' @title Compute individual posterior variance from marginal normal mean model
#' @description internal function to compute posterior mean and sds
t_ind_var <- function (fit, data, i,...)
  UseMethod("t_ind_var")


#' @title Compute individual fdr value como/como2 model with centered normal mixture
#' @description Compute individual fdr value como/como2 model with centered normal mixture
#'
get_fdr <- function (fit,data, i,...)
  UseMethod("get_fdr")

#' @title Compute individual lfdr value como/como2 model with centered normal mixture
#' @description Compute individual lfdr value como/como2 model with centered normal mixture
#'
get_lfdr <- function (fit,data, i,...)
  UseMethod("get_lfdr")




#' @title Cal purity ouput como2
#' @description  Cal purity ouput como2
#'
cal_purity <- function (fit,data, ...)
  UseMethod("cal_purity")


# Not in operator

"%!in%" <- function(x, y) !("%in%"(x, y))



#' adapted from autoselect.mxisqp
#' try to select a default range for the sigmaa values
#' that should be used, based on the values of betahat and sebetahat
#' mode is the location about which inference is going to be centered
#' gridmult is the multiplier by which the sds differ across the grid
#' @export
autoselect_scales_mix_norm <- function(betahat, sebetahat, max_class, mult = 2) {
  sigmaamin <- min(sebetahat) / 10 # so that the minimum is small compared with measurement precision
  if (all(betahat^2 <= sebetahat^2)) {
    sigmaamax <- 8 * sigmaamin # to deal with the occassional odd case where this could happen; 8 is arbitrary
  } else {
    sigmaamax <- 2 * sqrt(max(betahat^2 - sebetahat^2)) # this computes a rough largest value you'd want to use, based on idea that sigmaamax^2 + sebetahat^2 should be at least betahat^2
  }

  if (mult == 0) {
    return(c(0, sigmaamax / 2))
  } else {
    npoint <- ceiling(log2(sigmaamax / sigmaamin) / log2(mult))
    out <- c(0,mult^((-npoint):0) * sigmaamax)#  mult^((-npoint):0) * sigmaamax
     if (!missing(max_class)) {
       if (!(length(out) == max_class) ) {
        out <- seq(min(out), max(out), length.out = max_class)
      }
    }
    return(out)
  }
}





#' adapted from autoselect.mxisqp
#' try to select a default range for the sigmaa values
#' that should be used, based on the values of betahat and sebetahat
#' mode is the location about which inference is going to be centered
#' gridmult is the multiplier by which the sds differ across the grid
#' @export
autoselect_scales_mix_exp <- function(betahat, sebetahat, max_class, mult = 1.5,tt=1.5) {
  sigmaamin <- min(sebetahat) / 10 # so that the minimum is small compared with measurement precision
  if (all(betahat^2 <= sebetahat^2)) {
    sigmaamax <- 8 * sigmaamin # to deal with the occassional odd case where this could happen; 8 is arbitrary
  } else {
    sigmaamax <- tt * sqrt(max(betahat^2 )) # this computes a rough largest value you'd want to use, based on idea that sigmaamax^2 + sebetahat^2 should be at least betahat^2
  }

  if (mult == 0) {
    return(c(0, sigmaamax / 2))
  } else {
    npoint <- ceiling(log2(sigmaamax / sigmaamin) / log2(mult))
    out <- c(0,mult^((-npoint):0) * sigmaamax)#  mult^((-npoint):0) * sigmaamax
    if (!missing(max_class)) {
      if (!(length(out) == max_class) ) {
        out <- seq(min(out), max(out), length.out = max_class)

        if(out[2] <1e-2 ){
          out[2:length(out)] <- out[2:length(out)] +1e-2
        }
      }
    }
    return(out)
  }
}


#' Prepare data for multinomial regression
#' @param betahat vector of effect estimates
#' @param se vector of standard errors for effect estimates
#' @param X matrix of features, each row an observation
#' @param Z matrix of fixed-effect covariates (including intercept), each row an observation

como_prep_data <- function(betahat, se, X, Z){
  data <- helper_prep(X, rep(0, length(betahat)), 1, Z)
  data$y <- NULL
  data$betahat <- betahat
  data$se <- se
  return(data)
}

helper_prep <- function (X, y, N, Z, shift = 0, shift_var = 0, center = TRUE,
                         scale = FALSE)
{
  #.check_X(X) problematic when X is a list of image
  if(inherits(X, "matrix")){
    X <- Matrix::Matrix(scale(X, center = center, scale = scale))
  }

  n <- length(y)
  if (length(N) == 1) {
    N <- rep(N, n)
  }
  if (is.null(Z)) {
    Z <- matrix(rep(1, n), nrow = n)
  }
  data <- list(X = X , Z = Z, y = y, N = N, shift = shift,
               shift_var = shift_var)
  return(data)
}


como_check_data <- function(data){
  # check for betahat, se, X, X2, Z, etc
  TRUE
}


.check_X <- function (X)
{
  stopifnot(`data$X must be matrix/Matrix` = inherits(X, c("matrix",
                                                           "Matrix")))
}

#' Sigmoid
#' sigmoid function coverts log-odds to probability
#' @param x Log odds
#' @return Returns the probability
sigmoid <- function(x) {
  return(1 / (1 + exp(-x)))
}

#' @export
logodds <- function(p) {
  return(log(p) - log(1 - p))
}

logSumExp <- matrixStats::logSumExp

softmax <- function(x) {
  return(exp(x - logSumExp(x)))
}

.monotone <- function(v) {
  return(all(tail(v, -1) - head(v, -1) >= 0))
}

#' helper function gets difference in ELBO between iterations
.diff <- function(fit) {
  delta <- diff(tail(fit$elbo, 2))
  if (delta < 0) {
    warning("ELBO decreased")
  }
  return(delta)
}

#' check if ELBO is converged to some tolerance threshold
#' @export
.converged <- function(fit, tol = 1e-3) {
  is.converged <- F
  if ((length(fit$elbo) > 1) & abs(.diff(fit)) <= tol) {
    is.converged <- T
  }
  return(is.converged)
}


rowCumSum <- function(X) {
  do.call(rbind, apply(X, 1, cumsum, simplify = F))
}


# implement normal and point mass component distributions

.clamp <- function(v, .upper = 100, .lower = -100) {
  return(pmax(.lower, pmin(v, .upper)))
}


is.odd <- function(x) {
  x %% 2 == 1
}
is.even <- function(x) {
  x %% 2 == 0
}


# convenient table of CSs from get_all_cs2
cs_tbl2 <- function(alpha) {
  get_all_cs2(alpha) %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(top_feaure = cs[1], top_feature_alpha = prob[1]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(L = paste0("L", 1:dplyr::n())) # %>% unnest_longer(c(cs, prob))
}


#' set xi, update tau
set_xi <- function(fit, xi) {
  fit$params$xi <- xi
  fit$params$tau <- compute_tau(fit)
  return(fit)
}



#from https://stackoverflow.com/questions/19465720/writing-a-function-to-calculate-divisors-in-r

divisors <- function(x){
  #  Vector of numberes to test against
  y <- seq_len(x)
  #  Modulo division. If remainder is 0 that number is a divisor of x so return it
  y[ x%%y == 0 ]
}

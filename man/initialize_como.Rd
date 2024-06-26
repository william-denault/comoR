% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/como.R
\name{initialize_como}
\alias{initialize_como}
\title{Function implementation the como mode}
\usage{
initialize_como(
  scales,
  n,
  p,
  p2,
  mu0 = 0,
  var0 = 1,
  nullweight = 0,
  mnreg_type = "constant",
  epoch = 10,
  batch_size = 100,
  param_nnet = list(size = 1, decay = 1),
  prior = c("mix_norm", "mix_exp"),
  g = NULL,
  verbose_keras = 0
)
}
\arguments{
\item{data}{an object of class data_como  see \link{\code{set_data_como}}}

\item{modeltype}{of model currently supported (normal and beta )}

\item{maxiter}{numeric, maximum numerous of iteration set to 100 by defaults}

\item{tol}{tolerance in term of change in ELBO value for stopping criterion}

\item{upper, }{logical, set to FALSE by default. Specific to beta distribution.
If true use a to set of mixture for fitting both end of the of the distribution as in the ZAP paper by Leung and Sunn}
}
\description{
Function implementation the como mode
}
\details{
Function implementation the como mode
}


# Covariate moderated normal mean methods

<!-- badges: start -->
<!-- badges: end -->

comoR implements covariate moderated Empirical Bayes matrix factorization methods

## Installation

 
``` r
devtools::install_github('william-denault/comoR')
```
## Usage
comoR has two main functions: cEBMF and como. cEBMF is a function that performs covariate moderated Empirical Bayes matrix factorization. como is a function that solves that covariate moderated Empirical Bayes normal mean (cEBNM) problem.
como can be easily interfaced with the flashier package to perform cEBMF using the flashier framework

 


## Example

For example of different cEBNM solvers (mixture of normal and mixture of exponential)
please have a look at the vignette 'custom cEBNM with tensorflow" and  "covaraite_moderated_exponential_mixture".


We also show how to solve the Empirical Bayes normal mean (EBNM) problem in the vignette "fitting a mixture of exponential using mix SQP".
This vignette showcases how to fit mixture of exponential prior using mixsqp and sequential quadratic programming method for fast maximum-likelihood estimation.



``` r

This is a basic example which shows you how to solve a common problem:

``` r
library(comoR)
## basic example code
```


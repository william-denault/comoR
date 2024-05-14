library(dplyr)
library(keras)
library(tensorflow)
set.seed(1)
# simulate from a exponential mixture cebnm problem --------

simulate <- function(){
  n <- 1000
  p <- 100
  K <- 10

  logsumexp <- function(x){
    m <- max(x)
    return(log(sum(exp(x-m))) + m)
  }

  softmax <- function(x){
    exp(x - logsumexp(x))
  }

  X <- matrix(rnorm(n*p), nrow=n)
  A <- matrix(rnorm(p*K), nrow=p) * 0.5

  Z <- X %*% A
  pi <- do.call(rbind, purrr::map(1:n, ~softmax(Z[.x,])))
  pimax <- purrr::map_dbl(1:n, ~max(pi[.x,]))
  z <- purrr::map_int(1:n, ~which(rmultinom(1, 1, pi[.x,])[,1] == 1))


  scales <- c(0, 2^seq(1:9)) #sqrt(var(X))
  rates <- pmin(sqrt(1/scales), 1e5)
  mu <- rexp(n, rate = rates[z])
  betahat <- rnorm(n) + mu
  sehat <- rep(1, n)
  return(list(betahat=betahat, sehat=sehat, X=X))
}

#-----------------------

#' Initialize keras nn
make_nn_parameters <- function(p, num_classes, l2_reg=0.001){
  l2_reg <- 0.001
  #define the nnet paramet using Keras syntax
  param_nnet <- keras_model_sequential() %>%
    layer_dense(units = 64,
                activation = 'relu',
                input_shape = c(p),
                kernel_regularizer = regularizer_l2(l2_reg)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 64,
                activation = 'relu',
                kernel_regularizer = regularizer_l2(l2_reg)) %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = num_classes,
                activation = 'softmax')
  return(param_nnet)
}


#' Convolution between  an exponential and Normal distribution
exponential_convolved_logpdf <- function(betahat, se, scale) {
  if(scale < 1e-6){
    return(dnorm(betahat, 0, se, log=T))
  } else{
    rate <- 1/scale
    return (log(rate) + 0.5*se^2 * rate^2 - betahat * rate + pnorm(betahat/se - se * rate, log.p=TRUE))
  }
}

#' log marginal likelihood of the mixture
#' @param L n x K matrix with log likelihood conditional on component membership
#' @param pi n x K matrix of prior probabilities of component membership for each observation
custom_loss2 <- function(L, pi){
  return(-sum(tf$reduce_logsumexp(L + log(tf$maximum(pi, 1e-5)), axis=as.integer(1))))
}


#' Fit a simple multi-layer perceptron
#' @param X an n x p matrix of observation level covariates
#' @param L the n x K matrix of log likelihoods for each observation for a fixed grid of mixture components
#' @param param_nnet a keras model, if NULL we will initialize with `make_nn_parameters`
fit_nn <- function(X, L, param_nnet = NULL){
  if(is.null(param_nnet)){
    param_nnet <- make_nn_parameters(ncol(X), ncol(L))
  }
  model <- keras::clone_model(param_nnet)
  custom_loss21 <- function(L, pi){
    # Ensure pi and the operations involving it are cast to float32
    pi_casted <- tf$cast(pi, tf$float32)
    L_casted <- tf$cast(L, tf$float32)

    # Use tf$reduce_logsumexp with casted tensors
    loss <- -k_sum(tf$reduce_logsumexp(L_casted + k_log(k_maximum(pi_casted, 1e-5)), axis=1L))

    lambda <- 0.01  # L1 regularization strength

    l1_penalty <- k_constant(0, dtype = tf$float32)
    for(weight in model$trainable_weights) {
      l1_penalty <- l1_penalty + k_sum(k_abs(tf$cast(weight, tf$float32)))
    }

    # Total loss: original loss + L1 penalty, ensuring consistent data types
    total_loss <- loss + lambda * l1_penalty

    return(total_loss)
  }
  model <-  model %>% compile(
    loss = custom_loss21,
    optimizer = optimizer_sgd(learning_rate = 0.01, momentum = 0.9),
    metrics = c('accuracy')
  )

  x_train=X
  y_train= L
  candidate_batch_size =divisors(nrow(y_train))
  idx = which.min(abs( divisors(nrow(y_train))-400))
  custom_batch_size <- candidate_batch_size[idx]
  history <- model %>% fit(
    x_train, y_train,
    epochs =200,
    batch_size = custom_batch_size,
    verbose  = TRUE
  )
  return(model)
}


#' Make a covariate moderated EBNM problem with data X
make_cebnm_expmix_fun <- function(X, num_classes=20){
  cebnm_expmix_nn <- function(x,s,g_init=FALSE,fix_g=TRUE, output){

    if (length(x) == 3){ ### just to satisfy check of custom function
      return (ebnm_flat(x))
    }

    # make a grid of scale parameters
    num_classes <- 20
    scales <- autoselect_scales_mix_exp(x, s, num_classes)
    rates <- 1/scales

    # compute log likelihood of each observation under each mixture component
    L <- do.call(cbind, purrr::map(scales, ~exponential_convolved_logpdf(x, s, .x)))
    model <- fit_nn(X, L)
    prediction <- predict(model, X)

    loglik <- -as.numeric(custom_loss2(L, prediction))

    # compute posterior mean and variance, for exp-normal it's truncated normal
    means <- (x - outer(s^2, rates, '*'))
    sds <- outer(s, rep(1, length(rates)))

    Mu <- ashr::my_etruncnorm(0, Inf, mean=means, sd=sds)
    mu <- Matrix::rowSums(Mu[, 2:num_classes] * prediction[, 2:num_classes])

    Mu2 <- ashr::my_e2truncnorm(0, Inf, mean=means, sd=sds)
    mu2 <- Matrix::rowSums(Mu2[, 2:num_classes] * prediction[, 2:num_classes])

    # put everything into ebnm style output to interface with flashier
    g <- ebnm::gammamix(rep(1/num_classes, num_classes), shape=1, scale=scales)
    out <- list( data= data.frame(x=x,s=s),
                 posterior = data.frame(mean= mu, second_moment=mu2) ,
                 fitted_g = g,
                 log_likelihood=loglik,
                 nn = list(model)
    )
  }
  return(cebnm_expmix_nn)
}

devtools::load_all(".")

# William's example:
  x <-runif(1000)
  y <-runif(1000)
  X = cbind(x,y)

  library(flashier)
  library(ggplot2)
  library(keras)
  library(tensorflow)

  set.seed(3)#problem fro set.seed(1)

  f <- matrix(NA, nrow = 3, ncol =200)
  for ( i in 1:ncol (f)){

    t1<- sample (c(0,1), size=1)
    t2<- sample (c(0,1), size=1)

    f[1,i] <- t1*rnorm(n=1)
    f[2,i] <- t2*rnorm(n=1)

    f[3,i] <- t2*rnorm(n=1)

  }
  L <- matrix(NA, ncol=3, nrow=length(x))

  factor  <- c()

  for (i in 1:length(x)){

    if ( (x[i] <.33 & y[i] <.33 )|(x[i] >.33 & y[i] >.33 &  x[i] <.66 & y[i] <.66) | (x[i] >.66 & y[i] >.66 )){
      L[i,] <- c(1,0,0)
      factor=c(factor,1)
    }else{
      if ( (x[i] <.33 & y[i] >.66 )|(x[i] >.33 & y[i] <.33 &  x[i] <.66  ) | (x[i] >.66 & y[i] >.33  & y[i] <.66)){
        L[i,] <- c(0,1,0)
        factor=c(factor,2)
      }else{
        L[i,] <- c(0,0,1)
        factor=c(factor,3)
      }
    }
  }

  df = data.frame(x=x,y=y, Factor=as.factor(factor))

  colors <- c("#D41159","#1A85FF","#40B0A6" )

  df <- data.frame(x=x,y=y, L=  L[,1])
  P01 <- ggplot(df, aes ( x,y, col = L ))+
    geom_point(size=2)+
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
    geom_hline(yintercept = 0.33)+
    geom_hline(yintercept = 0.66)+
    geom_vline(xintercept = 0.66)+
    geom_vline(xintercept = 0.33)+
    ylab("Ground Truth ")+xlab(" ")+ ggtitle("factor 1")+
    theme_minimal()+theme( axis.text.y=element_blank() ,

                           axis.ticks.y=element_blank(),
                           axis.text.x=element_blank(),
                           axis.ticks.x=element_blank(),
                           plot.title = element_text(size = 20), # Set plot title size
                           axis.title.x = element_text(size = 20), # Set X axis title size
                           axis.title.y = element_text(size = 20))
  df <- data.frame(x=x,y=y, L=  L[,2])
  P02 <- ggplot(df, aes ( x,y, col = L ))+
    geom_point(size=2)+
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
    geom_hline(yintercept = 0.33)+
    geom_hline(yintercept = 0.66)+
    geom_vline(xintercept = 0.66)+
    geom_vline(xintercept = 0.33)+
    ylab(" ")+xlab(" ")+ ggtitle("factor 2")+
    theme_minimal()+theme( axis.text.y=element_blank(),

                           axis.ticks.y=element_blank(),
                           axis.text.x=element_blank(),
                           axis.ticks.x=element_blank(),
                           plot.title = element_text(size = 20), # Set plot title size
                           axis.title.x = element_text(size = 20), # Set X axis title size
                           axis.title.y = element_text(size = 20))

  df <- data.frame(x=x,y=y, L=  L[,3])
  P03 <- ggplot(df, aes ( x,y, col = L ))+
    geom_point(size=2)+
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
    geom_hline(yintercept = 0.33)+
    geom_hline(yintercept = 0.66)+
    geom_vline(xintercept = 0.66)+
    geom_vline(xintercept = 0.33)+
    ylab(" ")+xlab(" ")+ ggtitle("factor 3")+
    theme_minimal()+theme( axis.text.y=element_blank(),

                           axis.ticks.y=element_blank(),
                           axis.text.x=element_blank(),
                           axis.ticks.x=element_blank(),
                           plot.title = element_text(size = 20), # Set plot title size
                           axis.title.x = element_text(size = 20), # Set X axis title size
                           axis.title.y = element_text(size = 20))

  Z = L%*%f + matrix(rnorm(nrow(L)* ncol(f), sd=1.5), nrow = nrow(L))



  library(flashier)
  fit_default <- flash_init(Z, var_type = 2) %>%
    flash_set_verbose(0) %>%
    flash_greedy(
      Kmax = 3,
      ebnm_fn = c(ebnm_point_exponential,  ebnm_point_exponential),
      maxiter=50
    )
  library(keras)
  library(tensorflow)
  library(comoR)
  library(flashier)


  cebnm_L <- make_cebnm_expmix_fun(X, 10)

  # fit_custom1 <- flash_init(Z, var_type = 2) %>%
  #   flash_set_verbose(0) %>%
  #   flash_greedy(
  #     Kmax = 1,
  #     ebnm_fn = c(cebnm_L, ebnm_ash),
  #     maxiter=500
  #   )
  #
  # fit_custom2 <- fit_custom1 %>%
  #   flash_greedy(
  #     Kmax = 1,
  #     ebnm_fn = c(cebnm_L, ebnm_ash),
  #     maxiter=500
  #   )
  #
  # fit_custom3 <- fit_custom2 %>%
  #   flash_set_verbose(3) %>%
  #   flash_greedy(
  #     Kmax = 1,
  #     ebnm_fn = c(cebnm_L, ebnm_ash),
  #     maxiter=500
  #   )
  #
  # fit_custom4 <- fit_custom3 %>%
  #   flash_set_verbose(3) %>%
  #   flash_greedy(
  #     Kmax = 1,
  #     ebnm_fn = c(cebnm_L, ebnm_ash),
  #     maxiter=500
  #   )

  fit_custom <- flash_init(Z, var_type = 2) %>%
    flash_set_verbose(0) %>%
    flash_greedy(
      Kmax = 3,
      ebnm_fn = c(cebnm_L,  ebnm_point_exponential),
      maxiter=50
    )

  svd_res  = svd(Z)

  df <- data.frame(x=x,y=y, L= fit_custom$L_pm[,1])
  P11 <- ggplot(df, aes ( x,y, col = L ))+
    geom_point(size=2)+
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
    geom_hline(yintercept = 0.33)+
    geom_hline(yintercept = 0.66)+
    geom_vline(xintercept = 0.66)+
    geom_vline(xintercept = 0.33)+
    ylab(" cEBMF")+xlab(" ")+
    ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank() ,

                                       axis.ticks.y=element_blank(),
                                       axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank(),
                                       plot.title = element_text(size = 20), # Set plot title size
                                       axis.title.x = element_text(size = 20), # Set X axis title size
                                       axis.title.y = element_text(size = 20))


  df <- data.frame(x=x,y=y, L= fit_default$L_pm[,1])
  P12 <- ggplot(df, aes ( x,y, col = L ))+
    geom_point(size=2)+
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
    geom_hline(yintercept = 0.33)+
    geom_hline(yintercept = 0.66)+
    geom_vline(xintercept = 0.66)+
    geom_vline(xintercept = 0.33)+
    ylab(" EBMF")+xlab(" ")+
    ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank(),

                                       axis.ticks.y=element_blank(),
                                       axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank(),
                                       plot.title = element_text(size = 20), # Set plot title size
                                       axis.title.x = element_text(size = 20), # Set X axis title size
                                       axis.title.y = element_text(size = 20))


  df <- data.frame(x=x,y=y, L= svd_res$u[,1] )
  P13 <- ggplot(df, aes ( x,y, col = L ))+
    geom_point(size=2)+
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
    geom_hline(yintercept = 0.33)+
    geom_hline(yintercept = 0.66)+
    geom_vline(xintercept = 0.66)+
    geom_vline(xintercept = 0.33)+
    ylab(" SVD")+xlab(" ")+
    ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank(),

                                       axis.ticks.y=element_blank(),
                                       axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank(),
                                       plot.title = element_text(size = 20), # Set plot title size
                                       axis.title.x = element_text(size = 20), # Set X axis title size
                                       axis.title.y = element_text(size = 20))


  df <- data.frame(x=x,y=y, L= fit_custom$L_pm[,2])

  P21 <-ggplot(df, aes ( x,y, col =  L  ))+
    geom_point(size=2)+
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
    geom_hline(yintercept = 0.33)+
    geom_hline(yintercept = 0.66)+
    geom_vline(xintercept = 0.66)+
    geom_vline(xintercept = 0.33)+
    ylab("  ")+xlab(" ")+
    ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank(),

                                       axis.ticks.y=element_blank(),
                                       axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank())



  df <- data.frame(x=x,y=y, L= fit_default$L_pm[,2])
  P22 <-ggplot(df, aes ( x,y, col = L ))+
    geom_point(size=2)+
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
    geom_hline(yintercept = 0.33)+
    geom_hline(yintercept = 0.66)+
    geom_vline(xintercept = 0.66)+
    geom_vline(xintercept = 0.33)+
    ggtitle(" ")+theme_minimal()+
    xlab(" ")+ylab(" ")+
    theme( axis.text.y=element_blank(),

           axis.ticks.y=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank())



  df <- data.frame(x=x,y=y, L= svd_res$u[,1] )
  P23 <- ggplot(df, aes ( x,y, col = L ))+
    geom_point(size=2)+
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
    geom_hline(yintercept = 0.33)+
    geom_hline(yintercept = 0.66)+
    geom_vline(xintercept = 0.66)+
    geom_vline(xintercept = 0.33)+
    ylab("  ")+xlab(" ")+
    ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank(),

                                       axis.ticks.y=element_blank(),
                                       axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank())


  df <- data.frame(x=x,y=y, L= fit_custom$L_pm[,3])
  P31 <-ggplot(df, aes ( x,y, col = abs( L)  ))+
    geom_point(size=2)+
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
    geom_hline(yintercept = 0.33)+
    geom_hline(yintercept = 0.66)+
    geom_vline(xintercept = 0.66)+
    geom_vline(xintercept = 0.33)+
    ylab("  ")+xlab(" ")+
    ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank(),
                                       axis.ticks.y=element_blank(),
                                       axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank())


  df <- data.frame(x=x,y=y, L= fit_default$L_pm[,3])
  P32 <-ggplot(df, aes ( x,y, col = L ))+
    geom_point(size=2)+
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
    geom_hline(yintercept = 0.33)+
    geom_hline(yintercept = 0.66)+
    geom_vline(xintercept = 0.66)+
    geom_vline(xintercept = 0.33)+
    xlab(" ")+ylab(" ")+
    ggtitle(" ")+theme_minimal()+theme( axis.text.y=element_blank(),

                                        axis.ticks.y=element_blank(),
                                        axis.text.x=element_blank(),
                                        axis.ticks.x=element_blank())


  df <- data.frame(x=x,y=y, L= svd_res$u[,1] )
  P33 <- ggplot(df, aes ( x,y, col =  L  ))+
    geom_point(size=2)+
    scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0  ) +
    geom_hline(yintercept = 0.33)+
    geom_hline(yintercept = 0.66)+
    geom_vline(xintercept = 0.66)+
    geom_vline(xintercept = 0.33)+
    ylab("  ")+xlab(" ")+
    ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank() ,

                                       axis.ticks.y=element_blank(),
                                       axis.text.x=element_blank(),
                                       axis.ticks.x=element_blank(),
                                       plot.title = element_text(size = 20), # Set plot title size
                                       axis.title.x = element_text(size = 20), # Set X axis title size
                                       axis.title.y = element_text(size = 20))


  library(cowplot)
  library(grid)
  #legend <- get_legend(
  #  P13+
  #    guides(color = guide_legend(nrow = 1)) #+
  # theme(legend.position = "bottom")
  # )
  # legend1 <- get_legend(
  #  P1
  #)



  fit_factor = ggdraw() +
    draw_plot(P01 + theme(legend.position = "none" ), x = 0   , y = 0.75, width= 0.25, height= 0.25) +
    draw_plot(P02 + theme(legend.position = "none"), x = 0.3 , y = 0.75, width= 0.25, height= 0.25) +
    draw_plot(P03 + theme(legend.position = "none"), x = 0.6 , y = 0.75, width= 0.25, height= 0.25) +
    draw_plot(P11 + theme(legend.position = "none"), x = 0   , y = 0.5, width= 0.25, height= 0.25) +
    draw_plot(P21 + theme(legend.position = "none"), x = 0.3 , y = 0.5, width= 0.25, height= 0.25) +
    draw_plot(P31 + theme(legend.position = "none"), x = 0.6 , y = 0.5, width= 0.25, height= 0.25) +

    draw_plot(P12 + theme(legend.position = "none"), x = 0   , y = 0.25, width= 0.25, height= 0.25) +
    draw_plot(P22 + theme(legend.position = "none"), x = 0.3 , y = 0.25, width= 0.25, height= 0.25) +
    draw_plot(P32 + theme(legend.position = "none"), x = 0.6 , y = 0.25, width= 0.25, height= 0.25) +

    draw_plot(P13 + theme(legend.position = "none"), x = 0   , y = 0.0 , width= 0.25, height= 0.25) +
    draw_plot(P23 + theme(legend.position = "none"), x = 0.3 , y = 0.0 , width= 0.25, height =0.25) +
    draw_plot(P33 + theme(legend.position = "none"), x = 0.6 , y = 0.0 , width= 0.25, height= 0.25)
  fit_factor


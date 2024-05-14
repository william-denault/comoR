
set.seed(1)#problem fro set.seed(1)
x <-runif(1000)
y <-runif(1000)
X = cbind(x,y)
plot (x,y)
library(flashier)
library(ggplot2)
library(keras)

library(tensorflow)

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
    L[i,] <- c(abs(rnorm(1)),0,0)
    factor=c(factor,1)
  }else{
    if ( (x[i] <.33 & y[i] >.66 )|(x[i] >.33 & y[i] <.33 &  x[i] <.66  ) | (x[i] >.66 & y[i] >.33  & y[i] <.66)){
      L[i,] <- c(0,abs(rnorm(1)),0)
      factor=c(factor,2)
    }else{
      L[i,] <- c(0,0,abs(rnorm(1)))
      factor=c(factor,3)
    }
  }


}

df = data.frame(x=x,y=y, Factor=as.factor(factor))

colors <- c("#D41159","#1A85FF","#40B0A6" )
#P1 <- ggplot(df, aes(x,y, col=Factor))+geom_point(size=3)+
#
#  geom_hline(yintercept = 0.33)+
#  geom_hline(yintercept = 0.66)+
#  geom_vline(xintercept = 0.66)+
#  geom_vline(xintercept = 0.33)+
#  xlab("")+ylab("")+
#  scale_color_manual(values = colors)+
#  theme_minimal()+theme( axis.text.y=element_blank(),element_text(size = 20),

#                         axis.ticks.y=element_blank(),
#                         axis.text.x=element_blank(),
#                         axis.ticks.x=element_blank())

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






Z = L%*%f + matrix(rnorm(nrow(L)* ncol(f), sd=6 ), nrow = nrow(L))
library(NNLM)
library(ggplot2)
library(cowplot)
set.seed(1)
cluster_colors <- c("darkorange","dodgerblue","darkblue")

sim <-  data.frame(x = x,y = y,cluster = 0)
for (k in 1:3)
  sim[ L[,k]> 0,"cluster"] <- k
sim <- transform(sim,cluster = factor(cluster))
p1 <- ggplot(sim,aes(x = x,y = y,color = cluster)) +
  geom_point() +
  scale_color_manual(values = cluster_colors) +
  theme_cowplot(font_size = 12)
p1
pca <- prcomp(Z)
pdat2 <- cbind(sim,pca$x[,1:2])
p2 <- ggplot(pdat2,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point() +
  scale_color_manual(values = cluster_colors) +
  theme_cowplot(font_size = 12)
p2
res <- kmeans(pca$x[,1:2],centers = 3)
print(table(true = sim$cluster,est = res$cluster))
library(flashier)


pca <- prcomp(Z)
pdat2 <- cbind(sim,pca$x[,1:3])
fit_flash <- flash_init(Z, var_type = 2, S=0.01)
fit_flash <- flash_factors_init(fit_flash,
                                list(pca$x[,1:3],pca$rotation[,1:3]),
                                ebnm_fn = c(ebnm_ash , ebnm_ash))
fit_flash <- flash_backfit(fit_flash)
fit_default<-fit_flash

library(keras)
library(tensorflow)
library(comoR)


cebnm_L <- function( x,s,g_init=FALSE,fix_g=TRUE, output){

  if (length(x) == 3){ ### just to satisfy check of custom function
    return (ebnm_flat(x))
  }
  Z <- matrix( 1, nrow=length(x), ncol=1)
  param_como = list(max_class= 10,
                    mnreg_type="keras",
                    prior    ='mix_norm',
                    epoch     =300)
  data <- comoR:::como_prep_data (betahat=x,
                                  se=s, X=X,
                                  Z =Z )

  # you need to retreive the actual number of mixture component in the model
  num_classes <- length( autoselect_scales_mix_norm(data$betahat, data$se,10))

  #define the nnet paramet using Keras syntax
  param_nnet =keras_model_sequential() %>%
    layer_dense(units = 64,
                activation = 'relu',
                input_shape = c(ncol(X))) %>%
    layer_dense(units = 64,
                activation = 'relu' ) %>%
    layer_dense(units = 64,
                activation = 'relu' ) %>%
    layer_dense(units = 64,
                activation = 'relu' ) %>%
    layer_dense(units = num_classes,
                activation = 'softmax')

  # run comoR
  fit  <- rlang::exec( "data_initialize_como", !!! param_como ,
                       data= data,
                       param_nnet= param_nnet) # initialize the model from the data
  fit <- comoR:::fit.como (  fit, data, max_iter = 1)


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

X_f =matrix(rnorm(2* ncol(Z), sd=3), nrow = ncol(Z))

l2_reg=0.1

X_l=X
library(flashier)

pca <- prcomp(Z)
pdat2 <- cbind(sim,pca$x[,1:3])
fit_flash <- flash_init(Z, var_type = 2, S=0.01)
fit_flash <- flash_factors_init(fit_flash,
                                list(pca$x[,1:3],pca$rotation[,1:3]),
                                ebnm_fn = c(cebnm_L , ebnm_ash))
fit_flash <- flash_backfit(fit_flash)
fit_custom<-fit_flash






svd_res  = svd(Z)
#load a spatial data
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/spatial_RNA_seq/res_spatial_PCA/run_spatial_DLPFC9.RData")


library(SpatialPCA)
LIBD @normalized_expr =t(Z)
LIBD @location =X
LIBD = SpatialPCA_buildKernel(LIBD, kerneltype="gaussian", bandwidthtype="SJ",bandwidth.set.by.user=NULL)
LIBD = SpatialPCA_EstimateLoading(LIBD,fast=FALSE,SpatialPCnum=20)
LIBD = SpatialPCA_SpatialPCs(LIBD, fast=FALSE)


#save(fit_default, fit_custom,LIBD ,svd_res ,Z, L,f , X, file = "fit_plot_Neurips.RData")




df <- data.frame(x=x,y=y, L=fit_custom$L_pm[,1])
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


df <- data.frame(x=x,y=y, L= LIBD@SpatialPCs[1,] )
P13 <- ggplot(df, aes ( x,y, col = L ))+
  geom_point(size=2)+
  scale_color_gradient2(low = "blue", mid = "grey", high = "red", midpoint = 0) +
  geom_hline(yintercept = 0.33)+
  geom_hline(yintercept = 0.66)+
  geom_vline(xintercept = 0.66)+
  geom_vline(xintercept = 0.33)+
  ylab("Spatial PCA")+xlab(" ")+
  ggtitle("")+theme_minimal()+theme( axis.text.y=element_blank(),

                                     axis.ticks.y=element_blank(),
                                     axis.text.x=element_blank(),
                                     axis.ticks.x=element_blank(),
                                     plot.title = element_text(size = 20), # Set plot title size
                                     axis.title.x = element_text(size = 20), # Set X axis title size
                                     axis.title.y = element_text(size = 20))


df <- data.frame(x=x,y=y, L=  fit_custom$L_pm[,2])

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



df <- data.frame(x=x,y=y, L=  fit_default$L_pm[,2])
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



df <- data.frame(x=x,y=y, L=LIBD@SpatialPCs[2,] )
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


df <- data.frame(x=x,y=y, L=  fit_custom$L_pm[,3])
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


df <- data.frame(x=x,y=y, L=   fit_default$L_pm[,3])
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


df <- data.frame(x=x,y=y, L=LIBD@SpatialPCs[3,] )
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

#ggsave(fit_factor , file="Fig_tilling2.pdf",
#       width =21 ,
#       height = 25,
#       units = "cm"
#)

hist(cEBMF.obj$loading
     [,c(1:3)], nclass = 100)

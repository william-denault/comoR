

set.seed(3)#problem for set.seed(1)

x <-runif(1000)
y <-runif(1000)
X = cbind(x,y)

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

Z = L%*%f + matrix(rnorm(nrow(L)* ncol(f), sd=3 ), nrow = nrow(L))


library(ggplot2)
library(cowplot)
set.seed(1)
cluster_colors <- c("darkorange","dodgerblue","darkblue")

sim <- data.frame(x = x,y = y,cluster = 0)
for (k in 1:3)
  sim[L[,k] == 1,"cluster"] <- k
sim <- transform(sim,cluster = factor(cluster))
pca <- prcomp(Z)
pdat <- cbind(sim,pca$x[,1:2])
ggplot(pdat,aes(x = PC1,y = PC2,color = cluster)) +
  geom_point() +
  scale_color_manual(values = cluster_colors) +
  theme_cowplot(font_size = 12)

ggplot(pdat,aes(x = PC1,y = PC2)) +
  geom_point() +
  scale_color_manual(values = cluster_colors) +
  theme_cowplot(font_size = 12)







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






l2_reg=0.01
X_l =X

X_f =matrix(rnorm(2* ncol(Z), sd=3), nrow = ncol(Z))

param_nnet.x =keras_model_sequential() %>%
  layer_dense(units = 64,
              activation = 'relu',
              input_shape = c(ncol(X_l))) %>%
  layer_dense(units = 64,
              activation = 'relu',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 64,
              activation = 'relu',
              kernel_regularizer = regularizer_l2(l2_reg)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 64,
              activation = 'relu' ) %>%
  layer_dense(units = 10,
              activation = 'softmax')


param_nnet.y =keras_model_sequential() %>%
  layer_dense(units = 64,
              activation = 'relu',
              input_shape = c(ncol(X_f))) %>%
  layer_dense(units = 10,
              activation = 'softmax')

library(softImpute)


res <-cEBMF  ( Y=Z,
               X_l,
               X_f,
               mnreg_type.x="keras",
               mnreg_type.y="constant_mnreg",
               K=3,
               type_noise='constant',
               init_type="flashier_NMF",
               maxit=20,
               tol=1e-3 ,
               param_como.x  = list(max_class=10,mnreg_type="keras",
                                    prior="mix_exp" ,
                                    epoch     =50,
                                    batch_size= 500),
               param_como.y  = list(max_class=10,mnreg_type='constant_mnreg',
                                    prior="mix_exp"  ),
               param_nnet.x  =param_nnet.x ,

               maxit_como  = 1)







load("C:/Document/Serieux/Travail/Data_analysis_and_papers/spatial_RNA_seq/res_spatial_PCA/run_spatial_DLPFC9.RData")


library(SpatialPCA)
LIBD @normalized_expr =t(Z)
LIBD @location =X
LIBD = SpatialPCA_buildKernel(LIBD, kerneltype="gaussian", bandwidthtype="SJ",bandwidth.set.by.user=NULL)
LIBD = SpatialPCA_EstimateLoading(LIBD,fast=FALSE,SpatialPCnum=20)
LIBD = SpatialPCA_SpatialPCs(LIBD, fast=FALSE)



svd_res  = svd(Z)

df <- data.frame(x=x,y=y, L= res$loading[,1])
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


df <- data.frame(x=x,y=y, L= res$loading[,2])

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


df <- data.frame(x=x,y=y, L= res$loading[,3])
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


df <- data.frame(x=x,y=y, L= 0*fit_default$L_pm[,2])
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
hist(res$loading, nclass = 100)



library(scatterpie)

d0 <- data.frame(x=X[,1], y=X[,2])

d <- data.frame(x=X[,1], y=X[,2])
tdf =   do.call ( cbind, lapply (1:ncol(res$loading), function (i) {
  res$loading[,i]
}))
colnames(tdf) <- LETTERS[1:ncol( res$loading)]
d <- cbind(d, tdf)
d <- data.frame(d)
P0 <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d , cols=c("A", "B", "C" ),
                                 pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
  scale_fill_manual(values =c("#9C9EDE" ,"#5CB85C" ,"#E377C2" ))+
  theme( axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         legend.position = "none")


P0



d <- data.frame(x=X[,1], y=X[,2])
tdf =   do.call ( cbind, lapply (1:ncol(fit_default$L_pm), function (i) {
  fit_default$L_pm[,i]
}))
colnames(tdf) <- LETTERS[1:ncol(fit_default$L_pm)]
d <- cbind(d, tdf)
d <- data.frame(d)

P2  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=c("A", "B"  ) ,
                                  pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
  scale_fill_manual(values =c("#9C9EDE" ,"#5CB85C" ,"#E377C2" ))+
  theme( axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         legend.position = "none")


library(gridExtra)
grid.arrange(P0, P2, ncol=2)

file_pc <-  list(x=x,y=y, L=  L,
                 f=f,
                 Z=Z,
                 fit_custom=res,
                 fit_default=fit_default,
                 LIBD=LIBD
)
save(file_pc, file = "fit_plot_Neurips.RData")

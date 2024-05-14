
library(keras)
library(tensorflow)
library(comoR)
library(flashier)
library(ggplot2)
library(fclust)
library(scatterpie)
library(gridExtra)
library(softImpute)
library(NNLM)

load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/DLPFC/LIBD_sample12.RData")
load("C:/Document/Serieux/Travail/Data_analysis_and_papers/cEBMF_RCC_experiments/data/res_spatial_PCA/run_spatial_DLPFC12.RData")
tt =  as.matrix(count_sub)
truth = KRM_manual_layers_sub$layer_guess_reordered[match(colnames(LIBD@normalized_expr),colnames(count_sub))]

loc =  LIBD @location
tt0 = ( t(as.matrix(LIBD@normalized_expr)) )

dim(tt0)
count_sub = count_sub[match(colnames(tt0),rownames(count_sub)),]
dim(count_sub)

tt0 <-  log(t(count_sub)+1)

maxit=100
X=loc
clusterNum=c(7,7,7,7,5,5,5,5,7,7,7,7) # each sample has different ground truth cluster number
i=12

fit_default <-   flash_init(tt0, var_type = 2, S=0.01) %>%

  flash_set_verbose(0) %>%
  flash_greedy(
    Kmax  = clusterNum[i ],
    ebnm_fn = c(ebnm_point_exponential, ebnm_point_exponential)
  )

fit_nmf <- nnmf(scale(tt0,center = TRUE,scale = FALSE),
            k = 7,method = "scd",loss = "mse",verbose = 2,
            n.threads = 2,rel.tol = 1e-8,max.iter = 200)
prop <-  fit_nmf $W/rowSums(fit_nmf$W)
res_nmf <- Fclust.compare(truth, prop)
res_nmf

my_col= c("#9C9EDE" ,"#5CB85C" ,"#E377C2", "#4DBBD5" ,"#FED439" ,"#FF9896", "#FFDC91")

l2_reg=0.001
Y=tt0
X_l =X

X_f =matrix(rnorm(2* ncol(tt0), sd=3), nrow = ncol(tt0))

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
  layer_dense(units = 10,
              activation = 'softmax')


param_nnet.y =keras_model_sequential() %>%
  layer_dense(units = 64,
              activation = 'relu',
              input_shape = c(ncol(X_f))) %>%
  layer_dense(units = 10,
              activation = 'softmax')

mnreg_type="keras"
K=3
type_noise='column_wise'
init_type="flashier_NMF"
maxit=10
tol=1e-3

param_como2 = list()
param_susie =  list(L=5)
maxit_como  = 2

param_como.x  = list(max_class=10,mnreg_type="keras",
                     prior="mix_exp" ,
                     epoch     =150,
                     batch_size= 1500)
param_como.y  = list(max_class=10,mnreg_type="constant_mnreg",
                     prior="mix_exp"  )
cEBMF.obj <- comoR:::init_cEBMF (Y,
                                 X_l,
                                 X_f,
                                 mnreg_type.x="keras",
                                 mnreg_type.y="constant_mnreg",
                                 K=clusterNum[i],
                                 type_noise    = type_noise,
                                 init_type     = init_type,
                                 param_como.x  =  param_como.x,
                                 param_como.y  =  param_como.y,
                                 maxit_como    = 1,
                                 param_nnet.x  = param_nnet.x,
                                 param_como2   = param_como2,
                                 param_susie   = param_susie )

 ### Need to carry info about como obj

  cEBMF.obj <- comoR:::cEBMF_iter  (cEBMF.obj)
  cEBMF.obj <- comoR:::out_prep.cEBMF(cEBMF.obj)


 res<- cEBMF.obj

 prop <-  fit_default$L_pm/rowSums(fit_default$L_pm)

 res_ebnm <- Fclust.compare(truth, prop)

 prop <- res$loading/rowSums(res$loading)

 res_cebnm <- Fclust.compare(truth, prop)

 res_ebnm
 res_cebnm
 res_nmf
# 0.259#.269#.271 lr =0.001

 d <- data.frame(x=loc[,1], y=loc[,2])
 tdf =   do.call ( cbind, lapply (1:ncol(res$loading), function (i) {
   res$loading[,i]
 }))
 colnames(tdf) <- LETTERS[1:ncol(res$loading)]
 d <- cbind(d, tdf)
 d <- data.frame(d)

 P1  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(res$loading)] ,
                                   pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()  +
   scale_fill_manual(values =my_col[1:ncol(res$loading)])+  theme( axis.text.y=element_blank(),
                                                                                   axis.ticks.y=element_blank(),
                                                                                   axis.text.x=element_blank(),
                                                                                   axis.ticks.x=element_blank(),
                                                                                   legend.position = "none")




 d <- data.frame(x=loc[,1], y=loc[,2])
 tdf =   do.call ( cbind, lapply (1:ncol(fit_default$L_pm), function (i) {
   fit_default$L_pm[,i]
 }))
 colnames(tdf) <- LETTERS[1:ncol(fit_default$L_pm)]
 d <- cbind(d, tdf)
 d <- data.frame(d)

 P2  <- ggplot() + geom_scatterpie(aes(x=x, y=y), data=d, cols=LETTERS[1:ncol(fit_default$L_pm)] ,
                                   pie_scale=0.35, color=NA) + coord_fixed()+ theme_minimal()+
   scale_fill_manual(values =my_col[1:ncol(fit_default$L_pm)]) +  theme( axis.text.y=element_blank(),
                                                                         axis.ticks.y=element_blank(),
                                                                         axis.text.x=element_blank(),
                                                                         axis.ticks.x=element_blank(),
                                                                         legend.position = "none")
 grid.arrange(P1, P2, ncol=2)

 library(e1071)
 # Example matrix

 # Fuzzy C-means clustering
 fuzzy_result <- cmeans(tt0, centers=7, m=2)  # 'm' is the fuzzifier

 # Membership matrix
 print(fuzzy_result$membership)
 cmean_clust <- Fclust.compare(truth, fuzzy_result$membership)



 res_ebnm
 res_cebnm
 cmean_clust



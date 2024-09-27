set.seed(1)
y <-runif(2000,min=-0.5, max=2.5)
X = cbind(y)
xtrue = 0*y
for (i in 1:length(xtrue)){


    xtrue[i]= rnorm(1,sd=0.5+ 2* (sin( pi*y[i])^2))


}

x = xtrue + rnorm(length(xtrue), sd=1)
s= rep(1,length(x))
Z <- matrix( 1, nrow=length(x), ncol=1)
plot (y,xtrue, main="true underlying effect",
      col =ifelse(xtrue==0, 3,4))
legend('topleft',c( expression(x[true] == 0) ,expression(x[true] != 0)),col=c(3,4),pch=1)

y <-runif(2000,min=-0.5, max=2.5)
X = cbind(y)
xtrue2 = 0*y
for (i in 1:length(xtrue)){


  xtrue2[i]= rnorm(1,sd=0.5+ 2* (cos( pi*y[i])^2))



}

x2 = xtrue2 + rnorm(length(xtrue), sd=1)
s= rep(1,length(x))
Z <- matrix( 1, nrow=length(x), ncol=1)
plot (y,xtrue2, main="true underlying effect",
      col =ifelse(xtrue==0, 3,4))
legend('topleft',c( expression(x[true] == 0) ,expression(x[true] != 0)),col=c(3,4),pch=1)
plot (c(y,y),c(xtrue2,xtrue), main="true underlying effect",
      col =ifelse(xtrue==0, 3,4))


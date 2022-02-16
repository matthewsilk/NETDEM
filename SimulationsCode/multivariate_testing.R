mu <- c(4.23, 3.01, 2.91)
stddev <- c(1.23, 0.92, 1.32)

corMat <- matrix(c(1, 0.78, 0.23,
                   0.78, 1, 0.27,
                   0.23, 0.27, 1),
                 ncol = 3)

covMat <- stddev %*% t(stddev) * corMat
covMat

stddev<-rep(10^0.5,50)

library(igraph)
network<-erdos.renyi.game(n=50,p.or.m=1,type="gnp",directed=FALSE,loops=FALSE)
networkm<-as_adjacency_matrix(network,type="both",sparse=FALSE)
weights<-matrix(rbeta(n=50^2,shape1=5,shape2=1.5),nr=50,nc=50)
network2m<-networkm*weights
for(i in 1:(ncol(network2m)-1)){
  for(j in (i+1):ncol(network2m)){
    network2m[j,i]<-network2m[i,j]
  }
}
network2m<-network2m/max(network2m)
max_cor<- 0.9
corMat<-max_cor*network2m
diag(corMat)<-1

covMat <- stddev %*% t(stddev) * corMat

library(Matrix)
covMat2<-as.matrix(Matrix::nearPD(covMat)$mat)

library(sna)

summary(netlm(y=covMat2,x=covMat))

#set.seed(1)
library(MASS)
dat1 <- mvrnorm(n = 1, mu = rep(0,50), Sigma = covMat2, empirical = FALSE)
network2<-graph.adjacency(network2m,mode="undirected",weighted=TRUE,diag=FALSE)
plot(network2,vertex.label=NA,vertex.size=25+dat1*2)







p <- length(mu)
if (!all(dim(Sigma) == c(p, p))) 
  stop("incompatible arguments")
if (EISPACK) 
  stop("'EISPACK' is no longer supported by R", domain = NA)
eS <- eigen(Sigma, symmetric = TRUE)
ev <- eS$values
if (!all(ev >= -tol * abs(ev[1L]))) 
  stop("'Sigma' is not positive definite")
X <- matrix(rnorm(p * n), n)
if (empirical) {
  X <- scale(X, TRUE, FALSE)
  X <- X %*% svd(X, nu = 0)$v
  X <- scale(X, FALSE, TRUE)
}
X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% 
  t(X)
nm <- names(mu)
if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) 
  nm <- dn[[1L]]
dimnames(X) <- list(nm, NULL)
if (n == 1) 
  drop(X)
else t(X)
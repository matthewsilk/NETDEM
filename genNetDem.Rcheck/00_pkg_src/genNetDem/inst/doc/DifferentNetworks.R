## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align="center",
  fig.width=8,
  fig.height=6)

## ----load_packages, message=FALSE,warning=FALSE-------------------------------

library(genNetDem)
library(asnipe)
library(igraph)
library(sna)
library(viridis)


## ----pop_gen, message=FALSE,warning=FALSE-------------------------------------

#Generate population
pop_info<-population_generation_basic(n=100,ng=100,plot=FALSE)
indiv_data<-pop_info[[1]]
dist_mat<-pop_info[[2]]


## ----trait_gen, message=FALSE,warning=FALSE-----------------------------------

indiv_info<-data.frame(indiv_data$indivs)
indiv_info<-indiv_info_gen(indiv_info,ii_tag=NULL,indiv_data=indiv_data,trait="sex",trait_type="fac",level_names=c("M","F"))
indiv_info<-indiv_info_gen(indiv_info[[1]],ii_tag=indiv_info[[2]],indiv_data,trait="size",trait_type="cov",x_dist="norm")


## ----net_gen, message=FALSE,warning=FALSE-------------------------------------

effs<-list()
effs[[1]]<-matrix(c(0,0,0,2,2,0,0,0),nr=1,nc=8)
effs[[1]]<-rbind(rep(0,8),effs[[1]])
effs[[2]]<-matrix(c(0,0,0,0,0,0,0,0),nr=1,nc=8)

net_info<-network_generation_covariates(indiv_data,dist_mat,indiv_info,
                                        p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                        p_og=0.05,wo_m=0.25,wo_v=0.025,
                                        d_effp=8,d_effw=8,
                                        covs=c(2,3),effs=effs,
                                        plot=TRUE)
pop_mat<-net_info[[1]]
pop_net<-net_info[[2]]


## ----show_net1,fig.width=8,fig.height=24--------------------------------------

par(mfrow=c(4,1))
cols<-rep("blue",nrow(indiv_info[[1]]))
cols[indiv_info[[1]]$sex=="M"]<-"red"          
plot(pop_net,vertex.label=NA,vertex.color=cols)

boxplot(igraph::eigen_centrality(pop_net)$vector~indiv_info[[1]]$sex,range=0,lty=1,xlab="Sex",ylab="Eigenvector Centrality",cex.lab=1.5,cex.axis=1.5)

plot(igraph::degree(pop_net)~indiv_info[[1]]$size,xlab="Size",ylab="Degree",pch=16,cex.lab=1.5,cex.axis=1.5)

plot(pop_mat[pop_mat>0]~dist_mat[pop_mat>0],pch=16,col=adjustcolor("firebrick",0.5),xlab="Distance Apart",ylab="Association Strength",xlim=c(0,1.5),cex.lab=1.5,cex.axis=1.5)

A<-hist(dist_mat[pop_mat==0],breaks=seq(0,1.5,0.1),plot=FALSE)
B<-hist(dist_mat,breaks=seq(0,1.5,0.1),plot=FALSE)

A$counts/B$counts

points(x=seq(0.05,1.45,0.1),y=rep(0,15),cex=3.5*(A$counts/B$counts)^10,pch=16,col=adjustcolor("navy",0.3))


## ----net_gen2, message=FALSE,warning=FALSE------------------------------------

effs<-list()
effs[[1]]<-matrix(c(0,0,0,0,0,0,0,0),nr=1,nc=8)
effs[[1]]<-rbind(rep(0,8),effs[[1]])
effs[[2]]<-matrix(c(0,0,0,0,0,0,0.5,0),nr=1,nc=8)

net_info<-network_generation_covariates(indiv_data,dist_mat,indiv_info,
                                        p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                        p_og=0.1,wo_m=0.1,wo_v=0.025,
                                        d_effp=0,d_effw=0,
                                        covs=c(2,3),effs=effs,
                                        plot=TRUE)
pop_mat<-net_info[[1]]
pop_net<-net_info[[2]]


## ----show_net2,fig.width=8,fig.height=18--------------------------------------

par(mfrow=c(3,1))
cols<-rep("blue",nrow(indiv_info[[1]]))
cols[indiv_info[[1]]$sex=="M"]<-"red"    
plot(pop_net,vertex.label=NA,vertex.color=cols,vertex.size=2*indiv_info[[1]]$size+15)

plot(pop_mat[pop_mat>0]~dist_mat[pop_mat>0],pch=16,col=adjustcolor("firebrick",0.5),xlab="Distance Apart",ylab="Association Strength",xlim=c(0,1.5),cex.lab=1.5,cex.axis=1.5)

A<-hist(dist_mat[pop_mat==0],breaks=seq(0,1.5,0.1),plot=FALSE)
B<-hist(dist_mat,breaks=seq(0,1.5,0.1),plot=FALSE)

points(x=seq(0.05,1.45,0.1),y=rep(0,15),cex=10*(A$counts/B$counts)^10,pch=16,col=adjustcolor("navy",0.3))

plot(igraph::degree(pop_net)~indiv_info[[1]]$size,xlab="Size",ylab="Degree",pch=16,cex.lab=1.5,cex.axis=1.5)


## ----pop_gen2, message=FALSE,warning=FALSE------------------------------------

#Generate population
pop_info<-population_generation_basic(n=100,ng=10,plot=FALSE)
indiv_data<-pop_info[[1]]
dist_mat<-pop_info[[2]]


## ----trait_gen2, message=FALSE,warning=FALSE----------------------------------

indiv_info<-data.frame(indiv_data$indivs)
indiv_info<-indiv_info_gen(indiv_info,ii_tag=NULL,indiv_data=indiv_data,trait="sex",trait_type="fac",level_names=c("M","F"))
indiv_info<-indiv_info_gen(indiv_info[[1]],ii_tag=indiv_info[[2]],indiv_data,trait="size",trait_type="cov",x_dist="norm")


## ----net_gen3, message=FALSE,warning=FALSE------------------------------------

effs<-list()
effs[[1]]<-matrix(c(0,0,0,0,0,0,0,0),nr=1,nc=8)
effs[[1]]<-rbind(rep(0,8),effs[[1]])
effs[[2]]<-matrix(c(0,0,0,0,0,0,0,0),nr=1,nc=8)

net_info<-network_generation_covariates(indiv_data,dist_mat,indiv_info,
                                        p_ig=0.8,wi_m=0.5,wi_v=0.05,
                                        p_og=0.1,wo_m=0.1,wo_v=0.025,
                                        d_effp=8,d_effw=8,
                                        covs=c(2,3),effs=effs,
                                        plot=TRUE)
pop_mat<-net_info[[1]]
pop_net<-net_info[[2]]


## ----show_net3,fig.width=8,fig.height=8---------------------------------------

par(mfrow=c(1,1))
cols<-viridis(10)
plot(pop_net,vertex.label=NA,vertex.color=cols[indiv_data$groups],vertex.size=12,edge.width=(igraph::E(pop_net)$weight*5)^1.5)


## ----net_gen4, message=FALSE,warning=FALSE------------------------------------

effs<-list()
effs[[1]]<-matrix(c(-0.1,-0.25,0,0.2,0.5,0,-2,0),nr=1,nc=8)
effs[[1]]<-rbind(rep(0,8),effs[[1]])
effs[[2]]<-matrix(c(0,0,0,0,0,0,0,0),nr=1,nc=8)

net_info<-network_generation_covariates(indiv_data,dist_mat,indiv_info,
                                        p_ig=0.8,wi_m=0.5,wi_v=0.05,
                                        p_og=0.1,wo_m=0.1,wo_v=0.025,
                                        d_effp=8,d_effw=8,
                                        covs=c(2,3),effs=effs,
                                        plot=TRUE)
pop_mat<-net_info[[1]]
pop_net<-net_info[[2]]


## ----show_net4,fig.width=8,fig.height=16--------------------------------------

par(mfrow=c(2,1))
cols<-viridis(10)
plot(pop_net,vertex.label=NA,vertex.color=cols[indiv_data$groups],vertex.size=12,edge.width=(igraph::E(pop_net)$weight*5)^1.5)

boxplot(igraph::betweenness(pop_net,weights=1/E(pop_net)$weight)~indiv_info[[1]]$sex,xlab="Sex",ylab="Betweenness Centrality",range=0,lty=1,pch=16,cex.lab=1.5,cex.axis=1.5)



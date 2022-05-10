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
library(tnet)


## ----pop_gen, message=FALSE,warning=FALSE-------------------------------------

#Generate population
pop_info<-population_generation_basic(n=100,ng=100)
indiv_data<-pop_info[[1]]
dist_mat<-pop_info[[2]]


## ----trait_gen, message=FALSE,warning=FALSE-----------------------------------

indiv_info<-data.frame(indiv_data$indivs)
indiv_info<-indiv_info_gen(indiv_info,ii_tag=NULL,indiv_data=indiv_data,trait="sex",trait_type="fac",level_names=c("M","F"))
indiv_info<-indiv_info_gen(indiv_info[[1]],ii_tag=indiv_info[[2]],indiv_data,trait="size",trait_type="cov",x_dist="norm")


## ----net_gen, message=FALSE,warning=FALSE-------------------------------------

effs<-list()
effs[[1]]<-matrix(c(0,0,0,1,1,1,0,0),nr=1,nc=8)
effs[[1]]<-rbind(rep(0,8),effs[[1]])
effs[[2]]<-matrix(c(0,0,0,0,0,0,-0.5,-0.5),nr=1,nc=8)

net_info<-network_generation_covariates(indiv_data,dist_mat,indiv_info,
                                        p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                        p_og=0.2,wo_m=0.25,wo_v=0.025,
                                        d_effp=4,d_effw=4,
                                        covs=c(2,3),effs=effs,
                                        plot=TRUE)
pop_mat<-net_info[[1]]
pop_net<-net_info[[2]]


## ----int_gen, message=FALSE,warning=FALSE-------------------------------------

#Sample over some behaviour only time steps
int_info<-interaction_generation_simul(indiv_data=indiv_data,pop_mat=pop_mat,
                                       mean_group_size=2,n_ts=20,
                                       float=0.000000001,par=50,pow=4)
gbi<-int_info[[1]]
samp_wind<-int_info[[2]]

network<-asnipe::get_network(gbi)


## ----surv1, message=FALSE,warning=FALSE---------------------------------------

#Add survival to initial population
indiv_data<-covariates_survival(indiv_data=indiv_data,indiv_info=indiv_info,network=network,
                                group_means=NULL,
                                ext_vars="size",ext_effs=0.2,scale_ext=FALSE,
                                net_vars="degree",net_effs=-0.5,net_packages="igraph",
                                net_cov=FALSE,
                                mps=0.95,lvps=0.5)

#Plots to demonstrate the function is working.
par(mfrow=c(1,1),mar=c(5,5,2,2))
plot(indiv_data$survival~colSums(sign(network)),ylab="Survival Prob",xlab="Degree",cex.lab=1.5,cex.axis=1.15,las=1,pch=16,col=grDevices::adjustcolor("black",0.5))
plot(indiv_data$survival~indiv_info[[1]]$size,ylab="Survival Prob",xlab="Size",cex.lab=1.5,cex.axis=1.15,las=1,pch=16,col=grDevices::adjustcolor("black",0.5))


## ----surv2, message=FALSE,warning=FALSE---------------------------------------

##A second test
#Add survival to initial population
indiv_data<-covariates_survival(indiv_data=indiv_data,indiv_info=indiv_info,network=network,
                                group_means=NULL,
                                ext_vars="sex",ext_effs=list(c(0,0.5)),scale_ext=FALSE,
                                net_vars="betweenness_w",net_effs=list(0.2),net_packages="tnet",
                                net_cov=FALSE,
                                mps=0.95,lvps=0.5)

#Plots to demonstrate the two types of variable are working.
#At least in this case
par(mfrow=c(1,1))
boxplot(indiv_data$survival~indiv_info[[1]]$sex,ylab="Survival Prob",xlab="Sex",cex.lab=1.5,cex.axis=1.15,las=1)
plot(indiv_data$survival~tnet::betweenness_w(tnet::as.tnet(network))[,1],ylab="Survival Prob",xlab="Betweenness",cex.lab=1.5,cex.axis=1.15,las=1,pch=16,col=grDevices::adjustcolor("black",0.5))



## ----surv3, message=FALSE,warning=FALSE---------------------------------------

##A third test
#Add survival to initial population
indiv_data<-covariates_survival(indiv_data=indiv_data,indiv_info=indiv_info,network=network,
                                group_means=NULL,
                                ext_vars="sex",ext_effs=list(c(0,-0.1)),scale_ext=FALSE,
                                net_vars="closeness",net_effs=list(1.2),net_packages="sna",
                                net_cov=FALSE,
                                mps=0.95,lvps=0.5)

#Plots to demonstrate the two types of variable are working.
#At least in this case
par(mfrow=c(1,1))
plot(indiv_data$survival~sna::closeness(network),ylab="Survival Prob",xlab="Closeness",cex.lab=1.5,cex.axis=1.15,las=1,pch=16,col=grDevices::adjustcolor("black",0.5))
boxplot(indiv_data$survival~indiv_info[[1]]$sex,ylab="Survival Prob",xlab="Sex",cex.lab=1.5,cex.axis=1.15,las=1)


## ----surv4, message=FALSE,warning=FALSE---------------------------------------

##A fourth test - with covariance
#Add survival to initial population
indiv_data<-covariates_survival(indiv_data=indiv_data,indiv_info=indiv_info,network=network,
                                group_means=NULL,
                                ext_vars="sex",ext_effs=list(c(0,-1)),scale_ext=FALSE,
                                net_vars="clustering_local_w",net_effs=list(0.25),net_packages="tnet",scale_net=TRUE,
                                net_cov=TRUE,max_cor=-0.9,
                                mps=0.95,lvps=0.5)

#Plots to demonstrate the two types of variable are working.
#At least in this case
par(mfrow=c(1,1))
boxplot(indiv_data$survival~indiv_info[[1]]$sex,ylab="Survival Prob",xlab="Sex",cex.lab=1.5,cex.axis=1.15,las=1)
plot(indiv_data$survival~tnet::clustering_local_w(as.tnet(network))[,2],ylab="Survival Prob",xlab="Clustering coefficient",cex.lab=1.5,cex.axis=1.15,las=1,pch=16,col=grDevices::adjustcolor("black",0.5))
v_sizes<-20+4*scale(indiv_data$survival)
v_sizes[v_sizes<0]<-0.1
igraph::plot.igraph(graph.adjacency(network,mode="undirected",weighted=TRUE),vertex.label=NA,vertex.size=v_sizes)


## ----surv5, message=FALSE,warning=FALSE---------------------------------------

##A fourth test - with covariance
#Add survival to initial population
indiv_data<-covariates_survival(indiv_data=indiv_data,indiv_info=indiv_info,network=network,
                                group_means=NULL,
                                ext_vars="sex",ext_effs=list(c(0,-1)),scale_ext=FALSE,
                                net_vars="clustering_local_w",net_effs=list(0.25),net_packages="tnet",scale_net=TRUE,
                                net_cov=TRUE,max_cor=0.9,
                                mps=0.95,lvps=0.5)

#Plots to demonstrate the two types of variable are working.
#At least in this case
par(mfrow=c(1,1))
boxplot(indiv_data$survival~indiv_info[[1]]$sex,ylab="Survival Prob",xlab="Sex",cex.lab=1.5,cex.axis=1.15,las=1)
plot(indiv_data$survival~tnet::clustering_local_w(as.tnet(network))[,2],ylab="Survival Prob",xlab="Clustering coefficient",cex.lab=1.5,cex.axis=1.15,las=1,pch=16,col=grDevices::adjustcolor("black",0.5))
v_sizes<-20+4*scale(indiv_data$survival)
v_sizes[v_sizes<0]<-0.1
igraph::plot.igraph(graph.adjacency(network,mode="undirected",weighted=TRUE),vertex.label=NA,vertex.size=v_sizes)



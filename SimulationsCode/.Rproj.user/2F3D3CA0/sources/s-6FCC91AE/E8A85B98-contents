###Time to test my skills on nimble modelling some of the simulated datasets

##Tests survival with covariates function

#Load necessary R packages
library(igraph)
library(asnipe)
library(sna)
library(tnet)
library(car)
library(boot)
library(Matrix)
library(MASS)

#Source required functions
source("get_network2.R")
source("network_generation_covariates.R")
source("population_generation_basic.R")
source("interaction_generation_seq.R")
source("interaction_generation_simul.R")
source("cap_and_obs.R")
source("basic_survival.R")
source("covariates_survival.R")
source("timestep_demographics.R")
source("cap_dat_gen.R")
source("indiv_info_gen.R")
source("network_rewire_covariates.R")

###################################################
###################################################

#Generate population
pop_info<-population_generation_basic(n=100,ng=100)
indiv_data<-pop_info[[1]]
dist_mat<-pop_info[[2]]

indiv_info<-data.frame(indiv_data$indivs)
indiv_info<-indiv_info_gen(indiv_info,ii_tag=NULL,indiv_data=indiv_data,trait="sex",trait_type="fac",level_names=c("M","F"))
indiv_info<-indiv_info_gen(indiv_info[[1]],ii_tag=indiv_info[[2]],indiv_data,trait="size",trait_type="cov",x_dist="norm")

#Generate first network
#First defining covariate effects on network structure
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

#Sample over some behaviour only time steps
int_info<-interaction_generation_simul(indiv_data=indiv_data,pop_mat=pop_mat,
                                       mean_group_size=2,n_ts=20,
                                       float=0.000000001,par=50,pow=4)
gbi<-int_info[[1]]
samp_wind<-int_info[[2]]

#Observe networks over these timesteps
obs_info<-cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                      pcg=0.75,pmi=0.9,pci=0.9,
                      start_obs=1,end_obs=max(samp_wind),interval_obs=2,
                      start_cap=1,end_cap=4,interval_cap=1)

captured_gbi<-obs_info[[1]]
captured_groups<-obs_info[[2]]
observed_gbi<-obs_info[[3]]
observed_groups<-obs_info[[4]]

network<-get_network2(gbi)

#Add survival to initial population
indiv_data<-covariates_survival(indiv_data=indiv_data,indiv_info=indiv_info,network=network,
                                group_means=NULL,
                                ext_vars="size",ext_effs=0.2,scale_ext=FALSE,
                                net_vars="degree",net_effs=-0.5,net_packages="igraph",
                                net_cov=FALSE,
                                mps=0.95,lvps=0.5)

#Plots to demonstrate the two types of variable are working.
#At least in this case
par(mfrow=c(1,1))
plot(indiv_data$survival~colSums(sign(network)),ylab="Survival Prob",xlab="Degree")
plot(indiv_data$survival~indiv_info[[1]]$size,ylab="Survival Prob",xlab="Size")

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
plot(indiv_data$survival~tnet::betweenness_w(tnet::as.tnet(network)),ylab="Survival Prob",xlab="Betweenness")
boxplot(indiv_data$survival~indiv_info[[1]]$sex,ylab="Survival Prob",xlab="Sex")


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
plot(indiv_data$survival~sna::closeness(network),ylab="Survival Prob",xlab="Closeness")
boxplot(indiv_data$survival~indiv_info[[1]]$sex,ylab="Survival Prob",xlab="Sex")


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
plot(indiv_data$survival~tnet::clustering_local_w(as.tnet(network))[,2],ylab="Survival Prob",xlab="Clustering coefficient")
boxplot(indiv_data$survival~indiv_info[[1]]$sex,ylab="Survival Prob",xlab="Sex")
plot(graph.adjacency(network,mode="undirected",weighted=TRUE),vertex.label=NA,vertex.size=20+4*scale(indiv_data$survival))

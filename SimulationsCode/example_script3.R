##Example script 3 tests the use of network generation with covariates

par(mfrow=c(1,1),mar=c(5,5,2,2))

#Load necessary R packages
library(igraph)
library(asnipe)
library(sna)
library(car)
library(boot)

#Source required functions
source("get_network2.R")
source("network_generation_covariates.R")
source("population_generation_basic.R")
source("interaction_generation_seq.R")
source("interaction_generation_simul.R")
source("cap_and_obs.R")
source("basic_survival.R")
source("timestep_demographics.R")
source("cap_dat_gen.R")

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
#net_info<-network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
#                                   p_ig=0.2,w_ig1=1.5,w_ig2=5,
#                                   p_og=0.2,w_og1=1,w_og2=5,
#                                   d_effp=4,d_effw=4,
#                                   plot=TRUE)

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


boxplot(igraph::degree(pop_net)~indiv_info[[1]][,2])
boxplot(igraph::strength(pop_net)~indiv_info[[1]][,2])

plot(igraph::degree(pop_net)~indiv_info[[1]][,3])
plot(igraph::strength(pop_net)~indiv_info[[1]][,3])

summary(lm(igraph::degree(pop_net)~indiv_info[[1]][,3]))
summary(lm(igraph::strength(pop_net)~indiv_info[[1]][,3]))


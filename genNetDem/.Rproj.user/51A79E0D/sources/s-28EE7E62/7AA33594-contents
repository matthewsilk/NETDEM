
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

#Check networks produced
network_checker_simul(gbi=gbi,pop_net=pop_net)

#Observe networks over these timesteps
obs_info<-cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                      pcg=0.75,pmi=0.9,pci=0.9,
                      start_obs=1,end_obs=max(samp_wind),interval_obs=2,
                      start_cap=1,end_cap=4,interval_cap=1)

captured_gbi<-obs_info[[1]]
captured_groups<-obs_info[[2]]
observed_gbi<-obs_info[[3]]
observed_groups<-obs_info[[4]]

#Add survival to initial population
indiv_data<-basic_survival(indiv_data,mps=0.95,lvps=0.5)

#Demographic timestep
new_info<-timestep_demographics(indiv_data=indiv_data,full_indiv_data=indiv_data,
                                mps=0.95,lvps=0.5)

prev_data<-indiv_data
indiv_data<-new_info[[1]]
full_indiv_data<-new_info[[2]]
dist_mat<-new_info[[3]]

indiv_info<-indiv_info_add(indiv_info[[1]],ii_tag=indiv_info[[2]],indiv_data=indiv_data)

net_info<-network_rewire_covariates(network=pop_mat,
                          indiv_data=indiv_data,prev_data=prev_data,dist_mat=dist_mat,indiv_info=indiv_info,
                          p_ig=0.2,wi_m=0.25,wi_v=0.025,
                          p_og=0.2,wo_m=0.25,wo_v=0.025,
                          d_effp=4,d_effw=4,
                          covs=2,effs,
                          p_wr_i=0,p_wr_e=0.5,
                          plot=TRUE)

pop_mat<-net_info[[1]]
pop_net<-net_info[[2]]

#######################################
#######################################

##First set of steps works well

#We may need a new function that rewires rather than recreates underlying network
#With a probability that each dyad changes its existing social connections
#Perhaps applied separately to within and between group connections

##But how about we set up a pipeline with network being recreated for now

##Step one: set up population

#Generate population
pop_info<-population_generation_basic(n=100,ng=100)
indiv_data<-pop_info[[1]]
dist_mat<-pop_info[[2]]

indiv_info<-data.frame(indiv_data$indivs)
indiv_info<-indiv_info_gen(indiv_info,ii_tag=NULL,indiv_data=indiv_data,trait="sex",trait_type="fac",level_names=c("M","F"))
indiv_info<-indiv_info_gen(indiv_info[[1]],ii_tag=indiv_info[[2]],indiv_data,trait="size",trait_type="cov",x_dist="norm")


#Generate first network
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

##Step two: iterate over behavioural and demographic time steps

beh_steps<-20
dem_steps<-10

CG<-list()
OG<-list()

SW_store<-list()

inds_alive<-list()
pre_cap<-NULL

for(ds in 1:dem_steps){
  
  inds_alive[[ds]]<-indiv_data$indivs
  
  #Sample over some behaviour only time steps
  int_info<-interaction_generation_simul(indiv_data=indiv_data,pop_mat=pop_mat,
                                         mean_group_size=2,n_ts=beh_steps,
                                         float=0.000000001,par=50,pow=4)
  gbi<-int_info[[1]]
  samp_wind<-int_info[[2]]
  
  #Check networks produced
  #Currently not implemented
  #network_checker_simul(gbi=gbi,pop_net=pop_net)
  
  if(ds>1){
    if(ds==2){
      pre_cap_t<-sign(pre_cap[!is.na(pre_cap[,ds]),ds])
    }
    if(ds>2){
      pre_cap_t<-sign(rowSums(pre_cap[!is.na(pre_cap[,ds]),2:ds],na.rm=TRUE))
    }
    pre_cap_t<-pre_cap_t[names(pre_cap_t)%in%indiv_data$indivs]
    pre_cap_t2<-c(pre_cap_t,rep(0,sum(indiv_data$indivs%in%names(pre_cap_t)==FALSE)))
  }
  
  #Observe networks over these timesteps
  if(ds==1){
    obs_info<-cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                          pcg=0.5,pmi=0.75,pci=0.75,
                          start_obs=1,end_obs=max(samp_wind),interval_obs=2,
                          start_cap=1,end_cap=4,interval_cap=1,
                          pre_cap=NULL)
  }
  if(ds>1){
    obs_info<-cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                          pcg=0.5,pmi=0.75,pci=0.75,
                          start_obs=1,end_obs=max(samp_wind),interval_obs=2,
                          start_cap=1,end_cap=4,interval_cap=1,
                          pre_cap=pre_cap_t2)
  }
  
  captured_gbi<-obs_info[[1]]
  captured_groups<-obs_info[[2]]
  observed_gbi<-obs_info[[3]]
  observed_groups<-obs_info[[4]]
  
  if(ds==1){
    pre_cap<-data.frame(indiv_data$indivs,sign(colSums(captured_gbi)))
    names(pre_cap)<-c("ID","cap")
  }
  if(ds>1){
    pre_cap_t<-data.frame(indiv_data$indivs,sign(colSums(captured_gbi)))
    names(pre_cap_t)<-c("ID","cap")
    pre_cap<-merge(pre_cap,pre_cap_t,by="ID",all.x=TRUE,all.y=TRUE)
  }
  
  CG[[ds]]<-captured_gbi
  OG[[ds]]<-observed_gbi
  
  SW_store[[ds]]<-samp_wind
  
  indiv_data<-basic_survival(indiv_data,mps=0.95,lvps=0.5)
  
  #Demographic timestep
  if(ds==1){
    new_info<-timestep_demographics(indiv_data=indiv_data,full_indiv_data=indiv_data,
                                    mps=0.95,lvps=0.5)
  }
  if(ds>1){
    new_info<-timestep_demographics(indiv_data=indiv_data,full_indiv_data=full_indiv_data,
                                    mps=0.95,lvps=0.5)
  }
  
  prev_data<-indiv_data
  indiv_data<-new_info[[1]]
  full_indiv_data<-new_info[[2]]
  dist_mat<-new_info[[3]]
  
  indiv_info<-indiv_info_add(indiv_info[[1]],ii_tag=indiv_info[[2]],indiv_data=indiv_data)
  
  net_info<-network_rewire_covariates(network=pop_mat,
                                      indiv_data=indiv_data,prev_data=prev_data,dist_mat=dist_mat,indiv_info=indiv_info,
                                      p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                      p_og=0.2,wo_m=0.25,wo_v=0.025,
                                      d_effp=4,d_effw=4,
                                      covs=2,effs,
                                      p_wr_i=0,p_wr_e=0.5,
                                      plot=TRUE)
  
  pop_mat<-net_info[[1]]
  pop_net<-net_info[[2]]
  
}

inds_alive[[1]]
inds_alive[[4]]
inds_alive[[10]]

capture_data<-cap_dat_gen(CG=OG,SW_store=SW_store,bs=beh_steps)

start_obs=1
end_obs=max(samp_wind)
interval_obs=2
start_cap=1
end_cap=4
interval_cap=1

which_keep<-unique(sort(c(seq(start_obs,end_obs,interval_obs),seq(start_cap,end_cap,interval_cap))))

keep_winds<-rep(0,beh_steps)
keep_winds[which_keep]<-1
#Add a 1 at the start to keep the IDs
#This version assumes demographic windows with the same capture strategy
#Can change later
keep_winds2<-c(1,rep(keep_winds,dem_steps))

att_caps<-capture_data[[2]]
att_caps<-att_caps[,keep_winds2==1]

head(att_caps)

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment="#>",
  fig.align="center",
  fig.width=8,
  fig.height=6)

## ----load_packages, message=FALSE,warning=FALSE-------------------------------

library(genNetDem)
library(asnipe)


## ----population_generation----------------------------------------------------

#Generate population
pop_info<-population_generation_basic(n=100,ng=100)
indiv_data<-pop_info[[1]]
dist_mat<-pop_info[[2]]

#individual data
head(indiv_data)

#distance matrix
dist_mat[1:5,1:5]


## ----individual_traits--------------------------------------------------------

indiv_info<-data.frame(indiv_data$indivs)
indiv_info<-indiv_info_gen(indiv_info,ii_tag=NULL,indiv_data=indiv_data,trait="sex",trait_type="fac",level_names=c("M","F"))
indiv_info<-indiv_info_gen(indiv_info[[1]],ii_tag=indiv_info[[2]],indiv_data,trait="size",trait_type="cov",x_dist="norm")

#trait data
head(indiv_info[[1]])
#tag for first trait
indiv_info[[2]][[1]]


## ----network_generation-------------------------------------------------------

net_info<-network_generation_basic(indiv_data=indiv_data,dist_mat=dist_mat,
                                   p_ig=0.2,w_ig1=1.5,w_ig2=5,
                                   p_og=0.2,w_og1=1,w_og2=5,
                                   d_effp=4,d_effw=4,
                                   plot=TRUE)

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


## ----netcheck_plot1, echo=FALSE-----------------------------------------------

boxplot(igraph::strength(pop_net)~indiv_info[[1]][,2],xlab="Sex",ylab="Strength",las=1)



## ----timestep_example_behaviour1----------------------------------------------

#Generate association data
int_info<-interaction_generation_simul(indiv_data=indiv_data,pop_mat=pop_mat,
                                       mean_group_size=2,n_ts=20,
                                       float=0.000000001,pm=50,pow=4)
gbi<-int_info[[1]]
samp_wind<-int_info[[2]]


## ----timestep_example_behaviour2----------------------------------------------

#Check networks produced
network_checker_simul(gbi=gbi,pop_net=pop_net)


## ----timestep_example_behaviour3----------------------------------------------

#Observe networks over these timesteps
obs_info<-cap_and_obs(samp_wind=samp_wind,gbi=gbi,
                      pcg=0.75,pmi=0.9,pci=0.9,
                      start_obs=1,end_obs=max(samp_wind),interval_obs=2,
                      start_cap=1,end_cap=4,interval_cap=1)

captured_gbi<-obs_info[[1]]
captured_groups<-obs_info[[2]]
observed_gbi<-obs_info[[3]]
observed_groups<-obs_info[[4]]


## ----timestep_example_behaviour4----------------------------------------------

obs_net_checker(observed_gbi,asnipe::get_network(gbi),pop_mat)


## ----timestep_example_demography----------------------------------------------

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


## ----full_example_worflow,warning=FALSE---------------------------------------

#Behavioural timesteps
beh_steps<-20
#Demographic timesteps
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
                                         mean_group_size=2,n_ts=beh_steps,float=0.000000001,pm=50,pow=4)
  gbi<-int_info[[1]]
  samp_wind<-int_info[[2]]
  
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
                                    mps=mean(indiv_data$survival),lvps=0.5)
  }
  if(ds>1){
    new_info<-timestep_demographics(indiv_data=indiv_data,full_indiv_data=full_indiv_data,
                                    mps=mean(indiv_data$survival),lvps=0.5)
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


## ----handling_outputs---------------------------------------------------------

#inspect which individuals are alive at timestep 4.
inds_alive[[4]]

#generate conventional capture-recapture dataset
capture_data<-cap_dat_gen(CG=OG,SW_store=SW_store,full_indiv_data=full_indiv_data,inds_alive=inds_alive,bs=beh_steps)

#Reminder of capture/observation steps and then vector of which timesteps were sampled.
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
keep_winds2<-c(1,rep(keep_winds,dem_steps))

#Remove capture data for all behavioural timesteps where captures were not attempted
att_caps<-capture_data[[2]]
att_caps<-att_caps[,keep_winds2==1]

#Show behavioural timestep capture data
print(att_caps[1:5,1:25])



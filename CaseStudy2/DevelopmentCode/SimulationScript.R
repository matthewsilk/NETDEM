pars2<-readRDS("parameters2.RDS")

#load R packages
library(genNetDem)
library(igraph)
library(asnipe)
library(sna)
library(tnet)
library(car)
library(boot)
library(Matrix)
library(MASS)
library(MCMCvis)
library(nimble)

##############################
##############################

path <- Sys.getenv('PATH')
newPath <- paste("C:\\rtools40\\bin;C:\\rtools40\\mingw_64\\bin;C:\\rtools40\\usr\\bin;",
                 path, sep = "")
Sys.setenv(PATH = newPath)


nim_mod <- nimbleCode({
  beta[1] ~ dnorm(mean=0,sd=10) #prior female
  beta[2] ~ dnorm(mean=0,sd=10) #prior male
  beta[3] ~ dnorm(mean=0,sd=10) #prior betweenness effect
  p ~ dunif(0, 1) # prior detection

  #prior for missing betweenness values
  for(i in 1:nrmc){
    clo2[miss_clo2[i,1],miss_clo2[i,2]] ~ dnorm(mean=me_t[miss_clo2[i,1]],sd=sd_t[miss_clo2[i,1]])
  }

  # likelihood
  omega[1,1] <- 1 - p    # Pr(alive t -> non-detected t)
  omega[1,2] <- p        # Pr(alive t -> detected t)
  omega[2,1] <- 1        # Pr(dead t -> non-detected t)
  omega[2,2] <- 0        # Pr(dead t -> detected t)
  for (i in 1:N){
    for(j in 1:(T-1)){
      logit(phi[i,j]) <- beta[sex[i]]+beta[3]*clo2[i,j]
      gamma[1,1,i,j] <- phi[i,j]      # Pr(alive t -> alive t+1)
      gamma[1,2,i,j] <- 1 - phi[i,j]  # Pr(alive t -> dead t+1)
      gamma[2,1,i,j] <- 0           # Pr(dead t -> alive t+1)
      gamma[2,2,i,j] <- 1           # Pr(dead t -> dead t+1)
    }
  }
  delta[1] <- 1          # Pr(alive t = 1) = 1
  delta[2] <- 0          # Pr(dead t = 1) = 0
  for (i in 1:N){
    z[i,1] ~ dcat(delta[1:2])
    for (j in 2:T){
      z[i,j] ~ dcat(gamma[z[i,j-1], 1:2, i,j-1])
      y[i,j] ~ dcat(omega[z[i,j], 1:2])
    }
  }
})

##############################
##############################

SAMP_RES<-array(NA,dim=c(4,7,nrow(pars2)))

pcgs<-unique(pars2$pcg)
seq_lengths<-unique(pars2$pcg)*10

##############################
##############################

for(i in 1:nrow(pars2)){

  ng<-pars2[i,1]
  pcg<-pars2[i,2]
  pmi<-pars2[i,3]
  net_vars<-as.character(pars2[i,4])
  ifelse(net_vars=="strength",net_vars2<-"strength",net_vars2<-"betweenness")
  ifelse(net_vars=="strength",net_packages<-"igraph",net_packages<-"tnet")
  net_effs<-pars2[i,5]
  reps<-pars2[i,6]

  ##Generate capture-recapture dataset

  #Generate population
  pop_info<-population_generation_basic(n=200,ng=ng,plot=FALSE)
  indiv_data<-pop_info[[1]]
  dist_mat<-pop_info[[2]]

  indiv_info<-data.frame(indiv_data$indivs)
  indiv_info<-indiv_info_gen(indiv_info,ii_tag=NULL,indiv_data=indiv_data,trait="sex",trait_type="fac",level_names=c("M","F"))

  start_info<-indiv_info

  #Generate first network
  effs<-list()
  effs[[1]]<-matrix(c(0,0,0,0,0,0,0,0),nr=1,nc=8)
  effs[[1]]<-rbind(rep(0,8),effs[[1]])

  net_info<-network_generation_covariates(indiv_data,dist_mat,indiv_info,
                                          p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                          p_og=0.2,wo_m=0.25,wo_v=0.025,
                                          d_effp=4,d_effw=4,
                                          covs=c(2),effs=effs,
                                          plot=FALSE)
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

  ts_surv<-numeric()

  clo<-matrix(NA,nr=nrow(indiv_data),nc=dem_steps)
  clo_tr<-matrix(NA,nr=nrow(indiv_data),nc=dem_steps)

  start_obs<-2
  end_obs<-max(seq(start_obs,20,by=seq_lengths[pcgs==pcg]))
  interval_obs<-seq_lengths[pcgs==pcg]

  for(ds in 1:dem_steps){

    if(nrow(indiv_data)<5){break}

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
      obs_info<-cap_and_obs2(samp_wind=samp_wind,gbi=gbi,
                            pcg=0.9,pmg=pcg,pmi=pmi,pci=0.9,
                            obs_seq=seq(start_obs,20,by=interval_obs),
                            cap_seq=1,
                            pre_cap=NULL)
    }
    if(ds>1){
      obs_info<-cap_and_obs2(samp_wind=samp_wind,gbi=gbi,
                            pcg=0.9,pmg=pcg,pmi=pmi,pci=0.9,
                            obs_seq=seq(start_obs,20,by=interval_obs),
                            cap_seq=1,
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

    network<-asnipe::get_network(gbi)

    observed_gbi2<-observed_gbi[,colSums(observed_gbi)>0]
    network_obs<-asnipe::get_network(observed_gbi2)

    network2<-graph.adjacency(network,mode="undirected",weighted=TRUE)

    ifelse(net_vars=="strength",clo_tmp<-eval(parse(text=paste0("igraph::",net_vars2,"(network2)"))),clo_tmp<-eval(parse(text=paste0("igraph::",net_vars2,"(network2,weights=1/E(network2)$weight,normalized=TRUE)"))))
    if(length(clo_tmp)<nrow(network)){
      clo_tmp<-c(clo_tmp,rep(NaN,(nrow(network)-length(clo_tmp))))
    }
    if(sum(is.na(clo_tmp)&colSums(gbi)>0)>0){
      clo_tmp[is.na(clo_tmp)&colSums(gbi)>0]<-0
    }

    clo_tr[indiv_data$indivs,ds]<-clo_tmp

    network_obs2<-graph.adjacency(network_obs,mode="undirected",weighted=TRUE)
    ifelse(net_vars=="strength",clo_tmp<-eval(parse(text=paste0("igraph::",net_vars2,"(network_obs2)"))),clo_tmp<-eval(parse(text=paste0("igraph::",net_vars2,"(network_obs2,weights=1/E(network_obs2)$weight,normalized=TRUE)"))))
    if(length(clo_tmp)<nrow(network_obs)){
      clo_tmp<-c(clo_tmp,rep(NaN,(nrow(network_obs)-length(clo_tmp))))
    }
    if(sum(is.na(clo_tmp)&colSums(observed_gbi2)>0)>0){
      clo_tmp[is.na(clo_tmp)&colSums(observed_gbi)>0]<-0
    }

    clo[indiv_data$indivs[colSums(observed_gbi)>0],ds]<-clo_tmp

    par(mfrow=c(1,2))
    plot(network2,vertex.label=NA,edge.width=(E(pop_net)$weight*10)^1.5,vertex.size=8)
    plot(network_obs2,vertex.label=NA,edge.width=(E(pop_net)$weight*10)^1.5,vertex.size=8)
    par(mfrow=c(1,1))

    indiv_data<-covariates_survival(indiv_data=indiv_data,indiv_info=indiv_info,network=network,
                                    group_means=NULL,
                                    ext_vars="sex",ext_effs=list(c(0,0.5)),scale_ext=FALSE,
                                    net_vars=net_vars,net_effs=list(net_effs),net_packages=net_packages,scale_net=TRUE,
                                    net_cov=FALSE,
                                    mps=0.8,lvps=0.5)

    par(mfrow=c(1,2))
    boxplot(indiv_data$survival~indiv_info[[1]]$sex)
    plot(indiv_data$survival~clo_tr[indiv_data$indivs,ds])
    par(mfrow=c(1,1))

    ts_surv[ds]<-mean(indiv_data$survival,na.rm=TRUE)

    #Demographic timestep
    if(ds==1){
      new_info<-timestep_demographics(indiv_data=indiv_data,full_indiv_data=indiv_data,recruitment=TRUE,
                                      mps=0.8,lvps=0.5)
    }
    if(ds>1){
      new_info<-timestep_demographics(indiv_data=indiv_data,full_indiv_data=full_indiv_data,recruitment=TRUE,
                                      mps=0.8,lvps=0.5)
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
                                        p_wr_i=0.1,p_wr_e=0.5,
                                        plot=FALSE)

    pop_mat<-net_info[[1]]
    pop_net<-net_info[[2]]

  }

  capture_data<-cap_dat_gen(CG=OG,SW_store=SW_store,full_indiv_data=full_indiv_data,inds_alive=inds_alive,bs=beh_steps)

  y<-capture_data[[1]]

  clo2<-apply(clo,2,scale)
  clo2<-clo2[,1:(ncol(clo2)-1)]
  clo2b<-clo2
  clo2b[is.na(clo2b)]<-0
  miss_clo<-apply(as.matrix(is.na(clo2)),2,as.numeric)
  miss_clo2<-which(miss_clo==1,arr.ind=T)

  me_t<-apply(clo2,1,mean,na.rm=T)
  me_t[is.na(me_t)]<-mean(me_t,na.rm=TRUE)
  sd_t<-apply(clo2,1,sd,na.rm=T)
  sd_t[is.na(sd_t)]<-mean(sd_t,na.rm=TRUE)

  my.constants <- list(N = nrow(y), T = ncol(y)-1, sex=as.numeric(start_info[[1]]$sex),
                       miss_clo2=miss_clo2,me_t=me_t,sd_t=sd_t,
                       nrmc=nrow(miss_clo2))
  my.data <- list(y = as.matrix(y[,2:ncol(y)]+1),clo2=clo2)
  zinits <- as.matrix(y[,2:ncol(y)] + 1) # non-detection -> alive
  zinits[zinits == 2] <- 1 # dead -> alive
  initial.values <- function() list(beta = rnorm(3,0,3),
                                    p = runif(1,0,1),
                                    z = zinits,
                                    clo2 = clo2b)
  parameters.to.save <- c("beta","p")

  n.iter <- 11000
  n.burnin <- 1000
  n.chains <- 1
  thin<-5

  mcmc.output <- nimbleMCMC(code =nim_mod,
                            constants = my.constants,
                            data = my.data,
                            inits = initial.values,
                            monitors = parameters.to.save,
                            niter = n.iter,
                            nburnin = n.burnin,
                            nchains = n.chains,
                            thin=thin)

  store_res<-MCMCsummary(mcmc.output, round = 2)

  SAMP_RES[,,i]<-as.matrix(store_res)

  print(i)
}

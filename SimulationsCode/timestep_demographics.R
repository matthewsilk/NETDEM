
timestep_demographics<-function(indiv_data,full_indiv_data,
                                mps=0.95,lvps=0.5){
  #Survival on logit scale
  lmps<-car::logit(mps,adjust=0.001)
  #Recruitment rate
  rr<-(1/mps-1)
  
  survived<-rbinom(nrow(indiv_data),1,prob=indiv_data$survival)
  dead<-sum(survived==0)
  which_dead<-which(survived==0)
  indivs_which_dead<-indiv_data$indivs[which(survived==0)]
  groups_which_dead<-indiv_data$groups[which(survived==0)]
  
  n_recruits<-rpois(1,rr*(sum(survived==1)))
  
  n<-length(unique(indiv_data$indivs))
  ng<-length(unique(indiv_data$groups))
  
  recruit_groups<-numeric()
  if(n==ng){
    tl<-length(groups_which_dead)
    if(tl==1&n_recruits>0){
      recruit_groups[1]<-groups_which_dead
    }
    if(tl>1&tl<=n_recruits){
      recruit_groups[1:tl]<-sample(groups_which_dead,tl,replace=FALSE)
    }
    if(tl>1&tl>n_recruits){
      recruit_groups[1:n_recruits]<-sample(groups_which_dead,n_recruits,replace=FALSE)
    }
    if(length(recruit_groups)<n_recruits){
      l_tmp<-length(recruit_groups)
      l_rem<-n_recruits-length(recruit_groups)
      recruit_groups[(l_tmp+1):(l_tmp+l_rem)]<-seq(max(indiv_data$groups)+1,max(indiv_data$groups)+l_rem,1)
    }
  }
  
  if(n>ng){
    recruit_groups<-sample(sort(unique(groups)),n_recruits,replace=TRUE,prob=1/table(groups))
  }
  
  new_groups<-recruit_groups[recruit_groups%in%indiv_data$groups==FALSE]
  new_x<-runif(length(new_groups),0,1)
  new_y<-runif(length(new_groups),0,1)
  new_gi<-data.frame(new_groups,new_x,new_y)
  
  recruit_indivs<-seq(max(indiv_data$indivs)+1,max(indiv_data$indivs)+n_recruits,1)
  
  new_indiv_data<-data.frame(recruit_indivs,recruit_groups)
  new_indiv_data$y<-new_indiv_data$x<-rep(NA,nrow(new_indiv_data))
  names(new_indiv_data)<-c("indivs","groups","x","y")
  for(i in 1:nrow(new_indiv_data)){
    if(new_indiv_data$groups[i]%in%new_groups==TRUE){
      new_indiv_data$x[i]<-new_gi$new_x[which(new_gi$new_groups==new_indiv_data$groups[i])]
      new_indiv_data$y[i]<-new_gi$new_y[which(new_gi$new_groups==new_indiv_data$groups[i])]
    }
    if(new_indiv_data$groups[i]%in%new_groups==FALSE){
      new_indiv_data$x[i]<-unique(indiv_data$x[indiv_data$groups==new_indiv_data$groups[i]])
      new_indiv_data$y[i]<-unique(indiv_data$y[indiv_data$groups==new_indiv_data$groups[i]])
    }
  }
  
  #This may need updating once they have been assigned a position in a social network
  #so could revert to NAs or leave here as a dummy variable to be replaced
  new_indiv_data$survival<-inv.logit(rnorm(nrow(new_indiv_data),lmps,lvps))
  
  indiv_data<-rbind(indiv_data,new_indiv_data)
  indiv_data$alive<-1
  indiv_data$alive[which_dead]<-0
  indiv_data<-indiv_data[which(indiv_data$alive==1),]
  indiv_data<-indiv_data[,1:5]
  
  full_indiv_data<-rbind(full_indiv_data,new_indiv_data)
  
  dist_mat<-as.matrix(dist(cbind(indiv_data$x,indiv_data$y),upper=TRUE))
  
  output<-list(indiv_data,full_indiv_data,dist_mat)
  
  return(output)
  
}

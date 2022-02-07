##Function to generate individual covariates

indiv_info_gen<-function(indiv_info,ii_tag=NULL,indiv_data,trait,trait_type=c("cov","fac"),x_dist=c("unif","norm","lnorm"),level_names=NULL,prob_levels=NULL){
  
  names(indiv_info)[1]<-"ID"
  n_obs<-nrow(indiv_info)
  obs<-rep(NA,n_obs)
  indiv_info$trait<-obs
  
  if(trait_type=="cov"){
    if(x_dist=="unif"){
      indiv_info$trait<-runif(n_obs,0,1)
    }
    if(x_dist=="norm"){
      indiv_info$trait<-rnorm(n_obs)
    }
    if(x_dist=="lnorm"){
      indiv_info$trait<-rlnorm(n_obs)
    }
  }
  
  if(trait_type=="fac"){
    levels<-length(level_names)
    if(is.vector(prob_levels)){prob<-prob_levels}
    if(is.vector(prob_levels)==FALSE){prob<-NULL}
    indiv_info$trait<-sample(level_names,n_obs,replace=TRUE,prob=prob)
    indiv_info$trait<-as.factor(indiv_info$trait)
  }

  if(is.list(ii_tag)==FALSE){ii_tag=list()}
  
  ii_tag[[which(names(indiv_info)=="trait")-1]]<-list()
  
  ii_tag[[which(names(indiv_info)=="trait")-1]][[1]]<-trait
  ii_tag[[which(names(indiv_info)=="trait")-1]][[2]]<-trait_type
  ii_tag[[which(names(indiv_info)=="trait")-1]][[3]]<-x_dist
  ii_tag[[which(names(indiv_info)=="trait")-1]][[4]]<-level_names
  ii_tag[[which(names(indiv_info)=="trait")-1]][[5]]<-prob_levels
  ii_tag[[which(names(indiv_info)=="trait")-1]][[6]]<-paste("Variable")
  
  names(indiv_info)[which(names(indiv_info)=="trait")]<-paste(trait)
  
  output<-list(indiv_info,ii_tag)
  
  return(output)
  
}

##Function to add covariate information for newly recruited individuals
#Not yet edited

indiv_info_add<-function(indiv_info,ii_tag,indiv_data){
  
  indiv_excl<-which(indiv_info[,1]%in%indiv_data$indivs==FALSE)
  indiv_add<-which(indiv_data$indivs%in%indiv_info[,1]==FALSE)
  
  indiv_info2<-indiv_info[-indiv_excl,]
  
  n_obs<-length(indiv_add)
  n_indiv_info<-data.frame(indiv_data$indivs[indiv_add])
  names(n_indiv_info)<-"ID"
  
  for(i in 1:length(ii_tag)){
    
    obs<-rep(NA,n_obs)
    n_indiv_info$trait<-obs
  
    trait_type<-ii_tag[[i]][[2]]
    x_dist<-ii_tag[[i]][[3]]
    level_names<-ii_tag[[i]][[4]]
    prob_levels<-ii_tag[[i]][[5]]
    
    if(trait_type=="cov"){
      if(x_dist=="unif"){
        n_indiv_info$trait<-runif(n_obs,0,1)
      }
      if(x_dist=="norm"){
        n_indiv_info$trait<-rnorm(n_obs)
      }
      if(x_dist=="norm"){
        n_indiv_info$trait<-rlnorm(n_obs)
      }
    }
  
    if(trait_type=="fac"){
      levels<-length(level_names)
      if(is.vector(prob_levels)){prob<-prob_levels}
      if(is.vector(prob_levels)==FALSE){prob<-NULL}
      n_indiv_info$trait<-sample(level_names,n_obs,replace=TRUE,prob=prob)
      n_indiv_info$trait<-as.factor(n_indiv_info$trait)
    }
  
    names(n_indiv_info)[which(names(n_indiv_info)=="trait")]<-ii_tag[[i]][[1]]
  
  }  
    
  indiv_info2<-rbind(indiv_info2,n_indiv_info)
  
  output<-list(indiv_info2,ii_tag)
  
  return(output)
  
}

indiv_info_merge<-function(indiv_infoA,indiv_infoB){
  indiv_info_comb<-merge(indiv_infoA,indiv_infoB,by="ID")
  return(indiv_info_comb)
}
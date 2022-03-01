##to do - community effect on survival?
#         network covariance effect on survival

covariates_survival<-function(indiv_data,indiv_info,network,
                              group_means=NULL,
                              ext_vars=NULL,ext_effs,scale_ext=TRUE,
                              net_vars=NULL,net_effs,net_packages,scale_net=TRUE,
                              net_cov=FALSE,cov_steps,cov_eff,
                              mps,lvps){
  
  lmps<-car::logit(mps,adjust=0.001)
  
  t_survival<-rnorm(nrow(indiv_data),lmps,lvps)
  
  if(is.vector(group_means)){
    t_survival<-t_survival+group_means[indiv_data$groups]
  }
  if(is.vector(ext_vars)){
    
    c_l<-length(ext_vars)
    
    for(i in 1:length(ext_vars)){
      
      t_i<-which(grepl(ext_vars[i],indiv_info[[2]]))
      
      if(indiv_info[[2]][[t_i]][[2]]=="cov"){
        if(scale_ext==FALSE){
          t_survival<-t_survival+ext_effs[[i]]*indiv_info[[1]][,t_i+1]
        }
        if(scale_ext==TRUE){
          t_survival<-t_survival+ext_effs[[i]]*scale(indiv_info[[1]][,t_i+1])
        }
      }
      
      if(indiv_info[[2]][[t_i]][[2]]=="fac"){
        for(j in 1:nrow(indiv_info[[1]])){
          t_survival[j]<-t_survival[j]+ext_effs[[i]][as.numeric(indiv_info[[1]][j,t_i+1])]
        }
      }
    }
  }
  
  if(is.vector(net_vars)){
    
    c_l<-length(net_vars)
    
    for(i in 1:length(net_vars)){
      
      if(is.na(net_packages[i])){
        met<-eval(parse(text=paste0(net_vars[i],"(network)")))
      }
      if(is.na(net_packages[i])==FALSE&net_packages[i]=="igraph"){
        network2<-graph.adjacency(network,mode="undirected",weighted=TRUE)
        met<-eval(parse(text=paste0(net_packages[i],"::",net_vars[i],"(network2)")))
      }
      if(is.na(net_packages[i])==FALSE&net_packages[i]=="sna"){
        met<-eval(parse(text=paste0(net_packages[i],"::",net_vars[i],"(network)")))
      }
      if(is.na(net_packages[i])==FALSE&net_packages[i]=="tnet"){
        network2<-tnet::as.tnet(network,type="weighted one-mode tnet")
        met<-eval(parse(text=paste0(net_packages[i],"::",net_vars[i],"(network2)")))
      }
      
      if(scale_net==FALSE){
        t_survival<-t_survival+net_effs[[i]]*met
      }
      if(scale_net==TRUE){
        t_survival<-t_survival+net_effs[[i]]*scale(met)
      }
      
    }
  }
  
  if(cov==TRUE){
    network2<-graph.adjacency(network,mode="undirected",weighted=TRUE)
    t_add<-numeric()
    for(i in 1:nrow(indiv_data)){
      e_net<-igraph::make_ego_graph(network2,order=cov_steps,nodes=i)
      ego_size(network2, order = cov_steps, nodes=i)
    }
  }
  
  indiv_data$survival<-boot::inv.logit(t_survival)
  return(indiv_data)
  
}
##to do - community effect on survival?
  #         network covariance effect on survival

covariates_survival<-function(indiv_data,indiv_info,network,
                              group_means=NULL,
                              ext_vars=NULL,ext_effs,scale_ext=TRUE,
                              net_vars=NULL,net_effs,net_packages,scale_net=TRUE,
                              net_cov=FALSE,max_cor=0.8,covMat_check=FALSE,
                              mps,lvps){
  
  lmps<-car::logit(mps,adjust=0.001)
  
  if(net_cov==FALSE){
    t_survival<-rnorm(nrow(indiv_data),lmps,lvps)
  }
  if(net_cov==TRUE){
    networkB<-network/max(network)
    corMat<-max_cor*networkB
    diag(corMat)<-1
    stddev<-rep(lvps^0.5,nrow(indiv_data))
    covMat <- stddev %*% t(stddev) * corMat
    covMat2<-as.matrix(Matrix::nearPD(covMat)$mat)
    if(covMat_check==TRUE){
      print(summary(sna::netlm(y=covMat2,x=covMat)))
    }
    t_survival <- mvrnorm(n = 1, mu = rep(lmps,nrow(indiv_data)), Sigma = covMat2, empirical = FALSE)
  }
  
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
        if(net_vars[i]%in%c("closeness","betweenness")){
          met<-eval(parse(text=paste0(net_packages[i],"::",net_vars[i],"(network2,weights=1/E(network2)$weight)")))
        } else{
          met<-eval(parse(text=paste0(net_packages[i],"::",net_vars[i],"(network2)")))
        }
        if(net_vars[i]=="eigen_centrality"){
          met<-met$vector
        }
      }
      if(is.na(net_packages[i])==FALSE&net_packages[i]=="sna"){
        met<-eval(parse(text=paste0(net_packages[i],"::",net_vars[i],"(network)")))
      }
      if(is.na(net_packages[i])==FALSE&net_packages[i]=="tnet"){
        network2<-tnet::as.tnet(network,type="weighted one-mode tnet")
        met<-eval(parse(text=paste0(net_packages[i],"::",net_vars[i],"(network2)")))
        met<-met[,ncol(met)]
        if(length(met)<nrow(network)){
          met<-c(met,rep(0,(nrow(network)-length(met))))
        }
      }
      
      #Convert indivs with NA for clustering coefficient to have clustering coefficient of 0
      if(net_vars[i]%in%c("clustering_local_w","clustering_local","transitivity")){
        met[is.na(met)]<-0
      }
      
      if(scale_net==FALSE){
        t_survival<-t_survival+net_effs[[i]]*met
      }
      if(scale_net==TRUE){
        if(sd(met,na.rm=TRUE)>0){
          t_survival<-t_survival+net_effs[[i]]*scale(met)
        }
        if(sd(met,na.rm=TRUE)==0){
          t_survival<-t_survival+net_effs[[i]]*(met-mean(met))
        }
      }
      
    }
  }
  
  indiv_data$survival<-boot::inv.logit(t_survival)
  return(indiv_data)
  
}
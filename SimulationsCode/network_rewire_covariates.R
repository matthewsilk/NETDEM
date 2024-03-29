##Function to add new individuals to and rewire a social network
##Uses the same structure as network_generation_covariates

#Probabilities of edges within and between groups (binomial)
#Weights of edges within and between groups (Beta, 2 parameter)
#Edge probabilities and weights depend on distance between groups
#Returns adjacency matrix and igraph network object (incl plot if desired)

##Additions from network_generation_basic() are that you can use covariates from
##indiv_info to generate differences in network behaviour

#Need to prevent the distance effect from going negative with the wrong values
#Same for the weight parameters of the Beta distribution

network_rewire_covariates<-function(network,
                                    indiv_data,prev_data,dist_mat,indiv_info,
                                    p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                    p_og=0.2,wo_m=0.25,wo_v=0.025,
                                    d_effp=4,d_effw=4,
                                    covs=2,effs,
                                    p_wr_i=0.2,p_wr_e=0.5,
                                    plot=TRUE){
  
  ################
  fn_sol <- function(x) {
    
    mean <- x[1]/(x[1]+x[2]) - beta_mean
    var <- (x[1]*x[2])/(((x[1]+x[2])^2)*(x[1]+x[2]+1)) - beta_var
    
    return(c(mean, var))
    
  }
  
  ################
  
  t_rem<-which(prev_data$indivs%in%indiv_data$indivs==FALSE)
  if(length(t_rem)>0){network2<-network[-t_rem,-t_rem]}
  if(length(t_rem)==0){network2<-network}
  
  n<-nrow(indiv_data)  
  network3<-matrix(0,nr=n,nc=n)
  
  network3[1:nrow(network2),1:ncol(network2)]<-network2
  
  for(i in 1:nrow(network2)){
    c_f<-rbinom(1,1,p_wr_i)
    if(c_f==1){
      for(j in 1:ncol(network2)){
        c_f2<-rbinom(1,1,p_wr_e)
        if(indiv_data$groups[i]==indiv_data$groups[j]){
            
          lprobs<-boot::logit(c(p_ig,wi_m,wi_v))
            
          c_l<-length(covs)
            
          for(cl in 1:c_l){
            if(indiv_info[[2]][[covs[cl]-1]][[2]]=="cov"){
              leffs<-effs[[covs[cl]-1]][1,1:3]
              lprobs<-lprobs+as.vector((indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[i],covs[cl]]*leffs+indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[j],covs[cl]]*leffs)/2)
            }
            if(indiv_info[[2]][[covs[cl]-1]][[2]]=="fac"){
              leffs<-effs[[covs[cl]-1]][,1:3]
              tr1<-as.numeric(indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[i],covs[cl]])
              tr2<-as.numeric(indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[j],covs[cl]])
              lprobs<-lprobs+as.vector((leffs[tr1,]+leffs[tr2,])/2)
            }
          }
          probs<-boot::inv.logit(lprobs)
            
          t_e1<-rbinom(1,1,probs[1])
          beta_mean<-probs[2]
          beta_var<-probs[3]
          weights<-nleqslv(c(1,1), fn_sol)$x
          t_e2<-rbeta(1,weights[1],weights[2])
          network3[i,j]<-network3[j,i]<-t_e1*t_e2
        }
          
        if(indiv_data$groups[i]!=indiv_data$groups[j]){
            
          lvals<-c(boot::logit(c(p_og,wo_m,wo_v)),log(c(d_effp,d_effw)))
            
          c_l<-length(covs)
            
          for(cl in 1:c_l){
            if(indiv_info[[2]][[covs[cl]-1]][[2]]=="cov"){
              leffs<-effs[[covs[cl]-1]][1,4:8]
              lvals<-lvals+as.vector((indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[i],covs[cl]]*leffs+indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[j],covs[cl]]*leffs)/2)
            }
            if(indiv_info[[2]][[covs[cl]-1]][[2]]=="fac"){
              leffs<-effs[[covs[cl]-1]][,4:8]
              tr1<-as.numeric(indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[i],covs[cl]])
              tr2<-as.numeric(indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[j],covs[cl]])
              lvals<-lvals+as.vector((leffs[tr1,]+leffs[tr2,])/2)
            }
          }
          vals<-c(boot::inv.logit(lvals[1:3]),exp(lvals[4:5]))
            
          t_e1<-rbinom(1,1,vals[1]*(1/vals[4])^dist_mat[i,j])
          beta_mean<-vals[2]
          beta_var<-vals[3]
          weights<-nleqslv(c(1,1), fn_sol)$x
          t_e2<-rbeta(1,weights[1],weights[2])*(1/vals[5])^dist_mat[i,j]
          network3[i,j]<-network3[j,i]<-t_e1*t_e2
        }
      }
    }
  }
  
  if(nrow(network3)>nrow(network2)){
  
  for(i in (nrow(network2)+1):nrow(network3)){
    for(j in 1:nrow(network3)){
      if(indiv_data$groups[i]==indiv_data$groups[j]){
        
        lprobs<-boot::logit(c(p_ig,wi_m,wi_v))
        
        c_l<-length(covs)
        
        for(cl in 1:c_l){
          if(indiv_info[[2]][[covs[cl]-1]][[2]]=="cov"){
            leffs<-effs[[covs[cl]-1]][1,1:3]
            lprobs<-lprobs+as.vector((indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[i],covs[cl]]*leffs+indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[j],covs[cl]]*leffs)/2)
          }
          if(indiv_info[[2]][[covs[cl]-1]][[2]]=="fac"){
            leffs<-effs[[covs[cl]-1]][,1:3]
            tr1<-as.numeric(indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[i],covs[cl]])
            tr2<-as.numeric(indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[j],covs[cl]])
            lprobs<-lprobs+as.vector((leffs[tr1,]+leffs[tr2,])/2)
          }
        }
        probs<-boot::inv.logit(lprobs)
        
        t_e1<-rbinom(1,1,probs[1])
        beta_mean<-probs[2]
        beta_var<-probs[3]
        weights<-nleqslv(c(1,1), fn_sol)$x
        t_e2<-rbeta(1,weights[1],weights[2])
        network3[i,j]<-network3[j,i]<-t_e1*t_e2
      }
      
      if(indiv_data$groups[i]!=indiv_data$groups[j]){
        
        lvals<-c(boot::logit(c(p_og,wo_m,wo_v)),log(c(d_effp,d_effw)))
        
        c_l<-length(covs)
        
        for(cl in 1:c_l){
          if(indiv_info[[2]][[covs[cl]-1]][[2]]=="cov"){
            leffs<-effs[[covs[cl]-1]][1,4:8]
            lvals<-lvals+as.vector((indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[i],covs[cl]]*leffs+indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[j],covs[cl]]*leffs)/2)
          }
          if(indiv_info[[2]][[covs[cl]-1]][[2]]=="fac"){
            leffs<-effs[[covs[cl]-1]][,4:8]
            tr1<-as.numeric(indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[i],covs[cl]])
            tr2<-as.numeric(indiv_info[[1]][indiv_info[[1]]$ID==indiv_data$indivs[j],covs[cl]])
            lvals<-lvals+as.vector((leffs[tr1,]+leffs[tr2,])/2)
          }
        }
        vals<-c(boot::inv.logit(lvals[1:3]),exp(lvals[4:5]))
        
        t_e1<-rbinom(1,1,vals[1]*(1/vals[4])^dist_mat[i,j])
        beta_mean<-vals[2]
        beta_var<-vals[3]
        weights<-nleqslv(c(1,1), fn_sol)$x
        t_e2<-rbeta(1,weights[1],weights[2])*(1/vals[5])^dist_mat[i,j]
        network3[i,j]<-network3[j,i]<-t_e1*t_e2
      }
    }
  }
  }
  
  diag(network3)<-0
  
  pop_mat<-network3
  pop_net<-graph.adjacency(pop_mat,mode="undirected",weighted=TRUE)
  
  if(plot==TRUE){
    par(mar=c(0,0,0,0))
    plot(pop_net,vertex.label=NA,vertex.size=8,edge.width=(E(pop_net)$weight*10)^1.5)
    par(mar=c(5,5,2,2))
  }
  
  output<-list(pop_mat,pop_net)
  
  return(output)
  
}
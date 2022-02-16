##More complex social network generation based on individual characteristics

#Probabilities of edges within and between groups (binomial)
#Weights of edges within and between groups (Beta, 2 parameter)
#Edge probabilities and weights depend on distance between groups
#Returns adjacency matrix and igraph network object (incl plot if desired)

##Additions from network_generation_basic() are that you can use covariates from
##indiv_info to generate differences in network behaviour

library(nleqslv)

network_generation_covariates<-function(indiv_data,dist_mat,indiv_info,
                                   p_ig=0.2,wi_m=0.25,wi_v=0.025,
                                   p_og=0.2,wo_m=0.25,wo_v=0.025,
                                   d_effp=4,d_effw=4,
                                   covs=2,effs,
                                   plot=TRUE){
  
  ################
  fn_sol <- function(x) {
    
    mean <- x[1]/(x[1]+x[2]) - beta_mean
    var <- (x[1]*x[2])/(((x[1]+x[2])^2)*(x[1]+x[2]+1)) - beta_var
    
    return(c(mean, var))
    
  }
  
  ################
  
  n<-nrow(indiv_data)  
  
  #Generate underlying network of affiliative relationships
  pop_mat<-matrix(0,nr=n,nc=n)
  
  #to add: assortativity, transitivity
  
  for(i in 1:(n-1)){
    for(j in (i+1):n){
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
        print(beta_mean)
        beta_var<-probs[3]
        print(beta_var)
        weights<-nleqslv(c(1,1), fn_sol)$x
        print(weights)
        t_e2<-rbeta(1,weights[1],weights[2])
        pop_mat[i,j]<-pop_mat[j,i]<-t_e1*t_e2
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
        weights<-nleqslv::nleqslv(c(1,1),fn_sol)$x
        t_e2<-rbeta(1,weights[1],weights[2])*(1/vals[5])^dist_mat[i,j]
        pop_mat[i,j]<-pop_mat[j,i]<-t_e1*t_e2
      }
    }
  }
  
  pop_net<-graph.adjacency(pop_mat,mode="undirected",weighted=TRUE)
  
  if(plot==TRUE){
    par(mar=c(0,0,0,0))
    plot(pop_net,vertex.label=NA,vertex.size=8,edge.width=(E(pop_net)$weight*10)^1.5)
    par(mar=c(5,5,2,2))
  }
  
  output<-list(pop_mat,pop_net)
  
  return(output)
  
}

#################################
#################################

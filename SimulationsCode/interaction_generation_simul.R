
#internal function to find integers between two non-integer number
ib_int <- function(a, b) {
  u <- sort(c(a, b))
  res <- setdiff(ceiling(u[1]):floor(u[2]), c(a, b))
  if (!length(res)) {
    NULL
  } else if(length(res)==1){
    res
  } else if(res[1]>res[2]){
    NULL
  } else{
    res
  }
}

#internal function to generate group sizes
group_calc<-function(ni,mgs){
  divides<-sort(c(0,runif((ni/mgs)-1,1,ni),ni+1))
  grs<-numeric()
  for(ps in 1:(length(divides)-1)){
    grs[ps]<-length(ib_int(divides[ps],divides[ps+1]))
  }
  return(grs)
}

#internal function
jpc<-function(group,par,float){
  ret<-prod(group)+sum(group)/par+float
  return(ret)
}


interaction_generation_simul<-function(indiv_data,pop_mat,
                                       mean_group_size=2,n_ts=20,
                                       float=0.000000001,par=50,pow=4){
  grs<-group_calc(nrow(indiv_data),mean_group_size)
  grs<-grs[grs>0]
  gbi2<-matrix(0,nr=length(grs),nc=nrow(indiv_data))
  t_data<-indiv_data$indivs
  for(i in 1:length(grs)){
    t_g<-numeric()
    if(length(t_data)>1){
      t_g[1]<-sample(t_data,1,replace=FALSE)
    }
    if(length(t_data)==1){
      t_g[1]<-t_data
    }
    if(grs[i]>1){
      for(j in 2:grs[i]){
        can_join<-t_data[-which(t_data%in%t_g)]
        t_mat<-pop_mat[which(sort(indiv_data$indivs)%in%t_g),which(sort(indiv_data$indivs)%in%can_join)]
        if(is.vector(t_mat)){
          join_probs<-t_mat+float
        } else{
          join_probs<-apply(t_mat,2,jpc,par=par,float=float)^pow
        }
        if(length(can_join)>1){
          t_g[j]<-sample(can_join,1,replace=FALSE,prob=join_probs)
        }
        if(length(can_join)==1){
          t_g[j]<-can_join
        }
      }
    }
    gbi2[i,which(sort(indiv_data$indivs)%in%t_g)]<-1
    t_data<-t_data[-which(t_data%in%t_g)]
  }
  
  samp_wind<-rep(1,nrow(gbi2))
  
  for(ts in 2:n_ts){
    grs<-group_calc(nrow(indiv_data),mean_group_size)
    grs<-grs[grs>0]
    gbi_t<-matrix(0,nr=length(grs),nc=nrow(indiv_data))
    t_data<-indiv_data$indivs
    for(i in 1:length(grs)){
      t_g<-numeric()
      if(length(t_data)>1){
        t_g[1]<-sample(t_data,1,replace=FALSE)
      }
      if(length(t_data)==1){
        t_g[1]<-t_data
      }
      if(grs[i]>1){
        for(j in 2:grs[i]){
          can_join<-t_data[-which(t_data%in%t_g)]
          t_mat<-pop_mat[which(sort(indiv_data$indivs)%in%t_g),which(sort(indiv_data$indivs)%in%can_join)]
          if(is.vector(t_mat)){
            join_probs<-t_mat+float
          } else{
            join_probs<-apply(t_mat,2,jpc,par=par,float=float)^pow
          }
          if(length(can_join)>1){
            t_g[j]<-sample(can_join,1,replace=FALSE,prob=join_probs)
          }
          if(length(can_join)==1){
            t_g[j]<-can_join
          }
        }
      }
      gbi_t[i,which(sort(indiv_data$indivs)%in%t_g)]<-1
      t_data<-t_data[-which(t_data%in%t_g)]
    }
    gbi2<-rbind(gbi2,gbi_t)
    samp_wind<-c(samp_wind,rep(ts,nrow(gbi_t)))
  }
  
  output<-list(gbi2,samp_wind)
  
  return(output)
  
}

network_checker_simul<-function(gbi,pop_net){
  
  gbi2<-gbi
  full_mat<-get_network2(gbi2)
  full_net<-graph.adjacency(full_mat,mode="undirected",weighted=TRUE)
  
  #Plot network
  par(mar=c(0,0,0,0),mfrow=c(1,1))
  plot(full_net,vertex.label=NA,vertex.size=8,edge.width=(E(full_net)$weight*2)^1.5)
  
  #Check correlation between networks
  cor_check<-netlm(full_mat,pop_mat,nullhyp="qapspp")
  print(summary(cor_check))
  
  #Plot correlation between centrality measures from underlying network
  #and that derived from groups/interactions
  par(mfrow=c(2,2))
  par(mar=c(5,5,2,2))
  plot(igraph::degree(full_net)~igraph::degree(pop_net))
  plot(igraph::strength(full_net)~igraph::strength(pop_net))
  plot(igraph::betweenness(full_net,weights=1/E(full_net)$weight)~igraph::betweenness(pop_net,weights=1/E(pop_net)$weight))
  plot(igraph::closeness(full_net,weights=1/E(full_net)$weight)~igraph::closeness(pop_net,weights=1/E(pop_net)$weight))
  
}





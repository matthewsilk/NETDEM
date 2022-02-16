jpc<-function(group,par,float){
  ret<-prod(group)+sum(group)/par+float
  return(ret)
}

interaction_generation_seq<-function(pop_mat,indiv_data,
                                     ne=1000,mgs=2,style_gs=c("rep","pois"),
                                     par=20,float=0.00001){
  if(style_gs=="rep"){
    gs<-rep(mgs,ne)
  }
  if(style_gs=="pois"){
    gs<-rpois(ne,5)
  }
  
  
  gbi<-matrix(0,nr=ne,nc=n)
  for(i in 1:ne){
    t_g<-numeric()
    t_g[1]<-sample(indiv_data$indivs,1,replace=FALSE)
    for(j in 2:gs[i]){
      can_join<-indiv_data$indivs[-which(indiv_data$indivs%in%t_g)]
      t_mat<-pop_mat[t_g,can_join]
      if(is.vector(t_mat)){
        join_probs<-t_mat+float
      }
      else{
        join_probs<-apply(t_mat,2,jpc,par=par,float=float)
      }
      if(length(can_join)>1){
        t_g[j]<-sample(can_join,1,replace=FALSE,prob=join_probs)
      }
      if(length(can_join)==1){
        t_g[j]<-can_join
      }
    }
    gbi[i,t_g]<-1
  }
  
  return(gbi)
  
}

network_checker_seq<-function(gbi,pop_net){
  
  full_mat<-get_network2(gbi)
  full_net<-graph.adjacency(full_mat,mode="undirected",weighted=TRUE)
  
  #Plot network based on groups/interactions
  par(mar=c(0,0,0,0),mfrow=c(1,1))
  plot(full_net,vertex.label=NA,vertex.size=8,edge.width=(E(full_net)$weight*2)^1.5)
  
  #Plots number of observations in groups/interactions against centrality in underlying network
  par(mfrow=c(2,2))
  par(mar=c(5,5,2,2))
  plot(colSums(gbi)~igraph::degree(pop_net))
  plot(colSums(gbi)~igraph::strength(pop_net))
  plot(colSums(gbi)~igraph::betweenness(pop_net,weights=1/E(pop_net)$weight))
  plot(colSums(gbi)~igraph::closeness(pop_net,weights=1/E(pop_net)$weight))
  
  #Check correlation between the 2 networks
  cor_check<-netlm(full_mat,pop_mat,nullhyp="qapspp")
  summary(cor_check)
  
  #Plot correlation between the centrality measures in the two networks
  par(mfrow=c(2,2))
  par(mar=c(5,5,2,2))
  plot(igraph::degree(full_net)~igraph::degree(pop_net))
  plot(igraph::strength(full_net)~igraph::strength(pop_net))
  plot(igraph::betweenness(full_net,weights=1/E(full_net)$weight)~igraph::betweenness(pop_net,weights=1/E(pop_net)$weight))
  plot(igraph::closeness(full_net,weights=1/E(full_net)$weight)~igraph::closeness(pop_net,weights=1/E(pop_net)$weight))
  
}



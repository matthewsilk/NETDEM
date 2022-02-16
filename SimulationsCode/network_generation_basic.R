
##Function for basic network generation

#Probabilities of edges within and between groups (binomial)
#Weights of edges within and between groups (Beta, 2 parameter)
#Edge probabilities and weights depend on distance between groups
#Returns adjacency matrix and igraph network object (incl plot if desired)

network_generation_basic<-function(indiv_data,dist_mat,
                                   p_ig=0.2,w_ig1=1.5,w_ig2=5,
                                   p_og=0.2,w_og1=1,w_og2=5,
                                   d_effp=4,d_effw=4,
                                   plot=TRUE){

n<-nrow(indiv_data)  
  
#Generate underlying network of affiliative relationships
pop_mat<-matrix(0,nr=n,nc=n)

#to add: assortativity, transitivity, affects of individual traits

for(i in 1:(n-1)){
  for(j in (i+1):n){
    if(indiv_data$groups[i]==indiv_data$groups[j]){
      t_e1<-rbinom(1,1,p_ig)
      t_e2<-rbeta(1,w_ig1,w_ig2)
      pop_mat[i,j]<-pop_mat[j,i]<-t_e1*t_e2
    }
    if(indiv_data$groups[i]!=indiv_data$groups[j]){
      t_e1<-rbinom(1,1,p_og*(1/d_effp)^dist_mat[i,j])
      t_e2<-rbeta(1,w_og1,w_og2)*(1/d_effw)^dist_mat[i,j]
      pop_mat[i,j]<-pop_mat[j,i]<-t_e1*t_e2
    }
  }
}

pop_net<-graph.adjacency(pop_mat,mode="undirected",weighted=TRUE)

if(plot==TRUE){
  par(mar=c(0,0,0,0))
  plot(pop_net,vertex.label=NA,vertex.size=8,edge.width=(E(pop_net)$weight*10)^1.5)
}

output<-list(pop_mat,pop_net)

return(output)

}


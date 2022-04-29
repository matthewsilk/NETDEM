#'network_checker_seq
#'
#'Used to check how well sequential interaction data reflect underlying network
#'
#'@param gbi group-by-individual matrix generated from the interaction generation function
#'@param pop_net underlying network used to generate GBI
#'
#'@details This function can be used to test various properties of the social network generated from the interaction dataset to see how closely it is related to the original underlying network.
#'@return 1) a plot of the network generated from interaction data; 2) plots of number of observations in the GBI vs. metrics in the underlying network; 3) an estimate of the correlation between the two networks; 4) plots of metric correlations between the two networks
#'
#'@export

network_checker_seq<-function(gbi,pop_net){

  full_mat<-asnipe::get_network(gbi)
  full_net<-igraph::graph.adjacency(full_mat,mode="undirected",weighted=TRUE)

  #Plot network based on groups/interactions
  graphics::par(mar=c(0,0,0,0),mfrow=c(1,1))
  plot(full_net,vertex.label=NA,vertex.size=8,edge.width=(igraph::E(full_net)$weight*2)^1.5)

  #Plots number of observations in groups/interactions against centrality in underlying network
  graphics::par(mfrow=c(2,2))
  graphics::par(mar=c(5,5,2,2))
  plot(colSums(gbi)~igraph::degree(pop_net))
  plot(colSums(gbi)~igraph::strength(pop_net))
  plot(colSums(gbi)~igraph::betweenness(pop_net,weights=1/igraph::E(pop_net)$weight))
  plot(colSums(gbi)~igraph::closeness(pop_net,weights=1/igraph::E(pop_net)$weight))

  pop_mat<-igraph::as_adjacency_matrix(pop_net,sparse=FALSE,attr="weight")

  #Check correlation between the 2 networks
  cor_check<-sna::netlm(full_mat,pop_mat,nullhyp="qapspp")
  summary(cor_check)

  #Plot correlation between the centrality measures in the two networks
  graphics::par(mfrow=c(2,2))
  graphics::par(mar=c(5,5,2,2))
  plot(igraph::degree(full_net)~igraph::degree(pop_net))
  plot(igraph::strength(full_net)~igraph::strength(pop_net))
  plot(igraph::betweenness(full_net,weights=1/igraph::E(full_net)$weight)~igraph::betweenness(pop_net,weights=1/igraph::E(pop_net)$weight))
  plot(igraph::closeness(full_net,weights=1/igraph::E(full_net)$weight)~igraph::closeness(pop_net,weights=1/igraph::E(pop_net)$weight))

}

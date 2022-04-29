#'obs_net_checker
#'
#'Generates observations that can be used to generate CMR data and observed social networks.
#'
#'@param gbi_o observed group-by-individual matrix
#'@param full_mat adjacency matrix for the social network generated from grouping events
#'@param pop_mat adjacency matrix for the underlying network
#'@details A series of checks of how well the observed network mtaches the actual and underlying networks
#'@return 1) A plot of the observed network; 2) Correlation with the actual network; 3) Correlation with the underlying network; 4) Correlations of key centrality measures between the observed and actual network
#'
#'@export

obs_net_checker<-function(gbi_o,full_mat,pop_mat){

  gbi_o<-gbi_o[rowSums(gbi_o)>0,]

  obs_mat<-asnipe::get_network(gbi_o)
  obs_net<-igraph::graph.adjacency(obs_mat,mode="undirected",weighted=TRUE)

  #Plot network
  graphics::par(mar=c(0,0,0,0),mfrow=c(1,1))
  plot(obs_net,vertex.label=NA,vertex.size=8,edge.width=(igraph::E(obs_net)$weight*2)^1.5)

  #Check correlation with actual network
  cor_check<-sna::netlm(obs_mat,full_mat,nullhyp="qapspp")
  summary(cor_check)

  #Check correlation with underlying network
  cor_check2<-sna::netlm(obs_mat,pop_mat,nullhyp="qapspp")
  summary(cor_check2)

  #Plot centrality correlations with actual network
  graphics::par(mfrow=c(2,2))
  graphics::par(mar=c(5,5,2,2))
  plot(igraph::degree(obs_net)~igraph::degree(full_net))
  plot(igraph::strength(obs_net)~igraph::strength(full_net))
  plot(igraph::betweenness(obs_net,weights=1/igraph::E(obs_net)$weight)~igraph::betweenness(full_net,weights=1/igraph::E(full_net)$weight))
  plot(igraph::closeness(obs_net,weights=1/igraph::E(obs_net)$weight)~igraph::closeness(full_net,weights=1/igraph::E(full_net)$weight))

}

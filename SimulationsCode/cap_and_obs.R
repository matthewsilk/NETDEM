#'cap_and_obs
#'
#'Generates observations that can be used to generate CMR data and observed social networks.
#'

cap_and_obs<-function(samp_wind,gbi=gbi,
                      pcg=0.5,pmi=0.9,pci=0.9,
                      start_obs=1,end_obs=max(samp_wind),interval_obs=5,
                      start_cap=1,end_cap=21,interval_cap=2,
                      pre_cap=NULL){
  
  n_samp_wind<-length(unique(samp_wind))
  samp_wind_size<-table(samp_wind)
  
  cap_winds<-seq(start_cap,end_cap,interval_cap)
  obs_winds<-seq(start_obs,end_obs,interval_obs)
  
  gbi_o<-NULL
  gbi_c<-NULL
  
  gbi2<-gbi
  
  #rm(c_grs)
  #rm(o_grs)
  
  for(sw in 1:n_samp_wind){
    if(sw%in%cap_winds){
      gbi_t<-gbi2[samp_wind==sw,]
      if(sw==1){
        st_row=0
      }
      if(sw>1){
        st_row<-max(which(samp_wind==(sw-1)))
      }
      t_grs<-seq(1,nrow(gbi_t),1)
      st_grs<-sample(c(0,1),length(t_grs),replace=TRUE,prob=c(1-pcg,pcg))
      st_grs2<-st_grs*t_grs
      st_grs2<-st_grs2[st_grs2>0]
      gbi_t<-gbi_t[st_grs2,]
      if(is.vector(gbi_t)){gbi_t<-t(as.matrix(gbi_t))}
      if(nrow(gbi_t)>0){
        gbi_mirr<-array(sample(c(0,1),length(gbi_t),replace=TRUE,prob=c(1-pmi,pmi)),dim=dim(gbi_t))
        gbi_t<-gbi_t*gbi_mirr
        if(is.matrix(gbi_c)==FALSE){
          gbi_c<-gbi_t
          samp_wind_c<-rep(sw,length(st_grs2))
          #print("a")
          #print(st_grs2)
          #print(st_row)
          #print("b")
          c_grs<-st_grs2+st_row
        } else if(is.matrix(gbi_c)){
          gbi_c<-rbind(gbi_c,gbi_t)
          samp_wind_c<-c(samp_wind_c,rep(sw,length(st_grs2)))
          #c_grs<-c(c_grs,st_grs2+st_row)
          #print("c")
          #print(st_grs2)
          #print(st_row)
          #print("d")
          if(!exists("c_grs")){c_grs<-st_grs2+st_row}
          if(exists("c_grs")){c_grs<-c(c_grs,st_grs2+st_row)}
        }
      }
      #print(st_grs2)
      #print(gbi_t)
      #print(c_grs)
      #print(gbi_c)
    }
    if(sw%in%cap_winds==FALSE&sw%in%obs_winds==TRUE){
      gbi_t<-gbi2[samp_wind==sw,]
      if(sw==1){
        st_row=0
      }
      if(sw>1){
        st_row<-max(which(samp_wind==(sw-1)))
      }
      t_grs<-seq(1,nrow(gbi_t),1)
      st_grs<-sample(c(0,1),length(t_grs),replace=TRUE,prob=c(1-pcg,pcg))
      st_grs2<-st_grs*t_grs
      st_grs2<-st_grs2[st_grs2>0]
      gbi_t<-gbi_t[st_grs2,]
      if(is.vector(gbi_t)){gbi_t<-t(as.matrix(gbi_t))}
      if(nrow(gbi_t>0)){
        gbi_mirr<-array(sample(c(0,1),length(gbi_t),replace=TRUE,prob=c(1-pci,pci)),dim=dim(gbi_t))
        gbi_t<-gbi_t*gbi_mirr
        if(is.matrix(gbi_o)==FALSE){
          gbi_o<-gbi_t
          samp_wind_o<-rep(sw,length(st_grs2))
          o_grs<-st_grs2+st_row
        } else if(is.matrix(gbi_o)){
          gbi_o<-rbind(gbi_o,gbi_t)
          samp_wind_o<-c(samp_wind_o,rep(sw,length(st_grs2)))
          #print(exists("o_grs"))
          if(!exists("o_grs")){o_grs<-st_grs2+st_row}
          if(exists("o_grs")){o_grs<-c(o_grs,st_grs2+st_row)}
        }
      }
    }
  }
  
  gbi_c2<-array(0,dim=dim(gbi2))
  if(exists("c_grs")){gbi_c2[c_grs,]<-gbi_c}
  gbi_c3<-sign(apply(gbi_c2,2,cumsum))
  if(is.vector(pre_cap)){
    #pre_cap2<-t(as.matrix(pre_cap))
    gbi_c3<-sign(sweep(gbi_c3, 2, pre_cap, "+"))
  }
  gbi_o3<-array(0,dim=dim(gbi2))
  if(exists("o_grs")){
    #print(o_grs)
    gbi_c4<-gbi_c3[o_grs,]
    gbi_o2<-gbi_o*gbi_c4
    gbi_o3[o_grs,]<-gbi_o2
  }
  gbi_o4<-gbi_o3+gbi_c2
  
  if(!exists("c_grs")){c_grs<-NULL}
  if(!exists("o_grs")){o_grs<-NULL}
  
  #May need to adjust output to save some of these alternative matrices
  output<-list(gbi_c2,c_grs,gbi_o4,o_grs)
  
  return(output)
  
}

obs_net_checker<-function(gbi_o,full_mat,pop_mat){
  
  gbi_o<-gbi_o[rowSums(gbi_o)>0,]
  
  obs_mat<-get_network2(gbi_o)
  obs_net<-graph.adjacency(obs_mat,mode="undirected",weighted=TRUE)
  
  #Plot network
  par(mar=c(0,0,0,0),mfrow=c(1,1))
  plot(obs_net,vertex.label=NA,vertex.size=8,edge.width=(E(obs_net)$weight*2)^1.5)
  
  #Check correlation with actual network
  cor_check<-netlm(obs_mat,full_mat,nullhyp="qapspp")
  summary(cor_check)
  
  #Check correlation with underlying network
  cor_check2<-netlm(obs_mat,pop_mat,nullhyp="qapspp")
  summary(cor_check2)
  
  #Plot centrality correlations with actual network
  par(mfrow=c(2,2))
  par(mar=c(5,5,2,2))
  plot(igraph::degree(obs_net)~igraph::degree(full_net))
  plot(igraph::strength(obs_net)~igraph::strength(full_net))
  plot(igraph::betweenness(obs_net,weights=1/E(obs_net)$weight)~igraph::betweenness(full_net,weights=1/E(full_net)$weight))
  plot(igraph::closeness(obs_net,weights=1/E(obs_net)$weight)~igraph::closeness(full_net,weights=1/E(full_net)$weight))
  
}

# pcg = probability of capturing a gathering
# pmi = probability of marking/tagging an individual in a gathering on first capture
# pci = probability of capturing/observing an individual within a gathering



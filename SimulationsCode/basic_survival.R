basic_survival<-function(indiv_data,mps,lvps){
  mps<-0.95
  lmps<-car::logit(mps,adjust=0.001)
  lvps<-0.5
  indiv_data$survival<-inv.logit(rnorm(nrow(indiv_data),lmps,lvps))
  return(indiv_data)
}
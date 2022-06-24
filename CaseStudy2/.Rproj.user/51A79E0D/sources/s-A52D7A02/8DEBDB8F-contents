
ng<-c(20,200)
pcg<-c(0.1,0.2,0.4,1)
pmi<-c(0.5,0.75,1)
net_vars<-c("strength","betweenness_w")
net_effs<-c(0,0.4,0.8)
reps<-seq(1,20,1)

parameters2<-expand.grid(ng,pcg,pmi,net_vars,net_effs,reps)
names(parameters2)<-c("ng","pcg","pmi","net_vars","net_effs","reps")

saveRDS(parameters2,"parameters2.RDS")

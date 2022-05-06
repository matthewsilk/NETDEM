
p_og<-c(0.2,0.12335)
wo_m<-c(0.25,0.15957)
p_wr_i<-c(0.1,0.5,0.9)
pcg<-c(0.25,0.5,0.75)
net_vars<-c("strength","betweenness_w")
net_effs<-c(0,0.4,0.8)
net_cov<-c(-0.8,0,0.8)
reps<-seq(1,10,1)

parameters1<-expand.grid(p_og,wo_m,p_wr_i,pcg,net_vars,net_effs,net_cov,reps)
names(parameters1)<-c("p_og","wo_m","p_wr_i","pcg","net_vars","net_effs","net_cov","reps")

saveRDS(parameters1,"parameters1.RDS")

library(tidyr)
library(readr)
##Read in data

files<-list.files("results/")
files<-files[complete.cases(files)]
reps<-readr::parse_number(files)


results<-list()
for(i in 1:length(files)){
  results[[reps[i]]]<-eval(parse(text=paste0("readRDS('results/",files[i],"')")))
}

full<-results

##Read in initial parameters
pars1<-readRDS("parameters1.RDS")
pars1<-pars1[sort(reps),]
head(pars1)

##Function to extract posterior medians for all results

med_ext<-function(a,b){
  medians<-matrix(NA,nr=b,nc=ncol(a[[1]]))
  for(i in 1:b){
    medians[i,]<-apply(a[[i]],2,median)
  }
  return(medians)
}

posterior_medians<-lapply(results[sign(unlist(lapply(results,length)))==1],med_ext,b=4)
posterior_medians2<-data.frame(do.call(rbind,posterior_medians))

names(posterior_medians2)<-c("Beta1","Beta2","Beta3","p")

head(pars1)

pars1B<-pars1[rep(1:nrow(pars1),each=4),]
m_inf<-rep(c("M1","M2","M3","M4"),nrow(pars1))

dft<-cbind(pars1B,m_inf)

test_data<-data.frame(dft,posterior_medians2)

head(test_data)

boxplot(Beta3~net_effs*net_cov,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5,])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.4,0.8),lty=2)

boxplot(Beta3~net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5,])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~net_effs*pcg,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5,])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~m_inf*net_vars*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5,])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~net_cov*net_vars*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf=="M3",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~pcg*net_vars*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf=="M4",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)


boxplot(Beta3~pcg*net_vars*net_effs*net_cov,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf=="M2",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~pcg*net_vars*net_effs*net_cov,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf=="M4",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)


#############################


sd_ext<-function(a,b){
  sds<-matrix(NA,nr=b,nc=ncol(a[[1]]))
  for(i in 1:b){
    sds[i,]<-apply(a[[i]],2,sd)
  }
  return(sds)
}

posterior_sds<-lapply(results[sign(unlist(lapply(results,length)))==1],sd_ext,b=4)
posterior_sds2<-data.frame(do.call(rbind,posterior_sds))

plot(abs(posterior_medians2[,3]),posterior_sds2[,3])
lines(x=c(-100,100),y=c(1,1))
lines(x=c(2.5,2.5),y=c(-100,100))

which(abs(posterior_medians2[,3])>2.5&posterior_sds2[,3]<1)

clu<-kmeans(cbind(posterior_medians2[,3],posterior_sds2[,3]),centers=2)

cols<-rev(c("firebrick","light blue"))

plot(abs(posterior_medians2[,3]),posterior_sds2[,3],col=cols[clu$cluster])
lines(x=c(-100,100),y=c(1,1))
lines(x=c(2.5,2.5),y=c(-100,100))

centers<-6
clu<-kmeans(cbind(posterior_medians2[,3],posterior_sds2[,3]),centers=centers)
library(viridis)
cols<-viridis(centers)

plot(abs(posterior_medians2[,3]),posterior_sds2[,3],col=adjustcolor(cols[clu$cluster],0.6),pch=15)
lines(x=c(-100,100),y=c(1,1))
lines(x=c(2.5,2.5),y=c(-100,100))

clu_in<-which(clu$centers[,1]<1&clu$centers[,2]<1)

clu2<-ifelse(clu$cluster%in%clu_in==TRUE,1,0)

test_data$conv<-clu2

centers2<-seq(2,10,1)
wss<-sapply(centers2,
              function(k){kmeans(cbind(posterior_medians2[,3],posterior_sds2[,3]), k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(centers2, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

############################

boxplot(Beta3~net_effs*net_cov,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf=="M1"|test_data$m_inf=="M2",],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~net_effs*net_cov,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf=="M3"|test_data$m_inf=="M4",],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)


boxplot(Beta3~m_inf*net_vars*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf=="M1"|test_data$m_inf=="M2",],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~m_inf*p_wr_i*net_vars*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf=="M1"|test_data$m_inf=="M2",],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~pcg*net_vars*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf=="M1"|test_data$m_inf=="M2",],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~pcg*net_vars*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf=="M3"|test_data$m_inf=="M4",],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

mod<-glm(Beta3~as.factor(net_effs)*net_vars*pcg,data=test_data[test_data$m_inf=="M1"|test_data$m_inf=="M2",])
summary(mod)

boxplot(Beta3~net_cov*pcg,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf%in%c("M1","M2")==TRUE&test_data$net_effs==0,],ylim=c(-0.3,0.3))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~net_cov*net_vars*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf%in%c("M1","M2")==TRUE&test_data$pcg==0.75,],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~net_vars*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf%in%c("M1","M2")==TRUE&test_data$pcg==0.75,],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~m_inf*p_wr_i*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf%in%c("M1","M2")==TRUE&test_data$pcg==0.75&test_data$net_vars=="strength",],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~m_inf*p_wr_i*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf%in%c("M1","M2")==TRUE&test_data$pcg==0.25&test_data$net_vars=="strength",],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~m_inf*p_wr_i*net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf%in%c("M1","M2")==TRUE&test_data$pcg==0.25,],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~m_inf*p_wr_i*net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$m_inf%in%c("M1","M2")==TRUE&test_data$pcg==0.75,],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)


##################################
##################################
##################################

library(HDInterval)
hdi(results[[10]][[4]][,3], cred_int = 0.89)

chain_infoM1<-matrix(NA,nr=length(results),nc=4)

for(i in 1:length(results)){
  if(length(results[[i]])>0){
  chain_infoM1[i,1:2]<-hdi(results[[i]][[1]][,3], cred_int = 0.89)
  chain_infoM1[i,3]<-sum(results[[i]][[1]][,3]>0)/nrow(results[[i]][[1]])
  chain_infoM1[i,4]<-ifelse(0<chain_infoM1[i,1]|0>chain_infoM1[i,2],1,0)
  }
}

chain_infoM2<-matrix(NA,nr=length(results),nc=4)

for(i in 1:length(results)){
  if(length(results[[i]])>0){
  chain_infoM2[i,1:2]<-hdi(results[[i]][[2]][,3], cred_int = 0.89)
  chain_infoM2[i,3]<-sum(results[[i]][[2]][,3]>0)/nrow(results[[i]][[2]])
  chain_infoM2[i,4]<-ifelse(0<chain_infoM2[i,1]|0>chain_infoM2[i,2],1,0)
  }
}

chain_infoM3<-matrix(NA,nr=length(results),nc=4)

for(i in 1:length(results)){
  if(length(results[[i]])>0){
  chain_infoM3[i,1:2]<-hdi(results[[i]][[3]][,3], cred_int = 0.89)
  chain_infoM3[i,3]<-sum(results[[i]][[3]][,3]>0)/nrow(results[[i]][[3]])
  chain_infoM3[i,4]<-ifelse(0<chain_infoM3[i,1]|0>chain_infoM3[i,2],1,0)
  }
}

chain_infoM4<-matrix(NA,nr=length(results),nc=4)

for(i in 1:length(results)){
  if(length(results[[i]])>0){
  chain_infoM4[i,1:2]<-hdi(results[[i]][[4]][,3], cred_int = 0.89)
  chain_infoM4[i,3]<-sum(results[[i]][[4]][,3]>0)/nrow(results[[i]][[4]])
  chain_infoM4[i,4]<-ifelse(0<chain_infoM4[i,1]|0>chain_infoM4[i,2],1,0)
  }
}



boxplot(chain_infoM1[which(complete.cases(chain_infoM1[,3])==TRUE),3]~test_data$net_vars[test_data$m_inf=="M1"&test_data$conv==1]*test_data$net_effs[test_data$m_inf=="M1"&test_data$conv==1])

dim(chain_infoM1[which(complete.cases(chain_infoM1[,3])==TRUE),])
dim(chain_infoM2[which(complete.cases(chain_infoM1[,3])==TRUE),])
dim(chain_infoM3[which(complete.cases(chain_infoM1[,3])==TRUE),])
dim(chain_infoM4[which(complete.cases(chain_infoM1[,3])==TRUE),])

test_data_add<-matrix(NA,nr=nrow(test_data),nc=4)
test_data_add[which(test_data$m_inf=="M1"),]<-chain_infoM1[which(complete.cases(chain_infoM1[,3])==TRUE),]
test_data_add[test_data$m_inf=="M2",]<-chain_infoM2[which(complete.cases(chain_infoM2[,3])==TRUE),]
test_data_add[test_data$m_inf=="M3",]<-chain_infoM3[which(complete.cases(chain_infoM3[,3])==TRUE),]
test_data_add[test_data$m_inf=="M4",]<-chain_infoM4[which(complete.cases(chain_infoM4[,3])==TRUE),]
test_data_add<-data.frame(test_data_add)
names(test_data_add)<-c("cIlo","CIhi","Big0","StatClear")

test_data2<-data.frame(test_data,test_data_add)


boxplot(Big0~pcg*net_effs*net_vars,data=test_data2[test_data2$conv==1&test_data2$m_inf%in%c("M1","M2"),])
boxplot(Big0~m_inf*p_wr_i*net_effs*net_vars,data=test_data2[test_data2$conv==1&test_data2$m_inf%in%c("M1","M2")&test_data2$pcg==0.75,])
boxplot(Big0~m_inf*p_wr_i*net_effs*net_vars,data=test_data2[test_data2$conv==1&test_data2$m_inf%in%c("M1","M2")&test_data2$pcg==0.25,])

boxplot(Big0~net_cov*net_effs*net_vars,data=test_data2[test_data2$conv==1&test_data2$m_inf%in%c("M1","M2")&test_data2$pcg==0.25,])
boxplot(Big0~net_cov*net_effs*net_vars,data=test_data2[test_data2$conv==1&test_data2$m_inf%in%c("M1","M2")&test_data2$pcg==0.75,])

test_data2A<-test_data2[test_data2$conv==1,]

aggregate(test_data2A$StatClear,by=list(test_data2A$m_inf,test_data2A$net_vars,test_data2A$net_effs),mean)

aggregate(test_data2A$StatClear,by=list(test_data2A$m_inf,test_data2A$net_vars,test_data2A$net_effs,test_data2A$pcg),mean)

aggregate(test_data2A$StatClear,by=list(test_data2A$m_inf,test_data2A$net_vars,test_data2A$net_effs,test_data2A$pcg),mean)

aggregate(test_data2A$StatClear,by=list(test_data2A$pcg,test_data2A$m_inf,test_data2A$net_vars,test_data2A$net_effs),mean)

aggregate(test_data2$conv,by=list(test_data2$pcg,test_data2$m_inf,test_data2$net_effs,test_data2$net_vars),mean)

######################
#####################

library(emdist)

a<-density(results[[1]][[1]][,3])
b<-density(results[[1]][[3]][,3])

c<-density(results[[1]][[2]][,3])
d<-density(results[[1]][[4]][,3])

emd(cbind(a$y,a$x),cbind(b$y,b$x),max.iter=10000)
emd(cbind(c$y,c$x),cbind(d$y,d$x),max.iter=10000)

emds<-matrix(NA,nr=length(results),nc=2)

for(i in 1:nrow(emds)){
  if(length(results[[i]])>0){
    a<-density(results[[i]][[1]][,3])
    b<-density(results[[i]][[3]][,3])
    c<-density(results[[i]][[2]][,3])
    d<-density(results[[i]][[4]][,3])
    emds[i,1]<-emd(cbind(a$y,a$x),cbind(b$y,b$x),max.iter=1000)
    emds[i,2]<-emd(cbind(c$y,c$x),cbind(d$y,d$x),max.iter=1000)
  }
}

emds2<-emds[complete.cases(emds[,1]),]

emd_pars<-data.frame(pars1,emds2)

boxplot(X1~net_effs*net_vars,data=emd_pars,ylim=c(0,2))
boxplot(X2~pcg*net_effs*net_vars,data=emd_pars,ylim=c(0,2))
boxplot(X2~pcg*net_effs*net_vars,data=emd_pars,ylim=c(0,2))
boxplot(X1~pcg*net_effs*net_vars,data=emd_pars,ylim=c(0,2))
boxplot(X2~net_cov*pcg*net_effs*net_vars,data=emd_pars,ylim=c(0,2))

emd_pars$conv1<-test_data2$conv[test_data2$m_inf=="M1"]
emd_pars$conv2<-test_data2$conv[test_data2$m_inf=="M2"]

par(mfrow=c(1,2))
boxplot(X1~net_effs*net_vars,data=emd_pars[emd_pars$conv1==1,],ylim=c(0,2))
boxplot(X2~net_effs*net_vars,data=emd_pars[emd_pars$conv2==1,],ylim=c(0,2))
par(mfrow=c(1,1))
boxplot(X2~pcg*net_effs*net_vars,data=emd_pars[emd_pars$conv2==1,],ylim=c(0,2))

par(mfrow=c(1,2))
boxplot(X1~pcg*net_effs*net_vars,data=emd_pars[emd_pars$conv1==1,],ylim=c(0,2))
boxplot(X2~pcg*net_effs*net_vars,data=emd_pars[emd_pars$conv2==1,],ylim=c(0,2))
par(mfrow=c(1,1))

par(mfrow=c(2,2))
boxplot(Beta3~pcg*net_vars*net_effs,data=test_data[test_data$conv==1&test_data$m_inf=="M1",],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)
boxplot(Beta3~pcg*net_vars*net_effs,data=test_data[test_data$conv==1&test_data$m_inf=="M2",],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)
boxplot(Beta3~pcg*net_vars*net_effs,data=test_data[test_data$conv==1&test_data$m_inf=="M3",],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)
boxplot(Beta3~pcg*net_vars*net_effs,data=test_data[test_data$conv==1&test_data$m_inf=="M4",],ylim=c(-0.25,1.25))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)
par(mfrow=c(1,1))

boxplot(X2~net_cov*pcg*net_effs*net_vars,data=emd_pars[emd_pars$conv1==2,],ylim=c(0,2))


boxplot((Beta2-Beta1)~net_effs*net_vars*pcg,data=test_data2A,ylim=c(-0.2,1.4))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.5,0.5),lty=2)

boxplot((Beta2-Beta1)~m_inf,data=test_data2A,ylim=c(-0.2,1.4))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.5,0.5),lty=2)

boxplot(boot::inv.logit(Beta1)~net_effs*net_vars*pcg,data=test_data2A,ylim=c(0,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(p~net_effs*net_vars*pcg,data=test_data2A,ylim=c(0,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(1-0.75^5,1-0.75^5),lty=2)
lines(x=c(-100,100),y=c(1-0.5^5,1-0.5^5),lty=2)
lines(x=c(-100,100),y=c(1-0.25^5,1-0.25^5),lty=2)


exp_mean<-(boot::inv.logit(boot::logit(0.8)+0.5)+0.8)/2

true_surv<-matrix(NA,nr=length(results),nc=10)
for(i in 1:length(results)){
  if(length(results[[i]])>0){
  true_surv[i,]<-unlist(lapply(results[[i]][[5]],colMeans))[seq(2,20,2)]
  }
}

par(mfrow=c(2,2))
plot((boot::inv.logit(test_data2$Beta1[test_data2$m_inf=="M1"])+boot::inv.logit(test_data2$Beta2[test_data2$m_inf=="M1"]))/2~rowMeans(true_surv[complete.cases(true_surv[,1]),]),ylim=c(0.7,0.9),xlim=c(0.7,0.9),pch=16,col=adjustcolor("light grey",0.2),las=1,ylab="Model estimate",xlab="Mean survival",cex.lab=1.4)
lines(x=c(exp_mean,exp_mean),y=c(-100,100),lwd=2,col="black",lty=2)
lines(y=c(exp_mean,exp_mean),x=c(-100,100),lwd=2,col="black",lty=2)
mts<-mean(rowMeans(true_surv[complete.cases(true_surv[,1]),]))
mms<-mean((boot::inv.logit(test_data2$Beta1[test_data2$m_inf=="M1"&test_data2$conv==1])+boot::inv.logit(test_data2$Beta2[test_data2$m_inf=="M1"&test_data2$conv==1]))/2)
points(x=mts,y=mts,cex=3,col="firebrick",pch=8,lwd=3)
points(x=mms,y=mms,cex=2,col="cornflowerblue",pch=8,lwd=2)
mtext("M1",side=3,adj=0.95,cex=1.5,line=-2)
plot((boot::inv.logit(test_data2$Beta1[test_data2$m_inf=="M2"])+boot::inv.logit(test_data2$Beta2[test_data2$m_inf=="M2"]))/2~rowMeans(true_surv[complete.cases(true_surv[,1]),]),ylim=c(0.7,0.9),xlim=c(0.7,0.9),pch=16,col=adjustcolor("light grey",0.2),las=1,ylab="Model estimate",xlab="Mean survival",cex.lab=1.4)
lines(x=c(exp_mean,exp_mean),y=c(-100,100),lwd=2,col="black",lty=2)
lines(y=c(exp_mean,exp_mean),x=c(-100,100),lwd=2,col="black",lty=2)
mms<-mean((boot::inv.logit(test_data2$Beta1[test_data2$m_inf=="M2"&test_data2$conv==1])+boot::inv.logit(test_data2$Beta2[test_data2$m_inf=="M2"&test_data2$conv==1]))/2)
points(x=mts,y=mts,cex=3,col="firebrick",pch=8,lwd=3)
points(x=mms,y=mms,cex=2,col="cornflowerblue",pch=8,lwd=2)
mtext("M2",side=3,adj=0.95,cex=1.5,line=-2)
plot((boot::inv.logit(test_data2$Beta1[test_data2$m_inf=="M3"])+boot::inv.logit(test_data2$Beta2[test_data2$m_inf=="M3"]))/2~rowMeans(true_surv[complete.cases(true_surv[,1]),]),ylim=c(0.7,0.9),xlim=c(0.7,0.9),pch=16,col=adjustcolor("light grey",0.2),las=1,ylab="Model estimate",xlab="Mean survival",cex.lab=1.4)
lines(x=c(exp_mean,exp_mean),y=c(-100,100),lwd=2,col="black",lty=2)
lines(y=c(exp_mean,exp_mean),x=c(-100,100),lwd=2,col="black",lty=2)
mms<-mean((boot::inv.logit(test_data2$Beta1[test_data2$m_inf=="M3"&test_data2$conv==1])+boot::inv.logit(test_data2$Beta2[test_data2$m_inf=="M3"&test_data2$conv==1]))/2)
points(x=mts,y=mts,cex=3,col="firebrick",pch=8,lwd=3)
points(x=mms,y=mms,cex=2,col="cornflowerblue",pch=8,lwd=2)
mtext("M3",side=3,adj=0.95,cex=1.5,line=-2)
plot((boot::inv.logit(test_data2$Beta1[test_data2$m_inf=="M4"])+boot::inv.logit(test_data2$Beta2[test_data2$m_inf=="M4"]))/2~rowMeans(true_surv[complete.cases(true_surv[,1]),]),ylim=c(0.7,0.9),xlim=c(0.7,0.9),pch=16,col=adjustcolor("light grey",0.2),las=1,ylab="Model estimate",xlab="Mean survival",cex.lab=1.4)
lines(x=c(exp_mean,exp_mean),y=c(-100,100),lwd=2,col="black",lty=2)
lines(y=c(exp_mean,exp_mean),x=c(-100,100),lwd=2,col="black",lty=2)
mms<-mean((boot::inv.logit(test_data2$Beta1[test_data2$m_inf=="M4"&test_data2$conv==1])+boot::inv.logit(test_data2$Beta2[test_data2$m_inf=="M4"&test_data2$conv==1]))/2)
points(x=mts,y=mts,cex=3,col="firebrick",pch=8,lwd=3)
points(x=mms,y=mms,cex=2,col="cornflowerblue",pch=8,lwd=2)
mtext("M4",side=3,adj=0.95,cex=1.5,line=-2)
par(mfrow=c(1,1))


boxplot(chain_info[,3]~test_data$pcg*test_data$net_vars*test_data$net_effs)

boxplot(chain_info[,3]~test_data$ng*test_data$net_vars*test_data$net_effs)
boxplot(test_data$Beta3~test_data$pcg*test_data$net_vars*test_data$net_effs)

boxplot(chain_info[,3]~test_data$pmi*test_data$net_vars*test_data$net_effs)


aggregate(chain_info[,3],by=list(test_data$ng,test_data$net_vars,test_data$net_effs),median)
aggregate(chain_info[,3],by=list(test_data$pcg,test_data$net_vars,test_data$net_effs),median)

boxplot(chain_info[,3]~test_data$ng*test_data$pmi*test_data$net_vars*test_data$net_effs)



aggregate(chain_info[,4],by=list(test_data$ng,test_data$net_vars,test_data$net_effs),mean)
aggregate(chain_info[,4],by=list(test_data$pcg,test_data$net_vars,test_data$net_effs),mean)


boxplot(chain_info[,3][test_data$pmi==1]~test_data$pcg[test_data$pmi==1]*test_data$net_vars[test_data$pmi==1]*test_data$net_effs[test_data$pmi==1])
boxplot(chain_info[,3][test_data$pmi==1]~test_data$ng[test_data$pmi==1]*test_data$net_vars[test_data$pmi==1]*test_data$net_effs[test_data$pmi==1])


aggregate(as.data.frame(chain_info[,4]),by=list(test_data$ng,test_data$pmi,test_data$pcg,test_data$net_vars,test_data$net_effs),mean)

aggregate(as.data.frame(chain_info[,4]),by=list(test_data$pmi,test_data$pcg,test_data$net_vars,test_data$net_effs),mean)


chain_info2<-data.frame(chain_info,test_data)
names(chain_info2)[1:4]<-c("lowCI","highCI","greater0","CIout")
mod<-glm(CIout~as.factor(ng)*net_vars*as.factor(net_effs),data=chain_info2,family=binomial)
summary(mod)

mod<-glm(CIout~as.factor(pcg)*net_vars*as.factor(net_effs),data=chain_info2,family=binomial)
summary(mod)

mod<-glm(CIout~as.factor(pmi)*net_vars*as.factor(net_effs),data=chain_info2,family=binomial)
summary(mod)

###########

mod<-glm(CIout~as.factor(ng)*net_vars*as.factor(net_effs),data=chain_info2[chain_info2$pmi==1,],family=binomial)
summary(mod)

mod<-glm(CIout~as.factor(pcg)*net_vars*as.factor(net_effs),data=chain_info2[chain_info2$pmi==1,],family=binomial)
summary(mod)

###########


#install.packages("emdist")
library(emdist)

a<-hist(results[[1]][,3],breaks=seq(-1,1,0.02))
b<-hist(results[[2]][,3],breaks=seq(-1,1,0.02))

emd(cbind(a$counts,a$mids),cbind(b$counts,b$mids))

a<-density(results[[1]][,3])
b<-density(results[[2]][,3])

emd(cbind(a$y,a$x),cbind(b$y,b$x),max.iter=10000)




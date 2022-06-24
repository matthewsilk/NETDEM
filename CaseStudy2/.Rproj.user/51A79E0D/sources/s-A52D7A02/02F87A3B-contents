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
pars1<-readRDS("parameters2.RDS")
pars1<-pars1[sort(reps),]
head(pars1)

##Function to extract posterior medians for all results

med_ext<-function(a){
  if(sum(class(a)=="list")>1){
    medians<-matrix(NA,nr=length(a),nc=ncol(a[[1]]))
    for(i in 1:length(a)){
      medians[i,]<-apply(a[[i]],2,median)
    }
  }
  if(sum(class(a)!="list")>1){
    medians<-apply(a,2,median)
  }
  return(medians)
}

sd_ext<-function(a){
  if(sum(class(a)=="list")>1){
    sds<-matrix(NA,nr=length(a),nc=ncol(a[[1]]))
    for(i in 1:length(a)){
      sds[i,]<-apply(a[[i]],2,sd)
    }
  }
  if(sum(class(a)!="list")>1){
    sds<-apply(a,2,sd)
  }
  return(sds)
}

results<-sapply(full,"[",1)

posterior_medians<-lapply(results,med_ext)[sign(unlist(lapply(results,length)))==1]
posterior_medians2<-data.frame(do.call(rbind,posterior_medians))

posterior_sds<-lapply(results,sd_ext)[sign(unlist(lapply(results,length)))==1]
posterior_sds2<-data.frame(do.call(rbind,posterior_sds))

names(posterior_medians2)<-c("Beta1","Beta2","Beta3","p")
names(posterior_sds2)<-c("sdBeta1","sdBeta2","sdBeta3","sdp")
head(pars1)

pars1B<-pars1[rep(1:nrow(pars1),each=1),]

dft<-cbind(pars1B)

test_data<-data.frame(dft[,],posterior_medians2,posterior_sds2)

head(test_data)





plot(abs(posterior_medians2[,3]),posterior_sds2[,3])
lines(x=c(-100,100),y=c(1,1))
lines(x=c(2.5,2.5),y=c(-100,100))

which(abs(posterior_medians2[,3])>2.5&posterior_sds2[,3]<1)

boxplot(Beta3~pcg*net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$pmi==1,],ylim=c(-0.25,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)
for(i in seq(0.02,0.38,0.02)){
  lines(x=c(-100,100),y=c(i,i),lty=3)
}

boxplot(Beta3~pcg*net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$pmi==0.75,],ylim=c(-0.25,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)
for(i in seq(0.02,0.38,0.02)){
  lines(x=c(-100,100),y=c(i,i),lty=3)
}

boxplot(Beta3~pcg*net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$pmi==0.5,],ylim=c(-0.25,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)
for(i in seq(0.02,0.38,0.02)){
  lines(x=c(-100,100),y=c(i,i),lty=3)
}

boxplot(Beta3~ng*net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$pmi==1,],ylim=c(-0.25,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)
for(i in seq(0.02,0.38,0.02)){
  lines(x=c(-100,100),y=c(i,i),lty=3)
}

boxplot(Beta3~ng*net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$pmi==0.75,],ylim=c(-0.25,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)
for(i in seq(0.02,0.38,0.02)){
  lines(x=c(-100,100),y=c(i,i),lty=3)
}

boxplot(Beta3~ng*net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$pmi==0.5,],ylim=c(-0.25,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)
for(i in seq(0.02,0.38,0.02)){
  lines(x=c(-100,100),y=c(i,i),lty=3)
}

boxplot((Beta2-Beta1)~pcg*net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$pmi==1,],ylim=c(0,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.5,0.5),lty=2)


boxplot(boot::inv.logit(Beta1)~pcg*net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$pmi==1,],ylim=c(0.7,0.9))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)


boxplot(Beta3~pcg*net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$pmi==0.75,],ylim=c(-0.25,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~pcg*net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$pmi==0.5,],ylim=c(-0.25,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)



boxplot(Beta3~net_effs*net_vars,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5,],ylim=c(-0.25,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)


boxplot(Beta3~net_effs*ng,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="strength",],ylim=c(-0.25,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~net_effs*ng,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="betweenness_w",],ylim=c(-0.25,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)


boxplot(Beta3~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="strength",],ylim=c(-0.25,0.6))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~ng*pmi*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="strength",],ylim=c(-0.25,0.6))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~ng*pmi*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="strength",],ylim=c(-0.25,0.6))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

####################################

boxplot(Beta3~ng*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="betweenness_w",],ylim=c(-0.25,1))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)


boxplot(Beta3~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="betweenness_w",],ylim=c(-0.25,0.6))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(Beta3~ng*pmi*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="betweenness_w",],ylim=c(-0.25,0.6))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

###############################################

boxplot(Beta3~ng*pmi*pcg,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="betweenness_w"&test_data$net_effs==0.8,],ylim=c(-0.25,0.6))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

##############################################

boxplot(sdBeta3~net_effs*net_vars,data=test_data[test_data$sdBeta3<0.5,])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)


boxplot(sdBeta3~ng*net_effs,data=test_data[test_data$sdBeta3<0.5&test_data$net_vars=="strength",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(sdBeta3~ng*pcg*net_effs,data=test_data[test_data$sdBeta3<0.5&test_data$net_vars=="strength",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(sdBeta3~ng*pmi*net_effs,data=test_data[test_data$sdBeta3<0.55&test_data$net_vars=="strength",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(sdBeta3~ng*net_effs,data=test_data[test_data$sdBeta3<0.5&test_data$net_vars=="betweenness_w",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(sdBeta3~ng*pcg*net_effs,data=test_data[test_data$sdBeta3<0.5&test_data$net_vars=="betweenness_w",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)

boxplot(sdBeta3~ng*pmi*net_effs,data=test_data[test_data$sdBeta3<0.55&test_data$net_vars=="betweenness_w",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)
lines(x=c(-100,100),y=c(0.8,0.8),lty=2)


#######################

boxplot(Beta3~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="strength",],ylim=c(-0.25,0.6))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.1,0.1),lty=2)
lines(x=c(-100,100),y=c(0.2,0.2),lty=2)
lines(x=c(-100,100),y=c(0.3,0.3),lty=2)
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)

boxplot(Beta3~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="betweenness_w",],ylim=c(-0.25,0.6))
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.1,0.1),lty=2)
lines(x=c(-100,100),y=c(0.2,0.2),lty=2)
lines(x=c(-100,100),y=c(0.3,0.3),lty=2)
lines(x=c(-100,100),y=c(0.4,0.4),lty=2)

boxplot(sdBeta3~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="strength",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.06,0.06),lty=2)
lines(x=c(-100,100),y=c(0.07,0.07),lty=2)
lines(x=c(-100,100),y=c(0.08,0.08),lty=2)
lines(x=c(-100,100),y=c(0.09,0.09),lty=2)

boxplot(sdBeta3~pcg*net_effs,data=test_data[test_data$sdBeta3<0.5&test_data$net_vars=="betweenness_w",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.06,0.06),lty=2)
lines(x=c(-100,100),y=c(0.07,0.07),lty=2)
lines(x=c(-100,100),y=c(0.08,0.08),lty=2)
lines(x=c(-100,100),y=c(0.09,0.09),lty=2)


boxplot(sdBeta3~ng*pcg*net_effs,data=test_data[test_data$sdBeta3<0.5&test_data$net_vars=="betweenness_w",])
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.06,0.06),lty=2)
lines(x=c(-100,100),y=c(0.07,0.07),lty=2)
lines(x=c(-100,100),y=c(0.08,0.08),lty=2)
lines(x=c(-100,100),y=c(0.09,0.09),lty=2)



#####################################


boxplot(p~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="strength",])
boxplot(boot::inv.logit(Beta1)~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="strength",])
boxplot(boot::inv.logit(Beta1)~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="betweenness_w",])
boxplot(p~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="betweenness_w",])

boxplot(boot::inv.logit(Beta2)~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="strength",])
boxplot(boot::inv.logit(Beta2)~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="betweenness_w",])
boxplot((Beta2-Beta1)~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="strength",])
boxplot((Beta2-Beta1)~ng*pcg*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="betweenness_w",])



boxplot(p~pcg*pmi*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="strength",])
boxplot(Beta1~pcg*pmi*net_effs,data=test_data[test_data$Beta3<1.5&test_data$Beta3>-1.5&test_data$net_vars=="strength",])


boxplot(sdp~pcg*pmi*net_effs,data=test_data[test_data$sdp<0.025&test_data$net_vars=="strength",])

#install.packages("HDInterval")
library(HDInterval)
hdi(results[[10]][,3], cred_int = 0.89)

chain_info<-matrix(NA,nr=length(results),nc=4)

for(i in 1:length(results)){
  chain_info[i,1:2]<-hdi(results[[i]][,3], cred_int = 0.89)
  chain_info[i,3]<-sum(results[[i]][,3]>0)/nrow(results[[i]])
  chain_info[i,4]<-ifelse(0<chain_info[i,1]|0>chain_info[i,2],1,0)
}

boxplot(chain_info[,3]~test_data$net_vars*test_data$net_effs)

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


##Load required R packages
library(tidyr)
library(readr)
library(viridis)
library(HDInterval)
library(emdist)
library(knitr)
library(kableExtra)
library(magrittr)
library(flextable)

################################
################################

##Define function to extract posterior medians for all results

med_ext<-function(a,b){
  medians<-matrix(NA,nr=b,nc=ncol(a[[1]]))
  for(i in 1:b){
    medians[i,]<-apply(a[[i]],2,median)
  }
  return(medians)
}

##Define function to extract posterior standard deviations for all results

sd_ext<-function(a,b){
  sds<-matrix(NA,nr=b,nc=ncol(a[[1]]))
  for(i in 1:b){
    sds[i,]<-apply(a[[i]],2,sd)
  }
  return(sds)
}

################################
################################

##Read in simulation results
files<-list.files("results/")
files<-files[complete.cases(files)]
reps<-readr::parse_number(files)
results<-list()
for(i in 1:length(files)){
  results[[reps[i]]]<-eval(parse(text=paste0("readRDS('results/",files[i],"')")))
}

##Store full results in separate vector
full<-results

##Read in initial parameters
pars1<-readRDS("parameters1.RDS")
pars1<-pars1[sort(reps),]

################################
################################

##Calculate posterior medians
posterior_medians<-lapply(results[sign(unlist(lapply(results,length)))==1],med_ext,b=4)
posterior_medians2<-data.frame(do.call(rbind,posterior_medians))
names(posterior_medians2)<-c("Beta1","Beta2","Beta3","p")

################################
################################

##Create dataframe with separate rows for each type of model
pars1B<-pars1[rep(1:nrow(pars1),each=4),]
m_inf<-rep(c("M1","M2","M3","M4"),nrow(pars1))
dft<-cbind(pars1B,m_inf)
test_data<-data.frame(dft,posterior_medians2)

################################
################################

##Calculate posterior standard deviations
posterior_sds<-lapply(results[sign(unlist(lapply(results,length)))==1],sd_ext,b=4)
posterior_sds2<-data.frame(do.call(rbind,posterior_sds))

##Identify which models have converged (for more full explanation see code
##to plot Fig. S1 below)
centers<-6
clu<-kmeans(cbind(posterior_medians2[,3],posterior_sds2[,3]),centers=centers)
clu_in<-which(clu$centers[,1]<1&clu$centers[,2]<1)
clu2<-ifelse(clu$cluster%in%clu_in==TRUE,1,0)
test_data$conv<-clu2

################################
################################

##Create dataframe storing information about credible intervals for each model type in turn
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

################################
################################

##Add these dataframes to the existing dataframe
test_data_add<-matrix(NA,nr=nrow(test_data),nc=4)
test_data_add[which(test_data$m_inf=="M1"),]<-chain_infoM1[which(complete.cases(chain_infoM1[,3])==TRUE),]
test_data_add[test_data$m_inf=="M2",]<-chain_infoM2[which(complete.cases(chain_infoM2[,3])==TRUE),]
test_data_add[test_data$m_inf=="M3",]<-chain_infoM3[which(complete.cases(chain_infoM3[,3])==TRUE),]
test_data_add[test_data$m_inf=="M4",]<-chain_infoM4[which(complete.cases(chain_infoM4[,3])==TRUE),]
test_data_add<-data.frame(test_data_add)
names(test_data_add)<-c("cIlo","CIhi","Big0","StatClear")
test_data2<-data.frame(test_data,test_data_add)

##Version for only converged models
test_data2A<-test_data2[test_data2$conv==1,]

################################
################################

##Calculate EMDs for model pairs 1-3 and 2-4
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

##Store as dataframe and add information on convergence
emds2<-emds[complete.cases(emds[,1]),]
emd_pars<-data.frame(pars1,emds2)
emd_pars$conv1<-test_data2$conv[test_data2$m_inf=="M1"]
emd_pars$conv2<-test_data2$conv[test_data2$m_inf=="M2"]

################################
################################

##Calculate input baseline survival to simulations and true value from
##simulations for later comparison
exp_mean<-(boot::inv.logit(boot::logit(0.8)+0.5)+0.8)/2
true_surv<-matrix(NA,nr=length(results),nc=10)
for(i in 1:length(results)){
  if(length(results[[i]])>0){
    true_surv[i,]<-unlist(lapply(results[[i]][[5]],colMeans))[seq(2,20,2)]
  }
}

################################
################################
################################
################################

##Table 1
test_data2B<-test_data2A[test_data2A$net_effs>0&test_data2A$pcg==0.5,]
stat_tab<-aggregate(test_data2B$StatClear,by=list(test_data2B$pcg,test_data2B$m_inf,test_data2B$net_vars,test_data2B$net_effs),mean)
stat_tab2<-stat_tab[,c(3,4,2,1,5)]
names(stat_tab2)<-c("Network measure","True effect","Model","Group capture probability","Detection rate")
stat_tab2[,1]<-as.character(stat_tab2[,1])
stat_tab2[stat_tab2[,1]=="strength",1]<-"Strength"
stat_tab2[stat_tab2[,1]=="betweenness_w",1]<-"Betweenness"
stat_tab2[,5]<-round(stat_tab2[,5],2)
T1<-regulartable(stat_tab2)
T1<-bold(T1,part="header")
T1<-font(T1,fontname="Arial")
T1<-fontsize(T1,size=9)
T1<-fontsize(T1,size=10,part="header")
T1<-padding(T1,i=seq(1,16,1),padding=c(1,1,1,1))
T1<-width(T1,j=seq(1,5,1),width=c(1,1,0.8,1.2,0.8))
T1<-height(T1,i=seq(1,16,1),height=rep(0.01,16))
T1<-align(T1,j=c(2,4),align="left")
T1<-align(T1,j=c(2,4),align="left",part="header")
T1<-hline(T1,i=seq(4,16,4),border=fp_border_default(width=2))
T1<-hline(T1,i=seq(8,16,8),border=fp_border_default(width=3))
T1

################################
################################

##Table 2
conv_tab<-aggregate(test_data2$conv,by=list(test_data2$pcg,test_data2$m_inf,test_data2$net_vars),mean)
conv_tab2<-conv_tab[,c(3,2,1,4)]
names(conv_tab2)<-c("Network measure","Model","Group capture probability","Convergence rate")
conv_tab2[,1]<-as.character(conv_tab2[,1])
conv_tab2[conv_tab2[,1]=="strength",1]<-"Strength"
conv_tab2[conv_tab2[,1]=="betweenness_w",1]<-"Betweenness"
conv_tab2[,4]<-round(conv_tab2[,4],2)
conv_table<-flextable(conv_tab2)
conv_table<-width(conv_table,width=c(1.5,0.5,2.1,1.5))
conv_table<-padding(conv_table,padding=c(0.3,0.3,0.3,0.3),part="all")
conv_table<-font(conv_table,fontname="Arial")
conv_table<-fontsize(conv_table,size=9)
conv_table<-bold(conv_table,part="header")
conv_table<-hline(conv_table,i=seq(3,24,3))
conv_table<-hline(conv_table,i=c(12,24),border=fp_border_default(width=3))
conv_table

################################
################################

##Figure 3
cols<-c("gray25","steelblue2","firebrick")
cols2<-rep(rep(cols,each=2),4)

effs<-unique(test_data2A$net_effs)

par(mfrow=c(3,1),mar=c(5,6,2,6))
boxplot(Beta3~net_vars*net_effs*m_inf,data=test_data2A[test_data2A$pcg==0.25,],las=1,xaxt="n",ylab="",xlab="",cex.axis=1.5,range=0,lty=1,col=cols2)
polygon(x=c(-10,6.5,6.5,-10),y=c(-10,-10,10,10),col="gray85",border=NA)
polygon(x=c(6.5,12.5,12.5,6.5),y=c(-10,-10,10,10),col="gray75",border=NA)
polygon(x=c(12.5,18.5,18.5,12.5),y=c(-10,-10,10,10),col="gray85",border=NA)
polygon(x=c(18.5,50,50,18.5),y=c(-10,-10,10,10),col="gray75",border=NA)
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=3,col="navy")
lines(x=c(-100,100),y=c(0.8,0.8),lty=3,col="firebrick")
mtext(expression("p"[d_group]*" = 0.25"),side=4,cex=1.5,line=3)
mtext("Estimate for social effect",side=2,line=3.5,cex=1.25)
mtext("M1",side=3,line=-2,cex=1.25,at=3.5)
mtext("M2",side=3,line=-2,cex=1.25,at=9.5)
mtext("M3",side=3,line=-2,cex=1.25,at=15.5)
mtext("M4",side=3,line=-2,cex=1.25,at=21.5)
mtext("a)",side=3,line=-2,cex=1.25,adj=0.02)
for(i in 1:24){
  if(i%%2>0){
    mtext("S",at=i,side=1,line=0.75)
  }
  if(i%%2==0){
    mtext("B",at=i,side=1,line=0.75)
  }
}
co<-1
for(i in seq(1.5,23.5,2)){
  mtext(effs[co],at=i,side=1,line=2.5,cex=1.25,col=cols[co])
  co<-co+1
  if(co>3){co<-1}
}
boxplot(add=TRUE,Beta3~net_vars*net_effs*m_inf,data=test_data2A[test_data2A$pcg==0.25,],las=1,xaxt="n",ylab="",xlab="",cex.axis=1.5,range=0,lty=1,col=cols2)

boxplot(Beta3~net_vars*net_effs*m_inf,data=test_data2A[test_data2A$pcg==0.5,],las=1,xaxt="n",ylab="",xlab="",cex.axis=1.5,range=0,lty=1,col=cols2)
polygon(x=c(-10,6.5,6.5,-10),y=c(-10,-10,10,10),col="gray85",border=NA)
polygon(x=c(6.5,12.5,12.5,6.5),y=c(-10,-10,10,10),col="gray75",border=NA)
polygon(x=c(12.5,18.5,18.5,12.5),y=c(-10,-10,10,10),col="gray85",border=NA)
polygon(x=c(18.5,50,50,18.5),y=c(-10,-10,10,10),col="gray75",border=NA)
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=3,col="navy")
lines(x=c(-100,100),y=c(0.8,0.8),lty=3,col="firebrick")
mtext(expression("p"[d_group]*" = 0.5"),side=4,cex=1.5,line=3)
mtext("Estimate for social effect",side=2,line=3.5,cex=1.25)
mtext("M1",side=3,line=-2,cex=1.25,at=3.5)
mtext("M2",side=3,line=-2,cex=1.25,at=9.5)
mtext("M3",side=3,line=-2,cex=1.25,at=15.5)
mtext("M4",side=3,line=-2,cex=1.25,at=21.5)
mtext("b)",side=3,line=-2,cex=1.25,adj=0.02)
for(i in 1:24){
  if(i%%2>0){
    mtext("S",at=i,side=1,line=0.75)
  }
  if(i%%2==0){
    mtext("B",at=i,side=1,line=0.75)
  }
}
co<-1
for(i in seq(1.5,23.5,2)){
  mtext(effs[co],at=i,side=1,line=2.5,cex=1.25,col=cols[co])
  co<-co+1
  if(co>3){co<-1}
}
boxplot(add=TRUE,Beta3~net_vars*net_effs*m_inf,data=test_data2A[test_data2A$pcg==0.5,],las=1,xaxt="n",ylab="",xlab="",cex.axis=1.5,range=0,lty=1,col=cols2)

boxplot(Beta3~net_vars*net_effs*m_inf,data=test_data2A[test_data2A$pcg==0.75,],las=1,xaxt="n",ylab="",xlab="",cex.axis=1.5,range=0,lty=1,col=cols2)
polygon(x=c(-10,6.5,6.5,-10),y=c(-10,-10,10,10),col="gray85",border=NA)
polygon(x=c(6.5,12.5,12.5,6.5),y=c(-10,-10,10,10),col="gray75",border=NA)
polygon(x=c(12.5,18.5,18.5,12.5),y=c(-10,-10,10,10),col="gray85",border=NA)
polygon(x=c(18.5,50,50,18.5),y=c(-10,-10,10,10),col="gray75",border=NA)
lines(x=c(-100,100),y=c(0,0))
lines(x=c(-100,100),y=c(0.4,0.4),lty=3,col="navy")
lines(x=c(-100,100),y=c(0.8,0.8),lty=3,col="firebrick")
mtext(expression("p"[d_group]*" = 0.75"),side=4,cex=1.5,line=3)
mtext("Estimate for social effect",side=2,line=3.5,cex=1.25)
mtext("M1",side=3,line=-2,cex=1.25,at=3.5)
mtext("M2",side=3,line=-2,cex=1.25,at=9.5)
mtext("M3",side=3,line=-2,cex=1.25,at=15.5)
mtext("M4",side=3,line=-2,cex=1.25,at=21.5)
mtext("c)",side=3,line=-2,cex=1.25,adj=0.02)
for(i in 1:24){
  if(i%%2>0){
    mtext("S",at=i,side=1,line=0.75)
  }
  if(i%%2==0){
    mtext("B",at=i,side=1,line=0.75)
  }
}
co<-1
for(i in seq(1.5,23.5,2)){
  mtext(effs[co],at=i,side=1,line=2.5,cex=1.25,col=cols[co])
  co<-co+1
  if(co>3){co<-1}
}
boxplot(add=TRUE,Beta3~net_vars*net_effs*m_inf,data=test_data2A[test_data2A$pcg==0.75,],las=1,xaxt="n",ylab="",xlab="",cex.axis=1.5,range=0,lty=1,col=cols2)

################################
################################

##Figure 4

cols<-c("gray25","steelblue2","firebrick")
cols2<-rep(rep(cols,each=3),2)

par(mfrow=c(1,2),mar=c(6,5,1,1))
boxplot(X1~pcg*net_effs*net_vars,data=emd_pars[emd_pars$conv1==1,],ylim=c(0,2),las=1,range=1.5,lty=1,ylab="",xlab="",xaxt="n",pch=20,col=cols2,at=seq(1,18,1))
lines(x=c(-100,100),y=c(0,0),lty=3)
mtext("a)",side=3,adj=0.025,line=-1.5,cex=1.25)
mtext("Earth mover's distance",side=2,line=3,cex=1.5)
par(xpd=NA)
text(x=seq(1,18,1), y=rep((par("usr")[3]-0.05),18),labels=rep(c(0.25,0.5,0.75),6),srt=60,adj=c(1,0))
par(xpd=FALSE)
mtext(rep(seq(0,0.8,0.4)),side=1,line=2.5,at=seq(2,18,3),col=cols,cex=1.2)
mtext(c("Strength","Betweenness"),side=1,line=4,at=c(5,14),cex=1.4)
boxplot(X2~pcg*net_effs*net_vars,data=emd_pars[emd_pars$conv2==1,],ylim=c(0,2),las=1,range=1.5,lty=1,ylab="",xlab="",xaxt="n",pch=20,col=cols2,at=seq(1,18,1))
lines(x=c(-100,100),y=c(0,0),lty=3)
mtext("b)",side=3,adj=0.025,line=-1.5,cex=1.25)
mtext("Earth mover's distance",side=2,line=3,cex=1.5)
par(xpd=NA)
text(x=seq(1,18,1), y=rep((par("usr")[3]-0.05),18),labels=rep(c(0.25,0.5,0.75),6),srt=60,adj=c(1,0))
par(xpd=FALSE)
mtext(rep(seq(0,0.8,0.4)),side=1,line=2.5,at=seq(2,18,3),col=cols,cex=1.2)
mtext(c("Strength","Betweenness"),side=1,line=4,at=c(5,14),cex=1.4)
par(mfrow=c(1,1))

################################
################################

##Figure 5

cols<-c("gray25","steelblue2","firebrick")
cols2<-rep(rep(cols,each=3),2)
par(mfrow=c(1,2),mar=c(6,5,1,1))
boxplot(X1~pcg*net_effs*net_vars,data=emd_pars[emd_pars$conv1==1,],ylim=c(0,2),las=1,range=1.5,lty=1,ylab="",xlab="",xaxt="n",pch=20,col=cols2,at=seq(1,18,1))
lines(x=c(-100,100),y=c(0,0),lty=3)
mtext("a)",side=3,adj=0.025,line=-1.5,cex=1.25)
mtext("Earth mover's distance",side=2,line=3,cex=1.5)
par(xpd=NA)
text(x=seq(1,18,1), y=rep((par("usr")[3]-0.05),18),labels=rep(c(0.25,0.5,0.75),6),srt=60,adj=c(1,0))
par(xpd=FALSE)
mtext(rep(seq(0,0.8,0.4)),side=1,line=2.5,at=seq(2,18,3),col=cols,cex=1.2)
mtext(c("Strength","Betweenness"),side=1,line=4,at=c(5,14),cex=1.4)
boxplot(X2~pcg*net_effs*net_vars,data=emd_pars[emd_pars$conv2==1,],ylim=c(0,2),las=1,range=1.5,lty=1,ylab="",xlab="",xaxt="n",pch=20,col=cols2,at=seq(1,18,1))
lines(x=c(-100,100),y=c(0,0),lty=3)
mtext("b)",side=3,adj=0.025,line=-1.5,cex=1.25)
mtext("Earth mover's distance",side=2,line=3,cex=1.5)
par(xpd=NA)
text(x=seq(1,18,1), y=rep((par("usr")[3]-0.05),18),labels=rep(c(0.25,0.5,0.75),6),srt=60,adj=c(1,0))
par(xpd=FALSE)
mtext(rep(seq(0,0.8,0.4)),side=1,line=2.5,at=seq(2,18,3),col=cols,cex=1.2)
mtext(c("Strength","Betweenness"),side=1,line=4,at=c(5,14),cex=1.4)
par(mfrow=c(1,1))

################################
################################
################################
################################

##Table S1

head(test_data2A)
test_data2B<-test_data2A[test_data2A$net_effs>=0,]
stat_tab<-aggregate(test_data2B$StatClear,by=list(test_data2B$pcg,test_data2B$m_inf,test_data2B$net_vars,test_data2B$net_effs),mean)
stat_tab2<-stat_tab[,c(3,4,2,1,5)]
names(stat_tab2)<-c("Network measure","True effect","Model","Group capture probability","Detection rate")
stat_tab2[,1]<-as.character(stat_tab2[,1])
stat_tab2[stat_tab2[,1]=="strength",1]<-"Strength"
stat_tab2[stat_tab2[,1]=="betweenness_w",1]<-"Betweenness"
stat_tab2[,5]<-round(stat_tab2[,5],2)

S1<-regulartable(stat_tab2)
S1<-bold(S1,part="header")
S1<-font(S1,fontname="Arial")
S1<-fontsize(S1,size=9)
S1<-fontsize(S1,size=10,part="header")
S1<-padding(S1,i=seq(1,72,1),padding=c(1,1,1,1))
S1<-width(S1,j=seq(1,5,1),width=c(1,1,0.8,1.2,0.8))
S1<-height(S1,i=seq(1,72,1),height=rep(0.01,72))
S1<-align(S1,j=c(2,4),align="left")
S1<-align(S1,j=c(2,4),align="left",part="header")
S1<-hline(S1,i=seq(3,72,3))
S1<-hline(S1,i=seq(12,72,12),border=fp_border_default(width=2))
S1<-hline(S1,i=seq(24,72,24),border=fp_border_default(width=3))
S1

################################
################################

##Table S2

conv_tab<-aggregate(test_data2$conv,by=list(test_data2$pcg,test_data2$m_inf,test_data2$net_vars,test_data2$net_effs),mean)

conv_tab2<-conv_tab[,c(3,4,2,1,5)]
names(conv_tab2)<-c("Network measure","True effect","Model","Group capture probability","Convergence rate")
conv_tab2[,1]<-as.character(conv_tab2[,1])
conv_tab2[conv_tab2[,1]=="strength",1]<-"Strength"
conv_tab2[conv_tab2[,1]=="betweenness_w",1]<-"Betweenness"
conv_tab2[,5]<-round(conv_tab2[,5],2)

conv_table2<-flextable(conv_tab2)
conv_table2<-width(conv_table2,width=c(1.5,0.5,2.1,1.5))
conv_table2<-padding(conv_table2,padding=c(0.3,0.3,0.3,0.3),part="all")
conv_table2<-font(conv_table2,fontname="Arial")
conv_table2<-fontsize(conv_table2,size=9)
conv_table2<-bold(conv_table2,part="header")
conv_table2<-hline(conv_table2,i=seq(3,72,3))
conv_table2<-hline(conv_table2,i=c(12,24,36,48,60,72),border=fp_border_default(width=3))
conv_table2<-width(conv_table2,j=seq(1,5,1),width=c(1,1,0.8,1.2,1))
conv_table2<-align(conv_table2,j=c(2,4),align="left")
conv_table2<-align(conv_table2,j=c(2,4),align="left",part="header")
conv_table2

################################
################################

##Table S3

head(test_data2A)
test_data2B<-test_data2A[test_data2A$pcg==0.75,]
stat_tab<-aggregate(test_data2B$StatClear,by=list(test_data2B$p_wr_i,test_data2B$m_inf,test_data2B$net_vars,test_data2B$net_effs),mean)
stat_tab2<-stat_tab[,c(3,4,2,1,5)]
names(stat_tab2)<-c("Network measure","True effect","Model","Rewiring probability","Detection rate")
stat_tab2[,1]<-as.character(stat_tab2[,1])
stat_tab2[stat_tab2[,1]=="strength",1]<-"Strength"
stat_tab2[stat_tab2[,1]=="betweenness_w",1]<-"Betweenness"
stat_tab2[,5]<-round(stat_tab2[,5],2)

S3<-regulartable(stat_tab2)
S3<-bold(S3,part="header")
S3<-font(S3,fontname="Arial")
S3<-fontsize(S3,size=9)
S3<-fontsize(S3,size=10,part="header")
S3<-padding(S3,i=seq(1,72,1),padding=c(1,1,1,1))
S3<-width(S3,j=seq(1,5,1),width=c(1,1,0.8,1.2,0.8))
S3<-height(S3,i=seq(1,72,1),height=rep(0.01,72))
S3<-align(S3,j=c(2,4),align="left")
S3<-align(S3,j=c(2,4),align="left",part="header")
S3<-hline(S3,i=seq(3,72,3))
S3<-hline(S3,i=seq(12,72,12),border=fp_border_default(width=2))
S3<-hline(S3,i=seq(24,72,24),border=fp_border_default(width=3))
S3

################################
################################

##Table S4

test_data2B<-test_data2A[test_data2A$pcg==0.25,]
stat_tab<-aggregate(test_data2B$StatClear,by=list(test_data2B$p_wr_i,test_data2B$m_inf,test_data2B$net_vars,test_data2B$net_effs),mean)
stat_tab2<-stat_tab[,c(3,4,2,1,5)]
names(stat_tab2)<-c("Network measure","True effect","Model","Rewiring probability","Detection rate")
stat_tab2[,1]<-as.character(stat_tab2[,1])
stat_tab2[stat_tab2[,1]=="strength",1]<-"Strength"
stat_tab2[stat_tab2[,1]=="betweenness_w",1]<-"Betweenness"
stat_tab2[,5]<-round(stat_tab2[,5],2)

S4<-regulartable(stat_tab2)
S4<-bold(S4,part="header")
S4<-font(S4,fontname="Arial")
S4<-fontsize(S4,size=9)
S4<-fontsize(S4,size=10,part="header")
S4<-padding(S4,i=seq(1,72,1),padding=c(1,1,1,1))
S4<-width(S4,j=seq(1,5,1),width=c(1,1,0.8,1.2,0.8))
S4<-height(S4,i=seq(1,72,1),height=rep(0.01,72))
S4<-align(S4,j=c(2,4),align="left")
S4<-align(S4,j=c(2,4),align="left",part="header")
S4<-hline(S4,i=seq(3,72,3))
S4<-hline(S4,i=seq(12,72,12),border=fp_border_default(width=2))
S4<-hline(S4,i=seq(24,72,24),border=fp_border_default(width=3))
S4

################################
################################

##Table S5

test_data2B<-test_data2A[test_data2A$pcg==0.75,]
stat_tab<-aggregate(test_data2B$StatClear,by=list(test_data2B$net_cov,test_data2B$m_inf,test_data2B$net_vars,test_data2B$net_effs),mean)
stat_tab2<-stat_tab[,c(3,4,2,1,5)]
names(stat_tab2)<-c("Network measure","True effect","Model","Network covariance","Detection rate")
stat_tab2[,1]<-as.character(stat_tab2[,1])
stat_tab2[stat_tab2[,1]=="strength",1]<-"Strength"
stat_tab2[stat_tab2[,1]=="betweenness_w",1]<-"Betweenness"
stat_tab2[,5]<-round(stat_tab2[,5],2)

S5<-regulartable(stat_tab2)
S5<-bold(S5,part="header")
S5<-font(S5,fontname="Arial")
S5<-fontsize(S5,size=9)
S5<-fontsize(S5,size=10,part="header")
S5<-padding(S5,i=seq(1,72,1),padding=c(1,1,1,1))
S5<-width(S5,j=seq(1,5,1),width=c(1,1,0.8,1.2,0.8))
S5<-height(S5,i=seq(1,72,1),height=rep(0.01,72))
S5<-align(S5,j=c(2,4),align="left")
S5<-align(S5,j=c(2,4),align="left",part="header")
S5<-hline(S5,i=seq(3,72,3))
S5<-hline(S5,i=seq(12,72,12),border=fp_border_default(width=2))
S5<-hline(S5,i=seq(24,72,24),border=fp_border_default(width=3))
S5

################################
################################

##Table S6

test_data2B<-test_data2A[test_data2A$pcg==0.25,]
stat_tab<-aggregate(test_data2B$StatClear,by=list(test_data2B$net_cov,test_data2B$m_inf,test_data2B$net_vars,test_data2B$net_effs),mean)
stat_tab2<-stat_tab[,c(3,4,2,1,5)]
names(stat_tab2)<-c("Network measure","True effect","Model","Network covariance","Detection rate")
stat_tab2[,1]<-as.character(stat_tab2[,1])
stat_tab2[stat_tab2[,1]=="strength",1]<-"Strength"
stat_tab2[stat_tab2[,1]=="betweenness_w",1]<-"Betweenness"
stat_tab2[,5]<-round(stat_tab2[,5],2)

S6<-regulartable(stat_tab2)
S6<-bold(S6,part="header")
S6<-font(S6,fontname="Arial")
S6<-fontsize(S6,size=9)
S6<-fontsize(S6,size=10,part="header")
S6<-padding(S6,i=seq(1,72,1),padding=c(1,1,1,1))
S6<-width(S6,j=seq(1,5,1),width=c(1,1,0.8,1.2,0.8))
S6<-height(S6,i=seq(1,72,1),height=rep(0.01,72))
S6<-align(S6,j=c(2,4),align="left")
S6<-align(S6,j=c(2,4),align="left",part="header")
S6<-hline(S6,i=seq(3,72,3))
S6<-hline(S6,i=seq(12,72,12),border=fp_border_default(width=2))
S6<-hline(S6,i=seq(24,72,24),border=fp_border_default(width=3))
S6

################################
################################

##Figure S1

centers2<-seq(2,10,1)
wss<-sapply(centers2,
            function(k){kmeans(cbind(posterior_medians2[,3],posterior_sds2[,3]), k, nstart=50,iter.max = 15 )$tot.withinss})

par(mfrow=c(1,2))
plot(centers2, wss,
     type="b",pch = 19,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     cex.lab=1.5,cex.axis=1.25)
points(x=centers2[5],y=wss[5],cex=4,col="red",lwd=2)
mtext(text="a)",side=3,line=-1.5,adj=0.95,cex=1.5)

plot(abs(posterior_medians2[,3]),posterior_sds2[,3],col=adjustcolor(cols[clu$cluster],0.6),pch=15,las=1,cex.lab=1.5,cex.axis=1.25,ylab="Posterior standard deviation",xlab="Posterior Median")
lines(x=c(-100,100),y=c(1,1),lty=2)
lines(x=c(2.5,2.5),y=c(-100,100),lty=2)
mtext(text="b)",side=3,line=-1.5,adj=0.95,cex=1.5)

################################
################################

##Figure S2

cols<-c("gray25","steelblue2","firebrick")
cols2<-rep(rep(cols,each=6),4)
cols3<-cols2

adju<-0.3
for(i in 1:length(cols3)){
  cols3[i]<-adjustcolor(cols3[i],adju)
  adju<-adju+0.3
  if(adju>1){adju<-0.3}
}

effs<-unique(test_data2A$net_effs)

par(mfrow=c(3,1),mar=c(5,6,2,6),xpd=FALSE)
for(j in c(0.25,0.5,0.75)){
  boxplot(Beta3~p_wr_i*net_vars*net_effs*m_inf,data=test_data2A[test_data2A$pcg==j,],las=1,xaxt="n",ylab="",xlab="",cex.axis=1.5,range=0,lty=1,col=cols3)
  lines(x=c(18.5,18.5),y=c(-10,10),col="black",lty=2,lwd=2)
  lines(x=c(36.5,36.5),y=c(-10,10),col="black",lty=2,lwd=2)
  lines(x=c(54.5,54.5),y=c(-10,10),col="black",lty=2,lwd=2)
  lines(x=c(-100,100),y=c(0,0))
  lines(x=c(-100,100),y=c(0.4,0.4),lty=3,col="navy")
  lines(x=c(-100,100),y=c(0.8,0.8),lty=3,col="firebrick")
  mtext(bquote(p[d_group] == .(j)),side=4,cex=1.5,line=3)
  mtext("Estimate for social effect",side=2,line=3.5,cex=1.25)
  mtext("M1",side=3,line=-2,cex=1.25,at=9.5)
  mtext("M2",side=3,line=-2,cex=1.25,at=27.5)
  mtext("M3",side=3,line=-2,cex=1.25,at=45.5)
  mtext("M4",side=3,line=-2,cex=1.25,at=63.5)
  if(j==0.25){mtext("a)",side=3,line=-2,cex=1.25,adj=0.02)}
  if(j==0.5){mtext("b)",side=3,line=-2,cex=1.25,adj=0.02)}
  if(j==0.75){mtext("c)",side=3,line=-2,cex=1.25,adj=0.02)}
  for(i in 1:72){
    if(i%in%seq(2,73,6)==TRUE){
      mtext("S",at=i,side=1,line=0.75)
    }
    if(i%in%seq(5,73,6)==TRUE){
      mtext("B",at=i,side=1,line=0.75)
    }
  }
  co<-1
  for(i in seq(3.5,73,6)){
    mtext(effs[co],at=i,side=1,line=2.5,cex=1.25,col=cols[co])
    co<-co+1
    if(co>3){co<-1}
  }
  if(j==0.25){
    mtext(text="Rewiring probability:",side=3,adj=0.01,line=0.5)
    mtext(text="0.0",side=3,adj=0.3,line=0.5)
    mtext(text="0.1",side=3,adj=0.4,line=0.5)
    mtext(text="0.5",side=3,adj=0.5,line=0.5)
    par(xpd=NA)
    points(x=19,y=2.13,pch=15,cex=3,col=cols3[1])
    points(x=26.4,y=2.13,pch=15,cex=3,col=cols3[2])
    points(x=33.8,y=2.13,pch=15,cex=3,col=cols3[3])
    par(xpd=FALSE)
  }
}

################################
################################

##Figure S3


cols<-c("gray25","steelblue2","firebrick")
cols2<-rep(rep(cols,each=6),4)
cols3<-cols2

adju<-0.3
for(i in 1:length(cols3)){
  cols3[i]<-adjustcolor(cols3[i],adju)
  adju<-adju+0.3
  if(adju>1){adju<-0.3}
}

effs<-unique(test_data2A$net_effs)

par(mfrow=c(3,1),mar=c(5,6,2,6),xpd=FALSE)
for(j in c(0.25,0.5,0.75)){
  boxplot(Beta3~net_cov*net_vars*net_effs*m_inf,data=test_data2A[test_data2A$pcg==j,],las=1,xaxt="n",ylab="",xlab="",cex.axis=1.5,range=0,lty=1,col=cols3)
  lines(x=c(18.5,18.5),y=c(-10,10),col="black",lty=2,lwd=2)
  lines(x=c(36.5,36.5),y=c(-10,10),col="black",lty=2,lwd=2)
  lines(x=c(54.5,54.5),y=c(-10,10),col="black",lty=2,lwd=2)
  lines(x=c(-100,100),y=c(0,0))
  lines(x=c(-100,100),y=c(0.4,0.4),lty=3,col="navy")
  lines(x=c(-100,100),y=c(0.8,0.8),lty=3,col="firebrick")
  mtext(bquote(p[d_group] == .(j)),side=4,cex=1.5,line=3)
  mtext("Estimate for social effect",side=2,line=3.5,cex=1.25)
  mtext("M1",side=3,line=-2,cex=1.25,at=9.5)
  mtext("M2",side=3,line=-2,cex=1.25,at=27.5)
  mtext("M3",side=3,line=-2,cex=1.25,at=45.5)
  mtext("M4",side=3,line=-2,cex=1.25,at=63.5)
  if(j==0.25){mtext("a)",side=3,line=-2,cex=1.25,adj=0.02)}
  if(j==0.5){mtext("b)",side=3,line=-2,cex=1.25,adj=0.02)}
  if(j==0.75){mtext("c)",side=3,line=-2,cex=1.25,adj=0.02)}
  for(i in 1:72){
    if(i%in%seq(2,73,6)==TRUE){
      mtext("S",at=i,side=1,line=0.75)
    }
    if(i%in%seq(5,73,6)==TRUE){
      mtext("B",at=i,side=1,line=0.75)
    }
  }
  co<-1
  for(i in seq(3.5,73,6)){
    mtext(effs[co],at=i,side=1,line=2.5,cex=1.25,col=cols[co])
    co<-co+1
    if(co>3){co<-1}
  }
  if(j==0.25){
    mtext(text="Network covariance:",side=3,adj=0.01,line=0.5)
    mtext(text="-",side=3,adj=0.3,line=0.5)
    mtext(text="0",side=3,adj=0.4,line=0.5)
    mtext(text="+",side=3,adj=0.5,line=0.5)
    par(xpd=NA)
    points(x=19,y=2.13,pch=15,cex=3,col=cols3[1])
    points(x=26.4,y=2.13,pch=15,cex=3,col=cols3[2])
    points(x=33.8,y=2.13,pch=15,cex=3,col=cols3[3])
    par(xpd=FALSE)
  }
}

################################
################################

##Figure S4

cols<-c("gray25","steelblue2","firebrick")
cols2<-rep(rep(cols,each=6),4)
cols3<-cols2

adju<-0.3
for(i in 1:length(cols3)){
  cols3[i]<-adjustcolor(cols3[i],adju)
  adju<-adju+0.3
  if(adju>1){adju<-0.3}
}

effs<-unique(test_data2A$net_effs)

par(mfrow=c(3,1),mar=c(5,6,2,6),xpd=FALSE)
for(j in c(0.25,0.5,0.75)){
  boxplot(boot::inv.logit(Beta1)~net_cov*net_vars*net_effs*m_inf,data=test_data2A[test_data2A$pcg==j,],las=1,xaxt="n",ylab="",xlab="",cex.axis=1.5,range=0,lty=1,col=cols3,ylim=c(0.6,1))
  lines(x=c(18.5,18.5),y=c(-10,10),col="black",lty=2,lwd=2)
  lines(x=c(36.5,36.5),y=c(-10,10),col="black",lty=2,lwd=2)
  lines(x=c(54.5,54.5),y=c(-10,10),col="black",lty=2,lwd=2)
  lines(x=c(-100,100),y=c(0,0))
  lines(x=c(-100,100),y=c(0.8,0.8),lty=3,col="black")
  mtext(bquote(p[d_group] == .(j)),side=4,cex=1.5,line=3)
  mtext("Estimate for social effect",side=2,line=3.5,cex=1.25)
  mtext("M1",side=3,line=-2,cex=1.25,at=9.5)
  mtext("M2",side=3,line=-2,cex=1.25,at=27.5)
  mtext("M3",side=3,line=-2,cex=1.25,at=45.5)
  mtext("M4",side=3,line=-2,cex=1.25,at=63.5)
  if(j==0.25){mtext("a)",side=3,line=-2,cex=1.25,adj=0.02)}
  if(j==0.5){mtext("b)",side=3,line=-2,cex=1.25,adj=0.02)}
  if(j==0.75){mtext("c)",side=3,line=-2,cex=1.25,adj=0.02)}
  for(i in 1:72){
    if(i%in%seq(2,73,6)==TRUE){
      mtext("S",at=i,side=1,line=0.75)
    }
    if(i%in%seq(5,73,6)==TRUE){
      mtext("B",at=i,side=1,line=0.75)
    }
  }
  co<-1
  for(i in seq(3.5,73,6)){
    mtext(effs[co],at=i,side=1,line=2.5,cex=1.25,col=cols[co])
    co<-co+1
    if(co>3){co<-1}
  }
  if(j==0.25){
    mtext(text="Network covariance:",side=3,adj=0.01,line=0.5)
    mtext(text="-",side=3,adj=0.3,line=0.5)
    mtext(text="0",side=3,adj=0.4,line=0.5)
    mtext(text="+",side=3,adj=0.5,line=0.5)
    par(xpd=NA)
    points(x=19,y=1.05,pch=15,cex=3,col=cols3[1])
    points(x=26.4,y=1.05,pch=15,cex=3,col=cols3[2])
    points(x=33.8,y=1.05,pch=15,cex=3,col=cols3[3])
    par(xpd=FALSE)
  }
}

################################
################################

##Figure S5

cols<-c("gray25","steelblue2","firebrick")
cols2<-rep(rep(cols,each=6),4)
cols3<-cols2

adju<-0.3
for(i in 1:length(cols3)){
  cols3[i]<-adjustcolor(cols3[i],adju)
  adju<-adju+0.3
  if(adju>1){adju<-0.3}
}

effs<-unique(test_data2A$net_effs)

par(mfrow=c(3,1),mar=c(5,6,2,6),xpd=FALSE)
for(j in c(0.25,0.5,0.75)){
  boxplot((Beta2-Beta1)~net_cov*net_vars*net_effs*m_inf,data=test_data2A[test_data2A$pcg==j,],las=1,xaxt="n",ylab="",xlab="",cex.axis=1.5,range=0,lty=1,col=cols3,ylim=c(-0.3,1.3))
  lines(x=c(18.5,18.5),y=c(-10,10),col="black",lty=2,lwd=2)
  lines(x=c(36.5,36.5),y=c(-10,10),col="black",lty=2,lwd=2)
  lines(x=c(54.5,54.5),y=c(-10,10),col="black",lty=2,lwd=2)
  lines(x=c(-100,100),y=c(0,0))
  lines(x=c(-100,100),y=c(0.5,0.5),lty=3,col="black")
  mtext(bquote(p[d_group] == .(j)),side=4,cex=1.5,line=3)
  mtext("Estimate for social effect",side=2,line=3.5,cex=1.25)
  mtext("M1",side=3,line=-2,cex=1.25,at=9.5)
  mtext("M2",side=3,line=-2,cex=1.25,at=27.5)
  mtext("M3",side=3,line=-2,cex=1.25,at=45.5)
  mtext("M4",side=3,line=-2,cex=1.25,at=63.5)
  if(j==0.25){mtext("a)",side=3,line=-2,cex=1.25,adj=0.02)}
  if(j==0.5){mtext("b)",side=3,line=-2,cex=1.25,adj=0.02)}
  if(j==0.75){mtext("c)",side=3,line=-2,cex=1.25,adj=0.02)}
  for(i in 1:72){
    if(i%in%seq(2,73,6)==TRUE){
      mtext("S",at=i,side=1,line=0.75)
    }
    if(i%in%seq(5,73,6)==TRUE){
      mtext("B",at=i,side=1,line=0.75)
    }
  }
  co<-1
  for(i in seq(3.5,73,6)){
    mtext(effs[co],at=i,side=1,line=2.5,cex=1.25,col=cols[co])
    co<-co+1
    if(co>3){co<-1}
  }
  if(j==0.25){
    mtext(text="Network covariance:",side=3,adj=0.01,line=0.5)
    mtext(text="-",side=3,adj=0.3,line=0.5)
    mtext(text="0",side=3,adj=0.4,line=0.5)
    mtext(text="+",side=3,adj=0.5,line=0.5)
    par(xpd=NA)
    points(x=19,y=1.475,pch=15,cex=3,col=cols3[1])
    points(x=26.4,y=1.475,pch=15,cex=3,col=cols3[2])
    points(x=33.8,y=1.475,pch=15,cex=3,col=cols3[3])
    par(xpd=FALSE)
  }
}

################################
################################

##Figure S6

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

################################
################################
################################
################################







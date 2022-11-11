
##Load required packages
library(tidyr)
library(readr)
library(HDInterval)
library(knitr)
library(kableExtra)
library(magrittr)
library(flextable)

################################
################################

##Define function to extract posterior medians for all results

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

##Define function to extract posterior standard deviations for all results

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

################################
################################

##Read in data
files<-list.files("results/")
files<-files[complete.cases(files)]
reps<-readr::parse_number(files)

results<-list()
for(i in 1:length(files)){
  results[[reps[i]]]<-eval(parse(text=paste0("readRDS('results/",files[i],"')")))
}

##Store full results in separate vector
full<-results

##New results vector with posterior distributions only
results<-sapply(full,"[",1)

##Read in initial parameters
pars1<-readRDS("parameters2.RDS")
pars1<-pars1[sort(reps),]

################################
################################

##Calculate posterior medians and store in dataframe
posterior_medians<-lapply(results,med_ext)[sign(unlist(lapply(results,length)))==1]
posterior_medians2<-data.frame(do.call(rbind,posterior_medians))

##Calculate posterior standard deviations and store in dataframe
posterior_sds<-lapply(results,sd_ext)[sign(unlist(lapply(results,length)))==1]
posterior_sds2<-data.frame(do.call(rbind,posterior_sds))

##Column names
names(posterior_medians2)<-c("Beta1","Beta2","Beta3","p")
names(posterior_sds2)<-c("sdBeta1","sdBeta2","sdBeta3","sdp")

##Create main dataframe by combining simulation parameters with posterior descriptors
pars1B<-pars1[rep(1:nrow(pars1),each=1),]
dft<-cbind(pars1B)
test_data<-data.frame(dft[,],posterior_medians2,posterior_sds2)

################################
################################

##Create dataframe storing information about credible intervals for each model type in turn
chain_info<-matrix(NA,nr=length(results),nc=4)
for(i in 1:length(results)){
  chain_info[i,1:2]<-hdi(results[[i]][,3], cred_int = 0.89)
  chain_info[i,3]<-sum(results[[i]][,3]>0)/nrow(results[[i]])
  chain_info[i,4]<-ifelse(0<chain_info[i,1]|0>chain_info[i,2],1,0)
}

################################
################################

##Combine the two existing dataframes
chain_info2<-data.frame(chain_info,test_data)
names(chain_info2)[1:4]<-c("lowCI","highCI","greater0","CIout")

################################
################################

##Calculate input baseline survival to simulations and true value from
##simulations for later comparison
test_data2<-chain_info2
exp_mean<-(boot::inv.logit(boot::logit(0.8)+0.5)+0.8)/2

true_surv<-matrix(NA,nr=length(results),nc=10)
for(i in 1:length(full)){
  if(length(full[[i]])>0){
    true_surv[i,]<-unlist(lapply(full[[i]][[2]],colMeans))[seq(2,20,2)]
  }
}

################################
################################

##Table 3

stat_tab<-aggregate(chain_info2$CIout[chain_info2$pmi==1],by=list(chain_info2$ng[chain_info2$pmi==1],chain_info2$pcg[chain_info2$pmi==1],chain_info2$net_vars[chain_info2$pmi==1],chain_info2$net_effs[chain_info2$pmi==1]),mean)

stat_tab2<-data.frame(stat_tab[,c(3,4,1,2,5)])
names(stat_tab2)<-c("Network measure","True effect","Social structure","Sampling design","Detection rate")
stat_tab2<-stat_tab2[stat_tab2$`True effect`>0,]
stat_tab2$`Network measure`<-as.character(stat_tab2$`Network measure`)
stat_tab2$`Network measure`[stat_tab2$`Network measure`=="strength"]<-"Strength"
stat_tab2$`Network measure`[stat_tab2$`Network measure`=="betweenness_w"]<-"Betweenness"
stat_tab2$`Social structure`<-as.character(stat_tab2$`Social structure`)
stat_tab2$`Social structure`[stat_tab2$`Social structure`=="20"]<-"Communities"
stat_tab2$`Social structure`[stat_tab2$`Social structure`=="200"]<-"No communities"


T3<-regulartable(stat_tab2)
T3<-bold(T3,part="header")
T3<-font(T3,fontname="Arial")
T3<-fontsize(T3,size=9)
T3<-fontsize(T3,size=10,part="header")
T3<-padding(T3,i=seq(1,32,1),padding=c(1,1,1,1))
T3<-width(T3,j=seq(1,5,1),width=c(1,0.75,1.1,1,0.8))
T3<-height(T3,i=seq(1,32,1),height=rep(0.01,32))
T3<-align(T3,j=c(2,4),align="left")
T3<-align(T3,j=c(2,4),align="left",part="header")
T3<-hline(T3,i=seq(8,32,8))
T3<-hline(T3,i=seq(16,32,16),border=fp_border_default(width=3))
T3

################################
################################

##Figure 5

pmis<-c(0.5,1)
pcgs<-unique(test_data$pcg)
ngs<-rev(unique(test_data$ng))

ats<-c(seq(1,12,1),seq(14,25,1))

cols<-c(rep("gray25",4),rep("steelblue2",4),rep("firebrick",4))

par(mfrow=c(2,2),mar=c(6,6,2,2))
for(i in 1:length(ngs)){
  for(j in 1:length(pmis)){
    if(i==1&j==1){
      par(mar=c(2,6,5,1))
    }
    if(i==1&j==2){
      par(mar=c(2,3,5,4))
    }
    if(i==2&j==1){
      par(mar=c(6,6,1,1))
    }
    if(i==2&j==2){
      par(mar=c(6,3,1,4))
    }
    boxplot(Beta3~pcg*net_effs*net_vars,data=test_data[test_data$pmi==pmis[j]&test_data$ng==ngs[i],],at=ats,ylim=c(-0.2,0.6),las=1,ylab="",xlab="",xaxt="n",cex.lab=1.5,cex.axis=1.25,lty=1,range=0,col=cols)
    if(j==1){mtext("Estimated social effect on survival",side=2,line=4,cex=1.25)}
    polygon(x=c(13,100,100,13),y=c(-100,-100,100,100),col="gray75",border=NA)
    lines(x=c(-100,100),y=c(0,0),lwd=2)
    lines(x=c(-100,100),y=c(0.4,0.4),lty=3,col="navy",lwd=2)
    for(k in c(0.1,0.2,0.3,0.5,0.6)){
      lines(x=c(-100,100),y=c(k,k),lwd=1,col="gray45")
    }
    boxplot(Beta3~pcg*net_effs*net_vars,data=test_data[test_data$pmi==pmis[j]&test_data$ng==ngs[i],],at=ats,ylim=c(-0.2,0.6),las=1,ylab="",xlab="",xaxt="n",cex.lab=1.5,cex.axis=1.25,lty=1,range=0,col=cols,add=TRUE)
    mtext("Strength",side=3,at=6.5,line=-2.25,cex=1.4)
    mtext("Betweenness",side=3,at=19.5,line=-2.25,cex=1.4)
    if(i==2){
      mtext("0",at=2.5,side=1,line=3,font=2,col=cols[1],cex=1.25)
      mtext("0.4",at=6.5,side=1,line=3,font=2,col=cols[5],cex=1.25)
      mtext("0.8",at=10.5,side=1,line=3,font=2,col=cols[9],cex=1.25)
      mtext("0",at=15.5,side=1,line=3,font=2,col=cols[1],cex=1.25)
      mtext("0.4",at=19.5,side=1,line=3,font=2,col=cols[5],cex=1.25)
      mtext("0.8",at=23.5,side=1,line=3,font=2,col=cols[9],cex=1.25)
      co<-1
      for(k in 1:24){
        mtext(pcgs[co],at=ifelse(k<13,k,k+1),side=1,line=1,cex=0.8)
        co<-co+1
        if(co>4){co<-1}
      }
    }
    if(i==1&j==1){
      mtext("Within-group detection probability: 0.5",side=3,line=2,cex=1.5)
    }
    if(i==1&j==2){
      mtext("Within-group detection probability: 1",side=3,line=2,cex=1.5)
      mtext("No social communities",side=4,line=2,cex=1.5)
    }
    if(i==2&j==1){
      mtext("Sampling design",side=1,line=1,adj=-0.15,cex=0.8)
      mtext("Effect size",side=1,line=3,adj=-0.15,cex=1.25)
    }
    if(i==2&j==2){
      mtext("Social communities",side=4,line=2,cex=1.5)
    }
  }
}

################################
################################

##Table S7

stat_tab<-aggregate(chain_info2$CIout[chain_info2$pmi==0.75&chain_info2$net_effs>0],by=list(chain_info2$ng[chain_info2$pmi==0.75&chain_info2$net_effs>0],chain_info2$pcg[chain_info2$pmi==0.75&chain_info2$net_effs>0],chain_info2$net_vars[chain_info2$pmi==0.75&chain_info2$net_effs>0],chain_info2$net_effs[chain_info2$pmi==0.75&chain_info2$net_effs>0]),mean)

stat_tab2<-data.frame(stat_tab[,c(3,4,1,2,5)])
names(stat_tab2)<-c("Network measure","True effect","Social structure","Sampling design","Detection rate")
stat_tab2$`Network measure`<-as.character(stat_tab2$`Network measure`)
stat_tab2$`Network measure`[stat_tab2$`Network measure`=="strength"]<-"Strength"
stat_tab2$`Network measure`[stat_tab2$`Network measure`=="betweenness_w"]<-"Betweenness"
stat_tab2$`Social structure`<-as.character(stat_tab2$`Social structure`)
stat_tab2$`Social structure`[stat_tab2$`Social structure`=="20"]<-"Communities"
stat_tab2$`Social structure`[stat_tab2$`Social structure`=="200"]<-"No communities"

S7<-regulartable(stat_tab2)
S7<-bold(S7,part="header")
S7<-font(S7,fontname="Arial")
S7<-fontsize(S7,size=9)
S7<-fontsize(S7,size=10,part="header")
S7<-padding(S7,i=seq(1,32,1),padding=c(1,1,1,1))
S7<-width(S7,j=seq(1,5,1),width=c(1,0.75,1.1,0.8,1))
S7<-height(S7,i=seq(1,32,1),height=rep(0.01,32))
S7<-align(S7,j=c(2),align="left")
S7<-align(S7,j=c(2),align="left",part="header")
S7<-hline(S7,i=seq(8,32,8))
S7<-hline(S7,i=seq(16,32,16),border=fp_border_default(width=3))
S7

################################
################################

stat_tab<-aggregate(chain_info2$CIout[chain_info2$pmi==0.5&chain_info2$net_effs>0],by=list(chain_info2$ng[chain_info2$pmi==0.5&chain_info2$net_effs>0],chain_info2$pcg[chain_info2$pmi==0.5&chain_info2$net_effs>0],chain_info2$net_vars[chain_info2$pmi==0.5&chain_info2$net_effs>0],chain_info2$net_effs[chain_info2$pmi==0.5&chain_info2$net_effs>0]),mean)

stat_tab2<-data.frame(stat_tab[,c(3,4,1,2,5)])
names(stat_tab2)<-c("Network measure","True effect","Social structure","Sampling design","Detection rate")
stat_tab2$`Network measure`<-as.character(stat_tab2$`Network measure`)
stat_tab2$`Network measure`[stat_tab2$`Network measure`=="strength"]<-"Strength"
stat_tab2$`Network measure`[stat_tab2$`Network measure`=="betweenness_w"]<-"Betweenness"
stat_tab2$`Social structure`<-as.character(stat_tab2$`Social structure`)
stat_tab2$`Social structure`[stat_tab2$`Social structure`=="20"]<-"Communities"
stat_tab2$`Social structure`[stat_tab2$`Social structure`=="200"]<-"No communities"

S8<-regulartable(stat_tab2)
S8<-bold(S8,part="header")
S8<-font(S8,fontname="Arial")
S8<-fontsize(S8,size=9)
S8<-fontsize(S8,size=10,part="header")
S8<-padding(S8,i=seq(1,32,1),padding=c(1,1,1,1))
S8<-width(S8,j=seq(1,5,1),width=c(1,0.75,1.1,0.8,1))
S8<-height(S8,i=seq(1,32,1),height=rep(0.01,32))
S8<-align(S8,j=c(2),align="left")
S8<-align(S8,j=c(2),align="left",part="header")
S8<-hline(S8,i=seq(8,32,8))
S8<-hline(S8,i=seq(16,32,16),border=fp_border_default(width=3))
S8

################################
################################

stat_tab<-aggregate(chain_info2$CIout[chain_info2$net_effs==0],by=list(chain_info2$ng[chain_info2$net_effs==0],chain_info2$pcg[chain_info2$net_effs==0],chain_info2$net_vars[chain_info2$net_effs==0],chain_info2$pmi[chain_info2$net_effs==0]),mean)

stat_tab2<-data.frame(stat_tab[,c(3,1,2,4,5)])
names(stat_tab2)<-c("Network measure","Social structure","Sampling design","Within-group detection probability","Detection rate")
stat_tab2$`Network measure`<-as.character(stat_tab2$`Network measure`)
stat_tab2$`Network measure`[stat_tab2$`Network measure`=="strength"]<-"Strength"
stat_tab2$`Network measure`[stat_tab2$`Network measure`=="betweenness_w"]<-"Betweenness"
stat_tab2$`Social structure`<-as.character(stat_tab2$`Social structure`)
stat_tab2$`Social structure`[stat_tab2$`Social structure`=="20"]<-"Communities"
stat_tab2$`Social structure`[stat_tab2$`Social structure`=="200"]<-"No communities"

S9<-regulartable(stat_tab2)
S9<-bold(S9,part="header")
S9<-font(S9,fontname="Arial")
S9<-fontsize(S9,size=9)
S9<-fontsize(S9,size=10,part="header")
S9<-padding(S9,i=seq(1,48,1),padding=c(1,1,1,1))
S9<-width(S9,j=seq(1,5,1),width=c(1,1.1,0.8,1.2,0.8))
S9<-height(S9,i=seq(1,48,1),height=rep(0.01,48))
S9<-align(S9,j=c(2),align="left")
S9<-align(S9,j=c(2),align="left",part="header")
S9<-hline(S9,i=seq(8,48,8))
S9<-hline(S9,i=seq(16,48,16),border=fp_border_default(width=3))
S9

################################
################################

##Figure S7

pmis<-unique(test_data$pmi)
pcgs<-unique(test_data$pcg)
ngs<-rev(unique(test_data$ng))

ats<-c(seq(1,12,1),seq(14,25,1))

cols<-c(rep("gray25",4),rep("steelblue2",4),rep("firebrick",4))
par(mfrow=c(2,3),mar=c(6,6,3,2))
for(i in 1:length(ngs)){
  for(j in 1:length(pmis)){
    boxplot(Beta3~pcg*net_effs*net_vars,data=test_data[test_data$pmi==pmis[j]&test_data$ng==ngs[i],],at=ats,ylim=c(-0.2,0.6),las=1,ylab="Estimated social effect on survival",xlab="",xaxt="n",cex.lab=1.5,cex.axis=1.25,lty=1,range=0,col=cols)
    polygon(x=c(13,100,100,13),y=c(-100,-100,100,100),col="gray75",border=NA)
    lines(x=c(-100,100),y=c(0,0),lwd=2)
    lines(x=c(-100,100),y=c(0.4,0.4),lty=3,col="navy",lwd=2)
    for(k in c(0.1,0.2,0.3,0.5,0.6)){
      lines(x=c(-100,100),y=c(k,k),lwd=1,col="gray45")
    }
    boxplot(Beta3~pcg*net_effs*net_vars,data=test_data[test_data$pmi==pmis[j]&test_data$ng==ngs[i],],at=ats,ylim=c(-0.2,0.6),las=1,ylab="Estimated social effect on survival",xlab="",xaxt="n",cex.lab=1.5,cex.axis=1.25,lty=1,range=0,col=cols,add=TRUE)
    mtext("0",at=2.5,side=1,line=3,font=2,col=cols[1],cex=1.2)
    mtext("0.4",at=6.5,side=1,line=3,font=2,col=cols[5],cex=1.2)
    mtext("0.8",at=10.5,side=1,line=3,font=2,col=cols[9],cex=1.2)
    mtext("0",at=15.5,side=1,line=3,font=2,col=cols[1],cex=1.2)
    mtext("0.4",at=19.5,side=1,line=3,font=2,col=cols[5],cex=1.2)
    mtext("0.8",at=23.5,side=1,line=3,font=2,col=cols[9],cex=1.2)
    co<-1
    for(k in 1:24){
      mtext(pcgs[co],at=ifelse(k<13,k,k+1),side=1,line=1,cex=0.6)
      co<-co+1
      if(co>4){co<-1}
    }
    if(ngs[i]==200){
      mtext(paste0("Within-group detection probability: ",pmis[j]),side=3,line=0.5,cex=1.5)
    }
  }
}

##############################
##############################

##Figure S8

pmis<-unique(test_data$pmi)
pcgs<-unique(test_data$pcg)
ngs<-rev(unique(test_data$ng))

ats<-c(seq(1,12,1),seq(14,25,1))

cols<-c(rep("gray25",4),rep("steelblue2",4),rep("firebrick",4))
par(mfrow=c(2,3),mar=c(6,6,3,2))
for(i in 1:length(ngs)){
  for(j in 1:length(pmis)){
    boxplot(boot::inv.logit(Beta1)~pcg*net_effs*net_vars,data=test_data[test_data$pmi==pmis[j]&test_data$ng==ngs[i],],at=ats,ylim=c(0.7,0.9),las=1,ylab="Estimated social effect on survival",xlab="",xaxt="n",cex.lab=1.5,cex.axis=1.25,lty=1,range=0,col=cols)
    polygon(x=c(13,100,100,13),y=c(-100,-100,100,100),col="gray75",border=NA)
    lines(x=c(-100,100),y=c(0,0),lwd=2)
    lines(x=c(-100,100),y=c(0.8,0.8),lty=1,col="black",lwd=2)
    boxplot(boot::inv.logit(Beta1)~pcg*net_effs*net_vars,data=test_data[test_data$pmi==pmis[j]&test_data$ng==ngs[i],],at=ats,ylim=c(0.7,0.9),las=1,ylab="Estimated social effect on survival",xlab="",xaxt="n",cex.lab=1.5,cex.axis=1.25,lty=1,range=0,col=cols,add=TRUE)
    mtext("0",at=2.5,side=1,line=3,font=2,col=cols[1],cex=1.2)
    mtext("0.4",at=6.5,side=1,line=3,font=2,col=cols[5],cex=1.2)
    mtext("0.8",at=10.5,side=1,line=3,font=2,col=cols[9],cex=1.2)
    mtext("0",at=15.5,side=1,line=3,font=2,col=cols[1],cex=1.2)
    mtext("0.4",at=19.5,side=1,line=3,font=2,col=cols[5],cex=1.2)
    mtext("0.8",at=23.5,side=1,line=3,font=2,col=cols[9],cex=1.2)
    co<-1
    for(k in 1:24){
      mtext(pcgs[co],at=ifelse(k<13,k,k+1),side=1,line=1,cex=0.6)
      co<-co+1
      if(co>4){co<-1}
    }
    if(ngs[i]==200){
      mtext(paste0("Within-group detection probability: ",pmis[j]),side=3,line=0.5,cex=1.5)
    }
  }
}

##############################
##############################

##Figure S9

pmis<-unique(test_data$pmi)
pcgs<-unique(test_data$pcg)
ngs<-rev(unique(test_data$ng))

ats<-c(seq(1,12,1),seq(14,25,1))

cols<-c(rep("gray25",4),rep("steelblue2",4),rep("firebrick",4))
par(mfrow=c(2,3),mar=c(6,6,3,2))
for(i in 1:length(ngs)){
  for(j in 1:length(pmis)){
    boxplot(Beta2-Beta1~pcg*net_effs*net_vars,data=test_data[test_data$pmi==pmis[j]&test_data$ng==ngs[i],],at=ats,ylim=c(0,1),las=1,ylab="Estimated social effect on survival",xlab="",xaxt="n",cex.lab=1.5,cex.axis=1.25,lty=1,range=0,col=cols)
    polygon(x=c(13,100,100,13),y=c(-100,-100,100,100),col="gray75",border=NA)
    lines(x=c(-100,100),y=c(0,0),lwd=1)
    lines(x=c(-100,100),y=c(0.5,0.5),lty=1,col="black",lwd=2)
    boxplot(Beta2-Beta1~pcg*net_effs*net_vars,data=test_data[test_data$pmi==pmis[j]&test_data$ng==ngs[i],],at=ats,ylim=c(0,1),las=1,ylab="Estimated social effect on survival",xlab="",xaxt="n",cex.lab=1.5,cex.axis=1.25,lty=1,range=0,col=cols,add=TRUE)
    mtext("0",at=2.5,side=1,line=3,font=2,col=cols[1],cex=1.2)
    mtext("0.4",at=6.5,side=1,line=3,font=2,col=cols[5],cex=1.2)
    mtext("0.8",at=10.5,side=1,line=3,font=2,col=cols[9],cex=1.2)
    mtext("0",at=15.5,side=1,line=3,font=2,col=cols[1],cex=1.2)
    mtext("0.4",at=19.5,side=1,line=3,font=2,col=cols[5],cex=1.2)
    mtext("0.8",at=23.5,side=1,line=3,font=2,col=cols[9],cex=1.2)
    co<-1
    for(k in 1:24){
      mtext(pcgs[co],at=ifelse(k<13,k,k+1),side=1,line=1,cex=0.6)
      co<-co+1
      if(co>4){co<-1}
    }
    if(ngs[i]==200){
      mtext(paste0("Within-group detection probability: ",pmis[j]),side=3,line=0.5,cex=1.5)
    }
  }
}

##############################
##############################

##Figure S10

colps<-c("honeydew2","honeydew3","honeydew4")
colps2<-rep(NA,nrow(test_data2))
for(i in 1:length(colps2)){
  ifelse(test_data2$net_effs[i]==0,colps2[i]<-colps[1],ifelse(test_data2$net_effs[i]==0.4,colps2[i]<-colps[2],colps2[i]<-colps[3]))
}
par(mfrow=c(1,1))
plot((boot::inv.logit(test_data2$Beta1)+boot::inv.logit(test_data2$Beta2))/2~rowMeans(true_surv[complete.cases(true_surv[,1]),]),ylim=c(0.7,0.9),xlim=c(0.7,0.9),pch=16,col=adjustcolor(colps2,0.2),las=1,ylab="Model estimate",xlab="Mean survival",cex.lab=1.4)
lines(x=c(exp_mean,exp_mean),y=c(-100,100),lwd=2,col="black",lty=2)
lines(y=c(exp_mean,exp_mean),x=c(-100,100),lwd=2,col="black",lty=2)
mts<-mean(rowMeans(true_surv[complete.cases(true_surv[,1]),]))
mms<-mean((boot::inv.logit(test_data2$Beta1)+boot::inv.logit(test_data2$Beta2))/2)
points(x=mts,y=mts,cex=3,col="firebrick",pch=8,lwd=3)
points(x=mms,y=mms,cex=2,col="cornflowerblue",pch=8,lwd=2)
text(x=rep(0.85,3),y=c(0.74),adj=c(0,0.5),labels=c("Network Effect"),cex=1.5)
points(x=rep(0.86,3),y=c(0.73,0.72,0.71),col=colps,pch=16,cex=3)
text(x=rep(0.87,3),y=c(0.73,0.72,0.71),adj=c(0,0.5),labels=c(0,0.4,0.8),cex=1.5)

##############################
##############################
##############################
##############################

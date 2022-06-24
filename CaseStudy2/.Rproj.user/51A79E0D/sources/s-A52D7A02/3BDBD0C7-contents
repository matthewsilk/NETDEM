
library(knitr)
library(kableExtra)
library(magrittr)
library(flextable)

####Final plots - need to run prelim_datavis code first for now at least

##full version - probably for sup mat

pmis<-unique(test_data$pmi)
pcgs<-unique(test_data$pcg)
ngs<-rev(unique(test_data$ng))

ats<-c(seq(1,12,1),seq(14,25,1))

cols<-c(rep("gray25",4),rep("steelblue2",4),rep("firebrick",4))

par(mfrow=c(2,3),mar=c(6,6,2,2))
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
  }
}

##############################
##############################

##reduced version - main text

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

##############################
##############################

mean(chain_info2$greater0[chain_info2$net_vars=="strength"&chain_info2$net_effs==0&chain_info2$pmi==0.5&chain_info2$ng==20],na.rm=T)
mean(chain_info2$greater0[chain_info2$net_vars=="strength"&chain_info2$net_effs==0&chain_info2$pmi==0.5&chain_info2$ng==200],na.rm=T)
mean(chain_info2$greater0[chain_info2$net_vars=="betweenness_w"&chain_info2$net_effs==0&chain_info2$pmi==0.5&chain_info2$ng==20],na.rm=T)
mean(chain_info2$greater0[chain_info2$net_vars=="betweenness_w"&chain_info2$net_effs==0&chain_info2$pmi==0.5&chain_info2$ng==200],na.rm=T)

mean(chain_info2$greater0[chain_info2$net_vars=="strength"&chain_info2$net_effs==0&chain_info2$pmi==0.75&chain_info2$ng==20],na.rm=T)
mean(chain_info2$greater0[chain_info2$net_vars=="strength"&chain_info2$net_effs==0&chain_info2$pmi==0.75&chain_info2$ng==200],na.rm=T)
mean(chain_info2$greater0[chain_info2$net_vars=="betweenness_w"&chain_info2$net_effs==0&chain_info2$pmi==0.75&chain_info2$ng==20],na.rm=T)
mean(chain_info2$greater0[chain_info2$net_vars=="betweenness_w"&chain_info2$net_effs==0&chain_info2$pmi==0.75&chain_info2$ng==200],na.rm=T)

mean(chain_info2$greater0[chain_info2$net_vars=="strength"&chain_info2$net_effs==0&chain_info2$pmi==1&chain_info2$ng==20],na.rm=T)
mean(chain_info2$greater0[chain_info2$net_vars=="strength"&chain_info2$net_effs==0&chain_info2$pmi==1&chain_info2$ng==200],na.rm=T)
mean(chain_info2$greater0[chain_info2$net_vars=="betweenness_w"&chain_info2$net_effs==0&chain_info2$pmi==1&chain_info2$ng==20],na.rm=T)
mean(chain_info2$greater0[chain_info2$net_vars=="betweenness_w"&chain_info2$net_effs==0&chain_info2$pmi==1&chain_info2$ng==200],na.rm=T)


##social structure effect - main text

cols<-c(rep("white",2),rep("gray75",2))
par(mfrow=c(2,1),mar=c(5,5,2,2))
boxplot(greater0~ng*net_vars,data=chain_info2[chain_info2$net_effs==0&chain_info2$pmi==0.5,],range=0,lty=1,las=1,col=cols,lwd=2,xaxt="n",xlab="",ylab="")
lines(x=c(-100,100),y=c(0.5,0.5))
mtext("Posterior bigger than zero",side=2,line=3,cex=1.25)
mtext("Social communities",at=1,side=1,line=0.75)
mtext("No social communities",at=2,side=1,line=1.5)
mtext("Social communities",at=3,side=1,line=0.75)
mtext("No social communities",at=4,side=1,line=1.5)
mtext("Strength",at=1.5,side=1,line=3.5,cex=1.5)
mtext("Betweenness",at=3.5,side=1,line=3.5,cex=1.5)
mtext("a)",side=3,adj=0.02,line=-1.5,cex=1.25)
boxplot(greater0~ng*net_vars,data=chain_info2[chain_info2$net_effs==0&chain_info2$pmi==0.75,],range=0,lty=1,las=1,col=cols,lwd=2,xaxt="n",xlab="",ylab="")
lines(x=c(-100,100),y=c(0.5,0.5))
mtext("Posterior bigger than zero",side=2,line=3,cex=1.25)
mtext("Social communities",at=1,side=1,line=0.75)
mtext("No social communities",at=2,side=1,line=1.5)
mtext("Social communities",at=3,side=1,line=0.75)
mtext("No social communities",at=4,side=1,line=1.5)
mtext("Strength",at=1.5,side=1,line=3.5,cex=1.5)
mtext("Betweenness",at=3.5,side=1,line=3.5,cex=1.5)
mtext("b)",side=3,adj=0.02,line=-1.5,cex=1.25)

################
################

cols<-c(rep("white",2),rep("gray75",2))
par(mfrow=c(1,1),mar=c(5,5,2,2))
boxplot(greater0~ng*net_vars,data=chain_info2[chain_info2$net_effs==0,],range=0,lty=1,las=1,col=cols,lwd=2,xaxt="n",xlab="",ylab="",notch=T)
lines(x=c(-100,100),y=c(0.5,0.5))
mtext("Posterior bigger than zero",side=2,line=3,cex=1.25)
mtext("Social communities",at=1,side=1,line=0.75)
mtext("No social communities",at=2,side=1,line=1.5)
mtext("Social communities",at=3,side=1,line=0.75)
mtext("No social communities",at=4,side=1,line=1.5)
mtext("Strength",at=1.5,side=1,line=3.5,cex=1.5)
mtext("Betweenness",at=3.5,side=1,line=3.5,cex=1.5)
mtext("a)",side=3,adj=0.02,line=-1.5,cex=1.25)

####################

cols<-c(rep("white",2))
par(mfrow=c(1,1),mar=c(5,5,2,2))
boxplot(greater0~ng,data=chain_info2[chain_info2$net_effs==0,],range=0,lty=1,las=1,col=cols,lwd=2,xaxt="n",xlab="",ylab="",notch=T,cex.axis=1.25)
lines(x=c(-100,100),y=c(0.5,0.5))
mtext("Proportion of posterior bigger than zero",side=2,line=3.5,cex=1.5)
mtext("Social communities",at=1,side=1,line=1,cex=1.5)
mtext("No social communities",at=2,side=1,line=1,cex=1.5)

####################

cols<-c(rep("white",2))
par(mfrow=c(1,1),mar=c(5,5,2,2))
boxplot(greater0~ng,data=chain_info2[chain_info2$net_effs==0&chain_info2$pmi==0.5,],range=0,lty=1,las=1,col=cols,lwd=2,xaxt="n",xlab="",ylab="",notch=T,cex.axis=1.25)
lines(x=c(-100,100),y=c(0.5,0.5))
mtext("Proportion of posterior bigger than zero",side=2,line=3.5,cex=1.5)
mtext("Social communities",at=1,side=1,line=1,cex=1.5)
mtext("No social communities",at=2,side=1,line=1,cex=1.5)

####################

par(mfrow=c(2,1),mar=c(5,5,2,2))
boxplot(greater0~ng*net_vars,data=chain_info2[chain_info2$net_effs==0.4&chain_info2$pmi==0.5,],range=0,lty=1,las=1)
boxplot(greater0~ng*net_vars,data=chain_info2[chain_info2$net_effs==0.4&chain_info2$pmi==1,],range=0,lty=1,las=1)

#############################
#############################

head(chain_info2)

stat_tab<-aggregate(chain_info2$CIout[chain_info2$pmi==1],by=list(chain_info2$ng[chain_info2$pmi==1],chain_info2$pcg[chain_info2$pmi==1],chain_info2$net_vars[chain_info2$pmi==1],chain_info2$net_effs[chain_info2$pmi==1]),mean)

stat_tab2<-data.frame(stat_tab[,c(3,4,1,2,5)])
names(stat_tab2)<-c("Network measure","True effect","Social structure","Sampling design","Detection rate")
stat_tab2$`Network measure`<-as.character(stat_tab2$`Network measure`)
stat_tab2$`Network measure`[stat_tab2$`Network measure`=="strength"]<-"Strength"
stat_tab2$`Network measure`[stat_tab2$`Network measure`=="betweenness_w"]<-"Betweenness"
stat_tab2$`Social structure`<-as.character(stat_tab2$`Social structure`)
stat_tab2$`Social structure`[stat_tab2$`Social structure`=="20"]<-"Communities"
stat_tab2$`Social structure`[stat_tab2$`Social structure`=="200"]<-"No communities"
#test<-kable(stat_tab2)
#print.kableExtra(test)
#kableExtra::save_kable(test,"test.txt")

test2<-regulartable(stat_tab2)
test2<-bold(test2,part="header")
test2<-font(test2,fontname="Arial")
test2<-fontsize(test2,size=9)
test2<-fontsize(test2,size=10,part="header")
test2<-padding(test2,i=seq(1,48,1),padding=c(1,1,1,1))
test2<-width(test2,j=seq(1,5,1),width=c(1,0.75,1.1,1,0.8))
test2<-height(test2,i=seq(1,48,1),height=rep(0.01,48))
test2<-align(test2,j=c(2,4),align="left")
test2<-align(test2,j=c(2,4),align="left",part="header")
test2<-hline(test2,i=seq(8,48,8))
test2<-hline(test2,i=seq(16,48,16),border=fp_border_default(width=3))
test2

path<-getwd()
save_as_docx(test2,path=paste0(path,"/Table3.docx"))

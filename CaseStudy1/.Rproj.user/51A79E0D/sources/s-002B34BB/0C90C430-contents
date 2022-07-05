
library(knitr)
library(kableExtra)
library(magrittr)
library(flextable)

###################

##Convergence table

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

path<-getwd()
save_as_docx(conv_table,path=paste0(path,"/Table2.docx"))

#####################

##results table

head(test_data2A)
test_data2B<-test_data2A[test_data2A$net_effs>0,]
stat_tab<-aggregate(test_data2B$StatClear,by=list(test_data2B$pcg,test_data2B$m_inf,test_data2B$net_vars,test_data2B$net_effs),mean)
stat_tab2<-stat_tab[,c(3,4,2,1,5)]
names(stat_tab2)<-c("Network measure","True effect","Model","Group capture probability","Detection rate")
stat_tab2[,1]<-as.character(stat_tab2[,1])
stat_tab2[stat_tab2[,1]=="strength",1]<-"Strength"
stat_tab2[stat_tab2[,1]=="betweenness_w",1]<-"Betweenness"
stat_tab2[,5]<-round(stat_tab2[,5],2)

test2<-regulartable(stat_tab2)
test2<-bold(test2,part="header")
test2<-font(test2,fontname="Arial")
test2<-fontsize(test2,size=9)
test2<-fontsize(test2,size=10,part="header")
test2<-padding(test2,i=seq(1,48,1),padding=c(1,1,1,1))
test2<-width(test2,j=seq(1,5,1),width=c(1,1,0.8,1.2,0.8))
test2<-height(test2,i=seq(1,48,1),height=rep(0.01,48))
test2<-align(test2,j=c(2,4),align="left")
test2<-align(test2,j=c(2,4),align="left",part="header")
test2<-hline(test2,i=seq(3,48,3))
test2<-hline(test2,i=seq(12,48,12),border=fp_border_default(width=2))
test2<-hline(test2,i=seq(24,48,24),border=fp_border_default(width=3))
test2

path<-getwd()
save_as_docx(test2,path=paste0(path,"/Table1.docx"))


##################################
#################################

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

#############################
#############################

##EMDs plot


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

sum(emd_pars[emd_pars$conv1==1,]$X1>2)

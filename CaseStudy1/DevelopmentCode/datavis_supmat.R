

library(knitr)
library(kableExtra)
library(magrittr)
library(flextable)


##Convergence table

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

path<-getwd()
#save_as_docx(conv_table2,path=paste0(path,"/TableS2.docx"))

#####################

##results table

head(test_data2A)
test_data2B<-test_data2A[test_data2A$net_effs>=0,]
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
test2<-padding(test2,i=seq(1,72,1),padding=c(1,1,1,1))
test2<-width(test2,j=seq(1,5,1),width=c(1,1,0.8,1.2,0.8))
test2<-height(test2,i=seq(1,72,1),height=rep(0.01,48))
test2<-align(test2,j=c(2,4),align="left")
test2<-align(test2,j=c(2,4),align="left",part="header")
test2<-hline(test2,i=seq(3,72,3))
test2<-hline(test2,i=seq(12,72,12),border=fp_border_default(width=2))
test2<-hline(test2,i=seq(24,72,24),border=fp_border_default(width=3))
test2

path<-getwd()
#save_as_docx(test2,path=paste0(path,"/TableS1.docx"))

#####################

head(test_data2A)
test_data2B<-test_data2A[test_data2A$pcg==0.75,]
stat_tab<-aggregate(test_data2B$StatClear,by=list(test_data2B$p_wr_i,test_data2B$m_inf,test_data2B$net_vars,test_data2B$net_effs),mean)
stat_tab2<-stat_tab[,c(3,4,2,1,5)]
names(stat_tab2)<-c("Network measure","True effect","Model","Rewiring probability","Detection rate")
stat_tab2[,1]<-as.character(stat_tab2[,1])
stat_tab2[stat_tab2[,1]=="strength",1]<-"Strength"
stat_tab2[stat_tab2[,1]=="betweenness_w",1]<-"Betweenness"
stat_tab2[,5]<-round(stat_tab2[,5],2)

test3<-regulartable(stat_tab2)
test3<-bold(test3,part="header")
test3<-font(test3,fontname="Arial")
test3<-fontsize(test3,size=9)
test3<-fontsize(test3,size=10,part="header")
test3<-padding(test3,i=seq(1,72,1),padding=c(1,1,1,1))
test3<-width(test3,j=seq(1,5,1),width=c(1,1,0.8,1.2,0.8))
test3<-height(test3,i=seq(1,72,1),height=rep(0.01,72))
test3<-align(test3,j=c(2,4),align="left")
test3<-align(test3,j=c(2,4),align="left",part="header")
test3<-hline(test3,i=seq(3,72,3))
test3<-hline(test3,i=seq(12,72,12),border=fp_border_default(width=2))
test3<-hline(test3,i=seq(24,72,24),border=fp_border_default(width=3))
test3

path<-getwd()
#save_as_docx(test3,path=paste0(path,"/TableS3.docx"))

#####################

test_data2B<-test_data2A[test_data2A$pcg==0.25,]
stat_tab<-aggregate(test_data2B$StatClear,by=list(test_data2B$p_wr_i,test_data2B$m_inf,test_data2B$net_vars,test_data2B$net_effs),mean)
stat_tab2<-stat_tab[,c(3,4,2,1,5)]
names(stat_tab2)<-c("Network measure","True effect","Model","Rewiring probability","Detection rate")
stat_tab2[,1]<-as.character(stat_tab2[,1])
stat_tab2[stat_tab2[,1]=="strength",1]<-"Strength"
stat_tab2[stat_tab2[,1]=="betweenness_w",1]<-"Betweenness"
stat_tab2[,5]<-round(stat_tab2[,5],2)

test4<-regulartable(stat_tab2)
test4<-bold(test4,part="header")
test4<-font(test4,fontname="Arial")
test4<-fontsize(test4,size=9)
test4<-fontsize(test4,size=10,part="header")
test4<-padding(test4,i=seq(1,72,1),padding=c(1,1,1,1))
test4<-width(test4,j=seq(1,5,1),width=c(1,1,0.8,1.2,0.8))
test4<-height(test4,i=seq(1,72,1),height=rep(0.01,72))
test4<-align(test4,j=c(2,4),align="left")
test4<-align(test4,j=c(2,4),align="left",part="header")
test4<-hline(test4,i=seq(3,72,3))
test4<-hline(test4,i=seq(12,72,12),border=fp_border_default(width=2))
test4<-hline(test4,i=seq(24,72,24),border=fp_border_default(width=3))
test4

path<-getwd()
#save_as_docx(test4,path=paste0(path,"/TableS4.docx"))

#####################

test_data2B<-test_data2A[test_data2A$pcg==0.75,]
stat_tab<-aggregate(test_data2B$StatClear,by=list(test_data2B$net_cov,test_data2B$m_inf,test_data2B$net_vars,test_data2B$net_effs),mean)
stat_tab2<-stat_tab[,c(3,4,2,1,5)]
names(stat_tab2)<-c("Network measure","True effect","Model","Network covariance","Detection rate")
stat_tab2[,1]<-as.character(stat_tab2[,1])
stat_tab2[stat_tab2[,1]=="strength",1]<-"Strength"
stat_tab2[stat_tab2[,1]=="betweenness_w",1]<-"Betweenness"
stat_tab2[,5]<-round(stat_tab2[,5],2)

test5<-regulartable(stat_tab2)
test5<-bold(test5,part="header")
test5<-font(test5,fontname="Arial")
test5<-fontsize(test5,size=9)
test5<-fontsize(test5,size=10,part="header")
test5<-padding(test5,i=seq(1,72,1),padding=c(1,1,1,1))
test5<-width(test5,j=seq(1,5,1),width=c(1,1,0.8,1.2,0.8))
test5<-height(test5,i=seq(1,72,1),height=rep(0.01,72))
test5<-align(test5,j=c(2,4),align="left")
test5<-align(test5,j=c(2,4),align="left",part="header")
test5<-hline(test5,i=seq(3,72,3))
test5<-hline(test5,i=seq(12,72,12),border=fp_border_default(width=2))
test5<-hline(test5,i=seq(24,72,24),border=fp_border_default(width=3))
test5

path<-getwd()
#save_as_docx(test5,path=paste0(path,"/TableS5.docx"))

#####################


test_data2B<-test_data2A[test_data2A$pcg==0.25,]
stat_tab<-aggregate(test_data2B$StatClear,by=list(test_data2B$net_cov,test_data2B$m_inf,test_data2B$net_vars,test_data2B$net_effs),mean)
stat_tab2<-stat_tab[,c(3,4,2,1,5)]
names(stat_tab2)<-c("Network measure","True effect","Model","Network covariance","Detection rate")
stat_tab2[,1]<-as.character(stat_tab2[,1])
stat_tab2[stat_tab2[,1]=="strength",1]<-"Strength"
stat_tab2[stat_tab2[,1]=="betweenness_w",1]<-"Betweenness"
stat_tab2[,5]<-round(stat_tab2[,5],2)

test6<-regulartable(stat_tab2)
test6<-bold(test6,part="header")
test6<-font(test6,fontname="Arial")
test6<-fontsize(test6,size=9)
test6<-fontsize(test6,size=10,part="header")
test6<-padding(test6,i=seq(1,72,1),padding=c(1,1,1,1))
test6<-width(test6,j=seq(1,5,1),width=c(1,1,0.8,1.2,0.8))
test6<-height(test6,i=seq(1,72,1),height=rep(0.01,72))
test6<-align(test6,j=c(2,4),align="left")
test6<-align(test6,j=c(2,4),align="left",part="header")
test6<-hline(test6,i=seq(3,72,3))
test6<-hline(test6,i=seq(12,72,12),border=fp_border_default(width=2))
test6<-hline(test6,i=seq(24,72,24),border=fp_border_default(width=3))
test6

path<-getwd()
#save_as_docx(test6,path=paste0(path,"/TableS6.docx"))

#####################
#####################

centers<-6
clu<-kmeans(cbind(posterior_medians2[,3],posterior_sds2[,3]),centers=centers)
#library(viridis)
#cols<-viridis(centers)
cols<-c("#332288", "#88CCEE", "#117733", "#DDCC77", "#CC6677","#AA4499")

plot(abs(posterior_medians2[,3]),posterior_sds2[,3],col=adjustcolor(cols[clu$cluster],0.6),pch=15,las=1,cex.lab=1.5,cex.axis=1.25,ylab="Posterior standard deviation",xlab="Posterior Median")
lines(x=c(-100,100),y=c(1,1),lty=2)
lines(x=c(2.5,2.5),y=c(-100,100),lty=2)

clu_in<-which(clu$centers[,1]<1&clu$centers[,2]<1)

clu2<-ifelse(clu$cluster%in%clu_in==TRUE,1,0)

test_data$conv<-clu2

centers2<-seq(2,10,1)
wss<-sapply(centers2,
            function(k){kmeans(cbind(posterior_medians2[,3],posterior_sds2[,3]), k, nstart=50,iter.max = 15 )$tot.withinss})
wss

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

#####################
#####################

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

#####################
#####################

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

#####################
#####################

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

#####################
#####################

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

#####################
#####################



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


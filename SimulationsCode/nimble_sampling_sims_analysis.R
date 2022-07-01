samp1<-readRDS("SAMP_RES.RDS")
full1<-readRDS("FULL_RES.RDS")
samp2<-readRDS("SAMP_RES2.RDS")
full2<-readRDS("FULL_RES2.RDS")

par(xpd=FALSE)
plot(samp1[3,1,]~full1[3,1,],xlim=c(-1,0),ylim=c(-1,0))
lines(x=seq(-1,0,0.01),y=seq(-1,0,0.01),lty=3)
lines(x=rep(-0.75,2),y=c(-100,100),lty=1)
lines(y=rep(-0.75,2),x=c(-100,100),lty=1)
cor.test(full1[3,1,],samp1[3,1,])

################

##Plot for first set of results

corr<-rep(0,20)
acc<-rep(0,20)

for(i in 1:20){
  if(full1[3,3,i] < -0.75&full1[3,5,i] > -0.75){acc[i]<-acc[i]+1}
  if(samp1[3,3,i] < -0.75&samp1[3,5,i] > -0.75){acc[i]<-acc[i]+1}
}

for(i in 1:20){
  if(full1[3,5,i] < 0){corr[i]<-corr[i]+1}
  if(samp1[3,5,i] < 0){corr[i]<-corr[i]+1}
}

bg_cols<-rep(NA,dim(samp1)[3])

for(i in 1:dim(samp1)[3]){
  bg_cols[i]<-adjustcolor("firebrick",(acc[i]+1)/3)
}

par(xpd=FALSE,bty="L")
plot(NULL,xlim=c(-1,0),ylim=c(-1,0.25),xlab="Estimate using true network measures",ylab="Estimate using sampled network meeasures")
lines(x=seq(-10,10,0.01),y=seq(-10,10,0.01),lty=3)
lines(x=rep(-0.75,2),y=c(-100,100),lty=1,lwd=4,col=adjustcolor("light grey",0.5))
lines(y=rep(-0.75,2),x=c(-100,100),lty=1,lwd=4,col=adjustcolor("light grey",0.5))
lines(x=rep(0,2),y=c(-100,100),lty=1)
lines(y=rep(0,2),x=c(-100,100),lty=1)
points(y=samp1[3,1,],x=full1[3,1,],pch=21,col=adjustcolor("darkblue",1),lwd=(corr+1)*2,bg=bg_cols,cex=3)
cor.test(full1[3,1,],samp1[3,1,])

mean(samp1[3,1,])
mean(full1[3,1,])

#####################################

##Plot for second set of results

corr<-rep(0,20)
acc<-rep(0,20)

for(i in 1:20){
  if(full2[3,3,i] < -0.75&full2[3,5,i] > -0.75){acc[i]<-acc[i]+1}
  if(samp2[3,3,i] < -0.75&samp2[3,5,i] > -0.75){acc[i]<-acc[i]+1}
}

for(i in 1:20){
  if(full2[3,5,i] < 0){corr[i]<-corr[i]+1}
  if(samp2[3,5,i] < 0){corr[i]<-corr[i]+1}
}

bg_cols<-rep(NA,dim(samp1)[3])

for(i in 1:dim(samp2)[3]){
  bg_cols[i]<-adjustcolor("firebrick",(acc[i]+1)/3)
}

par(xpd=FALSE,bty="L")
plot(NULL,xlim=c(-1,0),ylim=c(-1,0.25),xlab="Estimate using true network measures",ylab="Estimate using sampled network meeasures")
lines(x=seq(-10,10,0.01),y=seq(-10,10,0.01),lty=3)
lines(x=rep(-0.75,2),y=c(-100,100),lty=1,lwd=4,col=adjustcolor("light grey",0.5))
lines(y=rep(-0.75,2),x=c(-100,100),lty=1,lwd=4,col=adjustcolor("light grey",0.5))
lines(x=rep(0,2),y=c(-100,100),lty=1)
lines(y=rep(0,2),x=c(-100,100),lty=1)
points(y=samp2[3,1,],x=full2[3,1,],pch=21,col=adjustcolor("darkblue",1),lwd=(corr+1)*2,bg=bg_cols,cex=3)
cor.test(full2[3,1,],samp2[3,1,])

mean(samp2[3,1,])
mean(full2[3,1,])

par(mfrow=c(1,2))
plot(samp1[4,1,]~full1[4,1,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
plot(samp2[4,1,]~full2[4,1,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
par(mfrow=c(1,1))



par(mfrow=c(1,2))
plot(samp1[4,2,]~full1[4,2,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
plot(samp2[4,2,]~full2[4,2,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
par(mfrow=c(1,1))


###


par(mfrow=c(2,2))
plot(samp1[1,1,]~full1[1,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp2[1,1,]~full2[1,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp1[2,1,]~full1[2,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp2[2,1,]~full2[2,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
par(mfrow=c(1,1))

###


par(mfrow=c(2,2))
plot(samp1[1,2,]~full1[1,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp2[1,2,]~full2[1,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp1[2,2,]~full1[2,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp2[2,2,]~full2[2,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
par(mfrow=c(1,1))

library(boot)
mean(inv.logit(samp1[1,1,]))
mean(inv.logit(full1[1,1,]))
mean(inv.logit(samp1[2,1,]))
mean(inv.logit(full1[2,1,]))
inv.logit(logit(0.8)-1)

get_overlap_coef <- function(mu1, mu2, sd1, sd2){
  xs  <- seq(min(mu1 - 4*sd1, mu2 - 4*sd2), 
             max(mu1 + 4*sd1, mu2 + 4*sd2), 
             length.out = 500)
  f1  <- dnorm(xs, mean=mu1, sd=sd1)
  f2  <- dnorm(xs, mean=mu2, sd=sd2)
  int <- xs[which.max(pmin(f1, f2))]
  l   <- pnorm(int, mu1, sd1, lower.tail = mu1>mu2)
  r   <- pnorm(int, mu2, sd2, lower.tail = mu1<mu2)
  l+r
}

get_overlap_coef(samp1[3,1,1],full1[3,1,1],samp1[3,2,1],full1[3,2,1])
get_overlap_coef(samp1[3,1,2],full1[3,1,2],samp1[3,2,2],full1[3,2,2])
get_overlap_coef(samp2[3,1,1],full2[3,1,1],samp2[3,2,1],full2[3,2,1])
get_overlap_coef(samp2[3,1,2],full2[3,1,2],samp2[3,2,2],full2[3,2,2])

s1<-samp1[3,1:2,]
f1<-full1[3,1:2,]
s2<-samp2[3,1:2,]
f2<-full2[3,1:2,]

overlaps1<-overlaps2<-numeric()

for(i in 1:20){
  overlaps1[i]<-get_overlap_coef(s1[1,i],f1[1,i],s1[2,i],f1[2,i])
  overlaps2[i]<-get_overlap_coef(s2[1,i],f2[1,i],s2[2,i],f2[2,i])
}

mean(overlaps1)
mean(overlaps2)

hist(overlaps1)
hist(overlaps2)

hist(overlaps2,xlim=c(0,1),ylim=c(0,10),breaks=seq(0,1,0.1),col=adjustcolor("darkblue",0.2),border="darkblue")
hist(overlaps1,xlim=c(0,1),ylim=c(0,10),breaks=seq(0,1,0.1),col=adjustcolor("firebrick",0.2),border="firebrick",add=TRUE)


#####################
#####################

samp3<-readRDS("SAMP_RES3.RDS")
full3<-readRDS("FULL_RES3.RDS")

for(i in 1:dim(samp3)[3]){
  if(max(samp3[,6,i])>1.2){
    samp3[,1:5,i]<-NA
  }
}

corr<-rep(0,20)
acc<-rep(0,20)

for(i in 1:20){
  if(is.na(full3[3,3,i])==FALSE & full3[3,3,i] < -0.75&full3[3,5,i] > -0.75){acc[i]<-acc[i]+1}
  if(is.na(samp3[3,3,i])==FALSE & samp3[3,3,i] < -0.75&samp3[3,5,i] > -0.75){acc[i]<-acc[i]+1}
}

for(i in 1:20){
  if(is.na(full3[3,5,i])==FALSE & full3[3,5,i] < 0){corr[i]<-corr[i]+1}
  if(is.na(samp3[3,5,i])==FALSE & samp3[3,5,i] < 0){corr[i]<-corr[i]+1}
}

bg_cols<-rep(NA,dim(samp1)[3])

for(i in 1:dim(samp1)[3]){
  bg_cols[i]<-adjustcolor("firebrick",(acc[i]+1)/3)
}

par(xpd=FALSE)
plot(NULL,xlim=c(-1.5,0.5),ylim=c(-1.5,0.5),xlab="Estimate using true network measures",ylab="Estimate using sampled network meeasures")
lines(x=seq(-10,10,0.01),y=seq(-10,10,0.01),lty=3)
lines(x=rep(-0.75,2),y=c(-100,100),lty=1,lwd=4,col=adjustcolor("light grey",0.5))
lines(y=rep(-0.75,2),x=c(-100,100),lty=1,lwd=4,col=adjustcolor("light grey",0.5))
lines(x=rep(0,2),y=c(-100,100),lty=1)
lines(y=rep(0,2),x=c(-100,100),lty=1)
points(samp3[3,1,]~full3[3,1,],pch=21,col=adjustcolor("darkblue",1),lwd=(corr+1)*2,bg=bg_cols,cex=3)
cor.test(full1[3,1,],samp1[3,1,])

mean(samp3[3,1,],na.rm=T)
mean(full3[3,1,])

#####


par(mfrow=c(1,3))
plot(samp1[4,1,]~full1[4,1,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
plot(samp2[4,1,]~full2[4,1,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
plot(samp3[4,1,]~full3[4,1,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
par(mfrow=c(1,1))



par(mfrow=c(1,3))
plot(samp1[4,2,]~full1[4,2,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
plot(samp2[4,2,]~full2[4,2,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
plot(samp3[4,2,]~full3[4,2,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
par(mfrow=c(1,1))


###


par(mfrow=c(2,3))
plot(samp1[1,1,]~full1[1,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp2[1,1,]~full2[1,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp3[1,1,]~full3[1,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp1[2,1,]~full1[2,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp2[2,1,]~full2[2,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp3[2,1,]~full3[2,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
par(mfrow=c(1,1))

###


par(mfrow=c(2,3))
plot(samp1[1,2,]~full1[1,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp2[1,2,]~full2[1,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp3[1,2,]~full3[1,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp1[2,2,]~full1[2,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp2[2,2,]~full2[2,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(samp3[2,2,]~full3[2,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
par(mfrow=c(1,1))

###########
###########


s1<-samp1[3,1:2,]
f1<-full1[3,1:2,]
s2<-samp2[3,1:2,]
f2<-full2[3,1:2,]
s3<-samp3[3,1:2,]
f3<-full3[3,1:2,]

overlaps1<-overlaps2<-overlaps3<-numeric()

for(i in 1:20){
  overlaps1[i]<-get_overlap_coef(s1[1,i],f1[1,i],s1[2,i],f1[2,i])
  overlaps2[i]<-get_overlap_coef(s2[1,i],f2[1,i],s2[2,i],f2[2,i])
  if(sum(is.na(s3[,i]))==0){
    overlaps3[i]<-get_overlap_coef(s3[1,i],f3[1,i],s3[2,i],f3[2,i])
  }
}

mean(overlaps1)
mean(overlaps2)
mean(overlaps3,na.rm=T)
hist(overlaps1)
hist(overlaps2)

hist(overlaps3,xlim=c(0,1),ylim=c(0,10),breaks=seq(0,1,0.1),col=adjustcolor("goldenrod",0.2),border="goldenrod")
#hist(overlaps2,xlim=c(0,1),ylim=c(0,10),breaks=seq(0,1,0.1),col=adjustcolor("darkblue",0.2),border="darkblue",add=TRUE)
hist(overlaps1,xlim=c(0,1),ylim=c(0,10),breaks=seq(0,1,0.1),col=adjustcolor("firebrick",0.2),border="firebrick",add=TRUE)

median(overlaps1)
median(overlaps2)
median(overlaps3,na.rm=T)

mean(samp3[3,1,]-full3[3,1,],na.rm=T)
mean(samp2[3,1,]-full2[3,1,],na.rm=T)
mean(samp1[3,1,]-full1[3,1,],na.rm=T)


######################
######################

sampn1<-readRDS("SAMP_RESN1.RDS")
fulln1<-readRDS("FULL_RESN1.RDS")
sampn2<-readRDS("SAMP_RESN2.RDS")
fulln2<-readRDS("FULL_RESN2.RDS")
sampn3<-readRDS("SAMP_RESN3.RDS")
fulln3<-readRDS("FULL_RESN3.RDS")

corr<-rep(0,20)
acc<-rep(0,20)

for(i in 1:20){
  if(fulln1[3,3,i] < -0.75&fulln1[3,5,i] > -0.75){acc[i]<-acc[i]+1}
  if(sampn1[3,3,i] < -0.75&sampn1[3,5,i] > -0.75){acc[i]<-acc[i]+1}
}

for(i in 1:20){
  if(fulln1[3,5,i] < 0){corr[i]<-corr[i]+1}
  if(sampn1[3,5,i] < 0){corr[i]<-corr[i]+1}
}

bg_cols<-rep(NA,dim(sampn1)[3])

for(i in 1:dim(sampn1)[3]){
  bg_cols[i]<-adjustcolor("firebrick",(acc[i]+1)/3)
}

par(xpd=FALSE,bty="L")
plot(NULL,xlim=c(-1.5,0),ylim=c(-1.5,0.25),xlab="Estimate using true network measures",ylab="Estimate using sampled network meeasures")
lines(x=seq(-10,10,0.01),y=seq(-10,10,0.01),lty=3)
lines(x=rep(-0.75,2),y=c(-100,100),lty=1,lwd=4,col=adjustcolor("light grey",0.5))
lines(y=rep(-0.75,2),x=c(-100,100),lty=1,lwd=4,col=adjustcolor("light grey",0.5))
lines(x=rep(0,2),y=c(-100,100),lty=1)
lines(y=rep(0,2),x=c(-100,100),lty=1)
points(y=sampn1[3,1,],x=fulln1[3,1,],pch=21,col=adjustcolor("darkblue",1),lwd=(corr+1)*2,bg=bg_cols,cex=3)
cor.test(fulln1[3,1,],sampn1[3,1,])

inv.logit(mean(sampn1[1,1,]))
inv.logit(mean(sampn1[2,1,]))

inv.logit(logit(0.8)-1)

###saveRDS(sampn1,"SAMP_RESN1.RDS")
###saveRDS(fulln1,"FULL_RESN1.RDS")

###############################

corr<-rep(0,20)
acc<-rep(0,20)

for(i in 1:20){
  if(fulln2[3,3,i] < -0.75&fulln2[3,5,i] > -0.75){acc[i]<-acc[i]+1}
  if(sampn2[3,3,i] < -0.75&sampn2[3,5,i] > -0.75){acc[i]<-acc[i]+1}
}

for(i in 1:20){
  if(fulln2[3,5,i] < 0){corr[i]<-corr[i]+1}
  if(sampn2[3,5,i] < 0){corr[i]<-corr[i]+1}
}

bg_cols<-rep(NA,dim(sampn2)[3])

for(i in 1:dim(sampn2)[3]){
  bg_cols[i]<-adjustcolor("firebrick",(acc[i]+1)/3)
}

par(xpd=FALSE,bty="L")
plot(NULL,xlim=c(-1.5,0),ylim=c(-1.5,0.25),xlab="Estimate using true network measures",ylab="Estimate using sampled network meeasures")
lines(x=seq(-10,10,0.01),y=seq(-10,10,0.01),lty=3)
lines(x=rep(-0.75,2),y=c(-100,100),lty=1,lwd=4,col=adjustcolor("light grey",0.5))
lines(y=rep(-0.75,2),x=c(-100,100),lty=1,lwd=4,col=adjustcolor("light grey",0.5))
lines(x=rep(0,2),y=c(-100,100),lty=1)
lines(y=rep(0,2),x=c(-100,100),lty=1)
points(y=sampn2[3,1,],x=fulln2[3,1,],pch=21,col=adjustcolor("darkblue",1),lwd=(corr+1)*2,bg=bg_cols,cex=3)
cor.test(fulln2[3,1,],sampn2[3,1,])

inv.logit(mean(sampn2[1,1,]))
inv.logit(mean(sampn2[2,1,]))

inv.logit(logit(0.8)-1)

##############################

corr<-rep(0,20)
acc<-rep(0,20)

for(i in 1:20){
  if(fulln3[3,3,i] < -0.75&fulln3[3,5,i] > -0.75){acc[i]<-acc[i]+1}
  if(sampn3[3,3,i] < -0.75&sampn3[3,5,i] > -0.75){acc[i]<-acc[i]+1}
}

for(i in 1:20){
  if(fulln3[3,5,i] < 0){corr[i]<-corr[i]+1}
  if(sampn3[3,5,i] < 0){corr[i]<-corr[i]+1}
}

bg_cols<-rep(NA,dim(sampn3)[3])

for(i in 1:dim(sampn3)[3]){
  bg_cols[i]<-adjustcolor("firebrick",(acc[i]+1)/3)
}

par(xpd=FALSE,bty="L")
plot(NULL,xlim=c(-1.5,0),ylim=c(-1.5,0.25),xlab="Estimate using true network measures",ylab="Estimate using sampled network meeasures")
lines(x=seq(-10,10,0.01),y=seq(-10,10,0.01),lty=3)
lines(x=rep(-0.75,2),y=c(-100,100),lty=1,lwd=4,col=adjustcolor("light grey",0.5))
lines(y=rep(-0.75,2),x=c(-100,100),lty=1,lwd=4,col=adjustcolor("light grey",0.5))
lines(x=rep(0,2),y=c(-100,100),lty=1)
lines(y=rep(0,2),x=c(-100,100),lty=1)
points(y=sampn3[3,1,],x=fulln3[3,1,],pch=21,col=adjustcolor("darkblue",1),lwd=(corr+1)*2,bg=bg_cols,cex=3)
cor.test(fulln3[3,1,],sampn3[3,1,])

inv.logit(mean(sampn3[1,1,]))
inv.logit(mean(sampn3[2,1,]))

inv.logit(logit(0.8)-1)

####################################
####################################


par(mfrow=c(1,3))
plot(sampn1[4,1,]~fulln1[4,1,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
plot(sampn2[4,1,]~fulln2[4,1,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
plot(sampn3[4,1,]~fulln3[4,1,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
par(mfrow=c(1,1))


par(mfrow=c(1,3))
plot(sampn1[4,2,]~fulln1[4,2,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
plot(sampn2[4,2,]~fulln2[4,2,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
plot(sampn3[4,2,]~fulln3[4,2,])
lines(x=seq(0,1,0.01),y=seq(0,1,0.01),lty=2)
par(mfrow=c(1,1))


###


par(mfrow=c(2,3))
plot(sampn1[1,1,]~fulln1[1,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(sampn2[1,1,]~fulln2[1,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(sampn3[1,1,]~fulln3[1,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(sampn1[2,1,]~fulln1[2,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(sampn2[2,1,]~fulln2[2,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(sampn3[2,1,]~fulln3[2,1,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
par(mfrow=c(1,1))

###


par(mfrow=c(2,3))
plot(sampn1[1,2,]~fulln1[1,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(sampn2[1,2,]~fulln2[1,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(sampn3[1,2,]~fulln3[1,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(sampn1[2,2,]~fulln1[2,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(sampn2[2,2,]~fulln2[2,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
plot(sampn3[2,2,]~fulln3[2,2,])
lines(x=seq(-100,100,1),y=seq(-100,100,1),lty=2)
par(mfrow=c(1,1))

###########
###########

s1<-sampn1[3,1:2,]
f1<-fulln1[3,1:2,]
s2<-sampn2[3,1:2,]
f2<-fulln2[3,1:2,]
s3<-sampn3[3,1:2,]
f3<-fulln3[3,1:2,]

overlaps1<-overlaps2<-overlaps3<-numeric()

for(i in 1:20){
  overlaps1[i]<-get_overlap_coef(s1[1,i],f1[1,i],s1[2,i],f1[2,i])
  overlaps2[i]<-get_overlap_coef(s2[1,i],f2[1,i],s2[2,i],f2[2,i])
  if(sum(is.na(s3[,i]))==0){
    overlaps3[i]<-get_overlap_coef(s3[1,i],f3[1,i],s3[2,i],f3[2,i])
  }
}

mean(overlaps1)
mean(overlaps2)
mean(overlaps3,na.rm=T)
hist(overlaps1)
hist(overlaps2)

hist(overlaps3,xlim=c(0,1),ylim=c(0,10),breaks=seq(0,1,0.1),col=adjustcolor("goldenrod",0.2),border="goldenrod")
#hist(overlaps2,xlim=c(0,1),ylim=c(0,10),breaks=seq(0,1,0.1),col=adjustcolor("darkblue",0.2),border="darkblue",add=TRUE)
hist(overlaps1,xlim=c(0,1),ylim=c(0,10),breaks=seq(0,1,0.1),col=adjustcolor("firebrick",0.2),border="firebrick",add=TRUE)

median(overlaps1)
median(overlaps2)
median(overlaps3,na.rm=T)

mean(sampn3[3,1,]-fulln3[3,1,],na.rm=T)
mean(sampn2[3,1,]-fulln2[3,1,],na.rm=T)
mean(sampn1[3,1,]-fulln1[3,1,],na.rm=T)


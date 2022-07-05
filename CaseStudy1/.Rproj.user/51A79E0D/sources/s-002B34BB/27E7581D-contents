

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


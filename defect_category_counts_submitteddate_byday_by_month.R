# Charm category breakdown (date and month scale)
# install.packages("gridExtra")
library(ggplot2)
library(scales)
library(gridExtra)
sensisoverall2<-read.csv("F:/AnalysisData/SENSIS_Overall.csv",header = TRUE,sep = ",")
subsettingdata2<-subset(sensisoverall2,select = c("Mess_Date","Category"))
subsettingdata2
date3<-subsettingdata2$Mess_Date

vectorconversion<-as.vector(date3)
vectorconversion
dateconversion<-as.Date(vectorconversion,format="%B %d, %Y")
dateconversion
dataframe_me_ca<-data.frame(dateconversion,subsettingdata2$Category)
dataframe_me_ca
datasince13<-subset(dataframe_me_ca,dateconversion>"2012-12-31")
datasince13
dat_frames1<-datasince13[order(datasince13$dateconversion),]
dat_frames1

value<-aggregate(dat_frames1,by=list(dat_frames1$subsettingdata2.Category,dat_frames1$dateconversion),FUN=length)
value


l<-ggplot(value, aes(x=value$Group.2, y=value$subsettingdata2.Category))+geom_bar(position="stack", stat="identity",aes(fill=value$Group.1))+scale_x_date(limits = as.Date(c('2012-11-01','2016-11-01')))+theme(axis.text=element_text(size=7, color="blue"),axis.title=element_text(size=14,face="bold", color="green"),axis.text.x = element_text(size = 8, color="red",vjust = 0, angle = 90 ),legend.position=c(.188, .8))+ggtitle("Charm Category timeline by day") +labs(x = "Charm Submitted Date", y = "Charm counts of each Day")+theme(legend.text=element_text(size=8), legend.direction = "horizontal",legend.title = element_text(colour = 'red'))
plot1<-l+scale_fill_discrete(name = "Charm category")
plot1
dat_frames1$month1<-as.Date(cut(dat_frames1$dateconversion,breaks = "month"))
dat_frames1

value_1<-aggregate(dat_frames1,by=list(dat_frames1$subsettingdata2.Category,dat_frames1$month1),FUN=length)

names(value_1)
class(value_1$month1)
bar_plot_4<-ggplot(value_1,aes(x=Group.2,y=subsettingdata2.Category))+geom_bar(position="stack", stat="identity",aes(fill=Group.1))+scale_x_date(labels = date_format("%Y-%b"),breaks=date_breaks("1 month"),limits = as.Date(c('2012-11-01','2016-11-01')))+theme(axis.text=element_text(size=7, color="blue"),axis.title=element_text(size=14,face="bold", color="green"),axis.text.x = element_text(size = 8, color="Red",vjust = 0, angle = 90 ),legend.position=c(.188, .7))+labs(x = "Charm Submitted Date", y = "count of charm")+ggtitle("Count of Charm vs Timeline by Month")+theme(legend.text=element_text(size=8), legend.direction = "horizontal",legend.title = element_text(colour = 'red'))
bar_plot_5<-bar_plot_4+scale_fill_discrete(name = "Charm Category")
bar_plot_5

grid.arrange(plot1,bar_plot_5, nrow = 2, heights = c(0.50, 0.50))#plot of overall_sensisdata by month and by day

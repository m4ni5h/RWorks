# Change in module on scales of date and month

ModuleChanges<-read.csv("F:/AnalysisData/ModuleChanges.txt",header = TRUE,sep = ",")
library(scales)
changes<-ModuleChanges$Change
mod_name<-ModuleChanges$module_name
date_sub<-ModuleChanges$DateTime
date_sub
vector_conrt<-as.vector(date_sub)
vector_conrt
date_conrt<-as.Date(vector_conrt,"%Y-%m-%d")
date_conrt
data_frame<-data.frame(date_conrt,mod_name,changes)
class(data_frame$date_conrt)
dat_frame_s<-data_frame[order(as.Date(date_conrt, format = "%Y-%m-%d")),]
dat_frame_s
sumofchange<-aggregate(cbind(changes)~mod_name+date_conrt, data=data_frame, FUN=sum)
sumofchange
class(sumofchange$date_conrt)
bar_plot<-ggplot(sumofchange, aes(x=date_conrt, y=changes, fill=mod_name))+geom_bar(stat="identity", na.rm = TRUE)+scale_x_date(limits = as.Date(c('2012-11-01','2016-11-01')))+theme(axis.text=element_text(size=7, color="blue"),axis.title=element_text(size=14,face="bold", color="green"),axis.text.x = element_text(size = 8, color="red",vjust = 0, angle = 90 ),legend.position=c(.1, .7))+labs(x = "Change Date", y = "Change")+ggtitle("Timeline of Changes in module_name by Day")
bar_plot_1<-bar_plot+scale_fill_discrete(name = "module")
bar_plot_1
dat_frame_s$month<-as.Date(cut(dat_frame_s$date_conrt,breaks = "month"))
dat_frame_s

sumofchange11<-aggregate(cbind(changes)~mod_name+month, data=dat_frame_s, FUN=sum)
sumofchange11

names(sumofchange)
bar_plot_2<-ggplot(sumofchange11,aes(x=month,y=changes,fill=mod_name))+geom_bar(position='stack', stat='identity')+scale_x_date(labels = date_format("%Y-%b"),breaks=date_breaks("1 month"),limits = as.Date(c('2012-11-01','2016-11-01')))+theme(axis.text=element_text(size=7, color="blue"),axis.title=element_text(size=14,face="bold", color="green"),axis.text.x = element_text(size = 8, color="red",vjust = 0, angle = 90 ),legend.position=c(.1, .7))+labs(x = "Change Date", y = "Change")+ggtitle("Timeline of Changes in module_name by Month")
bar_plot_3<-bar_plot_2+scale_fill_discrete(name = "module")
bar_plot_3

install.packages("gapminder")
library(gapminder)
library(ggplot2)
library(gridExtra)
grid.arrange(bar_plot_1,bar_plot_3,nrow=2, heights=c(0.50, 0.50))#plot of module by month and by day

# Defect category breakdown (date and month scale)
# install.packages("gridExtra")
# install.packages("scales")
# install.packages("ggplot2")
library(ggplot2)
library(scales)
library(gridExtra)
project_data<-read.csv("F:/AnalysisData/DataFromProject.csv",header = TRUE,sep = ",")
subset_data<-subset(project_data,select = c("defect_submission_date","Category"))
subset_data
defect_date_data<-subset_data$defect_submission_date

vectorconversion<-as.vector(defect_date_data)
vectorconversion
dateconversion<-as.Date(vectorconversion,format="%B %d, %Y")
dateconversion
defect_frame_category<-data.frame(dateconversion,subset_data$Category)
defect_frame_category
datasince13<-subset(defect_frame_category,dateconversion>"2012-12-31")
datasince13
date_date_frame<-datasince13[order(datasince13$dateconversion),]
date_date_frame

aggregated_value<-aggregate(date_date_frame,by=list(date_date_frame$subset_data.Category,date_date_frame$dateconversion),FUN=length)
aggregated_value

myggplot<-ggplot(aggregated_value, aes(x=aggregated_value$Group.2, y=aggregated_value$subset_data.Category))+geom_bar(position="stack", stat="identity",aes(fill=aggregated_value$Group.1))+scale_x_date(limits = as.Date(c('2012-11-01','2016-11-01')))+theme(axis.text=element_text(size=7, color="blue"),axis.title=element_text(size=14,face="bold", color="green"),axis.text.x = element_text(size = 8, color="red",vjust = 0, angle = 90 ),legend.position=c(.188, .8))+ggtitle("Defect Category timeline by day") +labs(x = "Defect Submitted Date", y = "Defect counts of each Day")+theme(legend.text=element_text(size=8), legend.direction = "horizontal",legend.title = element_text(colour = 'red'))
myggplot_discreet<-myggplot+scale_fill_discrete(name = "Defect category")
myggplot_discreet
date_date_frame$month1<-as.Date(cut(date_date_frame$dateconversion,breaks = "month"))
date_date_frame

value_1<-aggregate(date_date_frame,by=list(date_date_frame$subset_data.Category,date_date_frame$month1),FUN=length)

names(value_1)
class(value_1$month1)
temp_barplot<-ggplot(value_1,aes(x=Group.2,y=subset_data.Category))+geom_bar(position="stack", stat="identity",aes(fill=Group.1))+scale_x_date(labels = date_format("%Y-%b"),breaks=date_breaks("1 month"),limits = as.Date(c('2012-11-01','2016-11-01')))+theme(axis.text=element_text(size=7, color="blue"),axis.title=element_text(size=14,face="bold", color="green"),axis.text.x = element_text(size = 8, color="Red",vjust = 0, angle = 90 ),legend.position=c(.188, .7))+labs(x = "Defect Submitted Date", y = "count of Defect")+ggtitle("Count of Defect vs Timeline by Month")+theme(legend.text=element_text(size=8), legend.direction = "horizontal",legend.title = element_text(colour = 'red'))
my_barplot<-temp_barplot+scale_fill_discrete(name = "Defect Category")
my_barplot

grid.arrange(myggplot_discreet,my_barplot, nrow = 2, heights = c(0.50, 0.50))

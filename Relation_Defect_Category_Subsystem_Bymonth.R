# Module changes and Defect count changes (Manual correlation)
ModuleChanges<-read.csv("F:/AnalysisData/ModuleChanges.txt",header = TRUE,sep = ",")
ChangeCount<-ModuleChanges$Change
ModuleName<-ModuleChanges$ModuleName
DateTimeOfChange<-ModuleChanges$DateTime 
DateTimeData<-as.Date(DateTimeOfChange,"%Y-%m-%d")
DataFrame1<-data.frame(ChangeCount,ModuleName,DateTimeData)
SortedData<-DataFrame1[order(as.Date(DateTimeData, format = "%Y-%m-%d")),]
names(SortedData)
library(ggplot2)
library(scales)
AggregateChangeData<-aggregate(cbind(ChangeCount)~ModuleName+DateTimeData, data=SortedData, FUN=sum)
AggregateChangeData
#convert data frame into csv file
write.csv(AggregateChangeData,"output4.csv")
NewChangeData<- read.csv("output4.csv")
print(NewChangeData)
names(NewChangeData)
levels(NewChangeData$ModuleName)

plot_3<-ggplot(AggregateChangeData, aes(x=AggregateChangeData$DateTimeData, y=AggregateChangeData$ChangeCount, fill=AggregateChangeData$ModuleName))+geom_bar(position='stack', stat='identity')+scale_x_date(limits = as.Date(c('2012-11-01','2016-11-01')))+theme(axis.text=element_text(size=7, color="blue"),axis.title=element_text(size=14,face="bold", color="green"),axis.text.x = element_text(size = 9, color="black",vjust = 0, angle = 90 ),legend.position=c(.25, .7))+labs(x = "Change Date", y = "Change")+ggtitle("Timeline of Changes in ModuleName by Day")+theme(legend.text=element_text(size=8), legend.direction = "horizontal",legend.title = element_text(colour = 'red',angle = 90,size = 13))
NewPlot1<-plot_3+scale_fill_discrete(name = "Subsystems")
NewPlot1

AggregateChangeData$month4<-as.Date(cut(AggregateChangeData$DateTimeData,breaks = "month"))
names(AggregateChangeData)

m_plot<-ggplot(AggregateChangeData, aes(x=month4, y=ChangeCount, fill=ModuleName))+stat_summary(fun.y =sum,geom ="bar")+scale_x_date(labels = date_format("%Y-%b"),breaks=date_breaks("1 month"),limits = as.Date(c('2012-11-01','2016-11-01')))+theme(axis.text=element_text(size=7, color="blue"),axis.title=element_text(size=14,face="bold", color="green"),axis.text.x = element_text(size = 9, color="red",vjust = 0, angle = 90 ),legend.position=c(.25, .7))+labs(x = "Change Date", y = "Change")+ggtitle("Timeline of Changes in ModuleName by Month")+theme(legend.text=element_text(size=7))+theme(legend.text=element_text(size=8), legend.direction = "horizontal",legend.title = element_text(colour = 'red',angle = 90,size = 13))
m_plot1<-m_plot+scale_fill_discrete(name = "Subsystems")

m_plot1

Project_All<-read.csv("F:/AnalysisData/Project_All.csv",header = TRUE,sep = ",")
Subset_All<-subset(Project_All,select = c("Mess_Date","Category"))
Subset_All
SubmissionDate<-Subset_All$Mess_Date
vector_conversion<-as.vector(SubmissionDate)
vector_conversion
date_conversion<-as.Date(vector_conversion,format="%B %d, %Y")
date_conversion
categ<-Subset_All$Category

dataframe_me_ca1<-data.frame(date_conversion,categ)
dataframe_me_ca1
datasince_013<-subset(dataframe_me_ca1,date_conversion>"2012-12-31")
datasince_013
datasince_013n<-datasince_013[order(datasince_013$date_conversion),]
names(datasince_013n)

fvalue<-aggregate(datasince_013n,by=list(datasince_013n$date_conversion,datasince_013n$categ),FUN=length)
fvalue
names(fvalue)
#convert the data frame into csv file
write.csv(fvalue,"fvaluue1.csv")
fvalue2<-read.csv("fvaluue1.csv")
print(fvalue2)
names(fvalue2)
fvalue2$Group.2

l_1<-ggplot(fvalue, aes(x=fvalue$Group.1, y=fvalue$categ))+geom_bar(position="stack", stat="identity",aes(fill=fvalue$Group.2))+scale_x_date(limits = as.Date(c('2012-11-01','2016-11-01')))+theme(axis.text=element_text(size=8, color="blue"),axis.title=element_text(size=14,face="bold", color="green"),axis.text.x = element_text(size = 9, color="red",vjust = 0, angle = 90 ),legend.position=c(.27, .8))+ggtitle("Defect Category timeline by Day") +labs(x = "Defect Submitted Date", y = "Defect counts by Day")+theme(legend.text=element_text(size=7), legend.direction = "horizontal",legend.title = element_text(colour = 'red'))
plot_2<-l_1+ scale_fill_discrete(name = "Defect Category")
plot_2

datasince_013n$month7<-as.Date(cut(datasince_013n$date_conversion,breaks = "month"))
names(datasince_013n)

value_3<-aggregate(datasince_013n,by=list(datasince_013n$categ,datasince_013n$month7),FUN=length)
value_3
names(value_3)
class(value_3$month7)
bar_plot_10<-ggplot(value_3,aes(x=Group.2,y=categ))+geom_bar(position="stack", stat="identity",aes(fill=Group.1))+scale_x_date(labels = date_format("%Y-%b"),breaks=date_breaks("1 month"),limits = as.Date(c('2012-11-01','2016-11-01')))+theme(axis.text=element_text(size=8, color="blue"),axis.title=element_text(size=14,face="bold", color="green"),axis.text.x = element_text(size = 8, color="Red",vjust = 0, angle = 90 ),legend.position=c(.2, .7))+labs(x = "Defect Submitted Date", y = "count of Defect")+ggtitle("Count of Defect vs Timeline by Month")+theme(legend.text=element_text(size=7), legend.direction = "horizontal",legend.title = element_text(colour = 'red'))
bar_plot_11<-bar_plot_10+scale_fill_discrete(name = "Defect Category")
bar_plot_11
library(gridExtra)
grid.arrange(NewPlot1,plot_2, nrow = 2, heights = c(0.50, 0.50))#plot for category and ModuleName of project by month
grid.arrange(m_plot1,bar_plot_11, nrow = 2, heights = c(0.50, 0.50))#plot for category and ModuleName of project by Day

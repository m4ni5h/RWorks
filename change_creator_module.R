# contribution of a Resource, subsystem wise.
# install.packages("ggplot2")
sub15<-read.csv("F:/AnalysisData/SubsystemChanges_Since2015 (2).txt",header = TRUE,sep = ",")

selc<-subset(sub15,select = c("Creator","Change","Subsystem"))
selc
r3<-aggregate(selc$Change, by=list(selc$Creator,selc$Subsystem), FUN=sum)
r3
cre<-r3$Group.1
subsystem1<-r3$Group.2
changes<-r3$x
cre
subsystem1
changes
t1<-data.frame(cre,subsystem1,changes)

library(ggplot2)
o2<-ggplot(t1, aes(x =subsystem1 , y = cre, size = changes)) +geom_point(shape = 21, colour = "black",fill = "red")+scale_size_area(max_size = 10)+ggtitle("Change by creator in Subsystem") +labs(x = "CharmID", y = "Creator")+theme(axis.text=element_text(size=7, color="blue"),axis.title=element_text(size=14,face="bold", color="green"))
o2

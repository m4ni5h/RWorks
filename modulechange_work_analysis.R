# Resource - Subsytem relationship
# Subsystem - Average change relationship
# Resource -  Average change relationship

ModuleChange<-read.csv('F:/AnalysisData/ModuleChanges.txt',header = TRUE,sep = ',')
par(mfrow=c(3,1))
average<-tapply(ModuleChange$Change,ModuleChange$Creator,mean)
average
head(average)
barplot(height = average,col="blue",xlab="name",ylab="Mean_change",las=3,ylim=c(0,60))
title("Average Work Analysis by a Developer",cex.main = 2,   font.main= 1, col.main= "Red")
box()
ChangeSubsystemRelationship<-tapply(ModuleChange$Change,ModuleChange$Subsystem,mean)
head(ChangeSubsystemRelationship)
barplot(height = ChangeSubsystemRelationship,col="red",las=3,xlab="subsystem",ylab = "Mean_change")
title("Change on Module",cex.main = 2,   font.main= 1, col.main= "lightblue")
box()
CreatorSubsystemChange<-table(ModuleChange$Creator,ModuleChange$Subsystem)
print(CreatorSubsystemChange)
spineplot(x=CreatorSubsystemChange,las=2,xlab="name",ylab="subsystem")
title("Work on Module",cex.main = 2,   font.main= 1, col.main= "lightblue")

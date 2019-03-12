f<-read.csv("F:/AnalysisData/defectdata.csv",header = TRUE,sep = ",")
e<-subset(f, select=c("Category","Submission_Date","Solution_Date"),columns.name=TRUE)
e
x.sub <- subset(e, Category == "SoftWare")
x.sub

pmd<-subset(x.sub, select="Submission_Date")
pmd

sld<-subset(x.sub, select="Solution_Date")
sld

class(pmd)

md<-as.matrix(pmd)
md

fd<-as.Date(md,"%m/%d/%Y")
fd

md1<-as.matrix(sld)
md1
fd1<-as.Date(md1,"%m/%d/%Y")
fd1
remove(dit7)
dit7<-fd1-fd
dit7
plot(fd,dit7,type = "h",col=c("green"),main = "Software Defect Analysis",ylab = "No. of days",xlab = "Submission_year(2010 - 2016-05-02)")
legend("topright", inset=.05, title="Defect",pch=c(15), c("SoftWare"), col=c("green"),text.col ="blue", title.col="red", title.adj = 0.5)

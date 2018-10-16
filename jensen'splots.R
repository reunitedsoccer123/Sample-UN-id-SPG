#Graphs Anova 
library(readxl)
modjensen <- read_excel("modjensen.xlsx")
attach(modjensen)

library(readxl)
means_jensen <- read_excel("means jensen.xlsx")
attach(means_jensen)

#Figure 1A time in common zone
totalmean<-means_jensen$`Time in zone common1 (proximal) (seconds)`+means_jensen$`Time in zone common2 distal (seconds)`
barplot(means_jensen$`Time in zone common1 (proximal) (seconds)`,col=c("blue","orange","yellow"),ylab="Total Time", Xlab="Time in zone common1 (proximal) (seconds)",ylim=c(0,150))
barplot(totalmean,col=c("blue","orange","yellow"),main="Total Time Means in Common Zones",ylab="Total Time (Total Means)",ylim=c(0,250),xlab="Type")


axis(
  1, at=1:3,labels = list("Sedentary", "Continous", "Interval"),
  ,las=1
)
#anova of 1A
aaov<-data.frame(totalmean,means_jensen$`Time in zone common1 (proximal) (seconds)`,means_jensen$`common2 zone contact (distal)`)
aov(totalmean~`Time in zone common1 (proximal) (seconds)`+ `Time in zone common2 distal (seconds)` ,data=means_jensen)
summary(fit1mean)
totaltime<-modjensen$`Time in zone common1 (proximal) (seconds)`+modjensen$`Time in zone common2 distal (seconds)`
modj2<-data.frame(totaltime,modjensen$`Time in zone common1 (proximal) (seconds)`+modjensen$`Time in zone common2 distal (seconds)`)
#aov(totaltime~modjensen$`Time in zone common1 (proximal) (seconds)`+modjensen$`Time in zone common2 distal (seconds)`,data=modj2)
#Figure 1B
percent<-c(0,50,100)
percentCol_Novel<-100*((`Novel Object time in zone (seconds)`)/(`Novel Object time in zone (seconds)`+`Common Object time in zone`))
percentCol_Common<-100*((`Common Object time in zone`)/(`Novel Object time in zone (seconds)`+`Common Object time in zone`))
threop1<-c(51.48825,61.3394,52.99184)
threop2<-c(48.51175,38.66606,47.00816)

threop<-cbind(threop1,threop2)
row.names(threop)<-c("Sed","Cont","Int")
colnames(threop)<-c("Novel","Common")
barplot(threop,col=c("blue","orange","yellow") ,main="Percent of Time in Zone",ylab="Percent of Novel and Common Time Means",xlab="Time Comparison on Novel vs. Common", ylim=c(0,65),
        legend.text  = row.names(threop), args.legend = list(x = "topright", bty = "n", inset=c(-0.1,-.1)),           fill = c("blue","orange","yellow")  , beside=TRUE)

#legend("topright", colnames(mydata), fill = colors, bty = "n")
#legend("Topright",legend = c("Sedentary","Continous","Interval"),
 #      fill = c("blue","orange","yellow")) 
#anova1b
B1<-data.frame(modjensen$`Novel Object time in zone (seconds)`,modjensen$`Common Object time in zone`,modjensen$Type)
your.aov<-aov(Type~`Novel Object time in zone (seconds)`+`Common Object time in zone`+`Common Object time in zone`:`Novel Object time in zone (seconds)`, data=modjensen)
aov(Type~`Novel Object time in zone (seconds)`,data=modjensen)
              Anova(type~H+I:H)
summary(your.aov)
your.aov<-aov(Type~`Novel Object time in zone (seconds)`,data=modjensen)
summary(your.aov)
#Figure 2A

C1<-aggregate(modjensen$`common1 zone contact (proximal)`,by=list(Type=modjensen$Type),FUN=sum)
C2<-aggregate(modjensen$`common2 zone contact (distal)`,by=list(Type=modjensen$Type),FUN=sum)
CC12<-cbind.data.frame(C1,C2)
colnames(CC12)<-c("Type","CZ1","Type","CZ2")
CC12[,-3]
rowSums(CC12[,2:3])
Ct<-(302+357)
Int<-(368+371)
Sed<-(515+616)
FCC12<-c(Ct,Int,Sed)
barplot(FCC12,col=c("blue","orange","yellow"), main="Total Interactions By Group",ylab="Total Interactions",xlab="Type")
axis(
  1, at=1:3,labels = list("Sedentary", "Continous", "Interval"),
  ,las=1
)
#Figure 2B
percentnovelcontact<-100*(means_jensen$`Novel Object # contact`/(means_jensen$`Novel Object # contact`+ means_jensen$`Common object # contact`))
percentcommoncontact<-100*(means_jensen$`Common object # contact`/(means_jensen$`Novel Object # contact`+ means_jensen$`Common object # contact`))
threop1<-c(43.90909,52.82051,54.25532)
threop2<-c(56.09091,47.17949,45.74468)

threop<-cbind(threop1,threop2)
row.names(threop)<-c("Sed","Cont","Int")
colnames(threop)<-c("Novel","Common")
barplot(threop,col=c("blue","orange","yellow") ,main="Percent of Contact in Zone",ylab="Percent of Mean",xlab="Contact Comparison", 
        legend.text  = row.names(threop), args.legend = list(x = "topright", bty = "n", inset=c(-0.1,-.1)), fill = c("blue","orange","yellow"),beside=TRUE)


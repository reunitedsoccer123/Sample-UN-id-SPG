

library(ReporteRs)
library(magrittr)
library(plyr)
library(reshape2)
library(calibrate)
library(ggplot2)
library(knitr)
library(rmarkdown)
library(gtools)
library(tableone)
attach(jkl_n186_new)
#jkl_n186_new_new
#ache Scores


x<- preproc1_ache
y<- postproc1_ache
ache_change1_1<- y - x
achedata2<-postproc1_ache<-ifelse(postproc1_ache == ".",NA,ache_change1_1)
print(achedata2)


z<- preproc2_ache
w<- postproc2_ache
ache_change2_2<- w - z
achedata3<-jkl_n186_new$postproc2_ache<-ifelse(postproc2_ache == ".",NA,ache_change2_2)
print(achedata3)

x1<- preproc3_ache
y2<- postproc3_ache
ache_change3_3<- y2 - x1
achedata4<-jkl_n186_new$postproc3_ache<-ifelse(postproc3_ache == ".",NA,ache_change3_3)
print(achedata4)

#Divide by achment

record_id<-record_id
proc1_dr<-proc1_dr
indication1<-indication1
gipo_strength1<-gipo_strength1
gipo_strength1_other<-gipo_strength1_other
hupi_strength1<-hupi_strength1
dose1 <- dose1
proc2_dr <- proc2_dr
indication2<- indication2
gipo_strength2<- gipo_strength2
hupi_strength2<- hupi_strength2
dose2 <- dose2
proc3_dr<-proc3_dr
indication3<- indication3
gipo_strength3<-gipo_strength3
gipo_strength3_other<-gipo_strength3_other
hupi_strength3<- hupi_strength3
dose3<- dose3
jkl_dataBF <-data.frame(record_id, proc1_dr, indication1, gipo_strength1, gipo_strength1_other, hupi_strength1, dose1, preproc1_ache, postproc1_ache, achedata2, proc2_dr, indication2, gipo_strength2, hupi_strength2, dose2, preproc2_ache, postproc2_ache, achedata3, proc3_dr, indication3, gipo_strength3, gipo_strength3_other, hupi_strength3, dose3, preproc3_ache, postproc3_ache, achedata4)
write.table(jkl_dataBF, file = "jklBF.csv", sep = ",", row.names =FALSE)
#zeros list
length11<-list11
list1<-ifelse(preproc1_ache==0,1,NA)
list1<-na.omit(list1)
length1<-list1
list2<-ifelse(preproc2_ache==0,1,NA)
list2<-na.omit(list2)
length2<-list2
list3<-ifelse(preproc3_ache==0,1,NA)
list3<-na.omit(list3)
length3<-list3
#9 in T1 2 in T2 in T3

#achescore 1

achedata2<-na.omit(achedata2)
n1<-length(achedata2)
sdache1<-round(sd(achedata2, na.rm = TRUE),digits = 1)
minache1<-min(achedata2, na.rm = TRUE)
maxache1<-max(achedata2, na.rm = TRUE)
medache1<-median(achedata2, na.rm = TRUE)
meanache1<-round(mean(achedata2, na.rm = TRUE) , digits =1)
firstqu.ache1<-round(quantile(achedata2, c(.25), na.rm = TRUE) , digits =1)
thirdqu.ache1<-round(quantile(achedata2, c(.75), na.rm = TRUE) , digits =1)



achment1<-data.frame(n1,minache1,firstqu.ache1,medache1,meanache1,sdache1,thirdqu.ache1,maxache1)
rownames(achment1)<- c("achment 1")
print(achment1)
write.table(achment1, file = "achment1.csv", sep = ",", row.names =FALSE, dec = ".")
preproc1<-jitter(jkl_dataBF$preproc1_ache, factor = 1, amount =NULL)
plot(preproc1,jkl_dataBF$achedata2, main="achment 1 Change in ache (N=186)", xlab="Pre-procedure ache",ylab="Difference in ache Score", xlim=c(0,10),ylim=c(-15,10))
textxy(9.5,-8,"min", m=c(0,0), cex=.5, offset =.5)
textxy(3.5,6,'max', m=c(0,0), cex=.5, offset =.5)

#achescore 2
achedata3<-na.omit(achedata3)
n2<-length(achedata3)
sdache2<-round(sd(achedata3, na.rm = TRUE) , digits =1)
minache2<-min(achedata3, na.rm = TRUE)
maxache2<-max(achedata3, na.rm = TRUE)
medache2<-median(achedata3, na.rm = TRUE)
meanache2<-round(mean(achedata3, na.rm = TRUE) , digits=1)
firstqu.ache2<-round(quantile(achedata3, c(.25), na.rm = TRUE) , digits =1)
thirdqu.ache2<-round(quantile(achedata3, c(.75), na.rm = TRUE) , digits =1)

achment2<-data.frame(n2,minache2,firstqu.ache2,medache2,meanache2,sdache2,thirdqu.ache2,maxache2)
rownames(achment2)<- c("achment 2")
print(achment2)
write.table(achment2, file = "achment2.csv", sep = ",", row.names =FALSE, dec = ".")
preproc2<-jitter(jkl_dataBF$preproc2_ache, factor = 1, amount =NULL)
plot(preproc2,jkl_dataBF$achedata3, main="achment 2 Change in ache (N=53)", xlab="Pre-procedure score",ylab="Differenced in ache Change",ylim= c(-15,10),xlim= c(0,10))
textxy(9,-9,"min", m=c(0,-8), cex=.5, offset =.5)
textxy(1.5,3,"max", m=c(0,0), cex=.5, offset =.5)
plot(preproc2,jkl_dataBF$achedata3, col=ifelse(postproc1_ache>5,"Red","Green"),main="achment 2 Change in ache (N=53)
     red = post achment1 score >5, 
     green= post achment1 score <= 5", xlab="Pre-Procedure Score",ylab="Difference in ache Score", ylim=c(-15,10), xlim=c(0,10))
plot(preproc2,jkl_dataBF$achedata3, col=ifelse(ache_change1_1>=0,"Red","Green"),main="achment 2 Change in ache (N=53)
     red = ache increase on achment1, 
     green= ache unchanged or decreased on achment 1", xlab="Pre-Procedure ache",ylab="Difference in ache Score", xlim=c(0,10), ylim=c(-15,10))



#acheScore 3
achedata4<-na.omit(achedata4)
n3<-length(achedata4)
sdache3<-round(sd(achedata4, na.rm = TRUE) , digits =1)
minache3<-min(achedata4, na.rm = TRUE)
maxache3<-max(achedata4, na.rm = TRUE)
medache3<-median(achedata4, na.rm = TRUE)
meanache3<-round(mean(achedata4, na.rm = TRUE) , digits =1 )
firstqu.ache3<-round(quantile(achedata4, c(.25), na.rm = TRUE) , digits =1)
thirdqu.ache3<-round(quantile(achedata4, c(.75), na.rm = TRUE) , digits =1)

achment3<-data.frame(n3,minache3,firstqu.ache3,medache3,meanache3,sdache3,thirdqu.ache3,maxache3)
rownames(achment3)<- c("achment 3")
print(achment3)
write.table(achment3, file = "achment3.csv", sep = ",", row.names =FALSE, dec = ".")
preproc3<-jitter(jkl_dataBF$preproc3_ache, factor = 1, amount =NULL)
plot(preproc3,jkl_dataBF$achedata4, main="achment 3 Change in ache (N=25)", xlab="Pre-procedure ache",ylab="Difference in ache Score", ylim=c(-15,10),xlim=c(0,10))
textxy(8.7,-8,"min", m=c(0,-8), cex=.5, offset =.5)
textxy(3.7,3,"max", m=c(0,0), cex=.5, offset =.5)
plot(preproc3,jkl_dataBF$achedata4, col=ifelse(jkl_dataBF$postproc2_ache>=5,"Red","Green"),main="achment 3 Change in ache (N=25)
     red = post achment2 score >5, 
     green= post achment2 score <= 5", xlab="Pre-Procedure ache",ylab="Difference in ache Score",xlim=c(0,10), ylim=c(-15,10))
plot(preproc3,jkl_dataBF$achedata4, col=ifelse(ache_change2_2>=0,"Red","Green"),main="achment 3 Change in ache (N=25)
     red = ache increase on achment2, 
     green= ache unchanged or decreased on achment 2", xlab="Pre-Procedure Score",ylab="Difference in ache Score", xlim=c(0,10),ylim=c(-15,10))

#Table for all achments
allach = rbind.fill(achment1,achment2, achment3)
colnames(allach)<-c("N","Minimum", "percentile25th"," Median","Mean", "SD", "percentile75th", "Maximum")
write.table(allach, file = "All achments.csv", sep = ",", row.names =FALSE, dec = ".")



#dose calculations
#dose cal for achment1
l0<-replace(gipo_strength1, gipo_strength1==(4.00), .005)
l1<-replace(l0, l0==(2.00), .04)
l2<-replace(l1, l1==(5), .02)
l11<-replace(l2, l2==(1), .01)
col1<-l12<-replace(l11, l11==(3), .03)
ifelse(l12 ==".",NA,l12)
l12=na.omit(l12)
dose1=na.omit(dose1)
l12 = as.numeric(as.character(l12))
dose1 = as.numeric(as.character(dose1))
lidestrength_dose1<-l12*dose1
#Forgipohupiscript
LL<-ifelse(l12 == .04,NA,l12)
#achment 1 plot
par(mar=c(4.1,4.1,10,0.5))
plot(preproc1,jkl_dataBF$achedata2, col=ifelse(is.na(col1),"black", ifelse(col1 == .04,"red","green")),main="achment 1 Change in ache (N=225)
     red = gipocaine at 4%, 
     green= All other gipocaine Strengths
     black= hupivacine", xlab="Pre-Procedure Score",ylab="Difference in ache Score", ylim=c(-15,10), xlim=c(0,10))

B1<-replace(hupi_strength1, hupi_strength1==(1.00), .005)
B2<-replace(B1, B1==(2.00), .01)
B3<-replace(B2, B2==(3), .02)
ifelse(B3 ==".",NA,B3)
B3=na.omit(B3)
dose1=na.omit(dose1)
B3 = as.numeric(as.character(B3))
hupistrength_dose1<-B3*dose1
plot(preproc1,jkl_dataBF$achedata2, col=ifelse(B3==.02,"Red","Green"),main="achment 1 Change in ache (N=225)
     red = hupivacaine at 2%, 
     green= All other hupivacaine Strengths", xlab="Pre-Procedure Score",ylab="Difference in ache Score", ylim=c(-15,10), xlim=c(0,10))
strengthdose_1table<-data.frame(lidestrength_dose1,hupistrength_dose1)
strengthdose_1table=na.omit(strengthdose_1table)
SL1mean = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=mean )[,-1]
SL1min = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=min )[,-1]
SL1q25 = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=quantile, probs=0.25 )[,-1]
SL1med = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=median )[,-1]
SL1sd = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=sd )[,-1]
SL1q75 =aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=quantile, probs=0.75 )[,-1]
SL1max = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=max )[,-1]
SL1count = count(strengthdose_1table$lidestrength_dose1)[,-1]
gipoSTR1= data.frame(
  lidestrength_dose1 = 1:20 ,
  N1 = SL1count ,
  mean = SL1mean[,2],
  min = SL1min[,2] ,
  quantile.25 = SL1q25[,2] ,
  median = SL1med[,2] ,
  standard_deviation = SL1sd[,2] ,
  quantile.75 = SL1q75[,2] ,
  max = SL1max [,2] 
  
)
write.table(gipoSTR1, file = "gipo1.csv", sep = ",", row.names =FALSE, dec = ".")

BU1mean = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$hupistrength_dose1), FUN=mean )[,-1]
BU1min = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$hupistrength_dose1), FUN=min )[,-1]
BU1q25 = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$hupistrength_dose1), FUN=quantile, probs=0.25 )[,-1]
BU1med = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$hupistrength_dose1), FUN=median )[,-1]
BU1sd = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$hupistrength_dose1), FUN=sd )[,-1]
BU1q75 =aggregate( x=strengthdose_1table, by=list(strengthdose_1table$hupistrength_dose1), FUN=quantile, probs=0.75 )[,-1]
BU1max = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$hupistrength_dose1), FUN=max )[,-1]
BU1count = count(strengthdose_1table$hupistrength_dose1)[,-1]
hupiSTR1= data.frame(
  hupistrength_dose1 = 1:14,
  N1 = BU1count ,
  mean = BU1mean[,2],
  min = BU1min[,2] ,
  quantile.25 = BU1q25[,2] ,
  median = BU1med[,2] ,
  standard_deviation = BU1sd[,2] ,
  quantile.75 = BU1q75[,2] ,
  max = BU1max [,2] 
  
)
write.table(hupiSTR1, file = "hupi1.csv", sep = ",", row.names =FALSE, dec = ".")
dosecalach1<-smartbind(gipoSTR1,hupiSTR1)
write.table(dosecalach1, file = "SBAnalysis1.csv", sep = ",", row.names =FALSE, dec = ".")

#dose cal for achment2
l13<-replace(gipo_strength2, gipo_strength2==(4.00), .005)
l14<-replace(l13, l13==(2.00), .04)
l15<-replace(l14, l14==(5), .02)
l16<-replace(l15, l15==(1), .01)
col2<-l17<-replace(l16, l16==(3), .03)
ifelse(l17 ==".",NA,l17)
l17=na.omit(l17)
dose2=na.omit(dose2)
l17 = as.numeric(as.character(l17))
dose2 = as.numeric(as.character(dose2))
lidestrength_dose2<-l17*dose2
#For gipo hupi script
SS<-ifelse(l17 == .04,NA,l17)

#achment 2 plot
par(mar=c(4.1,4.1,10,0.5))
plot(preproc2,jkl_dataBF$achedata3, col=ifelse(is.na(col2),"black", ifelse(col2 == .04,"red","green")),main="achment 2 Change in ache (N=70)
     red = gipocaine at 4%, 
     green= All other gipocaine Strengths
     black= hupivacine", xlab="Pre-Procedure Score",ylab="Difference in ache Score", ylim=c(-15,10), xlim=c(0,10))

B4<-replace(hupi_strength1, hupi_strength1==(1.00), .005)
B5<-replace(B4, B4==(2.00), .01)
B6<-replace(B5, B5==(3), .02)
ifelse(B6 ==".",NA,B6)
B6=na.omit(B6)
dose2=na.omit(dose2)
B6 = as.numeric(as.character(B6))
hupistrength_dose2<-B6*dose2

plot(preproc2,jkl_dataBF$achedata3, col=ifelse(B6==.02,"Red","Green"),main="achment 2 Change in ache (N=70)
     red = hupivacaine at 2%, 
     green= All other hupivacaine Strengths", xlab="Pre-Procedure Score",ylab="Difference in ache Score", ylim=c(-15,10), xlim=c(0,10))


strengthdose_2table<-data.frame(lidestrength_dose2,hupistrength_dose2)
strengthdose_2table=na.omit(strengthdose_2table)
SL2mean = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=mean )[,-1]
SL2min = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=min )[,-1]
SL2q25 = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=quantile, probs=0.25 )[,-1]
SL2med = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=median )[,-1]
SL2sd = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=sd )[,-1]
SL2q75 =aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=quantile, probs=0.75 )[,-1]
SL2max = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=max )[,-1]
SL2count = count(strengthdose_2table$lidestrength_dose2)[,-1]
gipoSTR2= data.frame(
  lidestrength_dose2 = 1:14 ,
  N1 = SL2count ,
  mean = SL2mean[,2],
  min = SL2min[,2] ,
  quantile.25 = SL2q25[,2] ,
  median = SL2med[,2] ,
  standard_deviation = SL2sd[,2] ,
  quantile.75 = SL2q75[,2] ,
  max = SL2max [,2] 
  
)
write.table(gipoSTR2, file = "gipo2.csv", sep = ",", row.names =FALSE, dec = ".")


BU2mean = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$hupistrength_dose2), FUN=mean )[,-1]
BU2min = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$hupistrength_dose2), FUN=min )[,-1]
BU2q25 = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$hupistrength_dose2), FUN=quantile, probs=0.25 )[,-1]
BU2med = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$hupistrength_dose2), FUN=median )[,-1]
BU2sd = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$hupistrength_dose2), FUN=sd )[,-1]
BU2q75 =aggregate( x=strengthdose_2table, by=list(strengthdose_2table$hupistrength_dose2), FUN=quantile, probs=0.75 )[,-1]
BU2max = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$hupistrength_dose2), FUN=max )[,-1]
BU2count = count(strengthdose_2table$hupistrength_dose2)[,-1]
hupiSTR2= data.frame(
  hupistrength_dose2 = 1:8 ,
  N1 = BU2count ,
  mean = BU2mean[,2],
  min = BU2min[,2] ,
  quantile.25 = BU2q25[,2] ,
  median = BU2med[,2] ,
  standard_deviation = BU2sd[,2] ,
  quantile.75 = BU2q75[,2] ,
  max = BU2max [,2] 
  
)
write.table(hupiSTR2, file = "hupi2.csv", sep = ",", row.names =FALSE, dec = ".")


dosecalach2<-smartbind(gipoSTR2,hupiSTR2)
write.table(dosecalach2, file = "SBAnalysis2.csv", sep = ",", row.names =FALSE, dec = ".")

#dose calculations
#dose cal for achment3
l3<-replace(gipo_strength3, gipo_strength3==(4.00), .005)
l4<-replace(l3, l3==(2.00), .04)
l5<-replace(l4, l4==(5), .02)
l6<-replace(l5, l5==(1), .01)
col3<-l7<-replace(l6, l6==(3), .03)
ifelse(l7 ==".",NA,l7)
l7=na.omit(l7)
dose3=na.omit(dose3)
l7 = as.numeric(as.character(l7))
dose3 = as.numeric(as.character(dose3))
lidestrength_dose3<-l7*dose3
#For gipo hupi script
WW<-ifelse(l7 == .04,NA,l7)

#achment 3 plot
par(mar=c(4.1,4.1,10,0.5))
plot(preproc3_ache,jkl_dataBF$achedata4, col=ifelse(is.na(col3),"black", ifelse(col3 == .04,"red","green")),main="achment 3 Change in ache (N=37)
     red = gipocaine at 4%, 
     green= All other gipocaine Strengths
     black= hupivacine", xlab="Pre-Procedure Score",ylab="Difference in ache Score", ylim=c(-15,10), xlim=c(0,10))

B7<-replace(hupi_strength3, hupi_strength3==(1.00), .005)
B8<-replace(B7, B7==(2.00), .01)
B9<-replace(B8, B8==(3), .02)
ifelse(B9 ==".",NA,B9)
B9=na.omit(B9)
dose3=na.omit(dose3)
B9 = as.numeric(as.character(B9))
hupistrength_dose3<-B9*dose3

plot(preproc3_ache,jkl_dataBF$achedata4, col=ifelse(B9==.02,"Red","Green"),main="achment 3 Change in ache (N=37)
     red =hupivacaine at 2%, 
     green= All other hupivacaine Strengths", xlab="Pre-Procedure Score",ylab="Difference in ache Score", ylim=c(-15,10), xlim=c(0,10))


strengthdose_3table<-data.frame(lidestrength_dose3,hupistrength_dose3)
strengthdose_3table=na.omit(strengthdose_3table)
SL3mean = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=mean )[,-1]
SL3min = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=min )[,-1]
SL3q25 = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=quantile, probs=0.25 )[,-1]
SL3med = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=median )[,-1]
SL3sd = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=sd )[,-1]
SL3q75 =aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=quantile, probs=0.75 )[,-1]
SL3max = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=max )[,-1]
SL3count = count(strengthdose_3table$lidestrength_dose3)[,-1]
gipoSTR3= data.frame(
  lidestrength_dose3 = 1:9 ,
  N1 = SL3count ,
  mean = SL3mean[,2],
  min = SL3min[,2] ,
  quantile.25 = SL3q25[,2] ,
  median = SL3med[,2] ,
  standard_deviation = SL3sd[,2] ,
  quantile.75 = SL3q75[,2] ,
  max = SL3max [,2] 
  
)
write.table(gipoSTR3, file = "gipo3.csv", sep = ",", row.names =FALSE, dec = ".")


BU3mean = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$hupistrength_dose3), FUN=mean )[,-1]
BU3min = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$hupistrength_dose3), FUN=min )[,-1]
BU3q25 = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$hupistrength_dose3), FUN=quantile, probs=0.25 )[,-1]
BU3med = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$hupistrength_dose3), FUN=median )[,-1]
BU3sd = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$hupistrength_dose3), FUN=sd )[,-1]
BU3q75 =aggregate( x=strengthdose_3table, by=list(strengthdose_3table$hupistrength_dose3), FUN=quantile, probs=0.75 )[,-1]
BU3max = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$hupistrength_dose3), FUN=max )[,-1]
BU3count = count(strengthdose_3table$hupistrength_dose3)[,-1]
hupiSTR3= data.frame(
  hupistrength_dose3 = 1:4 ,
  N1 = BU3count ,
  mean = BU3mean[,2],
  min = BU3min[,2] ,
  quantile.25 = BU3q25[,2] ,
  median = BU3med[,2] ,
  standard_deviation = BU3sd[,2] ,
  quantile.75 = BU3q75[,2] ,
  max = BU3max [,2] 
  
)
write.table(hupiSTR3, file = "hupi3.csv", sep = ",", row.names =FALSE, dec = ".")


dosecalach3<-smartbind(gipoSTR3,hupiSTR3)
write.table(dosecalach3, file = "SBAnalysis3.csv", sep = ",", row.names =FALSE, dec = ".")

#Divide my Doctor

ach1 = subset(jkl_dataBF, select = c(proc1_dr, achedata2))
ach1 = na.omit(ach1)

t1mean = aggregate( x=ach1, by=list(ach1$proc1_dr), FUN=mean )[,-1]
t1min = aggregate( x=ach1, by=list(ach1$proc1_dr), FUN=min )[,-1]
t1q25 = aggregate( x=ach1, by=list(ach1$proc1_dr), FUN=quantile, probs=0.25 )[,-1]
t1med = aggregate( x=ach1, by=list(ach1$proc1_dr), FUN=median )[,-1]
t1sd = aggregate( x=ach1, by=list(ach1$proc1_dr), FUN=sd )[,-1]
t1q75 =aggregate( x=ach1, by=list(ach1$proc1_dr), FUN=quantile, probs=0.75 )[,-1]
t1max = aggregate( x=ach1, by=list(ach1$proc1_dr), FUN=max )[,-1]
t1count = count(ach1$proc1_dr)[,-1]



Change_in_ache_By_Dr_ach1 = data.frame(
  proc1_dr = 1:6 ,
  N1 = t1count ,
  mean = t1mean[,2],
  min = t1min[,2] ,
  quantile.25 = t1q25[,2] ,
  median = t1med[,2] ,
  standard_deviation = t1sd[,2] ,
  quantile.75 = t1q75[,2] ,
  max = t1max [,2] 
  
)

print(Change_in_ache_By_Dr_ach1)
write.table(Change_in_ache_By_Dr_ach1, file = "CPDr1.csv", sep = ",", row.names =FALSE, dec = ".")



ach2 = subset(jkl_dataBF, select = c(proc2_dr, achedata3))
ach2 = na.omit(ach2)
ach2$proc2_dr = as.numeric(as.character(ach2$proc2_dr))
print(ach2)
t2mean = aggregate( x=ach2, by=list(ach2$proc2_dr), FUN=mean)[,-1]
t2min = aggregate( x=ach2, by=list(ach2$proc2_dr), FUN=min )[,-1]
t2q25 = aggregate( x=ach2, by=list(ach2$proc2_dr), FUN=quantile, probs=0.25 )[,-1]
t2med = aggregate( x=ach2, by=list(ach2$proc2_dr), FUN=median )[,-1]
t2sd = aggregate( x=ach2, by=list(ach2$proc2_dr), FUN=sd )[,-1]
t2q75 =aggregate( x=ach2, by=list(ach2$proc2_dr), FUN=quantile, probs=0.75 )[,-1]
t2max = aggregate( x=ach2, by=list(ach2$proc2_dr), FUN=max )[,-1]
t2count = count(ach2$proc2_dr)[,-1]

Change_in_ache_By_Dr_ach2= data.frame(
  proc2_dr = 1:6 ,
  N2 = t2count ,
  mean = t2mean[,2],
  min = t2min[,2] ,
  quantile.25 = t2q25[,2] ,
  median = t2med[,2] ,
  standard_deviation = t2sd[,2] ,
  quantile.75 = t2q75[,2] ,
  max = t2max [,2] 
  
)
print(Change_in_ache_By_Dr_ach2)
write.table(Change_in_ache_By_Dr_ach2, file = "CPDr2.csv", sep = ",", row.names =FALSE, dec = ".")



ach3 = subset(jkl_dataBF, select = c(proc3_dr, achedata4))
ach3 = na.omit(ach3)
ach3$proc3_dr = as.numeric(as.character(ach3$proc3_dr))
print(ach3)
t3mean = aggregate( x=ach3, by=list(ach3$proc3_dr), FUN=mean)[,-1]
t3min = aggregate( x=ach3, by=list(ach3$proc3_dr), FUN=min )[,-1]
t3q25 = aggregate( x=ach3, by=list(ach3$proc3_dr), FUN=quantile, probs=0.25 )[,-1]
t3med = aggregate( x=ach3, by=list(ach3$proc3_dr), FUN=median )[,-1]
t3sd = aggregate( x=ach3, by=list(ach3$proc3_dr), FUN=sd )[,-1]
t3q75 =aggregate(x=ach3, by=list(ach3$proc3_dr), FUN=quantile, probs=0.75 )[,-1]
t3max = aggregate(x=ach3, by=list(ach3$proc3_dr), FUN=max )[,-1]
t3count = count(ach3$proc3_dr)[,-1]

Change_in_ache_By_Dr_ach3 = data.frame(
  proc1_dr = 1:5 ,
  N3 = t3count ,
  mean = t3mean[,2],
  min = t3min[,2] ,
  quantile.25 = t3q25[,2] ,
  median = t3med[,2] ,
  standard_deviation = t3sd[,2] ,
  quantile.75 = t3q75[,2] ,
  max = t3max [,2] 
)

print(Change_in_ache_By_Dr_ach3)
write.table(Change_in_ache_By_Dr_ach3, file = "CPDr3.csv", sep = ",", row.names =FALSE, dec = ".")

#Procedure Date Data
jklwide <- read_sas("S:/Biostats/BIO-STAT/Radiology_Hill/jklblockade_DeBacker/Data/jklwide.sas7bdat", 
                    NULL)
jkldatetable<-data.frame(record_id,proc1_date,proc2_date,proc3_date)
jkl_dataBF1_2<-jkl_dataBF[,-1]
jkldateBF<-cbind(jklwide, jkl_dataBF1_2)
write.table(jkldateBF, file = "jkldateBF.csv", sep = ",", row.names =FALSE, dec = ".")
jkldateBF$proc2_date <- as.POSIXct(jkl_n186_new$proc2_date,
                                   format='%m/%d/%Y')
jkldateBF$proc3_date <- as.POSIXct(jkl_n186_new$proc3_date,
                                   format='%m/%d/%Y')
diffs2 <- difftime(jkl_n186_new$proc3_date,jkl_n186_new$proc2_date ,units="days")
diffs2 = as.numeric(as.difftime(diffs2))
diffs2<-abs(diffs2)
Dateproc2<-jitter(diffs2, factor = 1, amount =NULL)
plot(Dateproc2,jkl_dataBF$achedata4, col=ifelse(jkl_dataBF$postproc2_ache>5,"Red","black"),main="Comparison of time between 
     achments and ache Change
     red= post achment2 score >5", 
     red = "post achment_2 score >5", xlab="Days from achment2 to achment 3",ylab="ache Change for achment3",ylim=c(-10,10))

jklwide <- read_sas("S:/Biostats/BIO-STAT/Radiology_Hill/jklblockade_DeBacker/Data/jklwide.sas7bdat", 
                    NULL)
jkldatetable<-data.frame(record_id,proc1_date,proc2_date,proc3_date)
jkl_dataBF1_2<-jkl_dataBF[,-1]
jkldateBF<-cbind(jklwide, jkl_dataBF1_2)
write.table(jkldateBF, file = "jkldateBF.csv", sep = ",", row.names =FALSE, dec = ".")
jkldateBF$proc1_date <- as.POSIXct(jkl_n186_new$proc1_date,
                                   format='%m/%d/%Y')
jkldateBF$proc2_date <- as.POSIXct(jkl_n186_new$proc2_date,
                                   format='%m/%d/%Y')
diffs2 <- difftime(jkl_n186_new$proc2_date,jkl_n186_new$proc1_date ,units="days")
diffs2 = as.numeric(as.difftime(diffs2))
diffs2<-abs(diffs2)
Dateproc2<-jitter(diffs2, factor = 1, amount =NULL)
plot(Dateproc2,jkl_dataBF$achedata4, col=ifelse(jkl_dataBF$postproc2_ache>5,"Red","black"),main="Comparison of time between 
     achments and ache Change
     red= post achment1 score >5", 
     red = "post achment_2 score >5", xlab="Days from achment2 to achment 1",ylab="ache Change for achment2",ylim=c(-10,10))

library(readxl)
jkl_wide <- read_excel("S:/Biostats/BIO-STAT/Radiology_Hill/jklblockade_DeBacker/Data/jkl_wide.xlsx")
attach(jkl_wide)
jkl_wide<-data.frame(jkl_wide)
jkl_wide$proc_date_2 <- as.POSIXct(jkl_wide$proc_date_2,
                                   format='%m/%d/%Y')
jkl_wide$proc_date_1 <- as.POSIXct(jkl_wide$proc_date_1,
                                   format='%m/%d/%Y')
jkl_wide<-data.frame(jkl_wide)

diffs2 <- difftime(proc_date_2,proc_date_1 ,units="days")
diffs2 = as.numeric(as.difftime(diffs2))
diffs2<-abs(diffs2)
Dateproc2<-jitter(diffs2, factor = 1, amount =NULL)
plot(Dateproc2,jkl_dataBF$achedata2, col=ifelse(postproc1_score>5,"Red","black"),main="Comparison of time between 
     achments and ache Change
     red= post achment1 score >5", 
     red= "post achment1 score >5",
     xlab="Days from achment1 to achment 2",ylab="ache Change for achment3",ylim=c(-10,10))

ab<-c(.005,.02,.01,.03)
l7 %in% ab
gipoother3<-l7 [! l7 %in% ab]

ab<-c(.005,.02,.01,.03)
l17 %in% ab
gipoother2<-l17 [! l17 %in% ab]

ab<-c(.005,.02,.01,.03)
l12 %in% ab
gipoother1<-l12 [! l12 %in% ab]

gipo_04<-c(gipoother1,gipoother2,gipoother3)

WW<-ifelse(gipo_strength3 == .04,NA,gipo_strength3)
WW<-na.omit(WW)
wW2<-ifelse(gipo_strength3 ==.03,NA,gipo_strength3)
#exclusion gipo
#other
ab<-c(.04)
l7 %in% ab
gipoother4<-l7 [! l7 %in% ab]

ab<-c(.04)
l17 %in% ab
gipoother5<-l17 [! l17 %in% ab]

ab<-c(.04)
l12 %in% ab
gipoother6<-l12 [! l12 %in% ab]

gipo_otherex<-c(gipoother4,gipoother5,gipoother6)
gipo_otherex<-na.omit(gipo_otherex)
xx<-gipo_strength1
yy<-gipo_strength2
zz<-gipo_strength3
gipototal<-c(gipo_strength1,gipo_strength2,gipo_strength3)
gipototal<-ifelse(gipototal == " ",NA,gipototal)
gipototal<-na.omit(gipototal)
dosetotal<-c(dose1,dose2,dose3)
dosetotal<-ifelse(dosetotal == " ",NA,dosetotal)
ld<-data.frame(gipototal,dosetotal)             
library(gtools)

#gipostrengthtotal2<-ifelse(factor(gipo) == .04,NA,gipostrengthtotal)
#gipostrengthtotal2<-na.omit(gipostrengthtotal2)
#gipo at other
#gipo_other<-c(LL,SS,WW)
gipo_otherdosetot<-gipo_otherex*dosetotal
gipo_otherdosetot<-ifelse(gipo_otherex== " ",NA,gipo_otherdosetot)
gipo_otherdosetot<-na.omit(gipo_otherdosetot)
n_other<-length(gipo_otherdosetot)
sdache_other<-round(sd(gipo_otherdosetot, na.rm = TRUE) , digits =3)
minache_other<-min(gipo_otherdosetot, na.rm = TRUE)
maxache_other<-max(gipo_otherdosetot, na.rm = TRUE)
medache_other<-median(gipo_otherdosetot, na.rm = TRUE)
meanache_other<-round(mean(gipo_otherdosetot, na.rm = TRUE) , digits =3 )
firstqu.ache_other<-round(quantile(gipo_otherdosetot, c(.25), na.rm = TRUE) , digits =3)
thirdqu.ache_other<-round(quantile(gipo_otherdosetot, c(.75), na.rm = TRUE) , digits =3)

achment_other<-data.frame(n_other,minache_other,firstqu.ache_other,medache_other,meanache_other,sdache_other,thirdqu.ache_other,maxache_other)
rownames(achment_other)<- c("gipocaine other")
#gipo 4%
gipo_04dosetot<-gipo_04*dosetotal
gipo_04dosetot<-ifelse(gipo_04== " ",NA,gipo_04dosetot)
gipo_04dosetot<-na.omit(gipo_04dosetot)
n_04<-length(gipo_04dosetot)
sdache_04<-round(sd(gipo_04dosetot, na.rm = TRUE) , digits =3)
minache_04<-min(gipo_04dosetot, na.rm = TRUE)
maxache_04<-max(gipo_04dosetot, na.rm = TRUE)
medache_04<-median(gipo_04dosetot, na.rm = TRUE)
meanache_04<-round(mean(gipo_04dosetot, na.rm = TRUE) , digits =3 )
firstqu.ache_04<-round(quantile(gipo_04dosetot, c(.25), na.rm = TRUE) , digits =3)
thirdqu.ache_04<-round(quantile(gipo_04dosetot, c(.75), na.rm = TRUE) , digits =3)

achment_04<-data.frame(n_04,minache_04,firstqu.ache_04,medache_04,meanache_04,sdache_04,thirdqu.ache_04,maxache_04)
rownames(achment_04)<- c("gipocaine 04")
#gipototal
gipostrengthtotal<-c(gipo_04dosetot,gipo_otherdosetot)
gipostrengthtotal<-na.omit(gipostrengthtotal)
n_gipo<-length(gipostrengthtotal)
sdache_gipo<-round(sd(gipostrengthtotal, na.rm = TRUE) , digits =3)
minache_gipo<-min(gipostrengthtotal, na.rm = TRUE)
maxache_gipo<-max(gipostrengthtotal, na.rm = TRUE)
medache_gipo<-median(gipostrengthtotal, na.rm = TRUE)
meanache_gipo<-round(mean(gipostrengthtotal, na.rm = TRUE) , digits =3 )
firstqu.ache_gipo<-round(quantile(gipostrengthtotal, c(.25), na.rm = TRUE) , digits =3)
thirdqu.ache_gipo<-round(quantile(gipostrengthtotal, c(.75), na.rm = TRUE) , digits =3)

achment_gipo2<-data.frame(n_gipo,minache_gipo,firstqu.ache_gipo,medache_gipo,meanache_gipo,sdache_gipo,thirdqu.ache_gipo,maxache_gipo)
rownames(achment_gipo2)<- c("gipocaine Total")

#hupitotal
hupitotal<-c(B3,B6,B9)
n_hupi<-length(hupitotal)
sdache_hupi<-round(sd(hupitotal, na.rm = TRUE) , digits =3)
minache_hupi<-min(hupitotal, na.rm = TRUE)
maxache_hupi<-max(hupitotal, na.rm = TRUE)
medache_hupi<-median(hupitotal, na.rm = TRUE)
meanache_hupi<-round(mean(hupitotal, na.rm = TRUE) , digits =3 )
firstqu.ache_hupi<-round(quantile(hupitotal, c(.25), na.rm = TRUE) , digits =3)
thirdqu.ache_hupi<-round(quantile(hupitotal, c(.75), na.rm = TRUE) , digits =3)

achment_hupi<-data.frame(n_hupi,minache_hupi,firstqu.ache_hupi,medache_hupi,meanache_hupi,sdache_hupi,thirdqu.ache_hupi,maxache_hupi)
rownames(achment_hupi)<- c("hupivacaine .5%")

achment_hupi2<-data.frame(n_hupi,minache_hupi,firstqu.ache_hupi,medache_hupi,meanache_hupi,sdache_hupi,thirdqu.ache_hupi,maxache_hupi)
rownames(achment_hupi2)<- c("hupivacaine Total")


dosetable<-smartbind(achment_04,achment_other,achment_hupi,achment_gipo2,achment_hupi2)
write.table(dosetable, file = "DosegipohupiTABLE.csv", sep = ",", row.names =FALSE)


#Count gipo and hupi table
gipo_strength1<-ifelse(gipo_strength1== " ",NA,gipo_strength1)
gipo_strength1<-na.omit(gipo_strength1)
gipo_strength2<-ifelse(gipo_strength2== " ",NA,gipo_strength2)
gipo_strength2<-na.omit(gipo_strength2)
gipo_strength3<-ifelse(gipo_strength3== " ",NA,gipo_strength3)
gipo_strength3<-na.omit(gipo_strength3)
Ld1<-length(gipo_strength1)
Ld2_3<-length(gipo_strength2)+length(gipo_strength3)

hupi_strength1
hupi_strength1<-ifelse(hupi_strength1== " ",NA,hupi_strength1)
hupi_strength1<-na.omit(hupi_strength1)
hupi_strength2<-ifelse(hupi_strength2== " ",NA,hupi_strength2)
hupi_strength2<-na.omit(hupi_strength2)
hupi_strength3<-ifelse(hupi_strength3== " ",NA,hupi_strength3)
hupi_strength3<-na.omit(hupi_strength3)

BUP1<-length(hupi_strength1)
BUP2_3<-length(hupi_strength2)+length(hupi_strength3)


xxx1<-length(which(gipo_strength1==.04&hupi_strength1==.05))
xxx2<-length(which(gipo_strength2==.04&hupi_strength2==.05))
xxx3<-length(which(gipo_strength3==.04&hupi_strength3==.05))

gipo_hupi_1<-xxx1
gipo_hupi23<-xxx2+xxx3

LB1=rbind(Ld1,gipo_hupi_1, BUP1)
LB23=rbind(Ld2_3,gipo_hupi23, BUP2_3)
gipo_hupitable=cbind(LB1,LB23)
rownames(gipo_hupitable)<- c("gipo Only", "gipo&hupi", "hupi Only")
colnames(gipo_hupitable)<- c("achment 1","achment 2&3")
write.table(gipo_hupitable, file = "gipo_hupiTable.csv", sep = ",", row.names =TRUE)


JK<-na.omit(ifelse(postproc2_ache>5,"T","F"))
jk1<-length(which(postproc2_ache>5))

#New Plots box and whiskers and ache change scatter plot
jklL <- read_sas("S:/Biostats/BIO-STAT/Radiology_Hill/jklblockade_DeBacker/Data/jkllong.sas7bdat", 
                 NULL)
jklW <- read_sas("S:/Biostats/BIO-STAT/Radiology_Hill/jklblockade_DeBacker/Data/jklwide.sas7bdat", 
                 NULL)
jklL2<- read_excel("S:/Biostats/BIO-STAT/Radiology_Hill/jklblockade_DeBacker/Data/jkl_long_2.xlsx")
jklLTX<-jklL2[!jklL2$Num_Tx=='1',]
plot(jklLTX$record_id,jklLTX$ache_change,
     main="TX2_3 ache_change"
     , xlab="Individuals",ylab="ache Change")


#Box and Whiskers
jklL3<-data.frame(jklL2$record_id,jklL2$ache_change,jklL2$Num_Tx)
boxplot(jklL2$ache_change~jklL2$Num_Tx, main="Box and Whiskers on Average ache Change
        1= At most Tx_1, 2=At most Tx_2 3=All 3 Tx", xlab="Treament Measure",ylab="Average ache Change", ylim=c(-10,10))




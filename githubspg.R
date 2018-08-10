
View(spg_n225_new)

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
attach(spg_n225_new)
#spg_n225_new_new
#Pain Scores


x<- preproc1_pain
y<- postproc1_pain
pain_change1_1<- y - x
paindata2<-postproc1_pain<-ifelse(postproc1_pain == ".",NA,pain_change1_1)
print(paindata2)


z<- preproc2_pain
w<- postproc2_pain
pain_change2_2<- w - z
paindata3<-spg_n225_new$postproc2_pain<-ifelse(postproc2_pain == ".",NA,pain_change2_2)
print(paindata3)

x1<- preproc3_pain
y2<- postproc3_pain
pain_change3_3<- y2 - x1
paindata4<-spg_n225_new$postproc3_pain<-ifelse(postproc3_pain == ".",NA,pain_change3_3)
print(paindata4)

#Divide by treatment

record_id<-record_id
proc1_dr<-proc1_dr
indication1<-indication1
lido_strength1<-lido_strength1
lido_strength1_other<-lido_strength1_other
bupi_strength1<-bupi_strength1
dose1 <- dose1
proc2_dr <- proc2_dr
indication2<- indication2
lido_strength2<- lido_strength2
bupi_strength2<- bupi_strength2
dose2 <- dose2
proc3_dr<-proc3_dr
indication3<- indication3
lido_strength3<-lido_strength3
lido_strength3_other<-lido_strength3_other
bupi_strength3<- bupi_strength3
dose3<- dose3
SPG_dataBF <-data.frame(record_id, proc1_dr, indication1, lido_strength1, lido_strength1_other, bupi_strength1, dose1, preproc1_pain, postproc1_pain, paindata2, proc2_dr, indication2, lido_strength2, bupi_strength2, dose2, preproc2_pain, postproc2_pain, paindata3, proc3_dr, indication3, lido_strength3, lido_strength3_other, bupi_strength3, dose3, preproc3_pain, postproc3_pain, paindata4)
write.table(SPG_dataBF, file = "SPGBF.csv", sep = ",", row.names =FALSE)
#zeros list
length11<-list11
list1<-ifelse(preproc1_pain==0,1,NA)
list1<-na.omit(list1)
length1<-list1
list2<-ifelse(preproc2_pain==0,1,NA)
list2<-na.omit(list2)
length2<-list2
list3<-ifelse(preproc3_pain==0,1,NA)
list3<-na.omit(list3)
length3<-list3
#9 in T1 2 in T2 in T3

#Painscore 1

paindata2<-na.omit(paindata2)
n1<-length(paindata2)
sdpain1<-round(sd(paindata2, na.rm = TRUE),digits = 1)
minpain1<-min(paindata2, na.rm = TRUE)
maxpain1<-max(paindata2, na.rm = TRUE)
medpain1<-median(paindata2, na.rm = TRUE)
meanpain1<-round(mean(paindata2, na.rm = TRUE) , digits =1)
firstqu.pain1<-round(quantile(paindata2, c(.25), na.rm = TRUE) , digits =1)
thirdqu.pain1<-round(quantile(paindata2, c(.75), na.rm = TRUE) , digits =1)



Treatment1<-data.frame(n1,minpain1,firstqu.pain1,medpain1,meanpain1,sdpain1,thirdqu.pain1,maxpain1)
rownames(Treatment1)<- c("Treatment 1")
print(Treatment1)
write.table(Treatment1, file = "Treatment1.csv", sep = ",", row.names =FALSE, dec = ".")
preproc1<-jitter(SPG_dataBF$preproc1_pain, factor = 1, amount =NULL)
plot(preproc1,SPG_dataBF$paindata2, main="Treatment 1 Change in Pain (N=225)", xlab="Pre-procedure pain",ylab="Difference in Pain Score", xlim=c(0,10),ylim=c(-15,10))
textxy(9.5,-8,"min", m=c(0,0), cex=.5, offset =.5)
textxy(3.5,6,'max', m=c(0,0), cex=.5, offset =.5)

#Painscore 2
paindata3<-na.omit(paindata3)
n2<-length(paindata3)
sdpain2<-round(sd(paindata3, na.rm = TRUE) , digits =1)
minpain2<-min(paindata3, na.rm = TRUE)
maxpain2<-max(paindata3, na.rm = TRUE)
medpain2<-median(paindata3, na.rm = TRUE)
meanpain2<-round(mean(paindata3, na.rm = TRUE) , digits=1)
firstqu.pain2<-round(quantile(paindata3, c(.25), na.rm = TRUE) , digits =1)
thirdqu.pain2<-round(quantile(paindata3, c(.75), na.rm = TRUE) , digits =1)

Treatment2<-data.frame(n2,minpain2,firstqu.pain2,medpain2,meanpain2,sdpain2,thirdqu.pain2,maxpain2)
rownames(Treatment2)<- c("Treatment 2")
print(Treatment2)
write.table(Treatment2, file = "Treatment2.csv", sep = ",", row.names =FALSE, dec = ".")
preproc2<-jitter(SPG_dataBF$preproc2_pain, factor = 1, amount =NULL)
plot(preproc2,SPG_dataBF$paindata3, main="Treatment 2 Change in Pain (N=70)", xlab="Pre-procedure score",ylab="Differenced in Pain Change",ylim= c(-15,10),xlim= c(0,10))
textxy(9,-9,"min", m=c(0,-8), cex=.5, offset =.5)
textxy(1.5,3,"max", m=c(0,0), cex=.5, offset =.5)
plot(preproc2,SPG_dataBF$paindata3, col=ifelse(postproc1_pain>5,"Red","Green"),main="Treatment 2 Change in Pain (N=70)
     red = post treatment1 score >5, 
     green= post treatment1 score <= 5", xlab="Pre-Procedure Score",ylab="Difference in Pain Score", ylim=c(-15,10), xlim=c(0,10))
plot(preproc2,SPG_dataBF$paindata3, col=ifelse(pain_change1_1>=0,"Red","Green"),main="Treatment 2 Change in Pain (N=70)
     red = pain increase on treatment1, 
     green= pain unchanged or decreased on treatment 1", xlab="Pre-Procedure Pain",ylab="Difference in Pain Score", xlim=c(0,10), ylim=c(-15,10))



#PainScore 3
paindata4<-na.omit(paindata4)
n3<-length(paindata4)
sdpain3<-round(sd(paindata4, na.rm = TRUE) , digits =1)
minpain3<-min(paindata4, na.rm = TRUE)
maxpain3<-max(paindata4, na.rm = TRUE)
medpain3<-median(paindata4, na.rm = TRUE)
meanpain3<-round(mean(paindata4, na.rm = TRUE) , digits =1 )
firstqu.pain3<-round(quantile(paindata4, c(.25), na.rm = TRUE) , digits =1)
thirdqu.pain3<-round(quantile(paindata4, c(.75), na.rm = TRUE) , digits =1)

Treatment3<-data.frame(n3,minpain3,firstqu.pain3,medpain3,meanpain3,sdpain3,thirdqu.pain3,maxpain3)
rownames(Treatment3)<- c("Treatment 3")
print(Treatment3)
write.table(Treatment3, file = "Treatment3.csv", sep = ",", row.names =FALSE, dec = ".")
preproc3<-jitter(SPG_dataBF$preproc3_pain, factor = 1, amount =NULL)
plot(preproc3,SPG_dataBF$paindata4, main="Treatment 3 Change in Pain (N=37)", xlab="Pre-procedure pain",ylab="Difference in Pain Score", ylim=c(-15,10),xlim=c(0,10))
textxy(8.7,-8,"min", m=c(0,-8), cex=.5, offset =.5)
textxy(3.7,3,"max", m=c(0,0), cex=.5, offset =.5)
plot(preproc3,SPG_dataBF$paindata4, col=ifelse(SPG_dataBF$postproc2_pain>=5,"Red","Green"),main="Treatment 3 Change in Pain (N=37)
     red = post treatment2 score >5, 
     green= post treatment2 score <= 5", xlab="Pre-Procedure pain",ylab="Difference in Pain Score",xlim=c(0,10), ylim=c(-15,10))
plot(preproc3,SPG_dataBF$paindata4, col=ifelse(pain_change2_2>=0,"Red","Green"),main="Treatment 3 Change in Pain (N=37)
     red = pain increase on treatment2, 
     green= pain unchanged or decreased on treatment 2", xlab="Pre-Procedure Score",ylab="Difference in Pain Score", xlim=c(0,10),ylim=c(-15,10))

#Table for all treatments
alltreat = rbind.fill(Treatment1,Treatment2, Treatment3)
colnames(alltreat)<-c("N","Minimum", "percentile25th"," Median","Mean", "SD", "percentile75th", "Maximum")
write.table(alltreat, file = "All Treatments.csv", sep = ",", row.names =FALSE, dec = ".")



#dose calculations
#dose cal for treatment1
l0<-replace(lido_strength1, lido_strength1==(4.00), .005)
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
#Forlidobupiscript
LL<-ifelse(l12 == .04,NA,l12)
#Treatment 1 plot
par(mar=c(4.1,4.1,10,0.5))
plot(preproc1,SPG_dataBF$paindata2, col=ifelse(is.na(col1),"black", ifelse(col1 == .04,"red","green")),main="Treatment 1 Change in Pain (N=225)
     red = lidocaine at 4%, 
     green= All other Lidocaine Strengths
     black= Bupivacine", xlab="Pre-Procedure Score",ylab="Difference in Pain Score", ylim=c(-15,10), xlim=c(0,10))

B1<-replace(bupi_strength1, bupi_strength1==(1.00), .005)
B2<-replace(B1, B1==(2.00), .01)
B3<-replace(B2, B2==(3), .02)
ifelse(B3 ==".",NA,B3)
B3=na.omit(B3)
dose1=na.omit(dose1)
B3 = as.numeric(as.character(B3))
bupistrength_dose1<-B3*dose1
plot(preproc1,SPG_dataBF$paindata2, col=ifelse(B3==.02,"Red","Green"),main="Treatment 1 Change in Pain (N=225)
     red = Bupivacaine at 2%, 
     green= All other Bupivacaine Strengths", xlab="Pre-Procedure Score",ylab="Difference in Pain Score", ylim=c(-15,10), xlim=c(0,10))
strengthdose_1table<-data.frame(lidestrength_dose1,bupistrength_dose1)
strengthdose_1table=na.omit(strengthdose_1table)
SL1mean = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=mean )[,-1]
SL1min = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=min )[,-1]
SL1q25 = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=quantile, probs=0.25 )[,-1]
SL1med = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=median )[,-1]
SL1sd = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=sd )[,-1]
SL1q75 =aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=quantile, probs=0.75 )[,-1]
SL1max = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$lidestrength_dose1), FUN=max )[,-1]
SL1count = count(strengthdose_1table$lidestrength_dose1)[,-1]
LIDOSTR1= data.frame(
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
write.table(LIDOSTR1, file = "LIDO1.csv", sep = ",", row.names =FALSE, dec = ".")

BU1mean = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$bupistrength_dose1), FUN=mean )[,-1]
BU1min = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$bupistrength_dose1), FUN=min )[,-1]
BU1q25 = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$bupistrength_dose1), FUN=quantile, probs=0.25 )[,-1]
BU1med = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$bupistrength_dose1), FUN=median )[,-1]
BU1sd = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$bupistrength_dose1), FUN=sd )[,-1]
BU1q75 =aggregate( x=strengthdose_1table, by=list(strengthdose_1table$bupistrength_dose1), FUN=quantile, probs=0.75 )[,-1]
BU1max = aggregate( x=strengthdose_1table, by=list(strengthdose_1table$bupistrength_dose1), FUN=max )[,-1]
BU1count = count(strengthdose_1table$bupistrength_dose1)[,-1]
BUPISTR1= data.frame(
  bupistrength_dose1 = 1:14,
  N1 = BU1count ,
  mean = BU1mean[,2],
  min = BU1min[,2] ,
  quantile.25 = BU1q25[,2] ,
  median = BU1med[,2] ,
  standard_deviation = BU1sd[,2] ,
  quantile.75 = BU1q75[,2] ,
  max = BU1max [,2] 
  
)
write.table(BUPISTR1, file = "BUPI1.csv", sep = ",", row.names =FALSE, dec = ".")
dosecaltreat1<-smartbind(LIDOSTR1,BUPISTR1)
write.table(dosecaltreat1, file = "SBAnalysis1.csv", sep = ",", row.names =FALSE, dec = ".")

#dose cal for treatment2
l13<-replace(lido_strength2, lido_strength2==(4.00), .005)
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
#For lido bupi script
SS<-ifelse(l17 == .04,NA,l17)

#Treatment 2 plot
par(mar=c(4.1,4.1,10,0.5))
plot(preproc2,SPG_dataBF$paindata3, col=ifelse(is.na(col2),"black", ifelse(col2 == .04,"red","green")),main="Treatment 2 Change in Pain (N=70)
     red = lidocaine at 4%, 
     green= All other Lidocaine Strengths
     black= Bupivacine", xlab="Pre-Procedure Score",ylab="Difference in Pain Score", ylim=c(-15,10), xlim=c(0,10))

B4<-replace(bupi_strength1, bupi_strength1==(1.00), .005)
B5<-replace(B4, B4==(2.00), .01)
B6<-replace(B5, B5==(3), .02)
ifelse(B6 ==".",NA,B6)
B6=na.omit(B6)
dose2=na.omit(dose2)
B6 = as.numeric(as.character(B6))
bupistrength_dose2<-B6*dose2

plot(preproc2,SPG_dataBF$paindata3, col=ifelse(B6==.02,"Red","Green"),main="Treatment 2 Change in Pain (N=70)
     red = Bupivacaine at 2%, 
     green= All other Bupivacaine Strengths", xlab="Pre-Procedure Score",ylab="Difference in Pain Score", ylim=c(-15,10), xlim=c(0,10))


strengthdose_2table<-data.frame(lidestrength_dose2,bupistrength_dose2)
strengthdose_2table=na.omit(strengthdose_2table)
SL2mean = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=mean )[,-1]
SL2min = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=min )[,-1]
SL2q25 = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=quantile, probs=0.25 )[,-1]
SL2med = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=median )[,-1]
SL2sd = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=sd )[,-1]
SL2q75 =aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=quantile, probs=0.75 )[,-1]
SL2max = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$lidestrength_dose2), FUN=max )[,-1]
SL2count = count(strengthdose_2table$lidestrength_dose2)[,-1]
LIDOSTR2= data.frame(
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
write.table(LIDOSTR2, file = "Lido2.csv", sep = ",", row.names =FALSE, dec = ".")


BU2mean = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$bupistrength_dose2), FUN=mean )[,-1]
BU2min = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$bupistrength_dose2), FUN=min )[,-1]
BU2q25 = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$bupistrength_dose2), FUN=quantile, probs=0.25 )[,-1]
BU2med = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$bupistrength_dose2), FUN=median )[,-1]
BU2sd = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$bupistrength_dose2), FUN=sd )[,-1]
BU2q75 =aggregate( x=strengthdose_2table, by=list(strengthdose_2table$bupistrength_dose2), FUN=quantile, probs=0.75 )[,-1]
BU2max = aggregate( x=strengthdose_2table, by=list(strengthdose_2table$bupistrength_dose2), FUN=max )[,-1]
BU2count = count(strengthdose_2table$bupistrength_dose2)[,-1]
BUPISTR2= data.frame(
  bupistrength_dose2 = 1:8 ,
  N1 = BU2count ,
  mean = BU2mean[,2],
  min = BU2min[,2] ,
  quantile.25 = BU2q25[,2] ,
  median = BU2med[,2] ,
  standard_deviation = BU2sd[,2] ,
  quantile.75 = BU2q75[,2] ,
  max = BU2max [,2] 
  
)
write.table(BUPISTR2, file = "BUPI2.csv", sep = ",", row.names =FALSE, dec = ".")


dosecaltreat2<-smartbind(LIDOSTR2,BUPISTR2)
write.table(dosecaltreat2, file = "SBAnalysis2.csv", sep = ",", row.names =FALSE, dec = ".")

#dose calculations
#dose cal for treatment3
l3<-replace(lido_strength3, lido_strength3==(4.00), .005)
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
#For lido bupi script
WW<-ifelse(l7 == .04,NA,l7)

#Treatment 3 plot
par(mar=c(4.1,4.1,10,0.5))
plot(preproc3_pain,SPG_dataBF$paindata4, col=ifelse(is.na(col3),"black", ifelse(col3 == .04,"red","green")),main="Treatment 3 Change in Pain (N=37)
     red = lidocaine at 4%, 
     green= All other Lidocaine Strengths
     black= Bupivacine", xlab="Pre-Procedure Score",ylab="Difference in Pain Score", ylim=c(-15,10), xlim=c(0,10))

B7<-replace(bupi_strength3, bupi_strength3==(1.00), .005)
B8<-replace(B7, B7==(2.00), .01)
B9<-replace(B8, B8==(3), .02)
ifelse(B9 ==".",NA,B9)
B9=na.omit(B9)
dose3=na.omit(dose3)
B9 = as.numeric(as.character(B9))
bupistrength_dose3<-B9*dose3

plot(preproc3_pain,SPG_dataBF$paindata4, col=ifelse(B9==.02,"Red","Green"),main="Treatment 3 Change in Pain (N=37)
     red =Bupivacaine at 2%, 
     green= All other Bupivacaine Strengths", xlab="Pre-Procedure Score",ylab="Difference in Pain Score", ylim=c(-15,10), xlim=c(0,10))


strengthdose_3table<-data.frame(lidestrength_dose3,bupistrength_dose3)
strengthdose_3table=na.omit(strengthdose_3table)
SL3mean = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=mean )[,-1]
SL3min = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=min )[,-1]
SL3q25 = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=quantile, probs=0.25 )[,-1]
SL3med = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=median )[,-1]
SL3sd = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=sd )[,-1]
SL3q75 =aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=quantile, probs=0.75 )[,-1]
SL3max = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$lidestrength_dose3), FUN=max )[,-1]
SL3count = count(strengthdose_3table$lidestrength_dose3)[,-1]
LIDOSTR3= data.frame(
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
write.table(LIDOSTR3, file = "LIDO3.csv", sep = ",", row.names =FALSE, dec = ".")


BU3mean = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$bupistrength_dose3), FUN=mean )[,-1]
BU3min = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$bupistrength_dose3), FUN=min )[,-1]
BU3q25 = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$bupistrength_dose3), FUN=quantile, probs=0.25 )[,-1]
BU3med = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$bupistrength_dose3), FUN=median )[,-1]
BU3sd = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$bupistrength_dose3), FUN=sd )[,-1]
BU3q75 =aggregate( x=strengthdose_3table, by=list(strengthdose_3table$bupistrength_dose3), FUN=quantile, probs=0.75 )[,-1]
BU3max = aggregate( x=strengthdose_3table, by=list(strengthdose_3table$bupistrength_dose3), FUN=max )[,-1]
BU3count = count(strengthdose_3table$bupistrength_dose3)[,-1]
BUPISTR3= data.frame(
  bupistrength_dose3 = 1:4 ,
  N1 = BU3count ,
  mean = BU3mean[,2],
  min = BU3min[,2] ,
  quantile.25 = BU3q25[,2] ,
  median = BU3med[,2] ,
  standard_deviation = BU3sd[,2] ,
  quantile.75 = BU3q75[,2] ,
  max = BU3max [,2] 
  
)
write.table(BUPISTR3, file = "BUPI3.csv", sep = ",", row.names =FALSE, dec = ".")


dosecaltreat3<-smartbind(LIDOSTR3,BUPISTR3)
write.table(dosecaltreat3, file = "SBAnalysis3.csv", sep = ",", row.names =FALSE, dec = ".")

#Divide my Doctor

treat1 = subset(SPG_dataBF, select = c(proc1_dr, paindata2))
treat1 = na.omit(treat1)

t1mean = aggregate( x=treat1, by=list(treat1$proc1_dr), FUN=mean )[,-1]
t1min = aggregate( x=treat1, by=list(treat1$proc1_dr), FUN=min )[,-1]
t1q25 = aggregate( x=treat1, by=list(treat1$proc1_dr), FUN=quantile, probs=0.25 )[,-1]
t1med = aggregate( x=treat1, by=list(treat1$proc1_dr), FUN=median )[,-1]
t1sd = aggregate( x=treat1, by=list(treat1$proc1_dr), FUN=sd )[,-1]
t1q75 =aggregate( x=treat1, by=list(treat1$proc1_dr), FUN=quantile, probs=0.75 )[,-1]
t1max = aggregate( x=treat1, by=list(treat1$proc1_dr), FUN=max )[,-1]
t1count = count(treat1$proc1_dr)[,-1]



Change_in_Pain_By_Dr_treat1 = data.frame(
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

print(Change_in_Pain_By_Dr_treat1)
write.table(Change_in_Pain_By_Dr_treat1, file = "CPDr1.csv", sep = ",", row.names =FALSE, dec = ".")



treat2 = subset(SPG_dataBF, select = c(proc2_dr, paindata3))
treat2 = na.omit(treat2)
treat2$proc2_dr = as.numeric(as.character(treat2$proc2_dr))
print(treat2)
t2mean = aggregate( x=treat2, by=list(treat2$proc2_dr), FUN=mean)[,-1]
t2min = aggregate( x=treat2, by=list(treat2$proc2_dr), FUN=min )[,-1]
t2q25 = aggregate( x=treat2, by=list(treat2$proc2_dr), FUN=quantile, probs=0.25 )[,-1]
t2med = aggregate( x=treat2, by=list(treat2$proc2_dr), FUN=median )[,-1]
t2sd = aggregate( x=treat2, by=list(treat2$proc2_dr), FUN=sd )[,-1]
t2q75 =aggregate( x=treat2, by=list(treat2$proc2_dr), FUN=quantile, probs=0.75 )[,-1]
t2max = aggregate( x=treat2, by=list(treat2$proc2_dr), FUN=max )[,-1]
t2count = count(treat2$proc2_dr)[,-1]

Change_in_Pain_By_Dr_treat2= data.frame(
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
print(Change_in_Pain_By_Dr_treat2)
write.table(Change_in_Pain_By_Dr_treat2, file = "CPDr2.csv", sep = ",", row.names =FALSE, dec = ".")



treat3 = subset(SPG_dataBF, select = c(proc3_dr, paindata4))
treat3 = na.omit(treat3)
treat3$proc3_dr = as.numeric(as.character(treat3$proc3_dr))
print(treat3)
t3mean = aggregate( x=treat3, by=list(treat3$proc3_dr), FUN=mean)[,-1]
t3min = aggregate( x=treat3, by=list(treat3$proc3_dr), FUN=min )[,-1]
t3q25 = aggregate( x=treat3, by=list(treat3$proc3_dr), FUN=quantile, probs=0.25 )[,-1]
t3med = aggregate( x=treat3, by=list(treat3$proc3_dr), FUN=median )[,-1]
t3sd = aggregate( x=treat3, by=list(treat3$proc3_dr), FUN=sd )[,-1]
t3q75 =aggregate(x=treat3, by=list(treat3$proc3_dr), FUN=quantile, probs=0.75 )[,-1]
t3max = aggregate(x=treat3, by=list(treat3$proc3_dr), FUN=max )[,-1]
t3count = count(treat3$proc3_dr)[,-1]

Change_in_Pain_By_Dr_treat3 = data.frame(
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

print(Change_in_Pain_By_Dr_treat3)
write.table(Change_in_Pain_By_Dr_treat3, file = "CPDr3.csv", sep = ",", row.names =FALSE, dec = ".")

#Procedure Date Data
spg_n225_new$proc1_date <- as.POSIXct(spg_n225_new$proc1_date,
                                      format='%m/%d/%Y')
spg_n225_new$proc2_date <- as.POSIXct(spg_n225_new$proc2_date,
                                      format='%m/%d/%Y')
diffs <- difftime(spg_n225_new$proc2_date,spg_n225_new$proc1_date ,units="days")
diffs = as.numeric(as.difftime(diffs))
diffs<-abs(diffs)
Dateproc<-jitter(diffs, factor = 1, amount =NULL)
plot(Dateproc,SPG_dataBF$paindata3, col=ifelse(SPG_dataBF$postproc1_pain>5,"Red","black"),main="Comparison of time between 
     Treatments and Pain Change (N=225)
     red= post treatment2 score >5",
     red= "post treatment1 score >5", 
     xlab="Days from Treatment1 to Treatment 2",ylab="Pain Change for Treatment2",ylim=c(-10,10))
#date data
spgwide <- read_sas("S:/Biostats/BIO-STAT/Radiology_Hill/HookHamate_2017/SPG_YWang/Data/spgwide.sas7bdat", 
                    NULL)
spgdatetable<-data.frame(record_id,proc1_date,proc2_date,proc3_date)
SPG_dataBF1_2<-SPG_dataBF[,-1]
spgdateBF<-cbind(spgwide, SPG_dataBF1_2)
write.table(spgdateBF, file = "spgdateBF.csv", sep = ",", row.names =FALSE, dec = ".")
spgdateBF$proc2_date <- as.POSIXct(spg_n225_new$proc2_date,
                                   format='%m/%d/%Y')
spgdateBF$proc3_date <- as.POSIXct(spg_n225_new$proc3_date,
                                   format='%m/%d/%Y')
diffs2 <- difftime(spg_n225_new$proc2_date,spg_n225_new$proc1_date ,units="days")
diffs2 = as.numeric(as.difftime(diffs2))
diffs2<-abs(diffs2)
Dateproc2<-jitter(diffs2, factor = 1, amount =NULL)
plot(Dateproc2,SPG_dataBF$paindata4, col=ifelse(SPG_dataBF$postproc2_pain>5,"Red","black"),main="Comparison of time between 
     Treatments and Pain Change
     red= post treatment2 score >5", 
     red = "post Treatment_2 score >5", xlab="Days from Treatment2 to Treatment 3",ylab="Pain Change for Treatment3",ylim=c(-10,10))

library(readxl)
SPG_wide <- read_excel("S:/Biostats/BIO-STAT/Radiology_Hill/SPGblockade_DeBacker/Data/SPG_wide.xlsx")
attach(SPG_wide)
SPG_wide<-data.frame(SPG_wide)
SPG_wide$proc_date_2 <- as.POSIXct(SPG_wide$proc_date_2,
                                   format='%m/%d/%Y')
SPG_wide$proc_date_1 <- as.POSIXct(SPG_wide$proc_date_1,
                                   format='%m/%d/%Y')
SPG_wide<-data.frame(SPG_wide)

diffs2 <- difftime(proc_date_2,proc_date_1 ,units="days")
diffs2 = as.numeric(as.difftime(diffs2))
diffs2<-abs(diffs2)
Dateproc2<-jitter(diffs2, factor = 1, amount =NULL)
plot(Dateproc2,SPG_dataBF$paindata2, col=ifelse(postproc1_score>5,"Red","black"),main="Comparison of time between 
     Treatments and Pain Change
     red= post treatment1 score >5", 
     red= "post treatment1 score >5",
     xlab="Days from Treatment1 to Treatment 2",ylab="Pain Change for Treatment3",ylim=c(-10,10))

ab<-c(.005,.02,.01,.03)
l7 %in% ab
lidoother3<-l7 [! l7 %in% ab]

ab<-c(.005,.02,.01,.03)
l17 %in% ab
lidoother2<-l17 [! l17 %in% ab]

ab<-c(.005,.02,.01,.03)
l12 %in% ab
lidoother1<-l12 [! l12 %in% ab]

lido_04<-c(lidoother1,lidoother2,lidoother3)

WW<-ifelse(lido_strength3 == .04,NA,lido_strength3)
WW<-na.omit(WW)
wW2<-ifelse(lido_strength3 ==.03,NA,lido_strength3)
#exclusion lido
#other
ab<-c(.04)
l7 %in% ab
lidoother4<-l7 [! l7 %in% ab]

ab<-c(.04)
l17 %in% ab
lidoother5<-l17 [! l17 %in% ab]

ab<-c(.04)
l12 %in% ab
lidoother6<-l12 [! l12 %in% ab]

lido_otherex<-c(lidoother4,lidoother5,lidoother6)
lido_otherex<-na.omit(lido_otherex)
xx<-lido_strength1
yy<-lido_strength2
zz<-lido_strength3
lidototal<-c(lido_strength1,lido_strength2,lido_strength3)
lidototal<-ifelse(lidototal == " ",NA,lidototal)
lidototal<-na.omit(lidototal)
dosetotal<-c(dose1,dose2,dose3)
dosetotal<-ifelse(dosetotal == " ",NA,dosetotal)
ld<-data.frame(lidototal,dosetotal)             
library(gtools)

#lidostrengthtotal2<-ifelse(factor(lido) == .04,NA,lidostrengthtotal)
#lidostrengthtotal2<-na.omit(lidostrengthtotal2)
#lido at other
#lido_other<-c(LL,SS,WW)
lido_otherdosetot<-lido_otherex*dosetotal
lido_otherdosetot<-ifelse(lido_otherex== " ",NA,lido_otherdosetot)
lido_otherdosetot<-na.omit(lido_otherdosetot)
n_other<-length(lido_otherdosetot)
sdpain_other<-round(sd(lido_otherdosetot, na.rm = TRUE) , digits =3)
minpain_other<-min(lido_otherdosetot, na.rm = TRUE)
maxpain_other<-max(lido_otherdosetot, na.rm = TRUE)
medpain_other<-median(lido_otherdosetot, na.rm = TRUE)
meanpain_other<-round(mean(lido_otherdosetot, na.rm = TRUE) , digits =3 )
firstqu.pain_other<-round(quantile(lido_otherdosetot, c(.25), na.rm = TRUE) , digits =3)
thirdqu.pain_other<-round(quantile(lido_otherdosetot, c(.75), na.rm = TRUE) , digits =3)

Treatment_other<-data.frame(n_other,minpain_other,firstqu.pain_other,medpain_other,meanpain_other,sdpain_other,thirdqu.pain_other,maxpain_other)
rownames(Treatment_other)<- c("Lidocaine other")
#lido 4%
lido_04dosetot<-lido_04*dosetotal
lido_04dosetot<-ifelse(lido_04== " ",NA,lido_04dosetot)
lido_04dosetot<-na.omit(lido_04dosetot)
n_04<-length(lido_04dosetot)
sdpain_04<-round(sd(lido_04dosetot, na.rm = TRUE) , digits =3)
minpain_04<-min(lido_04dosetot, na.rm = TRUE)
maxpain_04<-max(lido_04dosetot, na.rm = TRUE)
medpain_04<-median(lido_04dosetot, na.rm = TRUE)
meanpain_04<-round(mean(lido_04dosetot, na.rm = TRUE) , digits =3 )
firstqu.pain_04<-round(quantile(lido_04dosetot, c(.25), na.rm = TRUE) , digits =3)
thirdqu.pain_04<-round(quantile(lido_04dosetot, c(.75), na.rm = TRUE) , digits =3)

Treatment_04<-data.frame(n_04,minpain_04,firstqu.pain_04,medpain_04,meanpain_04,sdpain_04,thirdqu.pain_04,maxpain_04)
rownames(Treatment_04)<- c("Lidocaine 04")
#lidototal
lidostrengthtotal<-c(lido_04dosetot,lido_otherdosetot)
lidostrengthtotal<-na.omit(lidostrengthtotal)
n_lido<-length(lidostrengthtotal)
sdpain_lido<-round(sd(lidostrengthtotal, na.rm = TRUE) , digits =3)
minpain_lido<-min(lidostrengthtotal, na.rm = TRUE)
maxpain_lido<-max(lidostrengthtotal, na.rm = TRUE)
medpain_lido<-median(lidostrengthtotal, na.rm = TRUE)
meanpain_lido<-round(mean(lidostrengthtotal, na.rm = TRUE) , digits =3 )
firstqu.pain_lido<-round(quantile(lidostrengthtotal, c(.25), na.rm = TRUE) , digits =3)
thirdqu.pain_lido<-round(quantile(lidostrengthtotal, c(.75), na.rm = TRUE) , digits =3)

Treatment_lido2<-data.frame(n_lido,minpain_lido,firstqu.pain_lido,medpain_lido,meanpain_lido,sdpain_lido,thirdqu.pain_lido,maxpain_lido)
rownames(Treatment_lido2)<- c("lidocaine Total")

#bupitotal
bupitotal<-c(B3,B6,B9)
n_bupi<-length(bupitotal)
sdpain_bupi<-round(sd(bupitotal, na.rm = TRUE) , digits =3)
minpain_bupi<-min(bupitotal, na.rm = TRUE)
maxpain_bupi<-max(bupitotal, na.rm = TRUE)
medpain_bupi<-median(bupitotal, na.rm = TRUE)
meanpain_bupi<-round(mean(bupitotal, na.rm = TRUE) , digits =3 )
firstqu.pain_bupi<-round(quantile(bupitotal, c(.25), na.rm = TRUE) , digits =3)
thirdqu.pain_bupi<-round(quantile(bupitotal, c(.75), na.rm = TRUE) , digits =3)

Treatment_bupi<-data.frame(n_bupi,minpain_bupi,firstqu.pain_bupi,medpain_bupi,meanpain_bupi,sdpain_bupi,thirdqu.pain_bupi,maxpain_bupi)
rownames(Treatment_bupi)<- c("Bupivacaine .5%")

Treatment_bupi2<-data.frame(n_bupi,minpain_bupi,firstqu.pain_bupi,medpain_bupi,meanpain_bupi,sdpain_bupi,thirdqu.pain_bupi,maxpain_bupi)
rownames(Treatment_bupi2)<- c("Bupivacaine Total")


dosetable<-smartbind(Treatment_04,Treatment_other,Treatment_bupi,Treatment_lido2,Treatment_bupi2)
write.table(dosetable, file = "DoseLIDOBUPITABLE.csv", sep = ",", row.names =FALSE)


#Count Lido and Bupi table
lido_strength_1<-ifelse(lido_strength_1== " ",NA,lido_strength_1)
lido_strength_1<-na.omit(lido_strength_1)
lido_strength_2<-ifelse(lido_strength_2== " ",NA,lido_strength_2)
lido_strength_2<-na.omit(lido_strength_2)
lido_strength_3<-ifelse(lido_strength_3== " ",NA,lido_strength_3)
lido_strength_3<-na.omit(lido_strength_3)
Ld1<-length(lido_strength_1)
Ld2_3<-length(lido_strength_2)+length(lido_strength_3)

bupi_strength_1
bupi_strength_1<-ifelse(bupi_strength_1== " ",NA,bupi_strength_1)
bupi_strength_1<-na.omit(bupi_strength_1)
bupi_strength_2<-ifelse(bupi_strength_2== " ",NA,bupi_strength_2)
bupi_strength_2<-na.omit(bupi_strength_2)
bupi_strength_3<-ifelse(bupi_strength_3== " ",NA,bupi_strength_3)
bupi_strength_3<-na.omit(bupi_strength_3)

BUP1<-length(bupi_strength_1)
BUP2_3<-length(bupi_strength_2)+length(bupi_strength_3)


xxx1<-length(which(lido_strength1>0&bupi_strength1>0))
xxx2<-length(which(lido_strength2>0&bupi_strength2>0))
xxx3<-length(which(lido_strength3>0&bupi_strength3>0))

lido_bupi_1<-xxx1
lido_bupi23<-xxx2+xxx3

LB1=rbind(Ld1,lido_bupi_1, BUP1)
LB23=rbind(Ld2_3,lido_bupi23, BUP2_3)
Lido_bupitable=cbind(LB1,LB23)
rownames(Lido_bupitable)<- c("Lido Only", "Lido&Bupi", "Bupi Only")
colnames(Lido_bupitable)<- c("Treatment 1","Treatment 2&3")
write.table(Lido_bupitable, file = "Lido_BUPITable.csv", sep = ",", row.names =TRUE)


JK<-na.omit(ifelse(postproc2_pain>5,"T","F"))
jk1<-length(which(postproc2_pain>5))


library(vcd)
reJJuire(grid)
reJJuire(lattice)
reJJuire(latticeExtra)
reJJuire(HH)

AC1<-table(survey_Survey$acadprgm)
JJ2=as.data.frame(AC1)
png(
  file="S:\\SON\\IPE Shared Files\\Statistics\\Graphs\\Phase1\\academic_program.png", 
  width=1800,height=600
)

barplot(AC1, main="JJ2.What is your academic program?",xlab="",
        ylab="FreJJuency Count of Participants",xaxt='n')
axis(
  1, at=1:23,labels = list("Audiology Biostatistics", "Clinical Laboratory Science", "Diagnostic Cardiac Sonography","Diagnostic Ultrasound",
                           "Dietetics and Nutrition","Health Information Management","Policy and Management","Health Services Administration IGPBS","Medicine - M.D","Molecular Biotechnology",
                           "Nurse Anesthesia","Nursing - B.S","Nursing - M.S","Nursing - D.N.P.","Occupational Therapy","Pharamacy","Physical Therapy",
                           "Public Health","Rehabilitation Science - Ph.D","Respiratory Care","Social Work","Speech Pathology","Therapeutic Science"
  ),
  ,las=3
)
title(xlab = "Programs", line=11)
dev.off()


AC2<-table(survey_Survey$optout___1)
JJ3=as.data.frame(AC2)
barplot(AC2,main="JJ3. Do you consent to having 
        your survey responses used in this research project?",xlab="Response",
        ylab="FreJJuency Count of Participants",ylim=c(0,400),xaxt='n')
axis(1,at=1:2, labels=list("Yes","No"))
survey_Survey<-survey_Survey[!(survey_Survey$optout___1=="2"),]


AC3<-table(survey_Survey$previpe)
JJ4=as.data.frame(AC3)
barplot(AC3,main="JJ4. Have you had any 
        previous Interprofessional Education experiences?",xlab="Response",
        ylab="FreJJuency Count of Participants",ylim=c(0,400),xaxt='n')
axis(1,at=1:3, labels=list("Yes","No","I'm Not Sure"))

AC4<-table(survey_Survey$influence)
JJ5=as.data.frame(AC4)

AC5<-table(survey_Survey$important)
JJ6=as.data.frame(AC5)

AC6<-table(survey_Survey$previpcp)
JJ7=as.data.frame(AC6)
barplot(AC6)

AC7<-table(survey_Survey$ipcpfuture)
JJ9=as.data.frame(AC7)

AC8<-table(survey_Survey$ipcpimportant)
JJ10=as.data.frame(AC8)

AC9<-table(survey_Survey$age)
JJ12=as.data.frame(AC9)
barplot(AC9)

AC10<-length(which(`JJ13_1:Associate's degree`==1))
AC10_1<-length(which(`JJ13_2:Bachelor's degree`==2))
AC10_2<-length(which(`JJ13_3:Master's degree`==3))
AC10_3<-length(which(`JJ13_4:Doctoral degree`==4))
AC10_3<-length(which(`JJ13_4:Doctoral degree`==4))
AC10_4<-length(which(`JJ13_5:Other (please specify)`==5))
AC10_5<-length(which(`JJ13_6:Not applicable`==0))
Edgree<-data.frame(AC10,AC10_1,AC10_2,AC10_3,AC10_4,AC10_5)
#3 category graph
threeop<-rbind.data.frame(JJ4,JJ7)
print(threeop)
barplot(threeop$FreJJ,col=c("darkblue","red","Green"),
        main="JJuestion 4 and 7",xlab="Response (Blue=Yes Red=No Green=I'm not sure)",
        ylab="FreJJuency Count of Participants",ylim=c(0,400))
data<- cbind(c(89,224,67),
             c(71,245,62))
JJ4<- cbind(data[, 1], 1, c(1:3))
JJ7 <- cbind(data[, 2], 2, c(1:3))



data           <- as.data.frame(rbind(JJ4,JJ7))
data$V3 <- as.character(data$V3)
data$V3[data$V3 == "1"] <- "Yes"
data$V3[data$V3 == "2"] <- "No"
data$V3[data$V3 == "3"] <- "I'm Not Sure"
colnames(data) <-c("FreJJuency", "Type", "Response")
data$Type      <- factor(data$Type, labels = c("JJ4","JJ7"))

library(ggplot2)

ggplot(data = data, aes(x = Type, y = FreJJuency, fill = Response)) + 
  geom_bar(stat = "identity")

ggplot(data = data, aes(x = Type, y = FreJJuency, fill = Response)) + 
  geom_bar(position="fill",stat = "identity")+
  scale_y_continuous(labels = percent_format())


#5 category graph
data<- rbind(c(56, 302,0,0,48),
             c(159,180,23,2,209),
             c(212,21,23,2,7),
             c(57,4,117,65,3),
             c(25,2,236,311,128))

JJ5 <- cbind(data[, 1], 1, c(1:5))
JJ6 <- cbind(data[, 2], 2, c(1:5))
JJ9 <- cbind(data[, 3], 3, c(1:5))
JJ10 <- cbind(data[, 4], 4, c(1:5))
JJ13<- cbind(data[,5],5,c(1:5))

data           <- as.data.frame(rbind(JJ5, JJ6, JJ9, JJ10,JJ13))
data$V3 <- as.character(data$V3)
data$V3[data$V3 == "1"] <- "Strongly Disagree"
data$V3[data$V3 == "2"] <- "Disagree"
data$V3[data$V3 == "3"] <- "Neutral"
data$V3[data$V3 == "4"] <- "Agree"
data$V3[data$V3 == "5"] <- "Strongly Agree"
colnames(data) <-c("FreJJuency", "Type", "Response")
data$Type      <- factor(data$Type, labels = c("JJ5", "JJ6","JJ9","JJ10","JJ13"))

library(ggplot2)

ggplot(data = data, aes(x = Type, y = FreJJuency, fill = Response)) + 
  geom_bar(stat = "identity")
ggplot(data = data, aes(x = Type, y = FreJJuency, fill = Response)) + 
  geom_bar(position="fill",stat = "identity")+
  scale_y_continuous(labels = percent_format())


#Baseline Summary for the S2 Survey
library(readxl)
X5_2018_S2_Survey <- read_excel("S:/SON/IPE Shared Files/Statistics/Raw Numeric/5.2018 S2 Survey.xlsx")
View(X5_2018_S2_Survey)

attach(X5_2018_S2_Survey)
S2_Survey<-X5_2018_S2_Survey 

png(
  file="S:\\SON\\IPE Shared Files\\Statistics\\Graphs\\Phase1\\academic_programS2.png", 
  width=1800,height=600
)

AC11<-table(S2_Survey$acadprgmpilot)
JJ2_1=as.data.frame(AC11)
barplot(AC11, main="JJ2.What is your academic program?",xlab="",
        ylab="FreJJuency Count of Participants",xaxt='n')
axis(
  1, at=1:23,labels = list("Audiology Biostatistics", "Clinical Laboratory Science", "Diagnostic Cardiac Sonography","Diagnostic Ultrasound",
                           "Dietetics and Nutrition","Health Information Management","Policy and Management","Health Services Administration IGPBS","Medicine - M.D","Molecular Biotechnology",
                           "Nurse Anesthesia","Nursing - B.S","Nursing - M.S","Nursing - D.N.P.","Occupational Therapy","Pharamacy","Physical Therapy",
                           "Public Health","Rehabilitation Science - Ph.D","Respiratory Care","Social Work","Speech Pathology","Therapeutic Science"
  ),
  ,las=3
)
title(xlab = "Programs", line=11)
dev.off()

AC21<-table(S2_Survey$optout___1)
JJ31=as.data.frame(AC21)
barplot(AC21,main="JJ3. Do you consent to having 
        your survey responses used in this research project?",xlab="Response (0=Yes 1=No)",
        ylab="FreJJuency Count of Participants",ylim=c(0,400))
S2_Survey<-S2_Survey[!(S2_Survey$optout___1=="1"),]

AC31<-table(S2_Survey$familiar_with_ipe)
JJ41=as.data.frame(AC31)
barplot(AC31,main="PRIOR to starting your academic program at the University of Kansas, 
        were you familiar with Interprofessional Education?",xlab="Response (1=Yes 2=No)",
        ylab="FreJJuency Count of Participants",ylim=c(0,400))

AC41<-table(S2_Survey$aware_ipe_at_ku)
JJ51=as.data.frame(AC41)
barplot(AC41)

AC51<-table(S2_Survey$previous_ipe)
JJ61=as.data.frame(AC51)
barplot(AC51)

AC61<-table(S2_Survey$importantpilot)
JJ71=as.data.frame(AC61)

AC71<-table(S2_Survey$feelpreparedpilot)
JJ111=as.data.frame(AC71)

AC81<-table(S2_Survey$satisfiedipepilot)
JJ121=as.data.frame(AC81)

AC91<-table(S2_Survey$ipcpimportantpilot)
JJ131=as.data.frame(AC91)
barplot(AC91)

AC101<-table(S2_Survey$ipcpfuturepilot)
JJ141=as.data.frame(AC101)

AC111<-table(S2_Survey$futureemploypilot)
JJ151=as.data.frame(AC111)

AC121<-table(S2_Survey$age)
JJ171=as.data.frame(AC121)
mids<-barplot(AC121, main="JJ17. What is your age?",xlab="Ages",
              ylab="FreJJuency Count of Participants",ylim=c(0,200),xaxt='n')
axis(
  1, at = mids - (mids[2]-mids[1])/3,labels = list("20-24","25-29","30-34","35-39","40-44","45-49","50-54",
                                                   "55-59","60+"))

AC131<-table(S2_Survey$profareapilot)
JJ181=as.data.frame(AC131)
mids<-barplot(AC131,main="JJ18.Within which setting do you
              hope to work after you graduate??",xlab="",
              ylab="FreJJuency Count of Participants",ylim=c(0,150),xaxt='n')
axis(
  1,at = mids - (mids[2]-mids[1])/3,labels = list("Hospital - Inpatient","Hospital - Outpatient","Long-term Care, Skilled Nursing Facility, Rehabilitation Center",
                                                  "Community (e.g. private practice, outpatient clinic, service organization, community center, CVS, HyVee, etc.)",
                                                  "Industry (e.g. vendor, etc.)","Research","Education (e.g. school system, higher education, etc.)",
                                                  "Other (please specify)") ,las=3
)
title(xlab = "Setting Choice", line=20)

#two category plot
threeop2<-rbind.data.frame(JJ31,JJ41,JJ51,JJ61)
print(threeop2)
barplot(threeop2$FreJJ,col=c("darkblue","red"), main="JJuestions:3,4,5,6",xlab="Response (Blue=Yes Red=No)",
        ylab="FreJJuency Count of Participants",ylim=c(0,400))

data<- cbind(c(316,2),
             c(104,212),
             c(76,239),
             c(49,266))
JJ31<- cbind(data[, 1], 1, c(1:2))
JJ41 <- cbind(data[, 2], 2, c(1:2))
JJ51 <- cbind(data[, 3], 3, c(1:2))
JJ61 <- cbind(data[, 4], 4, c(1:2))

data           <- as.data.frame(rbind(JJ31,JJ41,JJ51,JJ61))
data$V3 <- as.character(data$V3)
data$V3[data$V3 == "1"] <- "Yes"
data$V3[data$V3 == "2"] <- "No"
colnames(data) <-c("FreJJuency", "Type", "Response")
data$Type      <- factor(data$Type, labels = c("JJ31","JJ41","JJ51","JJ61"))

library(ggplot2)

ggplot(data = data, aes(x = Type, y = FreJJuency, fill = Response)) + 
  geom_bar(stat = "identity")

ggplot(data = data, aes(x = Type, y = FreJJuency, fill = Response)) + 
  geom_bar(position="fill",stat = "identity")+
  scale_y_continuous(labels = percent_format())

#five category plot
data<- rbind(c(208,175,113,248,200,164),
             c(91,122,148,64,96,107),
             c(13,15,46,3,16,39),
             c(1,3,11,0,1,5),
             c(2,0,3,0,2,0))

JJ71 <- cbind(data[, 1], 1, c(1:5))
JJ111 <- cbind(data[, 2], 2, c(1:5))
JJ121 <- cbind(data[, 3], 3, c(1:5))
JJ131 <- cbind(data[, 4], 4, c(1:5))
JJ141 <- cbind(data[, 5], 5, c(1:5))
JJ151<- cbind(data[,6], 6,c(1:5))

data           <- as.data.frame(rbind(JJ71, JJ111, JJ121,JJ131, JJ141,JJ151))
data$V3 <- as.character(data$V3)
data$V3[data$V3 == "1"] <- "Strongly Disagree"
data$V3[data$V3 == "2"] <- "Disagree"
data$V3[data$V3 == "3"] <- "Neutral"
data$V3[data$V3 == "4"] <- "Agree"
data$V3[data$V3 == "5"] <- "Strongly Agree"
colnames(data) <-c("FreJJuency", "Type", "Response")
data$Type      <- factor(data$Type, labels = c("JJ711", "JJ111","JJ121","JJ131","JJ141","JJ151"))

library(ggplot2)
library(scales)

ggplot(data = data, aes(x = Type, y = FreJJuency, fill = Response)) + 
  geom_bar(stat = "identity")

ggplot(data = data, aes(x = Type, y = FreJJuency, fill = Response)) + 
  geom_bar(position="fill",stat = "identity")+
  scale_y_continuous(labels = percent_format())


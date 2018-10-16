library(readxl)
jensensedentary <- read_excel("jensensedentary.xlsx")
attach(jensensedentary)
library(readxl)
jenseninterval <- read_excel("jenseninterval.xlsx")
attach(jenseninterval)
library(readxl)
jensencontinous <- read_excel("jensencontinous.xlsx")
attach(jensencontinous)
# Anova Sedentary zone contac
fit1 <- aov(`common1 zone contact (proximal)` ~`common2 zone contact (distal)`, data=jensensedentary) 

summary(fit1) # display Type I ANOVA table
drop1(fit1,~.,test="F") # type III SS and F Tests 
#Anova sedentary time zone
fit2 <- aov(`Time in zone common1 (proximal) (seconds)` ~`Time in zone common2 distal (seconds)`, data=jensensedentary) 

summary(fit2) # display Type I ANOVA table
drop1(fit2,~.,test="F") # type III SS and F Tests 

#Anova Secondary #contact
fit3 <- aov(`Novel Object # contact` ~ `common object#contact` , data=jensensedentary) 

summary(fit3) # display Type I ANOVA table
drop1(fit3,~.,test="F") # type III SS and F Tests 


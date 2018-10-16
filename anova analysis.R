library(readxl)
jensensedentary <- read_excel("jensensedentary.xlsx")
attach(jensensedentary)
library(readxl)
jenseninterval <- read_excel("jenseninterval.xlsx")
attach(jenseninterval)
library(readxl)
jensencontinous <- read_excel("jensencontinous.xlsx")
attach(jensencontinous)
library(readxl)
means_jensen <- read_excel("means jensen.xlsx")
attach(means_jensen)
library(readxl)
modjensen <- read_excel("modjensen.xlsx")
attach(modjensen)
#Sedentary

# Anova Sedentary zone contact
fit1 = aov(`common2 zone contact (distal)`  ~ `common1 zone contact (proximal)`, data = jensensedentary)
summary(fit1)

#Anova Time in zone Sedentary
fit2 = aov(`Time in zone common2 distal (seconds)` ~ `Time in zone common1 (proximal) (seconds)`, data = jensensedentary)
summary(fit2)
aov(fit2)
#Anova sedentary contact
fit3 = aov(`common object # contact` ~ `Novel Object # contact`, data = jensensedentary)
summary(fit3)
aov(fit3)
#Anova common time in zone
fit4 = aov(`Common Object time in zone` ~ `Novel Object time in zone (seconds)`, data = jensensedentary)
summary(fit4)
aov(fit4)

#continous
fit5 = aov(`common2 zone contact (distal)`  ~ `common1 zone contact (proximal)`, data = jenseninterval)
summary(fit5)
aov(fit5)
#Anova Time in zone Sedentary
fit6 = aov(`Time in zone common2 distal (seconds)` ~ `Time in zone common1 (proximal) (seconds)`, data = jenseninterval)
summary(fit6)
aov(fit6)
#Anova sedentary contact
fit7 = aov(`common object # contact` ~ `Novel Object # contact`, data = jenseninterval)
summary(fit7)
aov(fit7)
#Anova common time in zone
fit8 = aov(`Common Object time in zone` ~ `Novel Object time in zone (seconds)`, data = jenseninterval)
summary(fit8)
aov(fit8)
t.test(`Common Object time in zone`~`Novel Object time in zone (seconds)`)


#Interval
#continous
fit9 = aov(`common2 zone contact (distal)`  ~ `common1 zone contact (proximal)`, data = jenseninterval)
summary(fit9)
aov(fit9)
#Anova Time in zone Sedentary
fit10 = aov(`Time in zone common2 distal (seconds)` ~ `Time in zone common1 (proximal) (seconds)`, data = jenseninterval)
summary(fit10)
aov(fit10)
#Anova sedentary contact
fit11 = aov(`common object # contact` ~ Type*`Novel Object # contact`, data = modjensen)
summary(fit11)
aov(fit11)
#Anova common time in zone
fit12 = aov(`Common Object time in zone` ~ `Novel Object time in zone (seconds)`, data = jenseninterval)
summary(fit12)
aov(fit12)


#Cross
install.packages("dplyr")
set.seed(1234)
dplyr::sample_n(modjensen, 28)
res.aov1 <- aov(`common2 zone contact (distal)` ~ Type+`common1 zone contact (proximal)`, data = modjensen)
summary(res.aov1)#significant
res.aov2 <- aov(`Time in zone common2 distal (seconds)` ~ Type +`Time in zone common1 (proximal) (seconds)`, data = modjensen)
summary(res.aov2)
res.aov3 <- aov(`common object # contact` ~ Type +`Novel Object # contact`, data = modjensen)
summary(res.aov3)#signifcant
res.aov4 <- aov(`Common Object time in zone` ~ Type  `Novel Object time in zone (seconds)`, data = modjensen)
summary(res.aov4)

#plots
boxplot(`common object # contact` ~ Type +`Novel Object # contact`, data=modjensen,frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="common object contact")
# Two-way interaction plot
interaction.plot(x.factor = modjensen$`common object # contact`,trace.factor = modjensen$Type, 
                 response = modjensen$ `Novel Object # contact`, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Novel Object contact", ylab="common contact",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))
library("ggpubr")
modjnov3<-data.frame(Type,`Novel Object # contact`,`common object # contact`)
ggline(modjnov3, x = `Novel Object # contact`, y = `common object # contact`, color = "Type",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))
ggboxplot(modjensen,x = "`Novel Object # contact`", y = "`common object # contact`", color = "Type",
          palette = c("#00AFBB", "#E7B800"))
res3 <- cor.test(modjensen$`Novel Object # contact`, modjensen$`common object # contact`, 
                method = "pearson")
print(res3)
res4<-cor.test(modjensen$`common2 zone contact (distal)`, modjensen$`common1 zone contact (proximal)`, 
               method = "pearson")
print(res4)
y<-modjensen$`common2 zone contact (distal)`
x<-modjensen$`common1 zone contact (proximal)`
t.test(y,x)
y1<-modjensen$`Novel Object # contact`
x1<-modjensen$`common object # contact`
t.test(y1,x1)

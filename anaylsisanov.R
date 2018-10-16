#anova 1a
attach(modjensen)
attach(means_jensen)
aaov<-data.frame(totalmean,means_jensen$`Time in zone common1 (proximal) (seconds)`,means_jensen$`common2 zone contact (distal)`)
aov.means<-aov(totalmean~means_jensen$`Time in zone common1 (proximal) (seconds)`,data=aaov)
ls(aov.means)
summary(aov.means)
aov.means<-aov(totalmean~means_jensen$`common2 zone contact (distal)`,data=aaov)
summary(aov.means)
#anova 1b
aov.means<-aov(`Novel Object time in zone (seconds)`~Type*`Common Object time in zone`, data=modjensen)
summary(aov.means)
aov.means<-aov(`Common Object time in zone`~Type*`Novel Object time in zone (seconds)`, data=modjensen)
summary(aov.means)
aov.means<-aov(Type~`Novel Object time in zone (seconds)`, data=modjensen)
summary(aov.means)
#anova2a
treatm1<-data.frame(modjensen$Type,modjensen$`common1 zone contact (proximal)`,modjensen$`common2 zone contact (distal)`)
aov.means<-aov(modjensen$`common1 zone contact (proximal)`~modjensen$Type*modjensen$`common2 zone contact (distal)`)
summary(aov.means)#common zone 2 significant
aov.means<-aov(modjensen$`common2 zone contact (distal)`~modjensen$Type*modjensen$`common1 zone contact (proximal)`)
summary(aov.means)#common zone 1 significant
res4<-cor.test(modjensen$`common2 zone contact (distal)`, modjensen$`common1 zone contact (proximal)`, 
               method = "pearson") 
#anova2b
aov.means<-aov(`Novel Object # contact`~modjensen$`Common object # contact`*Type,data=modjensen)
fit12 = aov(`Common Object time in zone` ~ `Novel Object time in zone (seconds)`, data = jenseninterval)
summary(fit12)
aov(fit12)
anova(modjensen)

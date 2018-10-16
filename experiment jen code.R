#Figure 1a anova
Type<-modjensen$Type
Timezonecommon1<-modjensen$`Time in zone common1 (proximal) (seconds)`
Timezonecommon2<-modjensen$`Time in zone common2 distal (seconds)`
aov(Type~Timezonecommon1)
summary(fig1)
fig2<-aov(type~Timezonecommon2)
summary(fig2)

datafilename="http://personality-project.org/R/datasets/R.appendix1.data"
data.ex1=read.table(datafilename,header=T)   #read the data into a table

aov.ex1 = aov(Alertness~Dosage,data=data.ex1)  #do the analysis of variance
summary(aov.ex1)                                    #show the summary table
print(model.tables(aov.ex1,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(Alertness~Dosage,data=data.ex1)        #graphical summary

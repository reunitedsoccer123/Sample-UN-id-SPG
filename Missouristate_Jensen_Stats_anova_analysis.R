# One Way Anova (Completely Randomized Design)
fit <- aov(cyl ~ mpg, data=mtcars) 
# Randomized Block Design (B is the blocking factor) 
fit <- aov(y ~ A + B, data=mydataframe) 
# Two Way Factorial Design 
fit <- aov(y ~ A + B + A:B, data=mydataframe)
fit <- aov(y ~ A*B, data=mydataframe) # same thing 
# Analysis of Covariance 
fit <- aov(y ~ A + x, data=mydataframe) 

# One Within Factor
fit <- aov(y~A+Error(Subject/A),data=mydataframe)
# Two Within Factors W1 W2, Two Between Factors B1 B2 
fit <- aov(y~(W1*W2*B1*B2)+Error(Subject/(W1*W2))+(B1*B2),
           data=mydataframe)



summary(fit) # display Type I ANOVA table
drop1(fit,~.,test="F") # type III SS and F Tests 

# Tukey Honestly Significant Differences
TukeyHSD(fit) # where fit comes from aov() 

# independent 2-group t-test
t.test(y~x) # where y is numeric and x is a binary factor 

# independent 2-group t-test
t.test(y1,y2) # where y1 and y2 are numeric 
# paired t-test
t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric 
# one sample t-test
t.test(y,mu=3) # Ho: mu=3

#Pearson
res <- cor.test(my_data$wt, my_data$mpg, 
                method = "pearson")
print(rest)

#Bernouilli
# Compute P(X=1) for X Bernoulli(0.7)
dbern(1, 0.7)

#Poisson
dpois(1,.7)

dbern(x, prob, log = FALSE)
pbern(q, prob, lower.tail = TRUE, log.p = FALSE)
qbern(p, prob, lower.tail = TRUE, log.p = FALSE)
rbern(n, prob)

# Two-way Interaction Plot 
attach(mtcars)
gears <- factor(gears)
cyl <- factor(cyl)
interaction.plot(cyl, gear, mpg, type="b", col=c(1:3), 
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22), 
                 xlab="Number of Cylinders", 
                 ylab="Mean Miles Per Gallon", 
                 main="Interaction Plot")
# Plot Means with Error Bars
library(gplots)
attach(mtcars)
cyl <- factor(cyl)
plotmeans(mpg~cyl,xlab="Number of Cylinders",
          ylab="Miles Per Gallon", main="Mean Plot\nwith 95% CI")
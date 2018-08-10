#Number 1,2,3
v <- c(1,2,3,4,5,6,7,8,9,10,1)#vector length of 10 and this vector is a sequence of integers
vec <- 1:10
v[vec]
mvec<- matrix(vec,nrow = 5,ncol = 2)#vector that has 5 rows and two columns

colnames(mvec) <- c("A","B")#naming the columns A and B

#Number 4,5,6
vec1 <- runif(12, min=1, max=12)#generating 12 random numbers from a uniform distribution
mvec1 <-matrix(vec1,nrow = 4,ncol = 3)#creating a 4X3 matrix
rownames(mvec1) <- c("1st Row","2nd Row","3rd Row","4th Row")#renaming the rows
mvec1[2,1]#this prints the element for the location of 2nd row, 1st column
r <- mvec1[,2]#assigns the whole second row to object r
#Number 7
V1 <- c(27,42,29,33)#ages of the people
V2<- c("James", "Art", "Kate", "Alex")#the names of the people
class(V1)#prints the metadata R has stored for this vector
class(V2)#prints the metadata R has stored for this vector
age <- data.frame(V2,V1)#creates a dataframe with the names of the people corresponding to the person's age
print(age)#prints out the dataframe
#Number 8
VC <- c(23,30,19,27)#vector of the temperature in celsius
VF <- (VC*(9/5)+32)#using the conversion equation to convert the celsius vector to fahrenheit to create a fahrenheit vector
#Number 9 and 10
state <- c("MO","KS","KS","KS","MO","NE","OK","MO","KS","NE","NE","OK","MO","KS","KS")
state_factor <- factor(state)
state_factor
str(state_factor)#shows the levels numerically easier to see
state_factor2 <- factor(state, levels = c("OK","KS","MO","NE"))#switching the order of the state categories
state_factor2
str(state_factor2)#you can see the difference from the orginal state_factor and shows the reordering
prod <- c(2341, 9873, 23933, 1999, 7214, 9089, 10999, 2389, 5435, 5757, 2521, 7333, 21909, 15110, 4369)
tapply(prod, state, mean)#mean of production for each state
tapply(prod, state, max)#max of production for each state
tapply(prod, state, min)#min of production for each state




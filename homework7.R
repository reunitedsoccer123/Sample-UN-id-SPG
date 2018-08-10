#Number 7
V1 <- c(27,42,29,33)#ages of the people
V2<- c("James", "Art", "Kate", "Alex")#the names of the people
class(V1)#prints the metadata R has stored for this vector
class(V2)#prints the metadata R has stored for this vector
age <- data.frame(V2,V1)#creates a dataframe with the names of the people corresponding to the person's age
print(age)#prints out the dataframe

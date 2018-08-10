#Number 4,5,6
vec1 <- runif(12, min=1, max=12)#generating 12 random numbers from a uniform distribution
mvec1 <-matrix(vec1,nrow = 4,ncol = 3)#creating a 4X3 matrix
rownames(mvec1) <- c("1st Row","2nd Row","3rd Row","4th Row")#renaming the rows
mvec1[2,1]#this prints the element for the location of 2nd row, 1st column
r <- mvec1[,2]#assigns the whole second row to object r

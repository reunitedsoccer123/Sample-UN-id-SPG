##First R Session

x <- c(1,2,4)#creates a vector x
q <- c(x, x, 8)#creates a vector q
y <- cbind(x,x)#combines two vectors of x in columns
z <- rbind(x,x)#combines two vectors of x in rows
?cbind#shows you how to use cbind
?rbind#shows you how to use rbind
data()#shows the different data sets
names(Orange) ##What are the variable names in the dataset?
Orange$age ## How to indicate a variable that is in a dataset
attach(Orange)## In short, so I can just ask for 'age' instead of having ## to tell R both the dataset name and the variable name
mean(age) ## asking for the mean of the variable 'age'
sd(age) ## Standard Deviation of 'age'
hist(age)## Plot a histogram of the data
hist(age, xlab = "Age (in years)")

y <- apply(z,1,mean)## Calculates row means of matrix z, saves them as y
x <- 5:1#starts vector manipulation. creates vector with five elements that is from 5 to 1.
x/2# divides each element of x by 2
x^2# squares each element of x
x%*%x# matrix multiplication between vectors
sqrt(x);min(x)#taking the square of each element of vector and the min of the vector  
x[2]#the second element of the vector
x[2:3]#the second and third index element of the vector
x[c(2,3)]#combining the follwing values into a list
x[-3]#takes 3 out of the vector
sort(x)#ordering the vector 
x <- 20:40#all elements from 20 to 40.
x[1:5]#the first five elements of x
x[-(1:5)]#subtracts the first five elements from the list of elements
c(1,2,3) + 1#add one to each element
c(1,2,3) + c(0,1)#adds zero first then 1 second and repeats sequence until finished
c(1,2,3) + c(1,2)#adds one first and 2 second to the list and repeats the sequence
c(1,2,3,4) + c(0,1)#continues the same process as last two lines
c(1,2,3,4) + c(1,2,3)#continues same process as last few lines
5%%2; 6%%2; 7%%2# 5 mod 2, 6 mod 2, and 7 mod 2
10%%4; 10%%5; 10%%6# 10 mod 4, 10 mod 5, and  10 mod 6
x <- c(1,2,4)#creates new list of x
names(x)#trying to find column names
names(x) <- c("a","b","ab")# assigning the column names
x;names(x)#shows the assignment of names to numbers and shows the independent name
x["b"]#shows want number correlates with b
x <- c(1:10); y <- c(10:1)#creates two lists of numbers of x and y
cbind(1,x,y)#combines two vectors x and y into columns
rbind(1,x,y)#combines two vectors x and y into rows
x <- list(1,x,y)#shows the three list with member types and what members are included in each list

##Matrix Manipulation
A <- matrix(nrow=2,ncol=2,c(1:4))#creates matrix A
B <- matrix(nrow=2,ncol=2,c(1:4))#creates matrix B 
x <- c(2,2)#vector x value 
A + B #adding the two matrix
A - B #difference between matrix A and matrix B
t(A)#t returns the transpose of A
t(t(A))#another transpose of the tranpose of the last line
A*B#matrix multiplication
A%*%B#matrix multiplication 
A%*%x#matrix multiplication
x%*%A%*%x#matrix multiplication
solve(A)#solving the system of equations from matrixes 
C <- matrix(nrow=4,ncol=4,c(1:16))#creates matrix C 
a1 <- A[1,]#all values of A in Row 1
a2 <- A[2,]#all values of A in Row 2
b1 <- B[,1]#all values of B in Column 1
b2 <- B[,2]#all values of B in Column 2
c_c <- C[,2:3]#all values in columns 2 and 3
c_r <- C[1:2,]#all values in rows 1 and 2 
C[c(1,3),] <- matrix(nrow=2,ncol=4,c(rep(1,8)))#rows one and 3 in matrix c where there are 8 ones that appear now  
y <- matrix(c(4,5,2,3), nrow = 2)#creates a 2x2 matrix
y[-2,]#eliminates row 2
x <- matrix(nrow=3,ncol=2,c(1:6))#creates a matrix x
x + c(0,1)#same process from above add 0 first and 1 second and repeat
z <- matrix(nrow=3,ncol=2,c(1:6))#creates a matrix z
apply(z,1,mean)#taking the mean of the rows
apply(z,2,mean)#taking the mean of the columns

#Character String
y <- "abc"#abc is a member of y
length(y)#number of members in y
mode(y)#type of members that is in 
z <- c("abc", "29 88")#assigning two members to z
length(z)#number of members in z
mode(z)#type of the members in z
w_w <- paste("Wonder", "Woman")#pasting wonder woman into w_W
ww <- strsplit(w_w, " ")#splits wonder woman into ww
is.list(ww)#showing that wonder woman is indeed in the list 
grep("West", c("North","South","South East","North West","West"))#location of where west is showing up in the list 
nchar("You're gonna need a bigger boat.")#counts the number of characters
substr("Jaws",2,3)#keeps the second and third characters and subtracts the others
regexpr("aw","Jaws")#evluating length,mode,T/F in the group of characters 
strsplit("6-16-2011", split="-")#splits the expression into three independent memebers

##Sequences
example(seg)#runs the example seg
n <- 10#assigns 10 to be a member of list of n
1:n-1#creates a squence that leads up to ten with a interval on one between the numbers
1:(n-1)#excludes zero
seq(-10, 10, by = 2) -> sequence.1 #creates a sequence between -10 and 10 with intervals on 2 between each number
sequence.2 <- seq(length = 11, from = -10, by = 2)#creates a sequence of 11 members starting from -10 and increasing by 2
sequence.3 <- rep(x, 5)#creates a sequence that repeats of x five times
sequence.4 <- rep(c(5,2,7),3)#creates a sequence that repeats 5,2,7 three times 
sequence.5 <- rep(c(5,2,7),each=3)#creates a sequence where a number is repeated 3 times until moving onto the next number

##Logicals
a <- c(TRUE,TRUE,FALSE,FALSE)#creates a vector 
b <- c(FALSE,TRUE,TRUE,FALSE)#creates a vector
a|b#Or operator
a&b#And operator
!a&b#Nand operator
!a&!b#Not A and Not B operator
!(a&b)#Not A and B Operator 
a+b#Addition of A and B and evaluation of number of Trues in each column
dummy.1 <- x > 3 #table of logical combinations greater than 3
dummy.2 <- x == 3 #table of logical combinations exactly equal to 3
x <- 1:10#creates a vector
any(x>8)#asking if there are any numbers >8
any(x>88)#asking if there are any numbers >88
all(x>88)#asking if all numbers >88
all(x>0)#asking if all numbers >0
x <- 1:3#creating new vector of x
y <- c(1,3,4)#creating vector y
x == y #asking when x is exactly equal to y
all(x == y)#asking if all x's are equal to all y's
identical(x,y)#asking if the objects are exactly equal
x <- 1:2#creating new vector of x
y <- c(1,2)#creating new vector of y
x;y#showing x and y first line x and second line y
identical(x,y)#asking if x is equal equal to y
typeof(x)#asking the type of number x is
typeof(y)#asking the type of number y is

##Missing Data
z <- c(1:3,NA)#creates a vector with a missing member
z.dummy <- is.na(z)#creating a vector with only NA's 
z.dummy2 <- z==NA #having the variable z exactly equal to NA
0/0#not a number
is.na(0/0)#shows list the numbers the avaliable
is.nan(0/0)#shows that if 0/0 is a not a number
is.nan(z)#shows if each member of  is a number or not
mean(z)#the mean of z
mean(z,na.rm=T)#the mean of z when taking away any value that's NA 
x <- c(88,NULL,12,168,13)#creates new vector 
mean(x)#the mean of x
u <- NULL#assigning the variable u to NULL member in vector x
length(u)#the number of members in u
v <- NA#assigning the variable v to NA
length(v)#the number of members in V

##Simply subsetting
x <- c(c(1:5),c(rep(NA,3)))#creates vector x where NA is repeated 3 times
is.na(x)#shows the which are the missing numbers
x[is.na(x)]#the list of missing numbers
x[!is.na(x)]#the list of non-missing numbers 
x > 2#shows which number are greater than 2
!is.na(x)&x > 2#a list of not avaliable values of >2
x[!is.na(x)&x > 2]#show the actually numbers from the line above 
x <- matrix(1:6,2,3)#creates a matrix 2 rows and three columns
w <- list(one = 1:5, two = 0.6, three = "HI")#creates a list w 
w$one#shows the members on line one in list w
w[["one"]]#same operation as the list above
w[[1]]#different way of doing the last two lines above
w[[c(1,3)]]#creating list from first line and number 3 in the value column
w[[1]][[3]]#another way to write the line above 
w[c(1,3)]#selecting the values from lines 1 and 3 of the w-list
z <- c(5,2,-3,8)#creating new vector 
z#listing members in z
z*z > 8#showing results when you multipling each member by itself(squaring each member in other words)
z[z*z > 8]#showing which members the current expression works for
w <- z[z*z > 8]#using the members from list above creating new vecotr
x <- c(6,1:3,NA,12)#creating new vector 
x[x>5]#the members that this can possibly work(I say possibly becuase NA will show up) 
subset(x,x>5)#To get rid of NA we used subset to get values that meet the following conditions  
x[x>3] <- 0#use a condition to get numbers that will work so we can get rid of the NA
which(z*z > 8)#get values true for the following condition and also shows what value NA eexactly is

##Arrays#t-test
first_test <- matrix(nrow = 3, ncol = 2, c(46,21,50,30,25,50))#creates a matrix 
second_test <- matrix(nrow = 3, ncol = 2, c(46,41,50,43,35,50))#creates a matrix
tests <- array(data=c(first_test,second_test),dim=c(3,2,2)) #the extension of the matrices which in this situation is first test and second test combined
attributes(tests)#shows the list being tested
tests[3,2,1];tests[3,2,2]#shows the value the comes up when tested 
tests#shows the matrix for each test from the lines above from first and second test

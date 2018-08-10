##First R Session

x <- c(1,2,4)
q <- c(x, x, 8)
y <- cbind(x,x)
z <- rbind(x,x)
?cbind
?rbind
data()
names(Orange) ##What are the variable names in the dataset?
Orange$age ## How to indicate a variable that is in a dataset
attach(Orange)## In short, so I can just ask for 'age' instead of having ## to tell R both the dataset name and the variable name
mean(age) ## asking for the mean of the variable 'age'
sd(age) ## Standard Deviation of 'age'
hist(age)## Plot a histogram of the data
hist(age, xlab = "Age (in years)")

y <- apply(z,1,mean)## Calculates row means of matrix z, saves them as y
x <- 5:1#starts vector manipulation
x/2
x%*%x
sqrt(x);min(x)
x[2]
x[2:3]
x[c(2,3)]
x[-3]
sort(x)
x <- 20:40
x[1:5]
x[-(1:5)]
c(1,2,3) + 1
c(1,2,3) + c(0,1)
c(1,2,3) + c(1,2)
c(1,2,3,4) + c(0,1)
c(1,2,3,4) + c(1,2,3)
5%%2; 6%%2; 7%%2
10%%4; 10%%5; 10%%6
x <- c(1,2,4)
names(x)
names(x) <- c("a","b","ab")
x;names(x)
x["b"]
x <- c(1:10); y <- c(10:1)
cbind(1,x,y)
rbind(1,x,y)
x <- list(1,x,y)

##Matrix Manipulation
A <- matrix(nrow=2,ncol=2,c(1:4))
B <- matrix(nrow=2,ncol=2,c(1:4)) 
x <- c(2,2)
A + B
A -B
t(A)
t(t(A))
A*B
A%*%B
A%*%x
x%*%A%*%x
solve(A)
C <- matrix(nrow=4,ncol=4,c(1:16)) 
a1 <- A[1,]
a2 <- A[2,]
b1 <- B[,1]
b2 <- B[,2]
c_c <- C[,2:3]
c_r <- C[1:2,] 
C[c(1,3),] <- matrix(nrow=2,ncol=4,c(rep(1,8))) 
y <- matrix(c(4,5,2,3), nrow = 2) 
y[-2,]
x <- matrix(nrow=3,ncol=2,c(1:6))
x + c(0,1)
z <- matrix(nrow=3,ncol=2,c(1:6))
apply(z,1,mean)
apply(z,2,mean)

#Character String
y <- "abc"
length(y)
mode(y)
z <- c("abc", "29 88")
length(z)
mode(z)
w_w <- paste("Wonder", "Woman")
ww <- strsplit(w_w, " ") 
is.list(ww) 
grep("West", c("North","South","South East","North West","West")) 
nchar("You're gonna need a bigger boat.")
substr("Jaws",2,3)
regexpr("aw","Jaws") 
strsplit("6-16-2011", split="-")

##Sequences
example(seg)
n <- 10
1:n-1
1:(n-1)
seq(-10, 10, by = 2) -> sequence.1 
sequence.2 <- seq(length = 11, from = -10, by = 2) 
sequence.3 <- rep(x, 5)
sequence.4 <- rep(c(5,2,7),3) 
sequence.5 <- rep(c(5,2,7),each=3)

##Logicals
a <- c(TRUE,TRUE,FALSE,FALSE) 
b <- c(FALSE,TRUE,TRUE,FALSE)
a|b
a&b
!a&b
!a&!b
!(a&b)
a+b
dummy.1 <- x > 3 
dummy.2 <- x == 3 
x <- 1:10
any(x>8)
any(x>88)
all(x>88)
all(x>0)
x <- 1:3
y <- c(1,3,4)
x == y
all(x == y)
identical(x,y)
x <- 1:2
y <- c(1,2)
x;y
identical(x,y)
typeof(x)
typeof(y)

##Missing Data
z <- c(1:3,NA)
z.dummy <- is.na(z) 
z.dummy2 <- z==NA 
0/0
is.na(0/0) 
is.nan(0/0)
is.nan(z)
mean(z)
mean(z,na.rm=T) 
x <- c(88,NULL,12,168,13) 
mean(x)
u <- NULL
length(u)
v <- NA
length(v)

##Simply subsetting
x <- c(c(1:5),c(rep(NA,3)))
is.na(x)
x[is.na(x)] 
x[!is.na(x)] 
x > 2
!is.na(x)&x > 2 
x[!is.na(x)&x > 2] 
x <- matrix(1:6,2,3)
w <- list(one = 1:5, two = 0.6, three = "HI") 
w$one
w[["one"]] 
w[[1]] 
w[[c(1,3)]] 
w[[1]][[3]] 
w[c(1,3)]
z <- c(5,2,-3,8) 
z
z*z > 8
z[z*z > 8]
w <- z[z*z > 8]
x <- c(6,1:3,NA,12) 
x[x>5] 
subset(x,x>5) 
x[x>3] <- 0
which(z*z > 8)

##Arrays
first_test <- matrix(nrow = 3, ncol = 2, c(46,21,50,30,25,50)) 
second_test <- matrix(nrow = 3, ncol = 2, c(46,41,50,43,35,50))
tests <- array(data=c(first_test,second_test,dim=c(3,2,2)) 
attributes(tests)
tests[3,2,1];tests[3,2,2] 
tests
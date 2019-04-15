a <- c(1,2,5.3,6,-2,4) # numeric vector
b <- c("one","two","three") # character vector
c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE) #logical vector
a[c(2,4)]


# generates 5 x 4 numeric matrix 
y=matrix(1:20, nrow=5,ncol=4)
y
t(y)
diag(y)
ginv(y)
diag(5,10)
solve(diag(5,10))
det(diag(5,10))
5^10
# another example
cells <- c(1,26,24,68)
rnames <- c("R1", "R2")
cnames <- c("C1", "C2") 
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,
                   dimnames=list(rnames, cnames))
y
y[,4] # 4th column of matrix
y[3,] # 3rd row of matrix 
y[2:4,1:3] # rows 2,3,4 of columns 1,2,3


d <- c(1,2,3,4)
e <- c("red", "white", "red", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
mydata <- data.frame(d,e,f)
names(mydata) <- c("ID","Color","Passed") # variable names


mydata[,2:3] # columns 3,4,5 of data frame
mydata[,c("ID")] # columns ID and Age from data frame
mydata$Color # variable x1 in the data frame

#
gender <- c(rep("male",20), rep("female", 30)) 
gender <- factor(gender) 
str(gender)
# stores gender as 20 1s and 30 2s and associates
# 1=female, 2=male internally (alphabetically)
# R now treats gender as a nominal variable 
summary(gender)

a <- c(1,2,5.3,6,-2,4) # numeric vector

w <- list(name="Fred", mynumbers=a, mymatrix=y, age=5.3)
w1 <- list(name="Hamid", mynumbers=5*a, mymatrix=2*y, age=5.3)

# example of a list containing two lists 
v <- c(w1,w)
length(y)
dim(y)
dim(a)
summary(y)

summary(mydata)

cbind(a, d, e)
rbind(a,d,e)


cbind( d,a, e)

rm(a)
p<-ls()

rm(list=ls())
5%%3
5%/%2 




x <- c(1:10)
x

x > 8
F F F F F F F F T T
x < 5
T T T T F F F F F F
x > 8 | x < 5
T T T T F F F F T T
x[c(T,T,T,T,F,F,F,F,T,T)]
1 2 3 4 9 10
x[(x>8) | (x<5)]



library(MASS)
ginv(y)

rowMeans(y)
colMeans(y)
a
#norm
sqrt(sum(a * a))

rep(1, 5)
7*y
y*7
y%*%a[-5:-6]
dim(y)
a[-5:-6]
det(y[-5,])
solve(y[-5,])
eigen(y%*%a[-5:-6])
s<-unname(cbind(y,a[-6]))


s<-matrix(mtcars$mpg,nrow = 5,ncol = 5)
solve(s)
eigen(s)





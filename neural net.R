attach(mtcars)
x<-disp ; y<-mpg

#no of observations
n=length(x);n

#include bias term
x<-as.matrix(cbind(1,x))

#guess initial weight 
w<-(c(0,1))

  
#define metric for distance
#Euclidean distance 

 # learning rate 
 etta=0.0000293
 
 
 for (i in 1:250000)
 {
   w <- w -  etta*((1 / n) * (colSums(t(x%*%w-y) %*% x)))
 }
 
 print(w)
 
 lm(mpg~disp)
 #Mean Squared error
 y.hat<-x%*%w
 MSE<- sum((y - y.hat) ^ 2)/n ;MSE
 mean(lm(mpg~disp)$residuals^2)
 
plot(x,y)
lines(x,y.hat)



#In NN format


w<-(c(0,1))


#define metric for distance
#Euclidean distance 

# learning rate 
etta=0.0000293

j=1

for (i in 1:1000000)
{
  for (j in 1:n)
  {gradient<- etta*((1 / n) * t(x[j,]%*%w-y[j])%*% x[j,])
  w <- w -  t(gradient)}
}
lm(mpg~disp)
print(w)









#import  data for x
#x <- c(2,5,3,7,-3,-2,0) 
x<-rnorm(10000,5,11)
#define log ML function for Normal distribution
fn <- function(theta) 
  {
  sum ( 0.5*(x - theta[1])^2/theta[2] + 0.5* log(theta[2]) )
}
#estimate with nlm or optim
nlm(fn, theta <- c(0,1), hessian=TRUE)
optim(theta <- c(0,1), fn, hessian=TRUE)

#average of samople with estimate[1]
mean(x)
 
# variance or estimate[2]
sum( (x -mean(x))^2 )/length(x)
 
 output1 <- optim( theta <- c(2,10), fn,hessian=TRUE)
 
 
  Std.Error <- sqrt(diag(solve(output1$hessian)))
 z.value <- output1$par/Std.Error
 pvalue <- 2*(1 - pnorm(abs(z.value)))
 results <- cbind(output1$par,Std.Error,z.value,pvalue)
 colnames(results) <- c("Estimate.", "Std. Error", "z.value", "P.value")
 print(results)   
 
### Example with a beta distribution
 y <- rbeta(10000,4,2)
 loglik <- function(mu, x) { 
     sum(-dbeta(x,mu[1],mu[2],log = TRUE)) 
     } 
 
 output1 <- optim(par = c(1,1), fn=loglik,x=y,method = "L-BFGS-B",lower=c(0,0))
 
 output1$par

 
 ###Pareto distribution  : https://en.wikibooks.org/wiki/R_Programming/Maximum_Likelihood
 
 library(actuar)
  y <- rpareto1(1000, shape = 14, min = 500)
 ll <- function(mu, x) { 
       sum(dpareto1(x,mu[1],min = min(x),log = TRUE)) 
     } 
 output1<-optimize(f = ll, x = y, interval = c(0,10), maximum = TRUE)
 output1$maximum

 
 
 #### Example with a poisson distribution

 f <- function(x,lambda) -sum(log(dpois(x,lambda)))
 
 x<-rpois(10000, lambda=6)
 output1<-optim(fn=f, par=c(1,20), x=x,method = "L-BFGS-B")
 output1$par

 
 #http://polisci2.ucsd.edu/dhughes/teaching/MLE_in_R.pdf
 
 x<-rpois(10000, lambda=6)
 
 f1<-function(lambda){
   n<-length(x)
   logl<-  -sum(x)*log(lambda)+n*lambda
   return(logl)
 }

 output2<-optim(fn=f1, par=c(6), method = "L-BFGS-B")
 output2$par
 
 optim(6,fn=f1,method="L-BFGS-B")
 
 #AIC : -2logL+2*K
 2*output2$value+2*1
 
 
 #https://web.stanford.edu/group/heeh/cgi-bin/web/node/197
 plague <- c(20, 40, 31, 17, 10, 12, 14, 4, 10, 12, 9, 16, 8, 5, 4, 4, 9) 
 plot(1982:1998,plague,
      type="l",
      xlab="Year",
      ylab="Plague Cases")
  
 output1<-optim(fn=f, par=c(1,20), x=plague,method = "L-BFGS-B")
 output1$par
 
 
 #OLS with MLE
 #http://polisci2.ucsd.edu/dhughes/teaching/MLE_in_R.pdf
 set.seed(123)
 x<-runif(1000)
 X<-cbind(1,x)
 #beta0, beta1, sigma2
 theta.true<-c(2,3,1)
 
 #y=2+3*x+error
 y<-X%*%theta.true[1:2] + rnorm(1000)
 
 
 ols.lf<-function(theta,y,X){
   n<-nrow(X)
   k<-ncol(X)
   beta<-theta[1:k]
   sigma2<-theta[k+1]
   e<-y-X%*%beta
   logl<- -.5*n*log(sigma2)-((t(e)%*%e)/(2*sigma2))
   return(-logl)
 }
 p<-optim(c(1,1,1),ols.lf,method="BFGS",hessian=T,y=y,X=X)
 
 p$par  
 OI<-solve(p$hessian)
 se<-sqrt(diag(OI))
 cbind(p$par,se)
 
 #check with OLS
 summary( lm(y ~ x))


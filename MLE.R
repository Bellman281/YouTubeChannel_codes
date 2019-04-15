gasdata<- read.csv("D:/Bil PhD/512/gasdata.csv",header = F)
names(gasdata)<-c("Contract Type","Hours","Bays","Convenience Store","Carwash","Self Service","Split Island","Volume")



### Probit log-likelihood function ##

probit_ln_Like <- function(beta) {
  
  F_probit <- pnorm(X%*%beta)
  log_like <- -sum(t(y)%*%log(F_probit) + (t(1 - y))%*%log(1 - F_probit))
  return(log_like)
}

## Probit gradient function ##

gradient.ln.probit <- function(beta) {
  g <- beta*0
  F_probit <- pnorm(X%*%beta) 
  a_i<-dnorm (X%*%beta) /(F_probit*(1-F_probit))
  for (i in 1:K) { 
    g[i] <- sum(X[,i] * (y - F_probit)*a_i)
  }
  return(-g)
}

#using ols for initials
initial <- lm(`Contract Type` ~ Hours + Bays + `Convenience Store`+Carwash+`Self Service`+`Split Island`+Volume) $coefficients
X <- cbind(1, Hours , Bays,`Convenience Store`,Carwash,`Self Service`,`Split Island`,Volume)
y <- `Contract Type`
K <- as.numeric(ncol(X))

probitmodel <- optim(initial, probit_ln_Like, gr=gradient.ln.probit, method="BFGS", control=list(trace=TRUE, REPORT=1), hessian=TRUE)
Std.Error <- sqrt(diag(solve(probitmodel$hessian)))
z.value <- probitmodel$par/Std.Error
pvalue <- 2*(1 - pnorm(abs(z.value)))
results_probit <- cbind(probitmodel$par,Std.Error,z.value,pvalue)
colnames(results_probit) <- c("Estimate.", "Std. Error", "z.value", "P.value")
print(results_probit)  


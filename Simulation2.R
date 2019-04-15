library(xtable)
j<-1
beta0 <-1.5
beta1<- 2.5
sims <- 1000

result <- data.frame(matrix(ncol = 3, nrow = 5))
#initialize a data frame to collect results
df2 <- data.frame(matrix(ncol = 2, nrow = sims))
colnames(df2) <- c('beta0', 'beta1')
#here
n<-200
for(i in 1:sims)
{
  ##vectors each  1000 long 
  xx1 <-runif(n, min = 10, max = 12)
  e <- rnorm(n)
  y <- beta0 + beta1*xx1 +e
  #collect results for each itter
  df2[i,] <- data.frame(t(lm(y ~xx1 )$coeff))
}
summary(lm(y~xx1))$r.square

result[j,]<-cbind(n,t(colMeans(df2))) 
apply(df2,2,var)

result[j,]<-cbind(n,t(colMeans(df2))) 
result
j<-j+1
#here






names(result)<-c("Sample Size","beta_0","beta_1")
result
xtable(result,digits = 8)
#sink("D:/hw/hw3_df2.txt")
#xtable(df2)


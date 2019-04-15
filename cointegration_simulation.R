library("quantmod")
library("tseries")
library("urca")
library("igraph")

set.seed(123)

## Simulated cointegrated series

z1 <- rep(0, 10000)
for (i in 2:10000) z1[i] <- z1[i-1] + rnorm(1)

z2 <- rep(0, 10000)
for (i in 2:10000) z2[i] <- z2[i-1] + rnorm(1)




p2 <- q2 <- r2 <-w2<- rep(0, 10000)

p2 <- 0.3*z2 + rnorm(10000)
q2 <- 0.5*z2 + rnorm(10000)

r2 <- 0.9*z2 + rnorm(10000)
w2 <- 0.7*z2 + rnorm(10000)

jotest2=ca.jo(data.frame(p2,q2,r2,w2), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest2)

r=0
if (jotest2@teststat[4]>jotest2@cval[[12]]) {print("No-CoIntegration is rejected... Wait for next comment  ") ; r=1}
if (jotest2@teststat[3]>jotest2@cval[[11]]) {print("There are atleast two cointegrated pairs (r>1)... Wait for next comment  ");r=2}
if (jotest2@teststat[2]>jotest2@cval[[10]]) {print("There are atleast three cointegrated pairs (r>2)... Wait for next comment  ");r=3}
if (jotest2@teststat[1]>jotest2@cval[[9]]) {print("There are atleast three cointegrated pairs (r>3)... Wait for next comment  ");r=4}

r
#
beta<-jotest2@V[,(4-r)]

s2 = beta[1]*p2 + beta[2]*q2 +beta[3]*r2+beta[4]*w2

p<-summary(ur.df(s2,type="none",selectlags = "AIC"))


if (p@teststat[1] < p@cval[1]) {print("stationary")}
if (p@teststat[1] >= p@cval[1]) {print("non - stationary")}
if (p@teststat[1] >= p@cval[2]) {print("non - stationary")}
if (p@teststat[1] >= p@cval[3]) {print("non - stationary")}





######

p1 <- q1 <- r1 <- rep(0, 10000)

p1 <- 0.2*z1 + rnorm(10000)
q1 <- 0.6*z1 + rnorm(10000)
r1 <- 0.8*z1 + rnorm(10000)

jotest1=ca.jo(data.frame(p1,q1,r1), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest1)

s1 = 1.000*p1 + 1.791324*q1 - 1.717271*r
#plot(s1, type="l")

#adf.test(s1)


#The Dickey-Fuller test statistic is very low, providing a low p-value and hence evidence to reject the null hypothesis of a unit root and thus evidence we have a stationary series formed from a linear combination.


# ADF test for stationarity
p<-summary(ur.df(s1,type="none",selectlags = "AIC"))

p@teststat[1]
if (p@teststat[1] < p@cval[1]) {print("stationary")}
if (p@teststat[1] >= p@cval[1]) {print("non - stationary")}
if (p@teststat[1] >= p@cval[2]) {print("non - stationary")}
if (p@teststat[1] >= p@cval[3]) {print("non - stationary")}



#### PRoblem to solve

jotest=ca.jo(data.frame(p1,q1,r1,p2,r2,q2,w2), type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)

y=c(1,1,1,2,2,2,2)


jotest@V

I33<-diag(3)
j33<-matrix(1,nrow=3,ncol=3)-I33
I44<-diag(4)
j44<-matrix(1,nrow=4,ncol=4)-I44

beta<-matrix(0,nrow=7,ncol=7)
beta2<-matrix(0,nrow=7,ncol=7)

beta[1:3,1:3]<-j33

beta[4:7,4:7]<-j44

beta2[1:3,1:3]<-matrix(1,nrow=3,ncol=3)

beta2[4:7,4:7]<-matrix(1,nrow=4,ncol=4)

beta2

g1 <- as.undirected(graph_from_adjacency_matrix( beta))

plot(g1)



###

X<-solve(jotest@V)%*%beta

round(jotest@V %*% X)

df<-data.frame(p1,q1,r1,p2,r2,q2,w2)

solve(var(data.frame(p1,q1,r1,p2,r2,q2,w2)))%*%alpha

pca<-prcomp(df,center = TRUE,scale. = TRUE)
pca$rotation %*% X 






#df<-data.frame(p1,q1,r1,p2,r2,q2,w2)
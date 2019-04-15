# AR_1 process Simulation
library(urca)
library(TSA)
#creare noise

n=500
e_t<- rnorm(n)
#initial
x_i=3
t=1
X_t<-vector()
X_t[1]=x_i+e_t[t]

teta=0.9

for (t in 1:n)
{X_t[t+1]=teta*X_t[t]+e_t[t]}
X_t

#the null of unit root cannot be rejected if pvalue <> 0

p<-summary(ur.df(X_t,type="none",selectlags = "AIC"))

if (p@teststat[1] < p@cval[1]) {print("stationary")}

ho_adf <- ur.df(X_t)
if ( unname(ho_adf@ teststat< -2.58 )) 
{ s<- c("Stationary")}
if ( unname(ho_adf@ teststat > -2.58 )) 
{ s<- c("Non-Stationary")}

print(s)




# AR_ 3 process Simulation

#creare noise

n=500
e_t<- rnorm(n)
#initial
x_i=3
t=1
X_t<-vector()
X_t[1]=x_i+e_t[t]
X_t[2]=-3*x_i+e_t[t]
X_t[3]=2*x_i+e_t[t]

teta=.9

for (t in 1:n)
{X_t[t+3]=teta*X_t[t]+teta*X_t[t+1]+e_t[t]}
X_t

#the null of unit root cannot be rejected if pvalue <> 0

p<-summary(ur.df(X_t,type="none",selectlags = "AIC"))

if (p@teststat[1] < p@cval[1]) {print("stationary")}

ho_adf <- ur.df(X_t)
if ( unname(ho_adf@ teststat< -2.58 )) 
{ s<- c("Stationary")}
if ( unname(ho_adf@ teststat > -2.58 )) 
{ s<- c("Non-Stationary")}

print(s)


adf.test(X_t, k=10)

library(forecast)
auto.arima(X_t)



# AR_ 4 process Simulation

#creare noise

n=5000
e_t<- rnorm(n)
#initial
x_i=3
t=1
X_t<-vector()
X_t[1]=x_i+e_t[t]
X_t[2]=3*x_i+e_t[t]
X_t[3]=2*x_i+e_t[t]
X_t[4]=7*x_i+e_t[t]

teta=1

for (t in 1:n)
{X_t[t+4]=teta*X_t[t]+e_t[t]}
X_t

#the null of unit root cannot be rejected if pvalue <> 0

p<-summary(ur.df(X_t,type="none",selectlags = "AIC"))

if (p@teststat[1] < p@cval[1]) {print("stationary")}

ho_adf <- ur.df(X_t)
if ( unname(ho_adf@ teststat< -2.58 )) 
{ s<- c("Stationary")}
if ( unname(ho_adf@ teststat > -2.58 )) 
{ s<- c("Non-Stationary")}

print(s)


adf.test(X_t, k=10)

library(forecast)
auto.arima(X_t)



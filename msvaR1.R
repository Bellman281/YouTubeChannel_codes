library(MSBVAR)
library(vars)
library(car)
df<-read.csv("D:/total01.csv")
str(df)

#df<-df[,-2]
# df$GDP.OIL<-log(df$GDP.OIL)
# df$I.Mchin<-log(df$I.Mchin)
# df$I.Realestate<-log(df$I.Realestate)

head(df)
#OLS regression
x1<-lm(CPI~., data = df)
vif(x1) # variance inflation factors 

head(df)

#make it Time series 
df <- ts(df, frequency=4)

#unit root in r

library(urca)
ur.df(df[,2],type="none",lags=0)

#determine lags

var=VARselect(df,lag.max=15);var

#VAR Model
var.2c <- VAR(df , type = "const")
#var.2c <- VAR(df , type = "trend")
#var.2c <- VAR(df , type = "both")
#var.2c <- VAR(df , type = "none")

colnames(df)
irf.tr<-irf(var.2c, impulse = "exchange.rate", response = c("GDP.OIL", "CPI"), boot =
      FALSE)

plot(irf.tr)
irf.tr<-irf(var.2c, impulse = "exchange.rate", boot =FALSE)

plot(irf.tr)

m2 <-msvar(df, p = 2, h = 2, niterblkopt = 10);m2


#example

a <- c(1.998513, 1.995302, 2.030693, 2.122130, 2.236770, 2.314639, 2.365214, 2.455784, 2.530696, 2.596537)
b <- c(0.6421369, 0.6341437, 0.6494933, 0.6760939, 0.7113511, 0.7173038, 0.7250545, 0.7812490, 0.7874657, 0.8275209)
length(a)
x <- matrix (NA,10,2)
x[,1] <- a
x[,2] <- b

x = rbind(x,x)
time.seriesx <- ts(x)
m2 <-msvar(time.seriesx, p = 2, h = 3, niterblkopt = 100)
m2

#fp.rec <- ts(m2$fp[,1], start=tsp(x)[1], freq=tsp(x)[3])
#plot(fp.rec)

nsteps=20
forecasts <- forecast.VAR(m2[[1]], nsteps=10, A0 = t(chol(m2[[1]]$mean.S)),
             shocks=matrix(0,nrow=nsteps,ncol=dim(m2[[1]]$ar.coefs)[1]),
             exog.fut=matrix(0,nrow=nsteps,ncol=length(m2[[1]]$pfit$num.exog)))

forecasts.only <- forecasts[(11:nrow(forecasts)),]

nrow(df)-20
# Plot forecasts and actual data
i2p <- ts(cbind(df[((nrow(df)-19):nrow(df)),1], forecasts.only[,1]),
          freq=4)

plot(i2p)

















markov.switchingx <- msvar(time.seriesx, p = 2, h = 2, niterblkopt = 100)

markov.switchingx$init.model
markov.switchingx$hreg
#MS-VAR works only for stationary time series
#de-trend your time series: either by subracting the mean, subtracting the linear trend, 
#the moving average, a smoothed curve

a <- c(1.998513, 1.995302, 2.030693, 2.122130, 2.236770, 2.314639, 2.365214, 2.455784, 2.530696, 2.596537, 2.647573, 2.735317, 2.705269, 2.699783, 2.659748, 2.641353, 2.641825, 2.613648, 2.627755, 2.627383)
b <- c(0.6421369, 0.6341437, 0.6494933, 0.6760939, 0.7113511, 0.7173038, 0.7250545, 0.7812490, 0.7874657, 0.8275209, 0.9079720, 0.9455602, 0.9426856, 0.9234943, 0.9072791, 0.9194827, 0.9021116, 0.8971606, 0.9047334, 0.8965786)

library("pracma")
xa = detrend(a) #a - mean(a) #detrend this
xb = detrend(b) #b - mean(b) #detrend this

x <- matrix (NA,20,2)
x[,1] <- xa
x[,2] <- xb
ts_x <- ts(x)

set.seed(1)
m_x <- msvar(ts_x, p = 2, h = 2, niterblkopt = 10)
fp <- ts(m_x$fp)

plot(ts_x)
plot(fp)


#
data(IsraelPalestineConflict)
Y.sample1 <- window(IsraelPalestineConflict, end=c(2002, 52))
Y.sample2 <- window(IsraelPalestineConflict, start=c(2003,1))

# Fit a BVAR model
fit.bvar <- szbvar(Y.sample1, p=6, lambda0=0.6, lambda1=0.1, lambda3=2,
                   lambda4=0.25, lambda5=0, mu5=0, mu6=0, prior=0)

# Forecast -- this gives back the sample PLUS the forecasts!

forecasts <- forecast(fit.bvar, nsteps=nrow(Y.sample2))
forecasts.only <- forecasts[(nrow(Y.sample1)+1):nrow(forecasts),]

# Plot forecasts and actual data
i2p <- ts(cbind(Y.sample2[,1], forecasts.only[,1]),
          start=c(2003,1), freq=52)

p2i <- ts(cbind(Y.sample2[,2], forecasts.only[,2]),
          start=c(2003,1), freq=52)

par(mfrow=c(2,1))
plot(i2p, plot.type=c("single"))
plot(p2i, plot.type=c("single"))


## Not run: 
# MSBVAR forecasts

# Fit model
m1 <- msbvar(Y.sample1, p=1, h=2, lambda0=0.8, lambda1=0.2,
             lambda3=1, lambda4=0.2, lambda5=0, mu5=0, mu6=0,
             qm=12, prior=0)

# Gibbs sampling
m1id <- gibbs.msbvar(m1, N1=1000, N2=10000, permute=FALSE, Sigma.idx=1)

# Forecast density estimation
msforc <- forecast(m1id, nsteps=nrow(Y.sample2), N1=1000, N2=10000)

# Summarize forecasts
apply(msforc$forecasts, c(2,3), mean)


## End(Not run)


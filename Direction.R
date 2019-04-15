library(quantmod)
library(TTR)
library(tseries)
library (urca)
library (forecast)
library(dplyr)

nasdaq100 <- read.csv("nasdaq100list.csv",
            stringsAsFactors = FALSE, strip.white = TRUE)

nasdaq <- new.env()
 for(i in nasdaq100$Symbol) 
   {
   cat("Downloading time series for symbol '", i, "' ...\n",
         sep = "")
   status <- tryCatch(getSymbols(i, env = nasdaq),
                        error = identity)
   if(inherits(status, "error"))
    cat("Symbol '", i, "' not downloadable!\n", sep = "")
   }

list.nasdaq<-setdiff(ls(nasdaq),c("LILAK","QVCA"))

df<-list(setNames(lapply(list.nasdaq, get, envir=nasdaq), list.nasdaq))

p<-data.frame()

dates<-vector()
n.dates<-vector()

portfo<-data.frame()
portfo[1:length(master.date),1]<-master.date
portfo[,1]<-master.date
names(portfo)[1]<-c("master.date")


j=2; 

k<-0
i="AAL"
i="AAPL"
#adjust dates
for (i in list.nasdaq )
{ 
  p<-get(i, envir=nasdaq)
  n.dates <- as.Date(index(p), "%Y-%M-%D")
  p<-data.frame(p)
  p[,2:7]<-p[,1:6]
  p[,1]<-n.dates 
  names(p)<-c("date","Open", "High", "Low", "Close", "Volume", "Adjusted.Close")
  
  result<-indicator(p,i)
  
  ho_adf <- ur.df(result[,2])
  if ( unname(ho_adf@ teststat< -2.58 )) 
  { s<- c("Stationary")}
  if ( unname(ho_adf@ teststat > -2.58 )) 
  { s<- c("Non-Stationary")}
  
  
 
  row.names(p) <- NULL
  result[,1]<-n.dates[-1:-50]


  
 # colnames(result) <- paste(i, colnames(result), sep = "_")
  colnames(result)[1]<-"date"
  
  write.csv(result, paste0(i,".csv"))
  #k<-j+91
  #portfo[,j:k]<-left_join(portfo,result, by = c("master.date"="date")) [,j:k]
  #j<-j+92
  
  }




getSymbols("MSFT")


names(MSFT)<-c("Open", "High", "Low", "Close", "Volume", "Adjusted.Close")
plot(MSFT$Close,col="red")
p<-data.frame(MSFT)

write.csv(p,"MSF_original.csv")


n=length(p$Adjusted.Close)
d<-cbind(tdy=p$Adjusted.Close[1:n-1],twm=p$Adjusted.Close[2:n],
          direct=sign( p$Adjusted.Close[2:n]-p$Adjusted.Close[1:n-1]))

plot(d[,1],type="l",col="red")
lines(d[,3],type="l",col = "blue")

plot(d[1:100,3],type="l",col = "blue")


table (d[,3])

d<-ts(d)
Ar.model <- auto.arima(d[,3], stationary = TRUE, seasonal = FALSE, ic="aic")

print(Ar.model)





#unit root test ADF test H0: unit root existance (H1: data is stationary)
# if p-value is close to zero then reject H0 (means there is no unit root, inferring it is stationary). 

ho_adf <- ur.df(d[,3])
summary(ho_adf)


# Confidence Intervals of Arima Model
confint(Ar.model)


# Model Testing Standard Residuals , ACF of Residuals, P-values LJUNG-BOX statistics
tsdiag(Ar.model)

# Plotting 
plot(OPECmod$x, lty = 1, main = "OPEC BASKET prices: raw data(black) vs. fitted values(red line)", ylab = "Return in percent", xlab = "Date")

lines(fitted(OPECmod), lty = 2,lwd = 2, col = "red")

#  accuracy ME (mean error), Root Mean squared error, mean absolute error, mean percentage error, 
#         mean absolute percentage error, and mean absolute scaled error.

accuracy(OPECmod)

predict(OPECmod, n.ahead=5)

plot(forecast(OPECmod,h=400))

plot(OPECmod)


plot(OPECmod$x, lty = 1, main = " ARIMA: raw(black) vs. fitted (red)", ylab = "in percent", xlab = "obs.")
lines(fitted(OPECmod), lty = 2,lwd = 2, col = "red")


#Classical Seasonal Decomposition by Moving Averages
plot(decompose(ts(OPEC$Value, frequency = 80)))






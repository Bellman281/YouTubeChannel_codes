# https://www.jstatsoft.org/article/view/v041i04/v41i04.pdf
#Nile : univariate Time-Series [1:100],Start = 1871 , End = 1970 
Frequency = 1 

fitNile <- StructTS(Nile, "level")
fitNile

summary(Nile)
Nile
str(Nile)

#plot the data
plot(Nile, type = "o",col="Green")
#The time series of the filtered estimates
 lines(fitted(fitNile), lty = "dashed", lwd = 2,col="blue")
 # smoothed estimates
 lines(tsSmooth(fitNile), lty = "dotted", lwd = 2)
 
plot(forecast(fitNile, level = c(50, 90), h = 10), xlim = c(1950, 1980))
 

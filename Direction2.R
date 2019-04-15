library(urca)
library(forecast)

HF<-read.csv("AAPL.csv")


nasdaq<-read.csv("nasdaq100list.csv")[,1]
filename<-paste0(nasdaq,".csv")
list1<-list.files()	
list1<-intersect(list1,filename)

st_firms<-vector()
setwd("~/nasdaq")

list1="MSFT"
for (i in list1)
{
print(i)

df<-data.frame(read.csv(i))
df<-df[complete.cases(df),]
X_t<-df[,4]
p<-summary(ur.df(X_t,type="none",selectlags = "AIC"))
if (p@teststat[1] < p@cval[1])
{print("stationary")
  st_firms<-c(st_firms,unlist(strsplit(i, split='.', fixed=TRUE))[1])
}
if (p@teststat[1] >= p@cval[1]) {print("non - stationary")}
if (p@teststat[1] >= p@cval[2]) {print("non - stationary")}
if (p@teststat[1] >= p@cval[3]) {print("non - stationary")}
}

















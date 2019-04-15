oil<-read.csv("http://yunus.hacettepe.edu.tr/~iozkan/data/oilcsv.csv", header=T, sep=";")

head(oil)
library(zoo)
oil<-oil[,-4]
oil.zoo=zoo(oil[,-1], order.by=as.Date(strptime(as.character(oil[,1]), "%d.%m.%Y")))
plot(oil.zoo, main="Brent and Crude Price Series")

#Let's import other series namely BIST100 index and TL/USD series.

xu100<-read.csv("http://yunus.hacettepe.edu.tr/~iozkan/data/endXU100.csv", header=T, sep=";")
head(xu100)

usd<-read.csv("http://yunus.hacettepe.edu.tr/~iozkan/data/usd.csv", header=T, sep=";")
head(usd)

#install.packages("chron")

library(chron)

xu100 <- xu100[xu100[,"SESSION"]!=1,-c(1,3,4,5)]

# now convert to zoo
xu100.zoo=zoo(xu100[,-1], order.by=as.Date(strptime(as.character(xu100[,1]), "%d.%m.%Y")))
colnames(xu100.zoo) <- c("Close", "USD", "Euro")

plot(xu100.zoo)

usd.zoo=zoo(usd[,-1], order.by=as.Date(strptime(as.character(usd[,1]), "%d.%m.%Y")))

head(is.weekend(time(usd.zoo)))


# usd2 contains weekdays obs.. others are holidays.. Do not delete..
usd <- usd.zoo[!is.weekend(time(usd.zoo))]
plot(usd, main="TL/USD Series", xlab="Date")
# 

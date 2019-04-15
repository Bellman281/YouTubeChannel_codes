
n=5000
x<-runif(n, min = 2, max = 4)
shapiro.test(x)
x1 <- (x - mean(x)) / sd(x)

shapiro.test((x1))




shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(1000, min = 2, max = 4))

ndat<-(runif(3000, min = 2, max = 4))

dat <- rnorm(3000,mean=100, sd=5)

require(nortest)
 lillie.test(dat$at)
 lillie.test(ndat)
 
 
 
 data<-read.csv("ntest.csv", header = F)
 str(data)
 
 plot(data$V1)
 
 dat<-as.numeric(data$V1)
 #dat<-data
 
 plot(dat)
 df<-unique(as.numeric(data$V1))
 dat <- (dat - mean(dat)) / sd(dat)
 plot(dat)
 
 lillie.test(dat)
 shapiro.test(dat)
 ad.test(dat) # Anderson-Darling normality test
 cvm.test(dat) # Cramer-von Mises normality test
 pearson.test(dat) # Pearson chi-square normality test
 sf.test(dat) # Shapiro-Francia normality test

 
 
df <- scale(df, center = TRUE ,scale = FALSE)

plot(df)

lillie.test(df)
shapiro.test(df)
ad.test(df) # Anderson-Darling normality test
cvm.test(df) # Cramer-von Mises normality test
pearson.test(df) # Pearson chi-square normality test
sf.test(df) # Shapiro-Francia normality test


 
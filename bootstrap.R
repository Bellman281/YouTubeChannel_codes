samplemean <- function(x, d) {
  return(mean(x[d]))
}

samplemedian <- function(x, d) {
  return(median(x[d]))
}
library(boot)
x=seq(10,50,1)
b = boot(x, samplemedian, R=1000)           # 1000 replications
print(sd(b$t[,1]))
median(x)

b = boot(x, samplemean, R=10000)           # 1000 replications
plot(b)


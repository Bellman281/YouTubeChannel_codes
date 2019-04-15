install.packages("MSBVAR")

library(MSBVAR)


rm(list = ls())
df1<-read.csv("D:/oil.csv")
str(df1)
#data is monthly so:
df1<-ts(df1, frequency = 12, start = c(1380, 1))

#droping first column
df1<-df1[,-1]

head(df1)
str(df1)


#log diffrecnce of data
df<-diff(log(df1))


#VAR LAG TEST ?
#it seems lag=1 ?!
var.lag.specification(df, lagmax=40)$results
max(var.lag.specification(df, lagmax=40)$results[,2])
max(var.lag.specification(df, lagmax=40)$results[,3])
max(var.lag.specification(df, lagmax=40)$results[,4])

#how to determine the number of regimes? #LR TEST

#assume 3 regims
xm <- msbvar(df, p=1, h=3,
             lambda0=0.8, lambda1=0.15, lambda3=1, lambda4=0.25,
             lambda5=1, mu5=0, mu6=0, qm=12,
             alpha.prior=c(100, 50,30)*diag(3) +
               matrix(12, 3, 3), prior=0, max.iter=40,
             initialize.opt=NULL)

fp.rec <- ts(xm$fp[,1], start=tsp(df)[1],frequency = 12)
plot(fp.rec)




#regime-specific moment matrices, VAR coefficients, and error covariances
# which one is VAR coefficinets? 
print(xm$hreg)
print(xm$hreg$moment)
print(xm$hreg$Bk)
print(xm$hreg$Sigmak)
print(xm$hreg$df)


#MARKOV The h x h Markov transition matrix.
print("The h x h Markov transition matrix.")
print(xm$Q)

#The T x h matrix of the filtered regime probabilities. 
#First column is the first regime, etc.
print("The T x h matrix of the filtered regime probabilities.")
print(xm$fp)


#HELP says use GIBBS Sampleing twice

# Now sample the posterior
N1 <- 1000
N2 <- 2000
# First, so this with random permutation sampling
x1 <- gibbs.msbvar(xm, N1=N1, N2=N2, permute=TRUE)



# how to Identify the regimes 
plotregimeid(x1, type="all")

# Now re-estimate based on desired regime identification seen in the
# plots. Here we are using the intercept of the first equation, so
# Beta.idx=c(7,1).
x2 <- gibbs.msbvar(xm, N1=N1, N2=N2, permute=FALSE,Beta.idx=c(4,2))
# Plot regimes
plot.SS(x2)

# Summary of transition matrix

summary(x2$Q.sample)
summary(x2$transition.sample)


# Plot of the variance elements
plot(x2$Sigma.sample, method = "Sims-Zha2")

posterior.impulses <- mc.irf(x2, nsteps=12, draws=1000)
plot(posterior.impulses, method = c("Percentile"))

plotregimeid(xm, x2, type="Sigma")


irf2 <- mc.irf(x2, nsteps=20)
plot.ms.irf(irf2)

#plot(x2$ss.sample[[2]], ylab="State Probabilities")

#Identifies and plots regime-specific coefficients from the random permutation sampler for regime identification


plotregimeid( x2, type="Sigma")
plotregimeid( x2, type="all")
write.csv(xm$Q,"D:/Markov_Q.csv")

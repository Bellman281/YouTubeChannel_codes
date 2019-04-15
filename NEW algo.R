head(cbind(alldata$pclose,alldata$tdy_ret,alldata$tmw_ret))

#d2=300
#out of bag
d3=100
d2=0

n4<-n-d3
n1<-n-d3-d2;n1
ind<-round(n1*.80);ind
j=1
i=1
n;n1
d=0
trainingSet<- alldata[(1+d*i):(ind+d*i),]
n2<-nrow(trainingSet)
print(c(paste0("Training set ",i," is now created from ",(1+d*i), " to ",
               (ind+d*i) ,". the training size is ",n2)))

testingSet<- alldata[(ind+i*d+1):(n1+d*i),]
n3<-nrow(testingSet)
print(c(paste0("Thr Forest ",i," will use data from ",(ind+i*d+1), " to ",
               (n1+d*i) ," to test its accuracy. the training size is ",n3)))


#expert i is added at time t_i
t_i<-n1+d*i+1
print(c(paste0("expert ", i," is added at time t_i ",t_i)))

# (nrow(trainingSet)+nrow(testingSet))*.8
pSet<-alldata[((n1+d*i+1):n),]
print(c(paste0("pSet is created from ", (n1+d*i+1)," to ",n)))


modelFit <- randomForest(tmw_ret ~., data = trainingSet, importance=TRUE,
                         proximity=TRUE)

print(c("random forest",i,"FROM",j,"OF ",q,"EACH DAYS",d))

#predicted values
pSet2<-alldata[((ind+i*d+1):n),]
p02 <- predict(modelFit,newdata = testingSet)

V<-data.frame(cbind(date=df$date,price=df$pclose,tdy_ret=alldata$tdy_ret,twm_ret=alldata$tmw_ret))
V[,5]<-0
V[(ind+i*d+1):(2232),5]<-p02
names(V)[5]<-c("prediction_Test")
V$signal<-sign(V$prediction_Test)
V$realy<-sign(V$twm_ret)

head(V[(ind+i*d+1):1932,])
imp<-importance(modelFit)
imp[order(imp[,1]),]


modelFit <- randomForest(tmw_ret ~ pclose+ChaikinAccumulation+roc22+OnBalanceVolume+macd12_24.signal +macd12_30.signal+sma10+bbands26.up+phigh +stochSMI.signal +macd12_18.signal, data = trainingSet, importance=TRUE,
                         proximity=TRUE)
p03 <- predict(modelFit,newdata = testingSet)

V[,6]<-0
V[(ind+i*d+1):(2232),6]<-p03
names(V)[6]<-c("prediction_1_Test")
# V$signal<-sign(V$prediction_Test)
# V$realy<-sign(V$twm_ret)

head(V[(ind+i*d+1):1932,])
imp2<-importance(modelFit)
imp2[order(imp2[,1]),]


df<-read.csv("D:/B/saipa-final.csv",sep = ";", header = T)

ksp<-df

library(TTR)

head(ksp)
ksp  <-alldata
pp01<-matrix(nrow = 3100,ncol = 100)
ww<-matrix(nrow = 3100,ncol = 100)
rr<-matrix(nrow = 3100,ncol = 100)
kk<-matrix(nrow = 3100,ncol = 100)
ramp1=matrix(nrow = 3100,ncol = 100)
mw1<-matrix(0,nrow = 3100,ncol = 5)
ff<-matrix(nrow=3100,ncol = 2)
colnames(ff)<-c("positive","negative")
ff1<-matrix(nrow=3100,ncol = 1)
colnames(ff1)<-c("BUY")


d2=300
n<-nrow(alldata)-100-d2;n
ind<-round(n*.80);ind

d=100
trainingSet<- alldata[(1+d*i):(ind+d*i),]
testingSet<- alldata[(ind+i*d+1):(n+d*i),]

#print(c(1+d*i,ind+d*i,ind+i*d+1,n+d*i))

#expert i is added at time t_i
t_i<-ind+i*d+1
print(c("expert", i," is added at time t_i",t_i))

# (nrow(trainingSet)+nrow(testingSet))*.8
pSet<-alldata[((ind+i*d+1):(nrow(alldata))),]
library(randomForest)
modelFit <- randomForest(pclose ~., data = trainingSet, importance=TRUE,
                         proximity=TRUE)


print(c("random forest",i,"FROM",j,"OF ",q,"EACH DAYS",d))

p01 <- predict(modelFit, pSet)
#save p01 in Matrix and in vector 
pp1<-c(rep(NA,ind+d*i),p01)

pp01[1:length(pp1),(i+q)]<-pp1

r<-pSet$pclose-p01

library(neuralnet)
n1 <- names(trainingSet)
f <- as.formula(paste("pclose ~", paste(n1[!n1 %in% "pclose"], collapse = " + ")))
nn <- neuralnet(f,data=trainingSet,hidden=c(5,3),linear.output=T)

plot(nn)
pr.nn <- compute(nn,testingSet[,1:47])

pr.nn_ <- pr.nn$net.result*(max(alldata$pclose)-min(alldata$pclose))+min(alldata$pclose)
#test.r <- (testingSet$pclose)*(max(alldata$pclose)-min(alldata$pclose))+min(data$pclose)

#MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

#print(paste(MSE.lm,MSE.nn))


getSymbols("GS") #Goldman OHLC from yahoo 
  #RSI
  rsi8<- RSI(ksp$pclose,8)
  rsi16<- RSI(ksp$pclose,16)
  rsi24<- RSI(ksp$pclose,24)
  
  plot(rsi8)
  
  
  # EMA
  ema10<- EMA(ksp$pclose,10)/ksp$pclose
  ema16<- EMA(ksp$pclose,16)/ksp$pclose
  ema22<- EMA(ksp$pclose,22)/ksp$pclose
  # SMA
  sma10<- SMA(ksp$pclose,10)/ksp$pclose
  sma16<- SMA(ksp$pclose,16)/ksp$pclose
  sma22<- SMA(ksp$pclose,22)/ksp$pclose
  # boolinge
  bbands20<-BBands(ksp[,c("phigh","plow","pclose")],20)
  colnames(bbands20) <- paste("bbands20",colnames(bbands20),sep=".")
  
  bbands26<-BBands(ksp[,c("phigh","plow","pclose")],26)
  colnames(bbands26) <- paste("bbands26",colnames(bbands26),sep=".")
  
  bbands32<-BBands(ksp[,c("phigh","plow","pclose")],32)
  colnames(bbands32) <- paste("bbands32",colnames(bbands32),sep=".")
  
  # MOMENTUM 
  mom12<-momentum(ksp$pclose,12)
  mom18<-momentum(ksp$pclose,18)
  mom24<-momentum(ksp$pclose,24)
  
  #ROC
  roc10<- ROC(ksp$pclose,10)
  roc16<- ROC(ksp$pclose,16)
  roc22<- ROC(ksp$pclose,22)
  
  #MACD
  
  macd12_18 <- MACD(ksp$pclose, nFast=12, nSlow=18, nSig=9, maType=SMA)
  colnames(macd12_18) <- paste("macd12_18",colnames(macd12_18),sep=".")
  
  macd12_24 <- MACD(ksp$pclose, nFast=12, nSlow=24, nSig=9, maType=SMA)
  colnames(macd12_24) <- paste("macd12_24",colnames(macd12_24),sep=".")
  
  macd12_30 <- MACD(ksp$pclose, nFast=12, nSlow=30, nSig=9, maType=SMA)
  colnames(macd12_30) <- paste("macd12_30",colnames(macd12_30),sep=".")
  
  #stoch
  stochWPR <- WPR(ksp[,c("phigh","plow","pclose")])
  stochSMI <- SMI(ksp[,c("phigh","plow","pclose")])
  colnames(stochSMI) <- paste("stochSMI",colnames(stochSMI),sep=".")
  
  
  chaikinVolatility10 <- chaikinVolatility(ksp[,c("phigh","plow","pclose")],n = 10)
  
  ChaikinAccumulation <- chaikinAD(ksp[,c("phigh","plow","pclose")],volume=ksp[,c("tvol")])
  OnBalanceVolume <- OBV(ksp$pclose,ksp$pclose)
  
  
  alldata  <-  data.frame(ksp
                          ,sma10,sma16,sma22
                          ,ema10,ema16,ema22
                          ,bbands26,bbands20,bbands32
                          ,mom12,mom18,mom24
                          ,roc10,roc16,roc22
                          ,macd12_18,macd12_24,macd12_30
                          ,rsi8,rsi16,rsi24
                          ,chaikinVolatility10
                          ,ChaikinAccumulation,OnBalanceVolume)
  
  
  alldata2  <-  data.frame(ksp
                           ,sma10,sma16,sma22
                           ,ema10,ema16,ema22
                           ,bbands26,bbands20,bbands32
                           ,mom12,mom18,mom24
                           ,roc10,roc16,roc22
                           ,macd12_18,macd12_24,macd12_30
                           ,rsi8,rsi16,rsi24
                           , stochWPR,macd12_30
                           ,chaikinVolatility10
                           ,ChaikinAccumulation,OnBalanceVolume)
  
  head(alldata)
  #alldata[,1:3]<- alldata[,1:3]/alldata$pclose
  #alldata[,5:9]<- alldata[,5:9]/alldata$pclose
  # n<- nrow(alldata)
  
  #for(i in 1:2526)
  #   {
  #     
  #     if(alldata[i,5]!=0)
  #       {
  #       alldata[i,45]<- alldata[i+1,5]/alldata[i,5]
  #       colnames(alldata)[46]<- "mvol"
  #       }
  #     else alldata[i,46]=0
  #     
  #     
  #     if(alldata[i,6]!=0)
  #       {
  #       alldata[i,47]<- alldata[i+1,6]/alldata[i,6]
  #       colnames(alldata)[47]<- "mval"
  #       }
  #     else alldata[i,47]=0
  #     
  #     if(alldata[i,4]!=0){
  #       alldata[i,48]<- alldata[i+1,4]/alldata[i,4]
  # colnames(alldata)[48]<- "ret"}
  #     else alldata[i,48]=0
  #   }
  
  
  
  
  str(df)
  
  #alldata<- df
  library(randomForest)
  #MAke NA's = 0 >>> change ir later
  alldata[is.na(alldata)] <- 0
  alldata<- alldata[,-4:-5]
  alldata<-alldata[,-1:-2]
  alldata<-alldata[,-2:-4]
  ret<-c(0,diff(log(alldata$pclose)))
  alldata$pclose<-ret
  
  train<-alldata[1:2300,]
  
  
  
  ###loop for d
  
  #p<-unname(prediction [1:d])
  d=0 #do this out of loop
  d1=c(20,40,80,150,200)
  accuracies <-c()
  
  d2=300
  d=20
  i=1
  n<-nrow(alldata)-100-d2;n
  ind<-round(n*.80);ind
  prediction<-c()
  #p1<-cbind(prediction,unname(prediction1),b)
  #colnames(p1)<-c(paste("predict",i),"row")
  #head(p1)
  
  
  for (d in d1)
  {
    #n-ind
    for( i in 1:(d2/d))
    {
      
      trainingSet<- alldata[(1+d*i):(ind+d*i),];
      
      testingSet<- alldata[(ind+i*d+1):(n+d*i),];nrow(testingSet)
      b<-c(1+d*i,ind+d*i,ind+i*d+1,n+d*i,nrow(trainingSet),nrow(testingSet));b
      
      (nrow(trainingSet)+nrow(testingSet))*.8
      pSet<-alldata[((ind+i*d+1):(nrow(alldata))),]
      #
    }}
  
  modelFit <- randomForest(pclose ~., data = trainingSet, importance=TRUE,
                           proximity=TRUE)
  #print(modelFit)
  #round(importance(modelFit), 2)
  
  p01 <- predict(modelFit, pSet)
  r<-pSet$pclose-p01
  i=1
  
  for (t in 1:length(r))
  {
    k<-sum(r[1:t])
    w<-exp(k)/sqrt(t)
    
    ww[t,i]<-w
  }
  ww[,2]<-r
  ww=data.frame(w)
  head(p01)
  tail(p01)
  
  
  #prediction <- predict(modelFit, testingSet)
  
  length(prediction1)
  head(pSet)
  pSet$rightPred <- prediction1 - pSet$pclose
  accuracy <- sum(pSet$rightPred)/nrow(pSet)
  accuracies <- c(accuracies,accuracy)
  print(accuracies)
}}

modelFit$confusion







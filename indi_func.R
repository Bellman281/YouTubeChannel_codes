

indicator<-function(p,name.c){
  # The following 4 lines are to make O/H/L/C consistent with Adjusted prices so
# that the med, typ and wc values are not impacted by splits and dividends.
p[,"Open"]<-p[,"Open"]*(p[,"Adjusted.Close"]/p[,"Close"])
p[,"High"]<-p[,"High"]*(p[,"Adjusted.Close"]/p[,"Close"])
p[,"Low"]<-p[,"Low"]*(p[,"Adjusted.Close"]/p[,"Close"])
p[,"Close"]<-p[,"Low"]*(p[,"Adjusted.Close"]/p[,"Close"])
p$med<-(Hi(p)+Lo(p))/2
p$typ<-(Hi(p)+Lo(p)+Cl(p))/3
p$wc<-(Hi(p)+Lo(p)+2*Cl(p))/4

result<-data.frame(n.dates=rep(NA,nrow(p)),row.names = index(p))

result$Adjusted_close<-p$Adjusted.Close
n<-nrow(p)
result$direct<-c(NA,sign( result$Adjusted_close[2:n]-result$Adjusted_close[1:n-1]))
#name.c=MSFT
result$c.name<-name.c

result$typ<-p$typ$Close
result$wc<-p$wc$Close
result$med<-p$med
result$low<-p$Low
result$high<-p$High
result$close<-p$Close
result$Adjusted_close<-p$Adjusted.Close




#RSI and Signal BUY and SELL
#http://blog.fosstrading.com/2009/04/testing-rsi2-with-r.html
result$rsi9<-RSI(p$Adjusted.Close,8)
signal_long <- ifelse(result$rsi9 < 30, 1, 0)
signal_short <- ifelse(result$rsi9 > 70, -1, 0)
signal_long[is.na(signal_long)] <- 0
signal_short[is.na(signal_short)] <- 0
result$sig9 <- signal_long + signal_short

result$rsi15<- RSI(p$Adjusted.Close,14)
signal_long <- ifelse(result$rsi15 < 30, 1, 0)
signal_short <- ifelse(result$rsi15 > 70, -1, 0)
signal_long[is.na(signal_long)] <- 0
signal_short[is.na(signal_short)] <- 0
result$sig15 <- signal_long + signal_short

result$rsi21<- RSI(p$Adjusted.Close,20)
signal_long <- ifelse(result$rsi21 < 30, 1, 0)
signal_short <- ifelse(result$rsi21 > 70, -1, 0)
signal_long[is.na(signal_long)] <- 0
signal_short[is.na(signal_short)] <- 0
result$sig21 <- signal_long + signal_short



# EMA

result$EMA1<-EMA(p$Adjusted.Close,n=10,ratio=0.9)/p$Adjusted.Close
result$EMA2<-EMA(p$Adjusted.Close,n=16,ratio=0.9)/p$Adjusted.Close
result$EMA3<-EMA(p$Adjusted.Close,n=22,ratio=0.9)/p$Adjusted.Close
result$EMA4<-EMA(p$Adjusted.Close,n=10,ratio=0.84)/p$Adjusted.Close
result$EMA5<-EMA(p$Adjusted.Close,n=16,ratio=0.84)/p$Adjusted.Close
result$EMA6<-EMA(p$Adjusted.Close,n=22,ratio=0.84)/p$Adjusted.Close
result$EMA7<-EMA(p$Adjusted.Close,n=10,ratio=0.78)/p$Adjusted.Close
result$EMA8<-EMA(p$Adjusted.Close,n=16,ratio=0.78)/p$Adjusted.Close
result$EMA9<-EMA(p$Adjusted.Close,n=22,ratio=0.78)/p$Adjusted.Close

# SMA
result$SMA10<-SMA(p$Adjusted.Close,10)/p$Adjusted.Close
result$SMA16<-SMA(p$Adjusted.Close,16)/p$Adjusted.Close
result$SMA22<-SMA(p$Adjusted.Close,22)/p$Adjusted.Close


# bollinge
#take a short position if the Close is greater than the Upper Band 
#and then close the position out when it crosses the Average.

temp<-data.frame(BBands(p$Adjusted.Close,n=20,sd=2))
result$P_BBands_20_PctB<-temp$pctB  # Not in paper, adding anyway
result$P_BBands_20_up<-p$Adjusted.Close/temp$up
result$P_BBands_20_dn<-p$Adjusted.Close/temp$dn
signal_short <-ifelse((p$Adjusted.Close >temp$up),-1,0)
signal_long <-ifelse((p$Adjusted.Close <temp$dn),1,0)
signal_exit <- ifelse((p$Adjusted.Close >temp$mavg),1,-1)
result$P_BBands_20_signal<-signal_short+signal_long
result$P_BBands_20_exit<-signal_exit



temp<-data.frame(BBands(p$Adjusted.Close,n=26,sd=2))
result$P_BBands_26_PctB<-temp$pctB  # Not in paper, adding anyway
result$P_BBands_26_up<-p$Adjusted.Close/temp$up
result$P_BBands_26_dn<-p$Adjusted.Close/temp$dn
signal_short <-ifelse((p$Adjusted.Close >temp$up),-1,0)
signal_long <-ifelse((p$Adjusted.Close <temp$dn),1,0)
signal_exit <- ifelse((p$Adjusted.Close >temp$mavg),1,-1)
result$P_BBands_26_signal<-signal_short+signal_long
result$P_BBands_26_exit<-signal_exit


temp<-data.frame(BBands(p$Adjusted.Close,n=32,sd=2))
result$P_BBands_32_PctB<-temp$pctB  # Not in paper, adding anyway
result$P_BBands_32_up<-p$Adjusted.Close/temp$up
result$P_BBands_32_dn<-p$Adjusted.Close/temp$dn
signal_short <-ifelse((p$Adjusted.Close >temp$up),-1,0)
signal_long <-ifelse((p$Adjusted.Close <temp$dn),1,0)
signal_exit <- ifelse((p$Adjusted.Close >temp$mavg),1,-1)
result$P_BBands_32_signal<-signal_short+signal_long
result$P_BBands_32_exit<-signal_exit


#cbind(momentum(p$Adjusted.Close,1),p$Adjusted.Close)

# MOMENTUM 
result$mom18<-momentum(p$Adjusted.Close,18)/p$Adjusted.Close #  //////////////////////////////
result$mom24<-momentum(p$Adjusted.Close,24)/p$Adjusted.Close #  //////////////////////////////


temp<-momentum(p$Adjusted.Close,n=12)
temp1<-EMA(temp,n=12,ratio=0.75)
acc<-temp-lag(temp)
result$MomEMA1<-temp/temp1
result$Momacc1<-temp/temp1


temp<-momentum(p$Adjusted.Close,n=18)
temp1<-EMA(temp,n=18,ratio=0.75)
acc<-temp-lag(temp)
result$MomEMA2<-temp/temp1
result$Momacc2<-temp/temp1


temp<-momentum(p$Adjusted.Close,n=24)
temp1<-EMA(temp,n=24,ratio=0.75)
acc<-temp-lag(temp)
result$MomEMA3<-temp/temp1
result$Momacc3<-temp/temp1


#ROC (log return of adjusted close)
result$ROC1<- ROC(p$Adjusted.Close,1)
result$ROC2<- ROC(p$Adjusted.Close,2)
result$ROC3<- ROC(p$Adjusted.Close,3)
result$ROC4<- ROC(p$Adjusted.Close,4)
result$ROC5<- ROC(p$Adjusted.Close,5)
result$ROC10<- ROC(p$Adjusted.Close,10)
result$ROC16<- ROC(p$Adjusted.Close,16)
result$ROC22<- ROC(p$Adjusted.Close,22)


# MACD
temp<-data.frame(MACD(p$Adjusted.Close,nFast = 12,nSlow=18,nsig=9,percent = TRUE))
result$MACD1<-temp$macd
result$MACDS1<-temp$signal
temp<-data.frame(MACD(p$Adjusted.Close,nFast = 12,nSlow=18,nsig=9,percent = FALSE))
result$MACDR1 <- temp$macd / temp$signal


temp<-data.frame(MACD(p$Adjusted.Close,nFast = 12,nSlow=24,nsig=9,percent = TRUE))
result$MACD2<-temp$macd
result$MACDS2<-temp$signal
temp<-data.frame(MACD(p$Adjusted.Close,nFast = 12,nSlow=24,nsig=9,percent = FALSE))
result$MACDR2 <- temp$macd / temp$signal

temp<-data.frame(MACD(p$Adjusted.Close,nFast = 12,nSlow=30,nsig=9,percent = TRUE))
result$MACD3<-temp$macd
result$MACDS3<-temp$signal
temp<-data.frame(MACD(p$Adjusted.Close,nFast = 12,nSlow=30,nsig=9,percent = FALSE))
result$MACDR3 <- temp$macd / temp$signal

#stoch

temp<-data.frame(stoch(ts(p[,c("High","Low","Close")]),nFastK=12,nSlowD = 3))
result$fastk1<-temp$fastK
result$fastD1<-temp$fastD
result$slowD1<-temp$slowD
result$slowRatio1<- temp$fastK/temp$slowD
result$fastRatio1<- temp$fastK/temp$fastD

temp<-data.frame(stoch(ts(p[,c("High","Low","Close")]),nFastK=18,nSlowD = 3))
result$fastk2<-temp$fastK
result$fastD2<-temp$fastD
result$slowD2<-temp$slowD
result$slowRatio2<- temp$fastK/temp$slowD
result$fastRatio2<- temp$fastK/temp$fastD


temp<-data.frame(stoch(ts(p[,c("High","Low","Close")]),nFastK=24,nSlowD = 3))
result$fastk24<-temp$fastK
result$fastD24<-temp$fastD
result$slowD24<-temp$slowD
result$slowRatio24<- temp$fastK/temp$slowD
result$fastRatio24<- temp$fastK/temp$fastD


result$WILL<-as.numeric(WPR(ts(p[,c("High","Low","Close")]),n=14))

temp<-data.frame(MFI(ts(p[,c("High","Low","Close")]),ts(p[,"Volume"]),n=14))
result$MFI<-temp$mfi

#stoch
temp <- data.frame(SMI(p[,c("High","Low","Adjusted.Close")]))
result$stochSMI<-temp$SMI
result$stochsignal<-temp$signal

temp<-chaikinVolatility(p[,c("High","Low")],n=10)
result$CHV<-temp$EMA

#result$CHV1<-temp$EMA/lag(temp$EMA,10)-1

#result$GKYZ<-as.numeric(data.frame(volatility(p[,c("Open","High","Low","Close")],calc = "gk.yz")))


temp<- chaikinVolatility(p[,c("High","Low","Adjusted.Close")],n = 10)
result$chaikinVolatility10<-temp$EMA
result$ChaikinAccumulation <- chaikinAD(ts(p[,c("High","Low","Adjusted.Close")]),volume=p[,c("Volume")])
result$OnBalanceVolume <- OBV(p$Adjusted.Close,p$Volume)


result$open_ad<-p$Open/p$Adjusted.Close
result$high_ad<-p$High/p$Adjusted.Close
result$low_ad<-p$Low/p$Adjusted.Close

result<-result[-1:-50,]
return(result)
}
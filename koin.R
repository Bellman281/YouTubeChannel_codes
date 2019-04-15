require(jsonlite)

nam<-names(koin)
BTC<-grep("BTC", nam )

repeat{ 
  koin<-data.frame(fromJSON(url))
  BTC.hist<-koin[,BTC]
  BTC.hist$BTC.current<-as.numeric(as.character(BTC.hist$BTC.current))
  BTC.hist$BTC.timestamp<-as.POSIXct(BTC.hist$BTC.timestamp,origin="1970-01-01")
  
  #BTC.hist.df<-BTC.hist
  BTC.hist.df<-rbind(BTC.hist.df,BTC.hist)
  BTC.hist.df$BTC.current<-as.numeric(as.character(BTC.hist.df$BTC.current))
  
  #BTC.hist.df$BTC.current<-as.numeric(BTC.hist.df$BTC.current)
  BTC.hist.df$BTC.timestamp<-as.POSIXct(BTC.hist.df$BTC.timestamp,origin="1970-01-01")
  #BTC.hist.df<-BTC.hist.df[-8,]
  
  plot(BTC.hist.df$BTC.timestamp,BTC.hist.df$BTC.current,"b",xlab="TIME",ylab ="BITCOIn" )
  Sys.sleep(60) 
  
}



url<-"https://koineks.com/ticker"
# str(BTC.hist)
koin<-data.frame(fromJSON(url))
nam<-names(koin)
BTC<-grep("BTC", nam )
BTC.hist<-koin[,BTC]
BTC.hist$BTC.current<-as.numeric(as.character(BTC.hist$BTC.current))
BTC.hist$BTC.timestamp<-as.POSIXct(BTC.hist$BTC.timestamp,origin="1970-01-01")

#BTC.hist.df<-BTC.hist
BTC.hist.df<-rbind(BTC.hist.df,BTC.hist)
BTC.hist.df
BTC.hist.df$BTC.current<-as.numeric(as.character(BTC.hist.df$BTC.current))

#BTC.hist.df$BTC.current<-as.numeric(BTC.hist.df$BTC.current)
BTC.hist.df$BTC.timestamp<-as.POSIXct(BTC.hist.df$BTC.timestamp,origin="1970-01-01")
BTC.hist.df
#BTC.hist.df<-BTC.hist.df[-7,]

plot(BTC.hist.df$BTC.timestamp,BTC.hist.df$BTC.current,"b",xlab="TIME",ylab ="BITCOIn" )



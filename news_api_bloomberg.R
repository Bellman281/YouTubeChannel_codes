library(jsonlite)
NEWSAPI_KEY <- "7b5acedb77bd4a41a830929572cb6072"

url<-paste0("https://newsapi.org/v2/top-headlines?sources=bloomberg&apiKey=",NEWSAPI_KEY)

df<-data.frame(fromJSON(url))
print(df)
df$articles.title
df$articles.description



#everything from bloomberg
NEWSAPI_KEY <- "bdfd5239961045b19c59e5d43f6b43df"

Symbols1<-read.csv("nasdaq_tick.csv")
stock="AHT"
stock.name<-Symbols1[Symbols1$ACT.Symbol ==stock,]$Security.Name
#stock.name <- Clean_String(stock.name)
stock.name<-paste(stock.name,c("+") ,sep = "", collapse = "")

search="Abbott+Laboratories"
search=stock
url<-paste0("https://newsapi.org/v2/everything?q=",search,"&sources=bloomberg&apiKey=",NEWSAPI_KEY)

df<-data.frame(fromJSON(url))
df$articles.title
df$articles.description


setwd("G:/Documents")
NEWSAPI_KEY <- "7b5acedb77bd4a41a830929572cb6072"
library(jsonlite)


gre<-data.frame(read.csv("gre.csv",header = T))
gre$WORD<-as.character(gre$WORD)
gre$DEFINITION<-as.character(gre$DEFINITION)
str(gre)

vocab<-gre[8,1]
stock="news"
search<-paste(stock,c("+") ,vocab, sep = "", collapse = "")

print(gre[8,])
#search<-"Abbott+Laboratories"
# source="national-geographic" 
 #source="bloomberg" 
 source="ESPN" 
 
 search=vocab
 
 url<-paste0("https://newsapi.org/v2/everything?q=",search,"&sources=",source,"&apiKey=",NEWSAPI_KEY)

  
 df<-data.frame(fromJSON(url))
 df$articles.title
 
 newurl2<-df$articles.url[4]
 html <- getURL(newurl2)

 
 # parse html
 doc = htmlParse(html, asText=TRUE)
 plain.text <- xpathSApply(doc, "//p", xmlValue);plain.text
 
 print(vocab)
 print(plain.text)
 #cat(paste(plain.text, collapse = "\n"))
 
 vocab<-paste0(vocab," ")
 plain.text[grep(vocab, plain.text )]
 i=0
 vlist<-vector()

  for (t in gre[,1])
    { 
      t1<-paste0(" ",t," ")
 
               s=grep(t1, plain.text )
               
               ifelse ((s> 0)
                       ,  {
                         print(paste0( "GRE WORD :  ", t1))
                         print(paste0( "Definition  :  ", gre[gre$WORD==t,2]))
                         print(paste0( "Source  :  ", source))
                         print(newurl2)
                         
                         print(  gsub("[\r\n]", "",plain.text[grep(t1, plain.text ,fixed = TRUE)][1]))
              
                         i=i+1
                         vlist<-c(t,vlist)
                       }
               )
         }
 
 plain.text[grep(" apt ", plain.text,fixed = TRUE)][1]
 gre[gre$WORD %in% vlist,]
 pagerank<-length(vlist)
 
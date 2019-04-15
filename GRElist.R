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

url<-paste0("https://newsapi.org/v2/everything?q=",search,"&sources=bloomberg&apiKey=",NEWSAPI_KEY)

df<-data.frame(fromJSON(url))
df$articles.title
df$articles.description
df$articles.source
newurl2<-df$articles.url[1]


#library(rvest)

library(RCurl)
library(XML)
print(newurl2)
 



html <- getURL(newurl2,.opts = list(ssl.verifypeer = FALSE))
 
 
 # parse html
 doc = htmlParse(html, asText=TRUE)
 plain.text <- xpathSApply(doc, "//p", xmlValue)
 #cat(paste(plain.text, collapse = "\n"))

 vocab<-paste0(vocab," ")
plain.text[grep(vocab, plain.text )]
i=0
vlist<-vector()
for (t in gre[,1])
 { t1<-paste0(" ",t," ")
 
  s=grep(t1, plain.text )
  
  ifelse ((s> 0)
   ,  {print( paste0( "GRE vocab ", t1, " Text found : ",plain.text[grep(t1, plain.text ,fixed = TRUE)][1]))
   
     i=i+1
     vlist<-c(t,vlist)
     }
     )
}

plain.text[grep(" apt ", plain.text,fixed = TRUE)][1]
gre[gre$WORD %in% vlist,]
pagerank<-length(vlist)
#translate
library(translate)
gkey<-"AIzaSyDrOuJDd27f6hrNlZFPfYf2SvwaDcEwUZM"
vocab<-"abacus"
url=paste0("https://translation.googleapis.com/language/translate/v2?q=",vocab,"&target=fa&key=",gkey)
url

set.key(gkey)
translate(vocab, source="en", target="fa")[[1]]
str(kk)





#oxford dictionary.
#install.packages("request")
library(jsonlite)
library(request)
app_key	<- c("ee8bfbb0ff2a95aea50a9068e6bb90ff")
app_id<- c("b56f4605")
language = 'en'
word_id = vocab
#word_id="writing"
url = paste0('https://od-api.oxforddictionaries.com:443/api/v1/inflections/' , language , '/' , word_id)


r<-GET(url, add_headers(Accept = "application/json",app_id = "b56f4605", 
                        app_key = "ee8bfbb0ff2a95aea50a9068e6bb90ff"))

list.1<-fromJSON(content(r, "text"), simplifyVector = FALSE)$results[[1]]$lexicalEntrie

#inflection of
print(list.1[[1]]$inflectionOf[[1]]$text)
#singular/plural
print(list.1[[1]]$`grammaticalFeatures`[[2]]$`text`)
#lexical category
print(list.1[[1]]$lexicalCategory)

#lexical 
print(list.1[[2]]$`grammaticalFeatures`[[1]]$`text`)




#synonyms

url = paste0('https://od-api.oxforddictionaries.com:443/api/v1/entries/' , language , '/' , word_id)


r<-GET(url, add_headers(Accept = "application/json",app_id = "b56f4605", 
                        app_key = "ee8bfbb0ff2a95aea50a9068e6bb90ff"))


list.2<-fromJSON(content(r, "text"), simplifyVector = FALSE)$results[[1]]

#origin
print(list.2$lexicalEntries[[1]]$`entries`[[1]]$`etymologies`[[1]])

#audiofile
print(list.2$lexicalEntries[[1]]$pronunciations[[1]]$`audioFile`)

download.file (list.2$lexicalEntries[[1]]$pronunciations[[1]]$`audioFile`,"abacus_gb_2.mp3")	

print(list.2$type)

#definition
print(list.2$lexicalEntries[[1]]$`entries`[[1]]$senses[[1]]$`definitions`[[1]])

#gre definition
gre[1,2]




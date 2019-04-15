song.l<-read.csv("songdata.csv")
song.l$artist<-as.character(song.l$artist)
song.l$song<-as.character(song.l$song)
song.l$link<-NULL
song.l$text<-as.character(song.l$text)

t1<-" darkness "
vocab<-" adequate "
s=grep(vocab,song.l$text  )
s
song.l$text[s]
song.l$artist[s]
song.l$song[s]

i=0
vlist<-vector()
for (t in gre[,1])
{ t1<-paste0(" ",t," ")

s=grep(vocab,song.l$text  )

s1=s[1]
song.l$text[22]

ifelse ((s> 0)
        ,  {print( paste0( "GRE vocab ", t1, " Text found : ",
                           song.l$text[s[1]]))
          
          i=i+1
          vlist<-c(t,vlist)
        }
)
}

gre[song.l$WORD %in% vlist,]
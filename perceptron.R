 x1 <- runif(100,-1,1)
 x2 <- runif(100,-1,1)
 x <- cbind(x1,x2)
 Y <- ifelse(x2>0.7+x1,+1,-1)
 plot(x,pch=ifelse(Y>0,"+","-"), xlim=c(-1,1),ylim=c(-1,1),cex=2)
 abline(0.7,1)
 
 
 distance.from.plane = function(z,w,b) {
  sum(z*w) + b
 }
  classify.linear = function(x,w,b) {
    distances = apply(x, 1, distance.from.plane, w, b)
    return(ifelse(distances < 0, -1, +1))
  }
  
  
  classify.linear(x,c(-1,1)/sqrt(2),-sqrt(2)/4)
  Y
  
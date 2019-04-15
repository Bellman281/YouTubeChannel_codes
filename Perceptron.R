iris_sub=iris[1:100, c(1, 3, 5)]
names(iris_sub)=c("sepal", "petal", "species") 

iris_sub <- iris_sub[sample(nrow(iris_sub)),]
head(iris_sub) 


epoch=1000


indx<-round(0.81*dim(iris_sub)[1])

trainset= iris_sub[1:indx,]
testset = iris_sub[(indx+1):nrow(iris_sub),]
X = cbind(1,trainset$sepal, trainset$petal)
Y = ifelse(trainset$species == "setosa", +1, -1)

w = vector(length = ncol(X))
Error = vector(length = epoch)

# initialize weights
w_initial=c(1,1,1)
learning.rate=0.1

w = w_initial
  
   for (iteration in 1:epoch)
   {
     for (i in 1: nrow(X))
     {

       yc=ifelse(sum(X[i,]*w) < 0, -1, +1)
       if (Y[i] != yc) 
         {
           w = w - learning.rate * (yc- Y[i])*X[i,]
           print(w)
          }
     }
     output=ifelse((X %*% w) < 0, -1, +1) -Y
     
     Error[iteration]=sum(output*output)
   }
     
plot(Error[1:50])
  
  
  # Prediction rate
  X = cbind(1,testset$sepal, testset$petal)
  Y = ifelse(testset$species == "setosa", +1, -1)
  
  yc=vector()
  for (i in 1: nrow(X))
  {
    yc[i]=ifelse(sum(X[i,]*w) < 0, -1, +1)
  }
  
   cbind(yc,Y,yc-Y)
  
   #misclassification rate
   sum(  ifelse((yc-Y) == 0, 0, +1))/length(Y)
   

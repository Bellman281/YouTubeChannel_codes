df<-data.frame(read.csv("data.csv"))
f.con=matrix(ncol=23,nrow=5)

df<-df[,-1]


m=3
s<-ncol(df)-m
N = nrow(df) # the number of DMUs is equal to number of rows of data matrix


m;s;


#second column
inputs = data.frame(df[,1:s] )# input variable at second column of the data matrix
names(inputs)<-NA
outputs = data.frame(df[,(s+1):(s+m)]) # output variables

names(outputs)<-NA
#print(outputs)

i=1
for (i in 1:N) {
  
  
  f.rhs = c(rep(0,s),unlist(unname(outputs[i,])))
  print(f.rhs)
  #0 45 46 23
  

  f.dir = c(rep(">=",s),rep(">=",m))
  print(f.dir)
  #"=>" "=>" "=>" "=>"
  
  
  f.obj = c(1, rep(0,N))
  print(f.obj)
  # 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  
  con1 = c(unlist(unname(inputs[i,])),-1*unlist(unname(inputs[,1])))
  
  print(con1)
  con2 = c(0,as.numeric(outputs[,1]))
  print(con2)
  con3 = c(0,as.numeric(outputs[,2]))
  print(con3)
  con4 = c(0,as.numeric(outputs[,3]))
  print(con4)
  
  f.con = rbind(con1, con2, con3, con4)
  print(f.con)
  
  results = lp('min', as.numeric(f.obj), f.con, f.dir, f.rhs, scale=0, compute.sens=TRUE)
  #print(results)
  
  if (i==1) {
    weights = results$solution              # tet , landa
    effcrs  = results$objval                # teta
    #lambdas = results$duals[seq(1,N)]      # v, u
  } else {
    weights = rbind(weights, results$solution)  
    effcrs  = rbind(effcrs , results$objval)    
    #lambdas = rbind(lambdas, results$duals[seq(1,N)] )    
    
  }
  
}

Es = data.frame(effcrs) 

Es

teta_0<-unname(Es[,1]);teta_0

str(teta_0[,1])

f.con1=matrix(ncol=22,nrow=7)

f.obj1<-c(rep(1,m),rep(1,s))
f.dir1<-c(rep("=",m+s))
#X

x_0<-teta_0*df[,1]
x_0


m=1
s<-ncol(df)-m
N = nrow(df)

weights1<-NA

for (j in 1:N) {
  
  for(i in 1:m)
  {
    f.con1[i,1:(nrow(df)+m+s)]<-c(df[,i],rep(1,m),rep(0,s))
  }
  #show
  #f.con1[1:m,]
  
  
  for(r in (m+1):(s+m))
  {
    f.con1[r,1:(nrow(df)+m+s)]<-c(df[,r],rep(0,m),rep(-1,s))
  }
  
  f.con1[5,1:(nrow(df)+m+s)]<-c(rep(1,N),rep(0,m),rep(0,s))
  f.con1[6,1:(nrow(df)+m+s)]<-c(rep(0,N),rep(1,m),rep(0,s))
  f.con1[7,1:(nrow(df)+m+s)]<-c(rep(0,N),rep(0,m),rep(1,s))
  
 # print(f.con[1:(m+s),])
  
  f.rhs1 = as.numeric(c(x_0[j],outputs[j,],rep(0,3)))
  #print(f.rhs1)
  #0 45 46 23
  
  
  results1 = lp('max', as.numeric(f.obj1), f.con1, f.dir1, f.rhs1, scale=0, compute.sens=TRUE)
  #print(c(results1,results1$solution))
  print(results1$objval)
  
  if (j==1) {
    weights1 = results1$solution              # tet , landa
    effcrs1  = results1$objval                # teta
    #lambdas = results$duals[seq(1,N)]      # v, u
  } else {
    weights1 = rbind(weights1, results1$solution)  
    effcrs1 = rbind(effcrs1 , results1$objval)    
    #lambdas = rbind(lambdas, results$duals[seq(1,N)] )    
    
  }
  
}
weights1
results1$objval


results1$solution

print(effcrs1)
Es1 = data.frame(effcrs1) 
Es1

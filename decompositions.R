install.packages("Matrix")
library(Matrix)
install.packages("matrixcalc")
library("matrixcalc")

I <- diag( 1, 5 )

x<-diag( 5)
 
y<-diag(runif(5,0,12))
x;y;
 
is.positive.definite( I )

diag(10, 3, 4) # guess what?

A <- matrix( seq( 1, 9, 1 ), nrow=3, byrow=TRUE )
is.positive.definite(A)

#eigen decomposition
diag(I)<-eigen(y)$values

eigen(y)$vectors%*%I%*%solve(eigen(y)$vectors)

solve(y)
(eigen(A)$values)


chol(y)
lu(y)
upper.triangle(I)

upper.triangle( y )

# L U decomposition
lu.decomposition(y)$L%*%lu.decomposition(y)$U

p<-lu.decomposition (A)
chol(as.integer(p))
#Error in lu.decomposition(A) : argument A is a singular matrix
matrix.rank (A)  #matrix.rank =70

# QR decomposition
qr.X(qr(A))
qr.Q(qr(A))%*%qr.R(qr(A))



s<-matrix(1:25,5)
s[lower.tri(s)] = t(s)[lower.tri(s)]

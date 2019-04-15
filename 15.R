#
# Kalman Filtering in R
#
#           See web site for Shumway & Stoffer for documents

load("tsa3.rda")

# --- Notation for model is as in textbook
#
#        State: xt =  F xt-1 + wt, Var(w) = Q
#        Obs  : yt =  H xt   + vt, Var(v) = R
#
#  Level 0 (no exogenous inputs)
#
# filtering:  Kfilter0(n,y,H,mu.0,sigma.0,F, sqrtQ, sqrtR)
# smoothing:  Ksmooth0(   "                              )


####################################################
#                                                  #
#  Apply to an AR(2) process, Akaike form          #
#                                                  #
####################################################

n       <- 200
phi     <-c(1.2,-0.8)
p       <- length(phi)
sigma.w <- 150;

psi <- c(1,ARMAtoMA(ar=phi, lag.max=p-1)) 
yt  <- arima.sim(list(ar=phi), n, sd=sigma.w)

acf2(yt)  

# --- use Akaike's predictor form with phi on the bottom, in reverse order
F  <- matrix(c(0,1,phi[p:1]),nrow=2,ncol=2,byrow=TRUE); F
G  <- psi[0:1 + 1]; G                      # zero-based origin would be handy for this
H  <- matrix(c(1,0), nrow=1, ncol=2); H    # not minimal dimension

# --- upper triangular factors
cQ <- sigma.w * matrix(c(psi,0,0),nrow=2,ncol=2,byrow=TRUE) # rank 1
cR <- 0

# --- Initialize: need 2x2 covariance matrix, so head for Y-W
# Mathematica version
# Solve[ #  {g0 == p1 g1 + p2 g2 + s2, #   g1 == p1 g0 + p2 g1, #   g2 == p1 g1 + p2 g0}, #  {g0, g1, g2}]

a <-matrix(c(1     ,-phi[1] ,-phi[2],  
            -phi[1],1-phi[2],  0    ,
            -phi[2], -phi[1],  1)    ,nrow=3,ncol=3,byrow=TRUE)
b <-c(sigma.w^2,0,0) 
gamma = solve(a,b); gamma           #  1.125000e+05  7.500000e+04 -1.665335e-11  

mu.0    <- rep(0,2)
sigma.0 <- chol(matrix(c(gamma[0+1],gamma[1+1],gamma[1+1],gamma[0+1]-sigma.w^2),nrow=2,ncol=2))
sigma.0

# --- Run the filter
kf <- Kfilter0(n,yt, H, mu.0,sigma.0, F, cQ, cR)
names(kf)

# --- Two results: one-step ahead prediction (t|t-1) and filtered (t|t)
##  notes:
## mu_t|t-1=ks$xp,  P_t|t-1=ks$Pp,   "p" for prediction
## mu_t|t=ks$xf,    P_t|t=ks$Pf,     "f" for filter
## mu_t|n=ks$xs,    P_t|n=ks$Ps,     "s" for smoother

dim(kf$xp)       # 2 1 n

cbind(kf$xp[,,51], kf$xp[,,52], kf$xp[,,53])
cbind(kf$xf[,,50], kf$xf[,,51], kf$xf[,,52])

# one step ahead predictions
predicted <- as.vector(kf$xp[1,1,])
plot(yt, type="o", col="seagreen"); lines(predicted,col="tan");

# filtered (which match the data because the first element of state is data)
filtered <- as.vector(kf$xf[1,1,]); 
lines(filtered,col="brown")

plot(filtered-yt); 

# --- One step variance is constant very quickly: why?  Whats a short-cut calculation?
kf$Pp[1,1,]

# --- What does smoothing do?  Not happy since first element of state = data
ks <- Ksmooth0(n,yt, H, mu.0,sigma.0, F, cQ, cR)
smoothed <- as.vector(ks$xs[1,1,]); 

plot(yt, type="o", col="seagreen"); lines(filtered,col="brown");
lines(smoothed)


####################################################
#                                                  #
#  Apply to an AR(2) process with additive noise   #
#                                                  #
####################################################

n       <- 200
phi     <-c(1.2,-0.8)
p       <- length(phi)
psi     <- c(1,ARMAtoMA(ar=phi, lag.max=p-1))
sigma.w <- 150;
sigma.n <- 100;  # compare to sd of yt, not to sd of wt

# x_t process is ar2
xt      <- arima.sim(list(ar=phi), n, sd=sigma.w)
# y_t observed adds noise
yt      <- xt + sigma.n*rnorm(n)

# --- How does noise affect the correlations?
acf2(yt); ay <- acf(yt, plot=FALSE); py <- pacf(yt, plot=FALSE)  # observed
acf2(xt); ax <- acf(xt, plot=FALSE); px <- pacf(xt, plot=FALSE)  # hidden state
par(mfrow=c(1,1))

gamma[(1:2)+1]/gamma[0 + 1]; 
cbind(2:10,ax$acf[2:10], ay$acf[2:10], px$acf[1:9], py$acf[1:9])  # skip 1's in acf


# --- mixed models look pretty good...
maxp <- maxq <- 4
aic.table <- matrix(NA, nrow=1+maxp, ncol=1+maxq)
rownames(aic.table)<-paste("p",0:maxp,sep="=")     # nice labels help!
colnames(aic.table)<-paste("q",0:maxq,sep="=")

for (p in 0:maxp) for (q in 0:maxq) {
	aic.table[1+p,1+q] <- arima(yt, order=c(p,0,q), include.mean=TRUE, method="ML")$aic
}
round(aic.table,digits=0)


# --- Only change in parameters
cR <- sigma.n

# --- Run the smoother (happy now since avoid singularity; not sure why)
ks <- Ksmooth0(n,yt, H, mu.0,sigma.0, F, cQ, cR)
names(ks)

# one step ahead predictions of underlying state
predicted <- as.vector(ks$xp[1,1,])
plot(yt, type="p", col="seagreen"); lines(predicted,col="tan");

# filtered 
filtered <- as.vector(ks$xf[1,1,]); 
lines(filtered,col="brown")

# smoothed 
smoothed <- as.vector(ks$xs[1,1,]); 
lines(filtered,col="red")

plot (yt-predicted, type="p", col="gray")
points(yt-filtered, type="p", col="black")
points(yt-smoothed, type="p", col="pink")

# function to put correlations on the lower half of scatterplot matrix
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.5/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r) # size of text prop to corr
}

pairs(cbind(predicted,filtered,smoothed,xt), lower.panel=panel.cor)


####################################################
#                                                  #
#  Getting log-like from filter                    #
#                                                  #
####################################################

# input data, m_{t|t-1}, p_{t|t-1}; returns -2 log like
log.like <- function(yt, yhat, var) {
	dev <- yt - yhat;
	n <- length(yt)
	sum(log(var)) + sum(((yt-yhat)^2)/var)
}

# input the correct values from the filter (depends on how state is defined)
log.like(yt,kf$xp[1,1,],kf$Pp[1,1,])

# useful subroutine
phi.to.covariances <- function(phi, sigma.w) {
	a <-matrix(c(1     ,-phi[1] ,-phi[2],  
                -phi[1],1-phi[2],  0    ,
                 -phi[2], -phi[1],  1),nrow=3,ncol=3,byrow=TRUE)
	b <-c(sigma.w^2,0,0) 
	solve(a,b)
}

# -2 log like from parameters, data
ar2.log.like <- function(yt,phi, sigma.w, sigma.n) {
	n <- length(yt)
	p <- length(phi)
	# --- arma properties
	psi <- c(1,ARMAtoMA(ar=phi, lag.max=p-1))
	gamma <- phi.to.covariances(phi,sigma.w)
	# cat("Covariance terms:  phi=", phi[1],phi[2], "->", gamma[0],gamma[1],sigma.w^2,"\n")
	# --- components of filter
	F  <- matrix(c(0,1,phi[p:1]),nrow=2,ncol=2,byrow=TRUE);
	G  <- psi[1:p]          
	H  <- matrix(c(1,0), nrow=1, ncol=2)
	cQ <- sigma.w * matrix(c(psi,0,0),nrow=2,ncol=2,byrow=TRUE) # rank 1
	cR <- sigma.n
	# --- initialize
	mu.0    <- rep(0,length(phi))
	sigma.0 <- chol(matrix(c(gamma[0+1],gamma[1+1],gamma[1+1],gamma[0+1]-sigma.w^2),nrow=2,ncol=2))
	# --- run filter
	kf <- Kfilter0(n,yt, H, mu.0,sigma.0, F, cQ, cR)
	log.like(yt,kf$xp[1,1,],kf$Pp[1,1,])
}

# draw a graph of the function
toplot <- matrix(NA, nrow=14, ncol=2)
x<- 0.7
for(i in 1:14) {
	toplot[i,1] <- x;
	toplot[i,2] <- ar2.log.like(yt,c(x,phi[2]),sigma.w, sigma.n)
	x <- x + 0.05
 }
plot(toplot[,1], toplot[,2],type="o")

toplot <- matrix(NA, nrow=12, ncol=2)
x<- -0.95
for(i in 1:12) {
	toplot[i,1] <- x;
	toplot[i,2] <- ar2.log.like(yt,c(phi[1],x),sigma.w, sigma.n)
	x <- x + 0.05
 }
plot(toplot[,1], toplot[,2],type="o")


# wiggle parameters around
ar2.log.like(yt,phi-.01,1.02*sigma.w,  0    )
ar2.log.like(yt,phi,sigma.w,sigma.n)


max.like.ar2 <- function(yt) {
	# --- initialize with ls
	init.fit <- ar(yt, aic=FALSE, method="ols", order.max=2)
	phi <- as.vector(init.fit$ar)
	sigma.w <- sqrt(init.fit$var.pred)
	sigma.n <- 1
	# --- call to optim binds data; variances on log scale
	ll <- function(parms) { 
		ll <- ar2.log.like(yt, parms[1:2], exp(parms[3]), exp(parms[4]))
		cat("-2 log like = ", ll, " @ ", parms,"\n")
		save[save.index,] <<- c(parms,ll); save.index <<- save.index+1
		ll
	 }
	optim(c(phi,log(sigma.w), sigma.n), ll, method="BFGS", hessian=TRUE,
		control=list(trace=TRUE, maxit=20, parscale=c(0.1,0.1,1,1)))
}


# place to save intermediate results
save <- matrix(0,nrow=200,ncol=4+1)
colnames(save) <- c("phi1", "phi2", "sigma.w", "sigma.n", "-2loglike")
save.index <- 1

results <- max.like.ar2(yt)

plot(save[1:save.index,"phi1"], save[1:save.index,"phi2"])
plot(save[1:save.index,"phi1"], save[1:save.index,"-2loglike"])
plot(save[1:save.index,"phi2"], save[1:save.index,"-2loglike"])



####################################################
#                                                  #
#  ML for an AR(1) process: S&S example            #
#                  Example 6.6                     #
#                                                  #
####################################################

# --- Generate Data
set.seed(999)
num = 100
N = num+1
x = arima.sim(n=N, list(ar = .8, sd=1))
y = ts(x[-1] + rnorm(num,0,1))           # plus additive noise

# --- Initial Estimates for ML
u = ts.intersect(y, lag(y,-1), lag(y,-2)) 
varu = var(u)
coru = cor(u) 
phi = coru[1,3]/coru[1,2]             
q = (1-phi^2)*varu[1,2]/phi   
r = varu[1,1] - q/(1-phi^2) 
(init.par = c(phi, sqrt(q), sqrt(r)))    # bundle parameters

# --- Function to evaluate the likelihood 
Linn=function(para){
  phi = para[1]; sigw = para[2]; sigv = para[3]   
  Sigma0 = (sigw^2)/(1-phi^2); Sigma0[Sigma0<0]=0   
  kf = Kfilter0(num,y,1,mu0=0,Sigma0,phi,sigw,sigv)
  return(kf$like)   
  }
  
# --- Estimation  
(est = optim(init.par, Linn, gr=NULL, method="BFGS", hessian=TRUE, control=list(trace=1,REPORT=1)))      
SE = sqrt(diag(solve(est$hessian)))
cbind(estimate=c(phi=est$par[1],sigw=est$par[2],sigv=est$par[3]), SE)

























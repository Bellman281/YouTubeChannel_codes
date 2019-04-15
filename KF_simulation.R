set.seed(123)
n = 100
w = rnorm(n+1,0,1)
v = rnorm(n,0,1)
x = cumsum(w) # state: x[0], x[1],..., x[50]
y = x[-1] + v # obs: y[1],..., y[50]
# filter and smooth (Ksmooth0 does both)
library(astsa)
ks = Ksmooth0(n, y, A=1, mu0 = 0, Sigma0=1, Phi=1, cQ=1, cR=1)

A=1; x0 = 0; Sigma0=1; Phi=1; cQ=1; cR=1;

Q=t(cQ)%*%cQ
R=t(cR)%*%cR
# y is n by q  (time=row series=col)
# A is a q by p matrix
# R is q by q
# x0 is p by 1
# Sigma0, Phi, Q are p by p
Phi=as.matrix(Phi)
pdim=nrow(Phi)    
y=as.matrix(y)
qdim=ncol(y)
xp=array(NA, dim=c(pdim,1,n))         # xp=x_t^{t-1}          
Pp=array(NA, dim=c(pdim,pdim,n))      # Pp=P_t^{t-1}
xf=array(NA, dim=c(pdim,1,n))         # xf=x_t^t
Pf=array(NA, dim=c(pdim,pdim,n))      # Pf=x_t^t
innov=array(NA, dim=c(qdim,1,n))      # innovations
sig=array(NA, dim=c(qdim,qdim,n))     # innov var-cov matrix
# initialize 
x00=as.matrix(x0, nrow=pdim, ncol=1)
P00=as.matrix(Sigma0, nrow=pdim, ncol=pdim)


xp[,,1]=Phi%*%x00
Pp[,,1]=Phi%*%P00%*%t(Phi)+Q


sigtemp=A%*%Pp[,,1]%*%t(A)+R
sig[,,1]=(t(sigtemp)+sigtemp)/2     # innov var - make sure it's symmetric
siginv=solve(sig[,,1])          
#kamlman
K=Pp[,,1]%*%t(A)%*%siginv
innov[,,1]=y[1,]-A%*%xp[,,1]


xf[,,1]=xp[,,1]+K%*%innov[,,1]
Pf[,,1]=Pp[,,1]-K%*%A%*%Pp[,,1]
sigmat=as.matrix(sig[,,1], nrow=qdim, ncol=qdim)
like = log(det(sigmat)) + t(innov[,,1])%*%siginv%*%innov[,,1]   # -log(likelihood)
########## start filter iterations ###################
for (i in 2:n){
  if (n < 2) break
  xp[,,i]=Phi%*%xf[,,i-1]
  Pp[,,i]=Phi%*%Pf[,,i-1]%*%t(Phi)+Q
  sigtemp=A%*%Pp[,,i]%*%t(A)+R
  sig[,,i]=(t(sigtemp)+sigtemp)/2     # innov var - make sure it's symmetric
  siginv=solve(sig[,,i])              
  K=Pp[,,i]%*%t(A)%*%siginv
  innov[,,i]=y[i,]-A%*%xp[,,i]
  xf[,,i]=xp[,,i]+K%*%innov[,,i]
  Pf[,,i]=Pp[,,i]-K%*%A%*%Pp[,,i]
  sigmat=as.matrix(sig[,,i], nrow=qdim, ncol=qdim)
  like= like + log(det(sigmat)) + t(innov[,,i])%*%siginv%*%innov[,,i]
}
like=0.5*like
kf=list(xp=xp,Pp=Pp,xf=xf,Pf=Pf,like=like,innov=innov,sig=sig,Kn=K)



    pdim=nrow(as.matrix(Phi))  
    xs=array(NA, dim=c(pdim,1,n))      # xs=x_t^n
    Ps=array(NA, dim=c(pdim,pdim,n))   # Ps=P_t^n
    J=array(NA, dim=c(pdim,pdim,n))    # J=J_t
    xs[,,n]=kf$xf[,,n] 
    Ps[,,n]=kf$Pf[,,n]
    for(k in n:2)  {
      J[,,k-1]=(kf$Pf[,,k-1]%*%t(Phi))%*%solve(kf$Pp[,,k])
      xs[,,k-1]=kf$xf[,,k-1]+J[,,k-1]%*%(xs[,,k]-kf$xp[,,k])
      Ps[,,k-1]=kf$Pf[,,k-1]+J[,,k-1]%*%(Ps[,,k]-kf$Pp[,,k])%*%t(J[,,k-1])
    }
    # and now for the initial values because R can't count backward to zero
    x00=x0
    P00=Sigma0
    J0=as.matrix((P00%*%t(Phi))%*%solve(kf$Pp[,,1]), nrow=pdim, ncol=pdim)
    x0n=as.matrix(x00+J0%*%(xs[,,1]-kf$xp[,,1]), nrow=pdim, ncol=1)
    P0n= P00 + J0%*%(Ps[,,1]-kf$Pp[,,1])%*%t(J0)
    list(xs=xs,Ps=Ps,x0n=x0n,P0n=P0n,J0=J0,J=J,xp=kf$xp,Pp=kf$Pp,xf=kf$xf,Pf=kf$Pf,like=kf$like,Kn=kf$K)







library(nlme)   # loads package nlme
# Generate data (same as Example 6.6)
set.seed(999); n = 100
x = arima.sim(n=n+1, list(ar = 0.7), sd=1)
y = ts(x[-1] + rnorm(n,0,1))
# Initial Estimates
u = ts.intersect(y, lag(y,-1), lag(y,-2))
varu = var(u)
coru = cor(u)
phi = coru[1,3]/coru[1,2]
q = (1-phi^2)*varu[1,2]/phi
r = varu[1,1] - q/(1-phi^2)
# EM procedure - output not shown
(em = EM0(n, y, A=1, mu0=0, Sigma0=2.8, Phi=phi, cQ=sqrt(q), cR=sqrt(r), max.iter=100, tol=.00001))
# Standard Errors  (this uses nlme)
phi = em$Phi; cq = sqrt(em$Q); cr = sqrt(em$R)
mu0 = em$mu0; Sigma0 = em$Sigma0
para = c(phi, cq, cr)
Linn = function(para){  # to evaluate likelihood at estimates
  kf = Kfilter0(n, y, 1, mu0, Sigma0, para[1], para[2], para[3])
  return(kf$like)  }
emhess = fdHess(para, function(para) Linn(para))
SE = sqrt(diag(solve(emhess$Hessian)))
# Display Summary of Estimation
estimate = c(para, em$mu0, em$Sigma0); SE = c(SE, NA, NA)
u = cbind(estimate, SE)
rownames(u) = c("phi","sigw","sigv","mu0","Sigma0") 
round(u,3) 



library(stargazer)

stargazer(round(u,3) )

# start figure
# BLTR
par(mfrow=c(3,1), mar = c(4, 4, 3, 2) + 0.1)

#-- pictures
## Notes:
## x_t^t-1 = ks$xp,  P_t^t-1 = ks$Pp,   t=  1,...,n and "p" for prediction
## x_t^t = ks$xf,    P_t^t = ks$Pf,     t=  1,...,n and "f" for filter  
## x_t^n = ks$xs,    P_t^n = ks$Ps,     t=  1,...,n and "s" for smoother

ks=kf
Time = 1:50
plot(Time, x[-1][1:50], main='Predict', ylim=c(-5,10), ylab = "State Variable Predictions")
lines(ks$xp,col="red")
lines(ks$xp+2*sqrt(ks$Pp), lty=2, col="blue")
lines(ks$xp-2*sqrt(ks$Pp), lty=2, col="blue")
plot(Time, x[-1], main='Filter', ylim=c(-5,10)) 
lines(ks$xf)
lines(ks$xf+2*sqrt(ks$Pf), lty=2, col=4) 
lines(ks$xf-2*sqrt(ks$Pf), lty=2, col=4)
plot(Time, x[-1], main='Smooth', ylim=c(-5,10))
lines(ks$xs)
lines(ks$xs+2*sqrt(ks$Ps), lty=2, col=4)
lines(ks$xs-2*sqrt(ks$Ps), lty=2, col=4)




# Generate Data
set.seed(999); n = 100
x = arima.sim(n=n+1, list(ar = .8), sd=1)
y = ts(x[-1] + rnorm(n,0,1))
# Initial Estimates
u = ts.intersect(y, lag(y,-1), lag(y,-2))
varu = var(u); coru = cor(u)
phi = coru[1,3]/coru[1,2]
q = (1-phi^2)*varu[1,2]/phi;  r = varu[1,1] - q/(1-phi^2)
(init.par = c(phi, sqrt(q), sqrt(r)))  # = .91, .51, 1.03
# Function to evaluate the likelihood
Linn=function(para){
  phi = para[1]; sigw = para[2]; sigv = para[3]
  Sigma0 = (sigw^2)/(1-phi^2); Sigma0[Sigma0<0]=0
  kf = Kfilter0(n, y, 1, mu0=0, Sigma0, phi, sigw, sigv)
  return(kf$like)   }
# Estimation   (partial output shown)
(est = optim(init.par, Linn, gr=NULL, method="BFGS", hessian=TRUE, control=list(trace=1, REPORT=1)))
SE = sqrt(diag(solve(est$hessian)))
round(cbind(estimate=c(phi=est$par[1],sigw=est$par[2],sigv=est$par[3]),SE), 3)



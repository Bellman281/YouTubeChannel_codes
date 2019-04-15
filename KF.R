#simulate the Model
set.seed(123) 
n = 100
Phi_1=0.7
v = rnorm(n,0,1)
x = arima.sim(n=n+1, list(ar = 0.7), sd=1)
y = ts(x[-1] + v)


#############
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



set.seed (123)
 n = 100
 
 Phi_1=0.7
 v = rnorm (n ,0 ,1)
 w = rnorm(n+1,0,1);
 mu = cumsum(w) 
 x = arima.sim (n=n+1, list (ar = Phi_1) , sd =1)
 y = ts(x[ -1] + v)
 #
 y = x[-1] + v
 
 
 ks = Ksmooth0(n, y, A=1, mu0 = 0, Sigma0=1, Phi=1, cQ=1, cR=1)
 
 
 par(mfrow=c(3,1)); Time = 1:n
 plot(Time, x[-1], main='Predict', ylim=c(-5,10))
 lines(ks$xp, col=2)
 lines(ks$xp+2*sqrt(ks$Pp), lty=2, col=4)
 lines(ks$xp-2*sqrt(ks$Pp), lty=2, col=4)
 
 plot(Time, x[-1], main='Filter', ylim=c(-5,10))
 lines(ks$xf, col=2)
 lines(ks$xf+2*sqrt(ks$Pf), lty=2, col=4)
 lines(ks$xf-2*sqrt(ks$Pf), lty=2, col=4)
 
 plot(Time, x[-1], main='Smooth', ylim=c(-5,10))
 lines(ks$xs, col=2)
 lines(ks$xs+2*sqrt(ks$Ps), lty=2, col=4)
  lines(ks$xs-2*sqrt(ks$Ps), lty=2, col=4)
 
  
  x[1]; ks$x0n; sqrt(ks$P0n) # initial value info
 
 
 #
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
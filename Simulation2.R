
set.seed (123)
n = 100

State_Phi_1=0.7
v = rnorm (n ,0 ,1)
w = rnorm(n+1,0,1);
#mu = cumsum(w) 
x = arima.sim (n=n+1, list (ar = State_Phi_1) , sd =1)
y = ts(x[ -1] + v)

# y is n by q  (time=row series=col)
# A is a q by p matrix
# R is q by q
# x0 is p by 1
# Sigma0, State_Phi, Q are p by p



#initialization


A_transition=1; x0 = 0; Sigma0 =1; State_Phi =1; State_mu0 = 0; State_Sigma0=1; Cholesky_Q_state =1; Cholesky_R_Observ =1; 
Q=t(Cholesky_Q_state)%*%Cholesky_Q_state ; R= t(Cholesky_R_Observ) %*% Cholesky_R_Observ


State_Phi=as.matrix(State_Phi)
q_State_dim=nrow(State_Phi)    
y=as.matrix(y)

q_Observ_dim=ncol(y)

State_x_vector_p_dim=array(NA, dim=c(q_State_dim,1,n))         # State_x_vector_p_dim=x_t^{t-1}          
P_State_VarCov_p_dim=array(NA, dim=c(q_State_dim,q_State_dim,n))      # P_State_VarCov_p_dim=P_t^{t-1}
xf=array(NA, dim=c(q_State_dim,1,n))         # xf=x_t^t
Pf=array(NA, dim=c(q_State_dim,q_State_dim,n))      # Pf=x_t^t
innov=array(NA, dim=c(q_Observ_dim,1,n))      # innovations
sig=array(NA, dim=c(q_Observ_dim,q_Observ_dim,n))     # innov var-cov matrix




# initialize (because R can't count from zero)


x00=as.matrix(State_mu0, nrow=q_State_dim, ncol=1)
P00=as.matrix(State_Sigma0, nrow=q_State_dim, ncol=q_State_dim)
State_x_vector_p_dim[,,1]=State_Phi%*%x00
P_P_State_VarCov_p_dim[,,1]=State_Phi%*%P00%*%t(State_Phi)+Q
sigtemp=A_transition%*%P_State_VarCov_p_dim[,,1]%*%t(A_transition)+R
sig[,,1]=(t(sigtemp)+sigtemp)/2     # innov var - make sure it's symmetric
siginv=solve(sig[,,1])          
K=P_State_VarCov_p_dim[,,1]%*%t(A_transition)%*%siginv
innov[,,1]=y[1,]-A_transition%*%State_x_vector_p_dim[,,1]
xf[,,1]=State_x_vector_p_dim[,,1]+K%*%innov[,,1]
Pf[,,1]=P_State_VarCov_p_dim[,,1]-K%*%A_transition%*%P_State_VarCov_p_dim[,,1]
sigmat=as.matrix(sig[,,1], nrow=q_Observ_dim, ncol=q_Observ_dim)
like = log(det(sigmat)) + t(innov[,,1])%*%siginv%*%innov[,,1]   # -log(likelihood)




###

for (i in 2:n){
  if (n < 2) break
  State_x_vector_p_dim[,,i]=State_Phi%*%xf[,,i-1]
  P_State_VarCov_p_dim[,,i]=State_Phi%*%Pf[,,i-1]%*%t(State_Phi)+Q
  sigtemp=A_transition%*%P_State_VarCov_p_dim[,,i]%*%t(A_transition)+R
  sig[,,i]=(t(sigtemp)+sigtemp)/2     # innov var - make sure it's symmetric
  siginv=solve(sig[,,i])              
  K=P_State_VarCov_p_dim[,,i]%*%t(A_transition)%*%siginv
  innov[,,i]=y[i,]-A_transition%*%State_x_vector_p_dim[,,i]
  xf[,,i]=State_x_vector_p_dim[,,i]+K%*%innov[,,i]
  Pf[,,i]=P_State_VarCov_p_dim[,,i]-K%*%A_transition%*%P_State_VarCov_p_dim[,,i]
  sigmat=as.matrix(sig[,,i], nrow=q_Observ_dim, ncol=q_Observ_dim)
  like= like + log(det(sigmat)) + t(innov[,,i])%*%siginv%*%innov[,,i]
}
like=0.5*like
list(State_x_vector_p_dim=State_x_vector_p_dim,P_State_VarCov_p_dim=P_State_VarCov_p_dim,xf=xf,Pf=Pf,like=like,innov=innov,sig=sig,Kn=K)


par(mfrow=c(3,1)); Time = 1:n
plot(Time, x[-1], main='Predict', ylim=c(-5,10))
lines(State_x_vector_p_dim, col=2)
lines(State_x_vector_p_dim+2*sqrt(P_State_VarCov_p_dim), lty=2, col=4)
lines(State_x_vector_p_dim-2*sqrt(P_State_VarCov_p_dim), lty=2, col=4)

plot(Time, x[-1], main='Filter', ylim=c(-5,10))
lines(xf, col=2)
lines(xf+2*sqrt(Pf), lty=2, col=4)
lines(xf-2*sqrt(Pf), lty=2, col=4)

#









#Ksmooth part

q_State_dim=nrow(as.matrix(State_Phi))  
xs=array(NA, dim=c(q_State_dim,1,n))      # xs=x_t^n
Ps=array(NA, dim=c(q_State_dim,q_State_dim,n))   # Ps=P_t^n
J=array(NA, dim=c(q_State_dim,q_State_dim,n))    # J=J_t


xs[,,n]=xf[,,n] 
Ps[,,n]=Pf[,,n]
for(k in n:2)  {
  J[,,k-1]=(Pf[,,k-1]%*%t(State_Phi))%*%solve(P_State_VarCov_p_dim[,,k])
  xs[,,k-1]=xf[,,k-1]+J[,,k-1]%*%(xs[,,k]-State_x_vector_p_dim[,,k])
  Ps[,,k-1]=Pf[,,k-1]+J[,,k-1]%*%(Ps[,,k]-P_State_VarCov_p_dim[,,k])%*%t(J[,,k-1])
}
x00=State_mu0
P00=State_Sigma0
J0=as.matrix((P00%*%t(State_Phi))%*%solve(P_State_VarCov_p_dim[,,1]), nrow=q_State_dim, ncol=q_State_dim)
x0n=as.matrix(x00+J0%*%(xs[,,1]-State_x_vector_p_dim[,,1]), nrow=q_State_dim, ncol=1)
P0n= P00 + J0%*%(Ps[,,1]-P_State_VarCov_p_dim[,,1])%*%t(J0)
list(xs=xs,Ps=Ps,x0n=x0n,P0n=P0n,J0=J0,J=J,State_x_vector_p_dim=State_x_vector_p_dim,P_State_VarCov_p_dim=P_State_VarCov_p_dim,xf=xf,Pf=Pf,like=like,Kn=K)




plot(Time, x[-1], main='Smooth', ylim=c(-5,10))
lines(xs, col=2)
lines(xs+2*sqrt(Ps), lty=2, col=4)
lines(xs-2*sqrt(Ps), lty=2, col=4)


x[1]; x0n; sqrt(P0n) # initial value info


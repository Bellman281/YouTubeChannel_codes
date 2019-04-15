
set.seed (123)
n = 100

State_Phi_1=0.7
v = rnorm (n ,0 ,1)
w = rnorm(n+1,0,1);
x = arima.sim (n=n+1, list (ar = State_Phi_1) , sd =1)
y = ts(x[ -1] + v)

####


A=1; mu0 = 0; Sigma0=1
State_Phi_p.p=State_Phi_1; 
Cholesky_Q_state=1; 
Cholesky_R_Observ=1
Q=t(Cholesky_Q_state)%*%Cholesky_Q_state
R=t(Cholesky_R_Observ)%*%Cholesky_R_Observ
# y is n by q  (time=row series=col)
# A is a q by p matrix
# R is q by q
# mu0 is p by 1
# Sigma0, State_Phi_p.p, Q are p by p


State_Phi_p.p=as.matrix(State_Phi_p.p)
p.state.dim=nrow(State_Phi_p.p)    
y=as.matrix(y)
q.obsv.dim=ncol(y)
Predict_State_x_t_t_1=array(NA, dim=c(p.state.dim,1,n))         # Predict_State_x_t_t_1=x_t^{t-1}          
Predict_State_VarCov_t_t_1=array(NA, dim=c(p.state.dim,p.state.dim,n))      # Predict_State_VarCov_t_t_1=P_t^{t-1}
Filter_State_x_t_t=array(NA, dim=c(p.state.dim,1,n))         # Filter_State_x_t_t=x_t^t
Filter_State_VarCov_t_t=array(NA, dim=c(p.state.dim,p.state.dim,n))      # Filter_State_VarCov_t_t=x_t^t
epsilon=array(NA, dim=c(q.obsv.dim,1,n))      # innovations
epsilon_VarCov=array(NA, dim=c(q.obsv.dim,q.obsv.dim,n))     # epsilon var-cov matrix



# initialize (because R can't count from zero)
x_initial_00=as.matrix(mu0, nrow=p.state.dim, ncol=1)
P_initial_00=as.matrix(Sigma0, nrow=p.state.dim, ncol=p.state.dim)
Predict_State_x_t_t_1[,,1]=State_Phi_p.p%*%x_initial_00
Predict_State_VarCov_t_t_1[,,1]=State_Phi_p.p%*%P_initial_00%*%t(State_Phi_p.p)+Q
sigma_temp=A%*%Predict_State_VarCov_t_t_1[,,1]%*%t(A)+R
epsilon_VarCov[,,1]=(t(sigma_temp)+sigma_temp)/2     # epsilon var - make sure it's symmetric
sig_inv=solve(epsilon_VarCov[,,1])          

Kalman_gain=Predict_State_VarCov_t_t_1[,,1]%*%t(A)%*%sig_inv
epsilon[,,1]=y[1,]-A%*%Predict_State_x_t_t_1[,,1]

Filter_State_x_t_t[,,1]=Predict_State_x_t_t_1[,,1]+Kalman_gain%*%epsilon[,,1]
Filter_State_VarCov_t_t[,,1]=Predict_State_VarCov_t_t_1[,,1]-Kalman_gain%*%A%*%Predict_State_VarCov_t_t_1[,,1]

sigma_t=as.matrix(epsilon_VarCov[,,1], nrow=q.obsv.dim, ncol=q.obsv.dim)
likelihood_est = log(det(sigma_t)) + t(epsilon[,,1])%*%sig_inv%*%epsilon[,,1]   # -log(likelihood_estlihood)

########## start filter iterations ###################
for (i in 2:n){
  if (n < 2) break
  Predict_State_x_t_t_1[,,i]=State_Phi_p.p%*%Filter_State_x_t_t[,,i-1]
  Predict_State_VarCov_t_t_1[,,i]=State_Phi_p.p%*%Filter_State_VarCov_t_t[,,i-1]%*%t(State_Phi_p.p)+Q
  sigma_temp=A%*%Predict_State_VarCov_t_t_1[,,i]%*%t(A)+R
  epsilon_VarCov[,,i]=(t(sigma_temp)+sigma_temp)/2     # epsilon var - make sure it's symmetric
  sig_inv=solve(epsilon_VarCov[,,i])              
  Kalman_gain=Predict_State_VarCov_t_t_1[,,i]%*%t(A)%*%sig_inv
  epsilon[,,i]=y[i,]-A%*%Predict_State_x_t_t_1[,,i]
  Filter_State_x_t_t[,,i]=Predict_State_x_t_t_1[,,i]+Kalman_gain%*%epsilon[,,i]
  Filter_State_VarCov_t_t[,,i]=Predict_State_VarCov_t_t_1[,,i]-Kalman_gain%*%A%*%Predict_State_VarCov_t_t_1[,,i]
  sigma_t=as.matrix(epsilon_VarCov[,,i], nrow=q.obsv.dim, ncol=q.obsv.dim)
  likelihood_est= likelihood_est + log(det(sigma_t)) + t(epsilon[,,i])%*%sig_inv%*%epsilon[,,i]
}
likelihood_est=0.5*likelihood_est
list(Predict_State_x_t_t_1=Predict_State_x_t_t_1,Predict_State_VarCov_t_t_1=Predict_State_VarCov_t_t_1,Filter_State_x_t_t=Filter_State_x_t_t,Filter_State_VarCov_t_t=Filter_State_VarCov_t_t,likelihood_est=likelihood_est,epsilon=epsilon,epsilon_VarCov=epsilon_VarCov,Kalman_gainn=Kalman_gain)




par(mfrow=c(3,1)); Time = 1:n
plot(Time, x[-1], main='Predict', ylim=c(-5,10))
lines(Predict_State_x_t_t_1, col=2)
lines(Predict_State_x_t_t_1+2*sqrt(Predict_State_VarCov_t_t_1), lty=2, col=4)
lines(Predict_State_x_t_t_1-2*sqrt(Predict_State_VarCov_t_t_1), lty=2, col=4)




plot(Time, x[-1], main='Filter', ylim=c(-5,10))
lines(Filter_State_x_t_t, col=2)
lines(Filter_State_x_t_t+2*sqrt(Filter_State_VarCov_t_t), lty=2, col=4)
lines(Filter_State_x_t_t-2*sqrt(Filter_State_VarCov_t_t), lty=2, col=4)



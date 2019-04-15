%
% Hansen (1985)
%
clear;
clc;
%
% Assigning Parameter Values
%
beta=0.95;
chi=2;
alpha=0.4;
sigma=1;
delta=0.1;
rho=0.95;
%
% Calculating the Steady State
%
k_lbar=((1-(1-delta)*beta)/(alpha*beta))^(1/(alpha-1));
cbar=((1-alpha)/chi)^(1/sigma)*k_lbar^(alpha/sigma);
kbar=cbar*(k_lbar^(alpha-1)-delta)^-1;
lbar=kbar*k_lbar^-1;
ybar=kbar^alpha*lbar^(1-alpha);
%
% State Space Representation (matrices A0, A1, and B0)
%
A0=zeros(4,4);
A1=zeros(4,4);
B0=zeros(4,1);
A0(1,1)=(1-(1-delta)*beta)*rho;
A0(1,2)=(alpha-1)*(1-(1-delta)*beta);
A0(1,3)=(1-alpha)*(1-(1-delta)*beta);
A0(1,4)=-sigma;
A0(2,1)=-ybar;
A0(2,2)=kbar;
A0(3,1)=1;
A0(4,1)=-1;
A1(1,4)=-sigma;
A1(2,2)=alpha*ybar+(1-delta)*kbar;
A1(2,3)=(1-alpha)*ybar;
A1(2,4)=-cbar;
A1(3,1)=rho;
A1(4,2)=alpha;
A1(4,3)=-alpha;
A1(4,4)=-sigma;
B0(3,1)=1;
%
% Matrices A and B
%
A=inv(A1)*A0;
B=inv(A1)*B0;
%
% Jordan Decomposition
%
[ve,MU]=eig(A);
%
% Sort Eigenvalues and Eigenvectors
%
t=flipud(sortrows([diag(MU) ve'],1));
MU=diag(t(:,1));
ve=t(:,2:5);
P=inv(ve');
%
% Partitioning MU and P
%
MU1=MU(1:2,1:2);
MU2=MU(3:4,3:4);
P11=P(1:2,1:2);
P12=P(1:2,3:4);
P21=P(3:4,1:2);
P22=P(3:4,3:4);
R=P*B;
%
% Solutions
%
yw=-inv(P22)*P21;
ye=-inv(P22)*R(3:4,1)
ww=inv(P11-P12*inv(P22)*P21)*inv(MU1)*(P11-P12*inv(P22)*P21)
we=inv(P11-P12*inv(P22)*P21)*inv(MU1)*(R(1:2,1)-P12*inv(P22)*R(3:4,1))

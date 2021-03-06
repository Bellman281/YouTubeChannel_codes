% Hansen (1985)

%末末末末末末末末末末�-
% 0. Housekeeping (close all graphic windows)
%末末末末末末末末末末�-

close all;

%末末末末末末末末末末�-
% 1. Defining variables
%末末末末末末末末末末�-

var y c k l z;
varexo e;
parameters beta chi delta alpha rho sigma;

%末末末末末末末末末末�-
% 2. Calibration
%末末末末末末末末末末�-

alpha = 0.4;
beta = 0.95;
delta = 0.1;
chi = 2;
rho = 0.95;
sigma = 1;

%末末末末末末末末末末�-
% 3. Model
%末末末末末末末末末末�-

model;
c^-sigma = beta*c(+1)^-sigma*(alpha*exp(z(+1))*k^(alpha-1)*l(+1)^(1-alpha)+1-delta);
chi = c^-sigma*(1-alpha)*exp(z)*k(-1)^alpha*l^-alpha;
c+k = exp(z)*k(-1)^alpha*l^(1-alpha)+(1-delta)*k(-1);
y = exp(z)*k(-1)^alpha*l^(1-alpha);
z = rho*z(-1)+e;
end;

%末末末末末末末末末末�-
% 4. Computation
%末末末末末末末末末末�-

initval;
k = 2;
c = 0.5;
l = 0.3;
z = 0;
e = 0;
end;

shocks;
var e = 1;
end;


steady;

stoch_simul(hp_filter = 1600, order = 1);
function [residual, g1, g2, g3] = hansen_static(y, x, params)
%
% Status : Computes static model for Dynare
%
% Inputs : 
%   y         [M_.endo_nbr by 1] double    vector of endogenous variables in declaration order
%   x         [M_.exo_nbr by 1] double     vector of exogenous variables in declaration order
%   params    [M_.param_nbr by 1] double   vector of parameter values in declaration order
%
% Outputs:
%   residual  [M_.endo_nbr by 1] double    vector of residuals of the static model equations 
%                                          in order of declaration of the equations.
%                                          Dynare may prepend or append auxiliary equations, see M_.aux_vars
%   g1        [M_.endo_nbr by M_.endo_nbr] double    Jacobian matrix of the static model equations;
%                                                       columns: variables in declaration order
%                                                       rows: equations in order of declaration
%   g2        [M_.endo_nbr by (M_.endo_nbr)^2] double   Hessian matrix of the static model equations;
%                                                       columns: variables in declaration order
%                                                       rows: equations in order of declaration
%   g3        [M_.endo_nbr by (M_.endo_nbr)^3] double   Third derivatives matrix of the static model equations;
%                                                       columns: variables in declaration order
%                                                       rows: equations in order of declaration
%
%
% Warning : this file is generated automatically by Dynare
%           from model file (.mod)

residual = zeros( 5, 1);

%
% Model equations
%

T11 = y(2)^(-params(6));
T21 = params(4)*exp(y(5))*y(3)^(params(4)-1);
T24 = y(4)^(1-params(4));
T28 = 1+T21*T24-params(3);
T34 = y(3)^params(4);
T37 = y(4)^(-params(4));
lhs =T11;
rhs =T11*params(1)*T28;
residual(1)= lhs-rhs;
lhs =params(2);
rhs =exp(y(5))*T11*(1-params(4))*T34*T37;
residual(2)= lhs-rhs;
lhs =y(2)+y(3);
rhs =T24*exp(y(5))*T34+y(3)*(1-params(3));
residual(3)= lhs-rhs;
lhs =y(1);
rhs =T24*exp(y(5))*T34;
residual(4)= lhs-rhs;
lhs =y(5);
rhs =y(5)*params(5)+x(1);
residual(5)= lhs-rhs;
if ~isreal(residual)
  residual = real(residual)+imag(residual).^2;
end
if nargout >= 2,
  g1 = zeros(5, 5);

  %
  % Jacobian matrix
  %

T54 = getPowerDeriv(y(2),(-params(6)),1);
T68 = getPowerDeriv(y(3),params(4),1);
T77 = getPowerDeriv(y(4),1-params(4),1);
  g1(1,2)=T54-T28*params(1)*T54;
  g1(1,3)=(-(T11*params(1)*T24*params(4)*exp(y(5))*getPowerDeriv(y(3),params(4)-1,1)));
  g1(1,4)=(-(T11*params(1)*T21*T77));
  g1(1,5)=(-(T11*params(1)*T21*T24));
  g1(2,2)=(-(T37*T34*exp(y(5))*(1-params(4))*T54));
  g1(2,3)=(-(T37*exp(y(5))*T11*(1-params(4))*T68));
  g1(2,4)=(-(exp(y(5))*T11*(1-params(4))*T34*getPowerDeriv(y(4),(-params(4)),1)));
  g1(2,5)=(-(exp(y(5))*T11*(1-params(4))*T34*T37));
  g1(3,2)=1;
  g1(3,3)=1-(1-params(3)+T24*exp(y(5))*T68);
  g1(3,4)=(-(exp(y(5))*T34*T77));
  g1(3,5)=(-(T24*exp(y(5))*T34));
  g1(4,1)=1;
  g1(4,3)=(-(T24*exp(y(5))*T68));
  g1(4,4)=(-(exp(y(5))*T34*T77));
  g1(4,5)=(-(T24*exp(y(5))*T34));
  g1(5,5)=1-params(5);
  if ~isreal(g1)
    g1 = real(g1)+2*imag(g1);
  end
if nargout >= 3,
  %
  % Hessian matrix
  %

  g2 = sparse([],[],[],5,25);
if nargout >= 4,
  %
  % Third order derivatives
  %

  g3 = sparse([],[],[],5,125);
end
end
end
end

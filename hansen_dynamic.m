function [residual, g1, g2, g3] = hansen_dynamic(y, x, params, steady_state, it_)
%
% Status : Computes dynamic model for Dynare
%
% Inputs :
%   y         [#dynamic variables by 1] double    vector of endogenous variables in the order stored
%                                                 in M_.lead_lag_incidence; see the Manual
%   x         [nperiods by M_.exo_nbr] double     matrix of exogenous variables (in declaration order)
%                                                 for all simulation periods
%   steady_state  [M_.endo_nbr by 1] double       vector of steady state values
%   params    [M_.param_nbr by 1] double          vector of parameter values in declaration order
%   it_       scalar double                       time period for exogenous variables for which to evaluate the model
%
% Outputs:
%   residual  [M_.endo_nbr by 1] double    vector of residuals of the dynamic model equations in order of 
%                                          declaration of the equations.
%                                          Dynare may prepend auxiliary equations, see M_.aux_vars
%   g1        [M_.endo_nbr by #dynamic variables] double    Jacobian matrix of the dynamic model equations;
%                                                           rows: equations in order of declaration
%                                                           columns: variables in order stored in M_.lead_lag_incidence followed by the ones in M_.exo_names
%   g2        [M_.endo_nbr by (#dynamic variables)^2] double   Hessian matrix of the dynamic model equations;
%                                                              rows: equations in order of declaration
%                                                              columns: variables in order stored in M_.lead_lag_incidence followed by the ones in M_.exo_names
%   g3        [M_.endo_nbr by (#dynamic variables)^3] double   Third order derivative matrix of the dynamic model equations;
%                                                              rows: equations in order of declaration
%                                                              columns: variables in order stored in M_.lead_lag_incidence followed by the ones in M_.exo_names
%
%
% Warning : this file is generated automatically by Dynare
%           from model file (.mod)

%
% Model equations
%

residual = zeros(5, 1);
T11 = y(4)^(-params(6));
T15 = params(1)*y(8)^(-params(6));
T23 = params(4)*exp(y(10))*y(5)^(params(4)-1);
T26 = y(9)^(1-params(4));
T30 = 1+T23*T26-params(3);
T39 = y(1)^params(4);
T43 = y(6)^(-params(4));
T48 = y(6)^(1-params(4));
T49 = exp(y(7))*T39*T48;
lhs =T11;
rhs =T15*T30;
residual(1)= lhs-rhs;
lhs =params(2);
rhs =T11*(1-params(4))*exp(y(7))*T39*T43;
residual(2)= lhs-rhs;
lhs =y(4)+y(5);
rhs =T49+y(1)*(1-params(3));
residual(3)= lhs-rhs;
lhs =y(3);
rhs =T49;
residual(4)= lhs-rhs;
lhs =y(7);
rhs =params(5)*y(2)+x(it_, 1);
residual(5)= lhs-rhs;
if nargout >= 2,
  g1 = zeros(5, 11);

  %
  % Jacobian matrix
  %

T62 = getPowerDeriv(y(4),(-params(6)),1);
T72 = getPowerDeriv(y(1),params(4),1);
T91 = (-(exp(y(7))*T39*getPowerDeriv(y(6),1-params(4),1)));
  g1(1,4)=T62;
  g1(1,8)=(-(T30*params(1)*getPowerDeriv(y(8),(-params(6)),1)));
  g1(1,5)=(-(T15*T26*params(4)*exp(y(10))*getPowerDeriv(y(5),params(4)-1,1)));
  g1(1,9)=(-(T15*T23*getPowerDeriv(y(9),1-params(4),1)));
  g1(1,10)=(-(T15*T23*T26));
  g1(2,4)=(-(T43*T39*exp(y(7))*(1-params(4))*T62));
  g1(2,1)=(-(T43*T11*(1-params(4))*exp(y(7))*T72));
  g1(2,6)=(-(T11*(1-params(4))*exp(y(7))*T39*getPowerDeriv(y(6),(-params(4)),1)));
  g1(2,7)=(-(T11*(1-params(4))*exp(y(7))*T39*T43));
  g1(3,4)=1;
  g1(3,1)=(-(1-params(3)+T48*exp(y(7))*T72));
  g1(3,5)=1;
  g1(3,6)=T91;
  g1(3,7)=(-T49);
  g1(4,3)=1;
  g1(4,1)=(-(T48*exp(y(7))*T72));
  g1(4,6)=T91;
  g1(4,7)=(-T49);
  g1(5,2)=(-params(5));
  g1(5,7)=1;
  g1(5,11)=(-1);

if nargout >= 3,
  %
  % Hessian matrix
  %

  g2 = sparse([],[],[],5,121);
if nargout >= 4,
  %
  % Third order derivatives
  %

  g3 = sparse([],[],[],5,1331);
end
end
end
end

library(tseries)

data(EuStockMarkets)  
head(EuStockMarkets)
dax <- diff(log(EuStockMarkets))[,"DAX"]
dax.garch <- garch(dax)  # Fit a GARCH(1,1) to DAX returns
summary(dax.garch)       # ARCH effects are filtered. However, 
plot(dax.garch)          # conditional normality seems to be violated




#We can then compute the ARMA(1,1)-GARCH(1,1) model as an example:


#install.packages("rugarch")
require(rugarch)

spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                         garchOrder = c(1, 1), 
                                         submodel = NULL, 
                                         external.regressors = NULL, 
                                         variance.targeting = FALSE), 
                   
                   mean.model     = list(armaOrder = c(1, 1), 
                                         external.regressors = NULL, 
                                         distribution.model = "norm", 
                                         start.pars = list(), 
                                         fixed.pars = list()))


garch.dax2 <- ugarchfit(spec = spec, data = dax, solver.control = list(trace=0))

#Retrieve ARMA(1,1) and GARCH(1,1) coefficients:
  
  garch.dax2@fit$coef
#Retrieve time-varying standard deviation:
  
  garch@fit$sigma
#Retrieve standardized N(0,1)N(0,1) ARMA(1,1) disturbances:
  
  garch@fit$z
  
  plot(garch.dax2)
  
  #test
  library(vars)
  data(Canada)
  var.2c <- VAR(Canada, p = 2, type = "const")
  arch.test(var.2c)
  

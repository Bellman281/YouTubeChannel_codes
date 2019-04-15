#https://en.wikibooks.org/wiki/R_Programming/Method_of_Moments 

library(gmm)
# Simple linear model
   N <- 1000
   u <- rnorm(N)
   x <- 1 + rnorm(N)
   y <- 1 + x + u
   res <- gmm(y ~ x, x)
   summary(res)
  lm(y ~ x)
   # Simple linear model with instrumental variables.
     library(gmm)
   N <- 1000
   u <- rnorm(N)
   z <- rnorm(N)
   x <- 1 + z + u + rnorm(N)
   y <- 1 + x + u
   res <- gmm(y ~ x, z)
   summary(res)
  
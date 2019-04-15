m <- 10000
x <- runif(m)
theta.hat <- mean(exp(-x))
print(theta.hat)
print(1 - exp(-1))


x <- seq(.1, 2.5, length = 10)
m <- 10000
y <- runif(m)
cdf <- numeric(length(x))
for (i in 1:length(x)) {
  g <- x[i] * exp(-(y * x[i])^2 / 2)
  cdf[i] <- mean(g) / sqrt(2 * pi) + 0.5
}

Phi <- pnorm(x)
print(round(rbind(x, cdf, Phi), 3))


x <- seq(.1, 2.5, length = 10)
m <- 10000
z <- rnorm(m)
dim(x) <- length(x)
p <- apply(x, MARGIN = 1,
           FUN = function(x, z) {mean(z < x)}, z = z)



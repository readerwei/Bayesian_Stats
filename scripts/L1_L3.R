theta = rgamma(100000, 50, rate = 37815)
50*756
beta = 1/theta
plot(density(beta, from = 400, to=1500, n=1000, bw=15))

install.packages("invgamma")
library(invgamma)
real = rinvgamma(10000, 50, 37815)
dth = seq(400, 1500, 15)
plot(dth, dinvgamma(dth, 50, 37815))
lines(density(beta, from = 400, to=1500, n=1000, bw=15))

set.seed(34)

n = 100
x = numeric(n)

for (i in 2:n) {
  x[i] = rnorm(1, mean=x[i-1], sd=1.0)
}

plot.ts(x)

Q = matrix(c(0.0, 0.5, 0.0, 0.0, 0.5,
             0.5, 0.0, 0.5, 0.0, 0.0,
             0.0, 0.5, 0.0, 0.5, 0.0,
             0.0, 0.0, 0.5, 0.0, 0.5,
             0.5, 0.0, 0.0, 0.5, 0.0), 
           nrow=5, byrow=TRUE)

Q %*% Q 

Q5 = Q %*% Q %*% Q %*% Q %*% Q # h=5 steps in the future
round(Q5, 3)

Q10 = Q %*% Q %*% Q %*% Q %*% Q %*% Q %*% Q %*% Q %*% Q %*% Q # h=10 steps in the future
round(Q10, 3)

Q30 = Q
for (i in 2:90) {
  Q30 = Q30 %*% Q
}
round(Q30, 3) # h=30 steps in the future

install.packages('rjags')
library('rjags')
library('coda')

# 99_test.R
# test poisson and log-transform
# sep 2023

N = 300
sd.e = 1
error = rnorm(n = N, mean = 0, sd = sd.e)
beta = 1
x = rnorm(n = N, mean = 0, sd = sd.e)
mu = 1 + x*beta + error
Y = round(exp(mu)) # counts
data = data.frame(Y = Y , x = x)

#
m1 = glm(Y ~ x, data = data, family=poisson)
summary(m1)

#
m2 = glm(log(Y+0.1) ~ x, data = data)
summary(m2)

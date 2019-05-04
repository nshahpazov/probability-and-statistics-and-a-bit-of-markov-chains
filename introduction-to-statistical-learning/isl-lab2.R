
# Linear Regression

xbar = mean(Advertising$TV)

ybar = mean(Advertising$sales)

linear.regression.lsm = function (x, y) {
  
  covariance = cov(x, y)
  slope = covariance / var(x)
  translation = mean(y) - beta1 * mean(x)
  
  return(c(translation = translation, slope = slope))
}

xs = 0:300 # x-axis
plot(x, y)
lsm.params = linear.regression.lsm(x, y)
translation = lsm.params["translation"]
slope = lsm.params["slope"]
lines(xs, translation + slope * xs)

x = Advertising$TV
y = Advertising$sales

show.error = function (beta0, beta1) {
  yhat = beta0 + beta1*x
  e = y - yhat
  return(sum(e^2)) 
}

# or just use
# lm(sales ~ TV, Advertising)

b0 <- seq(-10, 10, 1)
b1 <- seq(-1, 1, 0.1)

z <- outer(b0, b1, function(x, y) mapply(show.error, x, y))

persp(x = b0, y = b1, z = z, theta=90, phi=40)

########################################################
# Statistical Inference, Chapter 3, Exercise 2
# pop size : 5260
# sample size : 131
# Number of items in the pop that are classified as successes : 1998
# Number of items in the sample that are classified as successes : 62
# phyper(62,1998,5260-1998,131)

# popsize: 100
# samplesize: 32
# number of items in population that are considered success: 6
# number of items in sample that are considered success: 6
# phyper(0, 6, 100 - 6, 32)

f = function (x) {
  return(exp(-x) * (1 + x) - 0.01)
}

########################################################
# Statistical Inference, Chapter 3, Exercise 7 
approx = function (f, a,b, epsilon=1e-7, nmax=10000) {
  i = 0
  while (i <= nmax) {
    c = (a + b) / 2
    print(c)
    if (f(c) == 0 || (b - a) / 2 < epsilon) {
      return(c)
    }
    i = i + 1
  }
  return(c)
}

########################################################
# Statistical Inference Chapter 3, Exercise 8
qbinom(0.99, 1000, 1/2)
# or use normal aproximation
qnorm(0.99, 500, sqrt(250))

########################################################
# Statistical Inference Chapter 3, Exercise 9
prob.in.a.single.school = 1 - pbinom(4, 60, 1/90)
# probability of the event happening at least somewhere is
#     1 - probability of not happening anywhere which is 
#     1 - bin(0, 62 * 5, prob.in.a.single.school)
single = 1 - pbinom(0, 62*5, prob.in.a.single.school)
# for any of the 50 states for any of the last 10 years
1 - pbinom(0, 10*50, single)

##############################################################################
# Takis Problem 10 - Getting out of Prison
options(digits = 9)
f = rep(1, 8)

coeff = rep(1, 8)
coeff[1] = 0.4
for (i in 2:7) {
  coeff[i] = 0.4 / round(1 - (0.6 * coeff[i-1]), 9)
}

# calculate the probabilities :- f[i-1] = c[i-1] * f[i]
for (i in 8:2) {
  f[i-1] = round(coeff[i-1] * f[i], 4)
}
#################################################################

x = rexp(1000)
plot(x)

#######################################

rbinom(10, 5, 0.3)

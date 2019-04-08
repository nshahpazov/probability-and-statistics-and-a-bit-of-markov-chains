alphas = c(0,2, 0.1, 0.05, 0.001)
zscores = qnorm(1 - alphas / 2)


# sigma here is the standard deviation (sqrt of variance)
# se = sigma/sqrt(n) :- see the Lyapunov CLT
simple.z.test = function (x, sigma, conf.level = 0.95) {
  alpha = 1 - conf.level
  n = length(x)
  xbar = mean(x)
  z = qnorm(1 - alpha / 2)
  se = sigma/sqrt(n)
  return(c(xbar- z*se, xbar + z*se))
}

xs = c(175, 176, 173, 174, 173, 173, 176, 173, 179)
simple.z.test(xs, 1.5)

# dynamic usage of normal or t-distribution according to whether there's
# a sigma as an argument
conf.interval = function (x, sigma = sd(x), conf.level=0.95) {
  xbar = mean(x)
  alpha = 1 - conf.level
  n = length(x)
  se <- sigma / sqrt(n)

  if (missing(sigma)) {
    q = qt(conf.level, df = n - 1)
  } else {
    q <- qnorm(1 - alpha / 2)
  }
  return(c(xbar - q * se, xbar + q * se))
}

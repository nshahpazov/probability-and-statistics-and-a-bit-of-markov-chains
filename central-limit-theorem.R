# Central Limit Theorem examples

n = 10000
p = 0.5
S = rbinom(100, n, p)

X = (S - 5000)/sqrt(n*p*(1-p))
hist(X)

gen.binom = function (n, p) {
  ex = n*p
  q = 1-p
  dx=sqrt(n*p*q)
  s = sum(rbinom(n, 1, p))
  return((s-ex)/dx)
}

d = sapply(1:n,  function (x) (gen.binom(n, 0.5)))
hist(d, prob=T)

s = 0
n=10000
k=100
for (i in 1:n) {
  xs = rnorm(k, 40, 500)
  s = s + mean(xs)
}

expected_xbar = s/n


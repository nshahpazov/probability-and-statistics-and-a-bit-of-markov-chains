              ############ APPLIED STATISTICS -> EXERCISE 2 ############

        
rm(list = ls()) # rm() -----> remove objects; ls() -----> list of all objects

norm.contaminated.sample <- function(mu1, sigma1, mu2 = 0, sigma2 = 1, proportion = 1/2, size=1) {
  p = runif(size, 0, 1) > proportion;
  return(rnorm(size, mu1 + (mu2 - mu1)*p, sigma1 + (sigma2-sigma1)*p))
}

generate.contaminated = function (n, prop, mu1=0, sigma1=1, mu2 = 0, sigma2 = 1) {
  u = runif(n, 0, 1)
  rand.samples = rep(NA, n)
  for (i in 1:n) {
    if (u[i] < prop) {
      rand.samples[i] = rnorm(1, mu1, sigma1)
    } else {
      rand.samples[i] = rnorm(1, mu2, sigma2)
    }
  }
  return(rand.samples)
}

x = generate.contaminated(n=100, prop=1/5, mu1=0, sigma1=1, mu2=4, sigma2=1)
y = norm.contaminated.sample(100, 1, proportion=1/5, size=20)
plot(density(x), main="Density Estimate of the Mixture Model")
plot(density(y), main="Density Estimate of the Mixture Model")

                                            ### Задача 1 ###
# Като използвате функцията norm.contaminated.sample(), генерирайте 100 проявления на случайната 
# величина Z с плътност f_Z(x)=f_X(x)*3/4+f_Y(x)*1/4, където f_X(x) и f_Y(x) са плътностите 
# съответно на X~N(0,1) и Y~N(4,1). Постройте boxplot и хистограма на получените данни. Можем ли
# да приемем нулевата хипотеза, че данните са нормално разпледелени с очакване 1 при ниво на съгласие
# alpha=0.05? Каква е стойността на p-value на статистиката? Нормално ли са разпределени данните?

n=100
data=norm.contaminated.sample(0,1,4,1,proportion=3/4,size=n)
boxplot(data,col = "lightgrey",outcol="red", outlwd=2)
{
  ?par
  ?points
  plot(1:25,pch=1:25,cex=seq(3,1,length.out = 25))
  abline(v=1:10,lty=1:10,col=c(1:8,rgb(0.5,0.5,0.5),rgb(165/255,75/255,165/255)),lwd=10:1)
}
d=density(data,bw="SJ")
h=hist(data,prob=TRUE)
lines(d,col='blue',lwd=2)
curve(dnorm(data, mean=mean(data), sd=sd(data)), add=TRUE, col=2,lwd=2)
(mu=mean(data))
(sd=sd(data))
(tStat=sqrt(n)*(mu-1)/sd)
alpha=0.05;
(criticalValue=abs(qt(alpha/2,n-1)))
(p_value=2*pt(abs(tStat),df=n-1,lower.tail=FALSE))

t.test(data, mu=1)
shapiro.test(data)
ks.test(data,"pnorm")

                                              ### Задача 2 ###
# Напишете функция, която връща доверителен интервал (lowerBound и upperBound) с ниво 
# confLevel (с default value 0.95) за средното на извадка x. Като изполвате функцията 
# missing(), конструирайте функцията така, че да използва нормално разпределение или
# Student разпределение според входните аргументи. Направете проверка за входната стойност 
# на аргумета confLevel.

conf.interval = function (x, confLevel = 0.95, sigma = sd(x)) {
  alpha = 1 - confLevel
  n = length(x)
  xbar = mean(x)
  se = sigma / sqrt(n)
  
  # calculate the critical value
  if (missing(sigma)) {
    critical.value = qt(1-alpha/2, df=n - 1)
  } else {
    critical.value = qnorm(1-alpha/2)
  }

  lower = xbar - (critical.value * se)
  upper = xbar + (critical.value * se)

  return(c(lower=lower, upper=upper))
}

### test it
x=rnorm(100,0,1.5)
meanCI(x)
meanCI(x,sd(x))                                        

conf.interval(x)

# binom test course project problem 0
binom.test(2,10,(1/2), alternative="two.sided")

############ course project - problem 10 Wilcoxon Signed-Ranks Test
n = 15
s = sqrt(n*(n+1)*(2*n+1)/24)
w = 14
numerator = w - (n*(n+1)/4)
z = numerator / s
2 * pvalue
# pval = 0.09668

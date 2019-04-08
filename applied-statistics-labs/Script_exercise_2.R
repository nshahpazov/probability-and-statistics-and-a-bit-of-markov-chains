              ############ APPLIED STATISTICS -> EXERCISE 2 ############

        
rm(list = ls()) # rm() -----> remove objects; ls() -----> list of all objects

norm.contaminated.sample <-function(mu1,sigma1,mu2=0,sigma2=1,proportion=1/2,size=1){
  p=runif(size,0,1)>proportion;
  return(rnorm(size,mu1+(mu2-mu1)*p,sigma1+(sigma2-sigma1)*p))
}
norm.contaminated.sample(100,1,proportion=1/5,size=20)

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
curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE, col=2,lwd=2)
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

meanCI = function(x,sigma=sd(x),confLevel=0.95){
  if (confLevel>=1 || confLevel<=0)
  {return ("The value of confLevel should be in the interval (0,1)")}
  else{
    n = length(x)
    meanX = mean(x)
    alpha = 1 - confLevel
    se = sigma/sqrt(n)
    if (missing(sigma))       
    {cv = qt(1-alpha/2,df=n-1)}
    else {cv=qnorm(1-alpha/2)}
    return(c(lowerBound=meanX-cv*se,upperBound=meanX+cv*se))
  }
}
### test it
x=rnorm(100,0,1.5)
meanCI(x)
meanCI(x,sd(x))                                        

              ############ APPLIED STATISTICS -> EXERCISE 1 ############


# http://cran.r-project.org/     ------> Official R website

# Alt + Shift + K   ------> All Keyboard Shortcuts


                                ### R as a Calculator ###
1*2*3*4*5
sqrt(10)
pi; exp(1)  #; for Multiple commands

aVariable1<-1
aVariable2<-2
aVariable3=3  # '<-' и '=' са еквивалентни
aVariable1
print(aVariable1+10) # print() може и да се пропусне
print(aVariable4<-4)
avariable1+10      # Главните букви са от значение
aVariable1+aVariable3

############################################################
rm(list = ls()) # rm() -----> remove objects; ls() -----> list of all objects

                              ### Variable types in R ###

# Variable types
x.numeric <- as.numeric(10.1)
x.integer <- as.integer(10)     # 10L
x.complex <- as.complex(10+2i)
x.char <- as.character("10+2i")
x.factor <- as.factor(10)
x.logical <- as.logical(TRUE)
x.logical <- as.logical(0)

# Casts
as.numeric(TRUE)
as.character(10)
as.integer(10.6)     
as.numeric("Hello")

# Class of variable
x<-2
class(x)
x+1
class(x.numeric)
class(x.integer)
class(x.factor)

# 'is.()' function
is.numeric(x.logical)
is.logical(x.logical)

# Cast factor variables
x.factor <- as.factor(10)
10+as.numeric(as.character(x.factor))


                                ### Vectors ###
x.vector <- c(3,2,1) # Define a vector

x.vector[2:3]
x.vector[3:1]
x.vector[c(TRUE,FALSE,TRUE)]

x.vector <- c("one",2:10,as.factor(99))
class(x.vector)
as.integer(x.vector)

x.vector<-1:5
x.vector[1]<-"One"
class(x.vector)

x.vector[c(2,3,1)]<-c(6,7,8)
class(x.vector) #character

x.vector[]<-5:1
class(x.vector) #character

x.vector<-5:1
class(x.vector) #integer


                        #mean();sd();median();mad()
x.vector<-c(1:20,100.5)
x.vector <- as.integer(x.vector)
mean(x.vector)
sd(x.vector)
median(x.vector)
mad(x.vector)

                                # which & any
any(abs((x.vector-median(x.vector)))>3*mad(x.vector))  
which(abs((x.vector-median(x.vector)))>3*mad(x.vector))


                                ### Задача 1 ### 
# Генерирайте 10000 данни от стандартно нормално рапределение (чрез rnorm).
# Пресметнете средната стойност на данните, като премахнете несъгласуваните наблюдения (>3*mad).
?rnorm
??rnorm
normData<-rnorm(10000,0,1)
mean(normData)
median(normData)
mad(normData)
sd(normData)
cutoff<-median(normData)+3*mad(normData)
mean(normData[which(normData<=cutoff & normData>=(-cutoff))])
mean(normData,trim=0.0026)
#################################################################################################

                                                  ### Задача 2 ### 
# Генерирайте 1000 проявления на случайната величина Z с плътност f_Z(x)=f_X(x)*99/100+f_Y(x)*1/100, 
# където f_X(x) и f_Y(x) са плътностите съответно на X~N(1,4) и Y~N(100,9). Има ли несъгласувани 
# наблюдения в получената извадка? Намерете ги и обяснете причината за наличието им.







                                ### Functions ###

mean.and.sd <- function (x) {
  av <- mean(x)
  sdev <- sd(x)
  return(c(mean=av, SD=sdev))
}
uniData<-runif(1000,min = 0,max = 1)  # runif - генерира равномерно разпределени сл.в.
1/2; sqrt(1/12)
mean.and.sd(uniData)

                                            ### Задача 3 ###
# Напишете функция rand.Circle(), която да генерира n на брой равномерно разпределени точки от кръг  
# с център (x_0,y_0) и радиус r. Използвайте функцията plot() за да изобразите резултата от изпълнението 
# на функцията rand.Circle().


rand.circle <- function (n, x_0, y_0, r) {
  theta = 2*pi*runif(n, 0, 1)
  # inverse transform sampling
  R = r * sqrt(runif(n, 0, r))
  
  x= x_0 + R*cos(theta)

  y = y_0 + R*sin(theta)

  return(cbind(x, y))
}

inverse.transform.sampling.exponential <- function (n, lambda) {
  # inverse transform sampling
  # F(x) = 1 - exp(-lx)
  results = -(1/lambda)*sapply(1-runif(n, 0, 1), log)
  return(results)
}
# test it with
# hist(inverse.transform.sampling.exponential(1000, 0.001))

# repeat pi generator
pi <- function (trials) {
  
}


plot(rand.circle(500, 2,2,10))
uniData<-runif(1000, min = 0,max = 1)  # runif - генерира равномерно разпределени сл.в.
1/2; sqrt(1/12)
mean.and.sd(uniData)

# Hint: Използвайте полярното представяне на точките от кръга:
# x=x_0+r*cos(theta)
# y=y_0+r*sin(theta)




                  ### if, else, for, repeat, while, break, next ###
# if Statement
a<-sample((-100):100,1)
if (a>0) print(paste("Числото",a,"е положително!",sep = " "))

# if () {} else {}
a<-sample(100:1000,1)
b=2
deliSe=paste("Числото",a,"се дели на",b,"!",sep = " ")
neSeDeli=paste("Числото",a,"не се дели на",b,"!",sep = " ")

if (a%%b==0) { print(deliSe) 
} else {print(neSeDeli)}

#ifelse()
ifelse(a%%b, neSeDeli, deliSe)

# for(... in ...){}
n=30
for (i in 1:n) {
  a<-sample(100:1000,1)
  if (a%%2==0 & a%%3==0) print(paste("Числото",a,"се дели на 6!",sep = " "))
}

# repeat + break
a=1
repeat {
  print(a)
  a=a+1
  if (a==10){
    break
  }
}

# while
a=1
while (a<10){
  print(a)
  a=a+1
}

# next
for(i in 1:10){
  if(i%%2==0) { print(i)
  } else next
}

                                                    ### Задача 4 ###
# Пресметнете вероятността в група от 23 човека да има поне двама, които имат рожден ден на една и
# съща дата от годината. Намерете приближение на тази вероятност като направите 10000 симулации на
# този експеримент.

p.comp=1;
for (i in 1:23){
  p.comp=p.comp*(366-i)/365;
}
(p=1-p.comp)

single.Sim <- function(n){
  group.Sim=as.integer(runif(n,1,366))
  any(duplicated(group.Sim))
  return(any(duplicated(group.Sim)))
}

count=0;
for (i in 1:10000){
  count=count+single.Sim(23);
}
(p.approx=count/10000)
#sum(sapply(rep(23,10000),single.Sim))/10000
####################################################################################################

                                                ### Задача 5.1 ###
# Напишете функция square.approx.Pi(), която да дава оценка на числото Пи, базирана на вероятността
# случайно избрана точка от единичния квадрат да е в единичния кръг. Нека аргумент на функцията е 
# trials - брой на генерираните точки от единичния квадрат.


square.approx.Pi = function (trials = 1000) {

  xs = runif(trials, -1, 1)
  ys = runif(trials, -1, 1)

  successes = sum(xs^2 + ys^2 <= 1)
  proportion = (successes / trials)
  approximation = 4 * proportion

  return(approximation)
}

square.approx.Pi(5000)
                                                  ### Задача 5.2 ###
# Направете 10000 оценки на числото Пи, като използвате функцията square.approx.Pi() 
# с аргумент trials=100. Съхранете полученият резултат във вектор approx.Pi и за него 
# намерете най-малкия елемент, най-големия елемент, средноаритметичното, медианата,
# стандартното отклонение, първия и третия квартил. Сравнете получените оценки за 
# очакване и дисперсия с теоретичните. Като използвате ЦГТ, постройте 95% доверителен интервал
# за очакването на approx.Pi.
n = 10000
trials = 100

approx.Pi = numeric(n)
for(i in 1:n) {
  approx.Pi[i] = square.approx.Pi(trials);
}
# approx.Pi = sapply(rep(trials,n),square.approx.Pi)

min(approx.Pi)
max(approx.Pi)
mean(approx.Pi)
pi # Theoretical mean
median(approx.Pi)
sd(approx.Pi) # emperical standard deviation of 4 * sample  proportion
th_sd = sqrt(pi*(4-pi) / (trials)) # Theoretical standard deviation of 4 * sample proportion
summary(approx.Pi)

alpha = 0.05
th.limits=c(lower.bound = qnorm(alpha/2,mean=pi,sd=sqrt(pi*(4-pi)/trials),lower.tail = TRUE),
           upper.bound=qnorm(alpha/2,mean=pi,sd=sqrt(pi*(4-pi)/trials),lower.tail = FALSE))
th.limits
emp.limits=c(lower.bound=qnorm(alpha/2,mean=mean(approx.Pi),sd=sd(approx.Pi),lower.tail = TRUE),
            upper.bound=qnorm(alpha/2,mean=mean(approx.Pi),sd=sd(approx.Pi),lower.tail = FALSE))
emp.limits

boxplot(approx.Pi)
hist(approx.Pi,probability = TRUE)

# beta and inverse integral transform
u = runif(20000); x = sqrt(u)
par(mfrow=c(1,2))
cutp.u = seq(0, 1, by=.1)
hist(u, prob=T, ylim=c(0,2), br=cutp.u, col=rainbow(12), main="UNIF(0,1)")
curve(dunif(x), add=T, n=10001, lwd=2)
cutp.x = sqrt(cutp.u)
hist(x, prob=T, ylim=c(0,2), br=cutp.x, col=rainbow(12), main="BETA(2,1)")
curve(dbeta(x,2,1), add=T, n=10001, lwd=2)
par(mfrow=c(1,1))

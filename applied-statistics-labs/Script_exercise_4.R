                        ######### APPLIED STATISTICS -> EXERCISE 4  #######
# experimenting
qnorm(seq(0.01,0.99,0.01), mean=0, sd=1)


rm(list = ls())                ########### Statistical test ##########

# generate 100 normally distributed observations
normal <-rnorm(100)
# with a bigger n the empirical will be more and more similar to the theoretical
hist(normal ,probability = TRUE)

quantile(normal, probs = seq(0, 1, 0.125))


# q values: theoretical vs emperical
qqnorm(normal, main = "Normal Dist")
qqline(normal, col=2)

tdist <- rt(100, df = 3)
hist(tdist, probability=TRUE)
qqnorm(tdist, main = "T Dist")
qqline(tdist, col=2)

shapiro.test(normal)
shapiro.test(tdist)

                                            ### Задача 1 ###  
# Постройте хистограма на данните mpg от mtcars и сравнете разпледелението им с нормалното  
# чрез q-q plot. Използвайки теста на Shapiro-Wilk, проверете хипотезата за нормално 
# разпределение на mpg. Какви изводи може да направите? 

data = mtcars$mpg
hist(data)

qqnorm(data, main = "mpg Dist")
qqline(data, col=2)

shapiro.test(data)
# we cannot reject based on that p-value
shapiro.test(tdist)

##############################################################################################

                                          ### Задача 2 ###  
# Като използвате тест на знаците, проверете дали има значима разлика в ефекта от двата типа 
# лекарства върху продължителността на съня на изследваните студенти от данните sleep.

data(sleep)
?sleep
sleep_wide <- data.frame(
  ID=1:10,
  group1=sleep$extra[1:10],
  group2=sleep$extra[11:20]
)
group1 = sleep_wide$group1
group2 = sleep_wide$group2

count.plus = sum((group1 - group2) >= 0);
n = length(sleep_wide$group1);
binom.test(count.plus, n, 0.5, alternative = c("two.sided"))

##############################################################################################
ndata<-rnorm(1000,mean = mean(mtcars$mpg), sd= sd(mtcars$mpg))
qqnorm(ndata,main = "Sample Dist")
qqline(ndata,col=2)
ks.test(mtcars$mpg,ndata)
ks.test(mtcars$mpg,"pnorm",mean(mtcars$mpg),sd(mtcars$mpg))

cor.test(normal,tdist,method = ("pearson"))
cor.test(mtcars$hp,mtcars$cyl,method = ("pearson"))
cor.test(mtcars$hp,mtcars$cyl,method = ("kendall"))
cor.test(mtcars$hp,mtcars$cyl,method = ("spearman"))

mean(mtcars$cyl)
t.test(mtcars$cyl,alternative = ("two.sided"),mu=6)
mean(mtcars$mpg)
t.test(mtcars$mpg,alternative = ("less"),mu=20)

t.test(mpg~am,data=mtcars)
t.test(mpg~am,data=mtcars,var.equal=TRUE)
var.test(mpg~am,data=mtcars)

install.packages("HSAUR")         # Install package for datasets

                                          ### Задача 3 ###
# Като използвате данните "water" от пакета "HSAUR", проверете дали има зависимост 
# между смъртността и положението на населеното място, както и между твърдостта 
# на водата и положението на населеното място.

data(water,package = "HSAUR")
??water
str(water)
edit(water)

morSouth=water$mortality[water$location=="South"]
morNorth=water$mortality[water$location=="North"]
hardSouth=water$hardness[water$location=="South"]
hardNorth=water$hardness[water$location=="North"]
ks.test(morSouth,morNorth)
ks.test(hardSouth,hardNorth)

cor.test(~mortality + hardness, data = water)
t.test(mortality~location,data=water)
t.test(hardness~location,data=water)
row1=tapply(water$mortality,water$location,mean)
row2=tapply(water$hardness,water$location,mean)
tab=rbind(row1,row2)
rownames(tab)=c("mortality","hardness")
tab

##############################################################################################

                                            ### Задача 4 ###
# Като използвате данните "waves" от пакета "HSAUR", проверете дали има значима разлика
# между двата метода на закрепване (mooring).








##############################################################################################
                                            ### Задача 5 ###
# Като използжате данните 'pi2000' от пакета 'UsingR', постройте barplot за вероятността за
# всяка цифра от 0 до 9, срещащи се в първите 2000 цифри на числото пи. Добавете линията,  
# сътветстваща на вероятността при равномерно разпределение. Статистически еднакви ли са 
# вероятностите за цифрите от 0 до 9?

install.packages("UsingR")
data("pi2000",package = "UsingR")
str(pi2000)
t=table(pi2000)
n=length(t)
barplot(t/length(pi2000),
        col=rgb(1:n,1:n,1:n,maxColorValue=n),cex.axis=1.5, cex.lab=1.5,cex.names=1.5,
        xlab = "Digit",ylab = "Probability", main = "First 2000 Digits in PI")
abline(h=1/n,col=2,lty=2,lwd=3)
chisq.test(t,p=rep(1/n,n))

##############################################################################################
                                            ### Задача 6 ###
# Като използвате данните "pistonrings" от пакета "HSAUR", проверете дали положението 
# на буталото (leg) оказва еднакво влияние за дефектите (failures) при различните 
# компресори (compressor).







##############################################################################################
                                        ### Задача 7 ###
# Симулирайте хвърляне на правилен зар и проверете дали получените честоти съвпадат 
# с теоретично очакваните. Повторете симулацията за правилен и неправилен зар.

roll = as.factor(sample(1:6, size=100, replace=TRUE))
(freq = table(roll))
probs = rep(1/6, 6)
chisq.test(freq, p = probs)

dieFair = as.factor(sample(1:6,100,p=c(1,1,1,1,1,1)/6,replace=T))
dieBias = as.factor(sample(1:6,100,p=c(0.5,0.5,0.5,1.5,1.5,1.5)/6,replace=T))
chisq.test(dieBias, dieFair) # WRONG!!!!
tFair = table(dieFair)
tBias = table(dieBias)
chisq.test(rbind(tFair,tBias))
##############################################################################################
riffle.shuffle <- function (cards) {
  N = length(cards);
  split = rbinom(1, N, 1/2);
  if (split == 0 | split == N) {
    return(cards);
  }
  c1 = 0;
  c2 = 0;
  result = integer(N);
  for (i in 1:N){
    if (runif(1) < (split-c1) / (N-c1-c2)){
      c1 = c1 + 1;
      result[i] = cards[c1];
    }
    else {
      c2 = c2+1;
      result[i] = cards[split+c2];
    }
  }
  return(result);
}
riffle.shuffle(1:10)
riffle.shuffle(1:32)
N=52;
(rf=riffle.shuffle(1:N))
if (any((rf-1:N)>0)) {
  split=rf[min(which((rf-1:N)>0))]
} else{split=N}
identity=1:N;
plot(identity,col=2+2*(identity<split),pch = 19, cex = 2, lty = "solid")
text(identity, labels=identity, cex= 1.25, pos=1+2*(identity<split))

plot(rf,col=2+2*(rf<split),pch = 19, cex = 2, lty = "solid")
text(rf, labels=rf, cex= 1.25, pos=1+2*(rf<split))

install.packages("manipulate")     
library(manipulate)
manipulate({plot(c(rf[1:x],x-sum(rf[1:x]<split)+(sum(rf[1:x]<split)+1):(split-1),(split+x-sum(rf[1:x]<split)):N),col=2+2*(c(rf[1:x],sort(rf[(x+1):N]))<split),pch = 19, cex = 2, lty = "solid");
           text(c(rf[1:x],x-sum(rf[1:x]<split)+(sum(rf[1:x]<split)+1):(split-1),(split+x-sum(rf[1:x]<split)):N), labels=c(rf[1:x],sort(rf[(x+1):N])), cex= 1.25, pos=1+2*(c(rf[1:x],sort(rf[(x+1):N]))<split))},           
           x=slider(1,N))

############################################### for homework ############################
                                                    ### Задача 8 ###
# Като използвате функцията riffle.shuffle() и хи-квадрат тест за съгласуваност, намерете колко пъти е необходимо
# да се приложи riffle shuffle за да се разбърка едно тесте карти по случаен начин.

m = 

for(i in 1:100) {
  m = max(runif(300)) 
}

                                      
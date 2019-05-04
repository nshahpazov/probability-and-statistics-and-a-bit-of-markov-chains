                         ######### APPLIED STATISTICS -> EXERCISE 5 #######

rm(list = ls())                   #### Simple Linear Regression ####         
install.packages("ISwR")
data("thuesen", package = "ISwR")
??thuesen
str(thuesen)
edit(thuesen)
options(na.action=na.omit) # omit NA's
attach(thuesen)
summary(thuesen)
plot(blood.glucose, short.velocity, xlim=c(0,20), ylim=c(1,2))
lm(short.velocity ~ blood.glucose)
model1 = lm(short.velocity ~ blood.glucose)
detach(thuesen)
theFormula = short.velocity ~ blood.glucose
model1 = lm(formula = theFormula,data=thuesen)
model1
str(model1)
summary(model1)               # analysis summary
abline(model1)                # add regression line
coef(model1)                  # regression coefficients
confint(model1,level = 0.95)  # confidence interval for regression coefficients
resid(model1)                 # residuals
fitted(model1)                # fitted values
deviance(model1)              # residual sum of squares
predict(model1,newdata=ndat)  # predict for new data

predict(model1)
x=as.data.frame(thuesen$blood.glucose[is.na(thuesen$short.velocity)])
names(x)="blood.glucose"
predict(model1,newdata=x)
sum(coef(model1)*c(1,as.numeric(x)))

plot(fitted(model1),resid(model1))
qqnorm(resid(model1))

plot(thuesen$blood.glucose, thuesen$short.velocity, xlim=c(0,20), ylim=c(1,2))
abline(model1$coefficients, col='red', lwd=2)
points(thuesen$blood.glucose,predict(model1),pch=10)    # WRONG!
mv = is.na(thuesen$short.velocity)
points(thuesen$blood.glucose[-which(mv)],predict(model1),pch=10)

segments(thuesen$blood.glucose,fitted(model1),
         thuesen$blood.glucose,thuesen$short.velocity) # WRONG!

plot(thuesen$blood.glucose, thuesen$short.velocity, xlim=c(0,20), ylim=c(1,2))
abline(model1$coefficients, col='red', lwd=2)
segments(thuesen$blood.glucose[!mv],fitted(model1),
         thuesen$blood.glucose[!mv],thuesen$short.velocity[!mv])

options(na.action=na.exclude) # exclude NA's

model1=lm(formula = theFormula,data=thuesen)
fitted(model1)
plot(thuesen$blood.glucose, thuesen$short.velocity, xlim=c(0,20), ylim=c(1,2))
abline(model1$coefficients, col='red', lwd=2)
segments(thuesen$blood.glucose,fitted(model1), thuesen$blood.glucose,thuesen$short.velocity)

                                            #### Задача 1 ####
# Начертайте и проектирайте в различен цвят наблюденията от данните 'thuesen', чиито грешки в 
# линейния модел са извън 90%-доверителен интерал, базиран на нормалното разпределение и оценената
# стандартна грешка от модела.

options(na.action=na.omit)
alpha=0.10
model1=lm(short.velocity ~ blood.glucose,data=thuesen)
residuals1=resid(model1)
se=sqrt(sum(residuals1^2)/(model1$df.residual))
cv=qnorm(alpha/2,0,se,lower.tail=TRUE)
mv = is.na(thuesen$short.velocity)
plot(thuesen$blood.glucose[!mv], thuesen$short.velocity[!mv], 
     col = 1+(abs(residuals1)>=abs(cv)), pch=16, xlim=c(0,20), ylim=c(1,2))
abline(model1,col=2, lwd=2)
segments(thuesen$blood.glucose[!mv],fitted(model1), 
         thuesen$blood.glucose[!mv],thuesen$short.velocity[!mv],
         col = 1+(abs(residuals1)>=abs(cv)), lty=2)
#############################################################################################

minBG=floor(min(thuesen$blood.glucose))
maxBG=floor(max(thuesen$blood.glucose))+1
predData <- data.frame(blood.glucose=minBG:maxBG)
pp <- predict(model1, int="p", newdata=predData)
pc <- predict(model1, int="c", newdata=predData)
plot(thuesen$blood.glucose,thuesen$short.velocity,
     ylim=range(thuesen$short.velocity, pp, na.rm=T))
matlines(predData, pc[,2:3], lty=c(2,2),col=c(4,4))
matlines(predData, pp[,2:3], lty=c(3,3),col=c(2,2))

rm(list=ls())
data("thuesen",package = "ISwR")
with(thuesen,{
  model1=lm(short.velocity ~ blood.glucose)
  summary(model1)
  minBG=floor(min(blood.glucose))
  maxBG=floor(max(blood.glucose))+1
  predData <- data.frame(blood.glucose=minBG:maxBG)
  pp <- predict(model1, int="p", newdata=predData,level=0.95)
  pc <- predict(model1, int="c", newdata=predData)
  plot(blood.glucose, short.velocity, xlim=range(blood.glucose),
       ylim=range(short.velocity, pp, na.rm=T),pch=21,cex=1.5,bg=rgb(1/3,1/3,1/3))
  abline(model1, col=rgb(220/255,0,0), lwd=3)
  # segments(blood.glucose,fitted(model1),blood.glucose,short.velocity,lty=2)
  matlines(predData, pc[,2:3], lty=c(2,2),col=rgb(0,1/2,0),lwd=c(2,2))
  matlines(predData, pp[,2:3], lty=c(3,3),col=rgb(0,0,0),lwd=c(2,2))
  legend('bottomright',
         legend = c('Regression line','Confidence lines','Prediction lines'),
         col=c(rgb(220/255,0,0),rgb(0,1/2,0),rgb(0,0,0)),lty=c(1,2,3),lwd=c(3,2,2))
})
##################################################################################################

options(na.action=na.exclude) # exclude NA's
install.packages("manipulate")     
library(manipulate)
manipulate(
  with(thuesen,{
    model1=lm(short.velocity[1:N] ~ blood.glucose[1:N])
    plot(blood.glucose[1:N], short.velocity[1:N],pch=21,cex=1.5,bg=rgb(1/3,1/3,1/3),
         xlim=range(blood.glucose),  ylim=range(short.velocity, na.rm=T))
    abline(model1, col=rgb(220/255,0,0), lwd=3)
    segments(blood.glucose[1:N],fitted(model1),blood.glucose[1:N],short.velocity[1:N],lty=2)
  }),           
  N=slider(2,length(thuesen$short.velocity)))
##################################################################################################

library(readxl)
read_excel("Data\\Cars_raw.xls",sheet=1)
dataM=as.data.frame(read_excel("Data\\Cars_raw.xls",sheet=2)) # data for modelling
dataP=as.data.frame(read_excel("Data\\Cars_raw.xls",sheet=3)) # data for predictions
str(dataM)
str(dataP)
                                            #### Задача 2 ####
# Като изплозвате данните 'dataM' по-горе, конструирайте и изследвайте прост линеен модел за
# предсказване на цената на автомобил втора употреба 'Y' на база на най-подходящата променлива 'X'. 
# Какви изводи може да направите?




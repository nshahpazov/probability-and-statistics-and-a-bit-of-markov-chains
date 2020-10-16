           ######### APPLIED STATISTICS -> EXERCISE 7 (continuation of Exercise 6) #######


rm(list=ls())                    ### Quasi-multicollinearity ###           
library(readxl)
read_excel("Data\\Cars_raw.xls",sheet=1)
dataM=as.data.frame(read_excel("Data\\Cars_raw.xls",sheet=2)) # data for modelling
# dataM=read.csv("Data\\Cars_raw_M.csv",sep = ";")
corTable=cor(dataM)

model2=lm(Y~X2+X4,data = dataM)
summary(model2)

QMC_X10=lm(X10~X2+X4,data = dataM)
summary(QMC_X10)
model3=lm(Y~X2+X4+X10,data = dataM)
summary(model3)

QMC_X1=lm(X1~X2+X4+X10,data = dataM)
summary(QMC_X1)
model4=lm(Y~X2+X4+X10+X1,data = dataM)
summary(model4)

QMC_X3=lm(X3~X2+X4+X10,data = dataM)
summary(QMC_X3)
model5=lm(Y~X2+X4+X10+X3,data = dataM)
summary(model5)

QMC_X5=lm(X5~X2+X4+X10+X3,data = dataM)
summary(QMC_X5)
model6=lm(Y~X2+X4+X10+X3+X5,data = dataM)
summary(model6)

QMC_X8=lm(X8~X2+X4+X10+X3+X5,data = dataM)
summary(QMC_X8)
model7=lm(Y~X2+X4+X10+X3+X5+X8,data = dataM)
summary(model7)

QMC_X7=lm(X7~X2+X4+X10+X3+X5,data = dataM)
summary(QMC_X7)

QMC_X9=lm(X9~X2+X4+X10+X3+X5,data = dataM)
summary(QMC_X9)
model8=lm(Y~X2+X4+X10+X3+X5+X9,data = dataM)
summary(model8)

QMC_X6=lm(X6~X2+X4+X10+X3+X5+X9,data = dataM)
summary(QMC_X6)
model9=lm(Y~X2+X4+X10+X3+X5+X9+X6,data = dataM)
summary(model9)

model=lm(Y~X2+X4+X10+X3+X5+X9,data = dataM)
summary(model)
plot(model)
# Check the assumptions of the model

                                            ### Predictions ###

se=sqrt(deviance(model)/df.residual(model))
se                  # standard error
rse=se/mean(dataM$Y)
rse                 # realtive standard error

dataP=as.data.frame(read_excel("Data\\Cars_raw.xls",sheet=3)) # data for modelling
# dataP=read.csv("Data\\Cars_raw_P.csv",sep = ";")
predict(model, int="p", newdata=dataP)
Y_predicted=predict(model, newdata=dataP)

MAE=mean(abs(dataP$Y-Y_predicted))  # Mean Absolute Error
MSE=mean((dataP$Y-Y_predicted)^2)   # Mean Squared Error
RMSE=sqrt(MSE)                      # Root Mean Squared Error
MAE; RMSE

MARE=mean(abs((dataP$Y-Y_predicted)/dataP$Y))  # Mean Absolute Relative Error
MSRE=mean(((dataP$Y-Y_predicted)/dataP$Y)^2)  # Mean Squared Relative Error
RMSRE=sqrt(MSRE)                              # Root Mean Squared Relative Error
MARE; RMSRE

PMAE=MAE/mean(dataP$Y)      # Percentage Mean Absolute Error
PRMSE=RMSE/mean(dataP$Y)    # Percentage Root Mean Squared Error
PMAE; PRMSE

Theil_coeff=sqrt(mean((dataP$Y-Y_predicted)^2))/sqrt(mean((dataP$Y)^2)+mean((Y_predicted)^2))

cor.test(Y_predicted,dataP$Y)

plot(dataP$Y, Y_predicted, pch=21,cex=1.5,bg=rgb(1/3,1/3,1/3))
abline(lm(dataP$Y~Y_predicted), col=rgb(220/255,0,0), lwd=3)
summary(lm(dataP$Y~Y_predicted))

plot(dataP$Y, Y_predicted, pch=21,cex=1.5,bg=rgb(1/3,1/3,1/3))
abline(lm(dataP$Y~Y_predicted-1), col=rgb(220/255,0,0), lwd=3)
summary(lm(dataP$Y~Y_predicted-1))
#################################################################################################

rm(list=ls())                          
Advertising=read.csv("./Advertising.csv",sep = ",",colClasses=c("NULL",NA,NA,NA,NA))
str(Advertising)


                                              #### Задача 3 ####
# Като изплозвате данните 'Advertising' по-горе, отговорете на следните въпроси:
# а) Има ли зависимост между продажбите и бюджета за реклама?
cor(Advertising$sales, Advertising$TV + Advertising$radio+Advertising$newspaper)

# is it normal, Shapiro
shapiro.test(Advertising$sales)
# p-value is small so they are not normal
# we can still use cor.test since from clt 
cor.test(Advertising$sales, Advertising$TV + Advertising$radio+Advertising$newspaper)

# б) Колко силна е тази зависимост?
# в) Линейна ли е зависимостта?
lmodel1 = lm(Advertising$sales ~ Advertising$TV + Advertising$radio+Advertising$newspaper)
summary(lmodel1)

# г) Кои медии допринасят за увеличение на продажбите?
cor(Advertising)
plot(Advertising)
# д) Колко голям е ефектът на всяка от медиите върху продажбите?
lmodel2 = lm(Advertising$sales ~ Advertising$TV)
summary(lmodel2)
# е) С каква точност могат да се предскажат бъдещите продажби?
# ж) Подходящо ли е използването на неадитивен модел?



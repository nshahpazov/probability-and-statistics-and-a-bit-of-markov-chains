# Зад 1.
# Създайте вектор x = { 8 3 8 7 15 9 12 4 9 10 5 1 }.
# Превърнете вектора в матрица 6х2. Задайте имена на редовете
# на матрицата - r1, r2 ... Добавете още една колона съдържаща
# нечетните числа- 1, 3, 5 ... Сортирайте матрицата по първа колона
# в нарастващ ред. Сортирайте по първите две колони.


# Зад 2.
# Разгледайте данните ’homedata’ от пакета ’UsingR’. Съз-
#  дайте два вектора съдържащи цените на къщите през 1970 и 2000г.
# Определете:
# а) най-скъпата и най-евтината къща през 2000г, техните цени
# през 1970г;
# б) цените на 5-те най скъпи къщи през 2000г;
# в) броят на къщите по-скъпи от 750 000 през 2000г;
# г) средната цена през 1970г. на къщите от в);
# д) цената през 2000г. на тези къщи, чиято цена е намаляла;
# е) 10-те къщи с най-голямо процентно увеличение на цената.
# Разделете къщите на три групи евтини с цена през 2000г. до
# 100 000, средни с цена от 100 000 до 500 000 и скъпи над 500 000.
# Пресметнете броя на къщите и средната цена във всяка от групите.

#########################################
###### PROBLEM 1 ########################
#########################################
x = c(8, 3, 8, 7, 15, 9, 12, 4, 9, 10, 5, 1)
# change dimensions and turn it into matrix
dim(x) = c(6, 2)
# or use matrix
m = matrix(x, nrow = 6, ncol = 2);

# change row names of a matrix
rownames(x) = c('r1', 'r2', 'r3', 'r4', 'r5', 'r6')
# create a sequence from 1 to 6 with difference 2
t = seq(1, by = 2, length.out = 6)
# append a vector as a column to the matrix
x = cbind(x, t)
# get indexes of sorted by first two columns
indexes_of_sorted = order(x[,1], x[, 2])
x[indexes_of_sorted, ]

########################################
######### PROBLEM 2 ####################
########################################
# install.packages("UsingR") - to install new packages
# library("UsingR")
data("homedata")
attach(homedata)
y2000 = homedata$y2000
y1970 = homedata$y1970
which(y2000 == max(y2000))
sum(y2000 > 7500000)
y2000[y2000 > 750000]
which(y2000 > 750000)

mean(y1970[y2000 > 750000])
# цената през 2000г. на тези къщи, чиято цена е намаляла;
which(y2000 < y1970)
# (x - y) / y
# е) 10-те къщи с най-голямо процентно увеличение на цената
sort((y1970 - y2000) / y1970)[1:20]
(y2000 - y1970) / y1970
# cut(x, c())
borders = c(0, 100000, 500000, max(y2000))
x2000 = cut(y2000, borders)

# structure
str(x2000)
# get names of factors
levels(x2000)
# set the names of the factors
levels(x2000) = c('cheap', 'normal', 'expensive')

# barplot - draws a barchart
# pie - draws a piechart
# table - summary of the result
# parameter
par(bg = '#336699')
# barplot(table(x2000),  col = c('magenta', '#339966', 'violet'))

# par(bg = '#336699')
pie(table(x2000),  col = c('#aa55FF', '#339966', '#ff5511'))

# get quantiles\percentiles
quantile(y2000, c(0.3, 0.5, 0.7))

# boxplot
boxplot(y2000, y1970, horizontal = T)
# boxplot(homedata, horizontal = T)

# histogram :- h = hist(y2000)
# density?
x = hist(y2000, probability = T)
# polygon diagram - connect the centers of the bars
lines(x$mids, x$counts)

cx = c(0, x$mids, max(x$breaks))
cy = c(0, x$counts, 0)
lines(cx, cy, col = 'red')

br = seq(0, max(y2000), 50000)
br[25] = 1200000

hist(y2000, breaks = br, probability = T)
d = density(y2000)
lines(d, col="#9911AA")

# split screen
split.screen(c(2, 1))
screen(2)
hist(y2000)
screen(1)
hist(y1970)

# P.S. R is the file extension
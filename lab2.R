# monika peteva - mpeteva@uni-sofia.bg
# The book for the course is simpleR

x <- c(1, 2, 3, 4, 5)

y = matrix(x, 5, 5)

# change dimension
r = 1:49
dim(r) = c(7, 7)
  
# printing an object
# print(y)

A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

# range of values
rg1 = 1:100
rg2 = range(5:10) 

# rep - repeats a value n times
fives = rep(5, times = 4)

# determinant of a matrix
detOfY = det(y)

# using the help
# help(rep)
# ?rep

rep(1:4, 5)

# each number from the range is repeated 3 times
rep(1:4, each = 3)

# component addition
c(1, 2, 3) + c(4, 5, 6) # 7 is added with 1



# installing packages and loading
# install.packages('UsingR')
# library('UsingR')

# typos on pages
pages.typos = c(5,4,12,17,16,22,6,22,5,22,22,12,13,12,1,2,5,8)
pages.typos2 = c(0,3,1,1,6,2,6,15,5,14,12,6,13,6,1,1,4,5)
indices = c(1,3,7)
pages.errors[indices] # 1st, 3rd and 7th value of the array
pages.typos[-3] # everything except the 3rd element
pages.typos[5:7] # values from 5 to seven including
errors = pages.typos # makes a deep copy of x and not just reference copying

worst = max(pages.typos) # get the maximum number of typos
pages.errors == worst # map to booleans for which the value is worst
which(pages.typos == worst) # get indices of those which have most typos

# another way is to create a vector of indices and map it for those
# values where the page typos are at it's worst
number_of_pages = length(pages.typos)
all_indices = 1:number_of_pages
all_indices[pages.typos == worst] # logical extraction. Very useful
one_less = pages.typos - 1 # reducing the typos of every page with 1

# generating sequences, type ?seq to see what it does
seq(from = 1, to = 50, by = 3) # creates a sequence from 1 to 50 by 3

sum(pages.typos) # get the total number of typos on all pages
sum(pages.typos > 4) # number of pages with more than 4 typos
pages.typos - pages.typos2 # difference of number of typos between the two drafts

# stock example
# suppose the daily closing price of your favorite stock
#   for two weeks is

stock.prices = c(45,43,46,48,51,46,50,47,46,45)
mean(stock.prices) # the mean
median(stock.prices) # the median
max(stock.prices) # the max
min(stock.prices) # the min
stock.more_prices = c(stock.prices, 55, 14, 12) # append more values
length(stock.more_prices)
stock.more_prices[1:5] = c(100, 101, 5000, 12, 44) # alter some of the values
# data.entry(stock.prices) # open gui for editing spreadsheet
# stock.prices = de(stock.prices) # same only, doesn’t save changes
# stock.prices = edit(stock.prices) # opens an editor
day=5
mean(stock.prices[day:(day + 4)]) # get the mean only for days 5 to 9
# the running maximum is a sequence {max(a1,...,ak)}(k=1...n)
cummax(stock.prices) # returns the running maximum
cummin(stock.prices) # returns the running minimum


# example: working with mathematics
whale = c(74, 122, 235, 111, 292, 111, 211, 133, 156, 79)
x = whale
var(whale) # variance

sqrt(var(whale)) # standard deviation is sqrt of the variance
# standard deviation as a formula
sqrt(sum((whale - mean(whale))^2 / (length(whale) - 1)))
# make a function for the standard deviation
std = function (x) sqrt(var(x))
sd(whale) # the actual function for standard deviation
whale[whale > 3] # all greater than some value
x[ x< -2 | x > 2] # bigger than or less than some values
x[(length(x)-5):length(x)] # last k elements
diff(whale, lag=2) # suitably lagged differences

############## PROBLEMS #########################
# commutes per month
miles = c(65311, 65624, 65908, 66219, 66499, 66821, 67145, 67447)
commutes = c(17, 16, 20, 24, 22, 15, 21, 15, 17, 22)
sum(commutes < 17) / length(commutes) # percentage of those < 17

# 2.3 year bills
bills = c(46, 33, 39, 37, 46, 30, 48, 32, 49, 35, 30, 48)
sum(bills > 40)

x = c(1,3,5,7,9)
y = c(2,3,5,7,11,13)

x = c(1, 8, 2, 6, 3, 8, 5, 5, 5, 5)
# logarithms
log(x, base=3)
log2(x)
log10(x)

(x-4.4) /2.875
max(x) - min(x) # diff between max and min

rep(1, 5) # repeats 1 5 times

############ lab 3 ###############
####### Categorical Data ############

smokers = c("maybe", "no", "maybe","maybe", "no", "yes","yes")
table(smokers) # shows frequency of each element

factor(smokers) # shows all the different categories ? maybe?

# show a scaled barplot
barplot(table(smokers) / length(smokers))

# piechart the smokers categoricaly constructed
pie(table(smokers), col = rainbow(24))

summary(x) # five number salary

fivenum(x)

class(x) # tells you the type of the variable

# probability that a person is a smoker
sum(survey$Smoke == "Regul", na.rm = TRUE) / length(survey$Smoke)

# probability for a man
man.prob = sum(survey$Sex == "Male", na.rm = TRUE) / length(survey$Sex)

# probability for a man and a smoker
regulars = survey$Smoke == "Regul"
men = survey$Sex == "Male"
size = length(survey$Smoke)
man.and.smoker.prob = sum(regulars & male, na.rm = TRUE) / size

# probability for a smoker if we know it is a man
# i.e. probability that a randomly selected man is a smoker
# that is conditional probability: P(A|B) = P(A^B) / P(B)
smoker.man.prob = man.and.smoker.prob / man.prob



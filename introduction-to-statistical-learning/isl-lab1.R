x <- c(0.5, 0.5, 0.25, 0.75) # create a vector
y <- c(1,2,3,4)
x + y # summing vectors


# show all variables
ls()

# remove a variable
rm("y")
rm(list = ls())

# creating a matrix by row
m = matrix(data = x, nrow = 2, ncol = 2, byrow = TRUE)
# operations with matrices
m^16; 2 * m; m + m 

set.seed(1003) # this doesn't work
rnorm(100) # generate 100 observations of the normal distribution

cor(x, 2*x) # corelation between x and y

# create a pdf report in current directory
pdf("./figure.pdf")
plot(x,y,col="green")
dev.off()

# seq(1,10) = 1:10

# create a contour
# contour(x, y, m)

# create a contour
l = -20:20
f = function (x, y) {
  return(sin(x)+cos(y))
}

call.func = function (a, b, f) {
  result = sapply(a, function(x) sapply(b, function(y) f(x,y)))
  return(result)
}

contour(l, l, call.func(l, f))

# heatmap
image(l, l, call.func(l, f))

# some perspective into it
persp(l, l, call.func(l, f), theta=120, phi=40)

# indexing data
m=matrix(1:4, 2,2)
m[1,1] # first element
m[1,] # first row
m[,1] # first column
dim(m) # dimension of a matrix

# loading and writing data
# load.table, load.csv
# write.csv(m, file = "./filename.csv")
plot(dat$TV, dat$sales)
identify(dat$TV, dat$sales) # lets you mark points with the mouse

summary(dat) # create a summary, duhh
# savehistory()
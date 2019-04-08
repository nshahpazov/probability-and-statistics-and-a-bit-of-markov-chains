# task 1
custom_function = function (x) {
  sum = 0;
  for(s in 1:x) {
    sum = sum + (100 * s^3) / (9 + 4 * x^4);
  }
  return(sum);
}

v = c(1, 2, 3, 4, 5, 6, 7, 8);
# apply to every element
w = sapply(v, custom_function);
mean(w);

# task 2 - birthdays problem
birthdays = function (p) {
  probability = 1;
  i = 1;
  days = 365:1;
  repeat {
    probability = probability * (days[i] / 366);
    if (probability < 1 - p) {
      break;
    }
    i = i + 1
  }
  return(i);
}

# task 3
# find the number of 6s in a sample 100 coin throws
sample_of_sixes = sample(1:6, 100, replace = TRUE);
number_of_6s = length(sample_of_sixes[sample_of_sixes == 6])

# task 4
library(stringr)
coins = function () {
  coins = paste(sample(c('E', 'T'), 100, replace = TRUE), collapse = '')
  str_locate_all(pattern ='EETET', coins)[[1]][1, 2]
}

# runif() :- uniform distribution

# task 5
# X is runif [0, 10], Y is runif [3, 9]
X = runif(100, 0, 10);
Y = runif(100, 3, 9);
split.screen(c(2, 1));
screen(1);
boxplot(X, horizontal=TRUE);
screen(2);
boxplot(Y, horizontal=TRUE);
mean_x = mean(X);
mean_y = mean(Y);
sd_x = sd(X)
sd_y = sd(Y)

# task 6
Z = runif(200)
hist(Z, probability = TRUE, col = colors)

# task 7
hist(X, probability = TRUE, col = colors)

d = density(X)
lines(d, col="#ff0000")

# task 8
x = pnorm(20, mean = 25, sd = 6)
x + (1 - x) / 2;

# task extra
br = seq(0, max(y2000), 50000);
br[25] = 1200000;

hist(y2000, breaks = br, probability = T);
d = density(y2000);
lines(d, col="#9911AA");


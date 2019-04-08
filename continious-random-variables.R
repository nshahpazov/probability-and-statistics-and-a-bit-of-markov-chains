# task 1
Z = runif(200);
hist(Z, probability = TRUE, col = colors);

# task 2
expo = rexp(200, rate = 1);
hist(expo, probability = TRUE, col = colors);
lines(density(expo), col="#ff0000");

rd = ruinf(200, rate = 1);
hist(rd, probability = TRUE, col = colors);
hist(rd, probability = TRUE, col = colors);
lines(density(rd), col="#ff0000");

# task 3
pnorm(1) - pnorm(-1);
pnorm(2) - pnorm(-2);
pnorm(3) - pnorm(-3);

# task 4
watermelons = pnorm(20, mean = 25, sd = 6);
watermelons + (1 - watermelons) / 2;

# task 5
# n = 2, 10, 100, 1000?
t = function (n) {
  foo <- c();
  for (i in 1:n) {
    x = rexp(100, rate=1);
    foo <- append(foo, x)
  }
  return(foo);
}

# task 6 Нека сл.в. X е гамма разпределена с параметри 2 и 0.5. Определете:
# а) P(X < 1);
pgamma(1, 2, 0.5);
# б) P(X > 2);
pgamma(2, 2, 0.5, lower.tail=FALSE);
# в) c, така че P(X > c) = 0.35;
qgamma(0.35, 2, 0.5);
# г) Q1, M, Q3.
xgamma = rgamma(1000, 2, 0.5);
quantile(xgamma)[2:4];

# task 7
# Хвърлят се 20 зара. Каква е вероятността да се паднат по-малко от 3 шестици?
# Сравнете теоретичната вероятност с експериментални данни.
sum(dbinom(0:3, 20, 0.16666666666));

# task 8
# Зад.8 Времето X, което клиент чака на опашка е експоненциално разпределена сл.в. с математическо очакване 5 мин. Каква е вероятноста клиентът да чака повече от 5 мин? Определете емпирично формулата за математическото очакване на X, ако параметъра на разпределението е λ.
pexp(5, 1/5, lower.tail=FALSE);


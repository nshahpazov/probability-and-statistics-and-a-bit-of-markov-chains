y = c(10, 6, 5, 12, 10, 15, 5, 12, 17, 20)
x = c(1.30, 2, 1.7, 1.5, 1.6, 1.2, 1.6, 1.4, 1, 1.10)
plot(x, y)

predicted_error = serror1 * sqrt(1.1 + ((1.63-mean(x))^2)/(sum((x-mean(x))^2)))
x1 = c(1.63)
X[,2]
pred1.2 = serror1 * 
  sqrt(1.1 + 
    t(x1-mean(X[,2])) %*% 
    solve(
      t(X[,2]-mean(X[,2])) %*% (X[,2]-mean(X[,2]))
    ) 
    %*% (x1-mean(X[,2])))

8.4398 + 2.306004 * predicted_error

X2 = matrix(c(
      1, 1, 1, 1, 1, 1, 1,
      -3, -2, -1, 0, 1, 2, 3,
       5, 0, -3, -4, -3, 0, 5,
       -1, 1, 1, 0, -1, -1, 1),
nrow=7, ncol=4)

lm(y ~ X2[,-1])

bhats = solve(t(X2) %*% X2) %*% t(X2) %*% y
X2 %*% betas2


serror2 = deviance(lm(y2 ~ X2[,-1])) / 3
means = c(mean(X2[,2]), mean(X2[,3]), mean(X2[,4]))


pred2 = serror2 * 
  sqrt(1 + 1/7 + t(X2[5,-1]) %*% solve(t(X2[,-1]) %*% X2[,-1]) %*% X2[5,-1])

# testing the hypothesis that beta1 = beta2 = 0
A = matrix(c(0,1,0,0,0,0,1,0), nrow=2, ncol=4, byrow = TRUE)
c = c(0,0)

Abhat = A %*% bhats
T = solve(A %*% solve(t(X2) %*% X2) %*% t(A))
F = (t((Abhat)) %*% T %*% Abhat)/2*serror2

csv
mpg = Auto[,1]
dm = data.matrix(Auto[,-c(1,8,9)])
lambda = 5
design = cbind(intercept=1, dm)
bhats = solve(t(X2) %*% X2 + lambda * diag(ncol(design))) %*% t(X2) %*% mpg
X2 %*% bhats

P=t(matrix(c(c(0.5,0.4,0.1),
             c(0.3,0.4,0.3),
             c(0.2,0.3,0.5)),
    nrow=3))

P2=t(matrix(c(c(0.9, 0.1, 0),
             c(0.6, 0.4, 0),
             c(0, 0.3, 0.7)),
           nrow=3))

pi_bru <- (P^100)[1,]
pi_bru - pi_bru%*%P < 1^-100

m = matrix(rep(1/2, 4), c(2,2))
matrix.power(m, 10)

matrix.power(P, 130)
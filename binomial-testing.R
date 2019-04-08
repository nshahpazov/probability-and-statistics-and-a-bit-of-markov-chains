
# the next two will equal each other
sum(sapply(0:4, function (i) dbinom(i, 10, 0.5)))
p = pbinom(4, 10, 0.5)

# the next one takes probability and gives back the oposite of pbinom
# this is saying: how much ns so that the probability is p
qbinom(p, 10, 0.5)

# the rest cases
pbinom(4, 10, 0.5, lower.tail = FALSE)

# generating random cases of binomial
# the number of successses with probability=0.5 of 50 trials
rbinom(1, 50, 0.5)

# it can also give a whole array of such results
rbinom(40, 50, 0.5)


pnorm(24,21,5)
qnorm(0.1,21,5)
1-pnorm(50, 45,3.2)

qnorm(0.2, 77, 5)

hist(cars$dist)

mu <- mean(cars$speed)
s <- sd(cars$speed)
n <- length(cars$speed)

sample <-rnorm(1000000, 15.4, 5.29)
hist(sample)

qqplot(sample, cars$speed, plot.it = T)
?qqplot

dbinom(70, 245, 0.25)

1-pnorm(70, 0.25*245, sqrt(245*0.25*0.75))
dbinom(600,1000,0.56)
dbinom(6,10,0.56)
1-pnorm(60,56,4.96)
sum(dbinom(60:100, 100, 0.56))

1- pnorm(1630, 1500,300)
pnorm(0.43,0,1)
pnorm(-0.333,0,1)
pnorm(2,0,1)

pnorm(76, 70, 3.3)
qnorm(0.4,70,3.3)
qnorm(0.82, 70, 3.3)

qnorm(0.95, 1500, 300)
qnorm(0.975, 70, 3.3)
1- pnorm(74, 70, 3.3)
pnorm(69, 70, 3.3)
pnorm(74, 70, 3.3)- pnorm(69, 70, 3.3)

pnorm(2000, 1500, 300)- pnorm(1500, 1500, 300)
pnorm(67, 70, 3.3)- pnorm(65, 70, 3.3)

pnorm(59, 80, 8)
pbinom(59, 400, 0.2)

qnorm(0.3,153,7.67) 

pbinom(92,100,0.9)
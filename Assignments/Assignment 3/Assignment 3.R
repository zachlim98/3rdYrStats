hist(runif(10))
hist(runif(100))
hist(runif(1000))

x_dunif <- seq(-1, 2, by = 0.01)     
y_dunif <- dunif(x_dunif) 
plot(y_dunif)


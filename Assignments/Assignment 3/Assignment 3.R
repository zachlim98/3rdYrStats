library(dplyr)
library(ggplot2)

hist(runif(10))
hist(runif(100))
hist(runif(100000))

x_var <- (seq(-1, 2, by = 0.01))
y_var <- dunif(x_var,min=0,max=1)
y_varr <- bind_cols(y_var,x_var)
plot <- ggplot(data=y_varr, aes(x=...2,y=...1)) + geom_point() + 
  labs(y = "Probability", x = "Seq of numbers") + 
  geom_segment(aes(x=0.37629,y=0,xend=0.37629,yend=1.0), color = "blue", size=1.5)

vote <- sample(c(1,0),100,replace=TRUE,prob=c(0.7,0.3))
hist(vote, main="Histogram from Sample function", xlab = "Voted")
total_vote <- sum(vote==1)

vote_bi <- rbinom(100, 1, 0.7)
hist(vote_bi, main="Histogram from rbinom function", xlab = "Voted")
total_vote_bi <- sum(vote_bi==1)
cat("People who voted: ", total_vote)
cat("People who voted (rbinom): ", total_vote_bi)

rbinom(1,100,0.7)
rbinom(100,1,0.7)

prob_pand <- dpois(4,1)
cat("Probability of 4 Pandemics in a decade: ", prob_pand)
plot(dpois(seq(0,20,1),1),xlim = c(0,20), ylim = c(0.0,0.5),xaxt="n")
axis(1,at=seq(1,20,1), las = 1)

for (i in c(3,7,10)){
  plot(dpois(seq(0,20,1),i),xlim = c(0,i+10), ylim = c(0.0,0.5))
}

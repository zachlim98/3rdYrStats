library(ggplot2)
library(dplyr)
set.seed(2210)

# run_no <- as.integer(readline(prompt = "How many simulation runs: "))
# people_no <- as.integer(readline(prompt = "How many people: "))
# 
# birth_match <- 0
# prob <- data.frame()
# 
# for (z in 2:people_no-1){
#   for (i in 1:run_no){
#     birthday <- sample(1:365, z, replace = TRUE)
#     birthday_wo <- unique(birthday)
#     if (length(birthday) == length(birthday_wo)){
#       birth_match = birth_match
#     } else if (length(birthday) > length(birthday_wo)){
#       birth_match = birth_match + 1
#     }
#     prob_match = birth_match/run_no
#   }
#   birth_match = 0
#   z = z + 1
#   prob <- rbind(prob,list(z, prob_match))
# }
# 
# plot <- ggplot(prob, aes(x=X2,y=X0)) + geom_point() + geom_line(linetype="solid",color="red",size=1) +
#   labs(x = "Number of People", y = "Probability") + # Axis labels
#   scale_fill_viridis_d()

### Monty Hall problem

#if stay
n <- 1000
correct_door <- replicate(n,sample(1:100,1))
my_pick <- replicate(n,sample(1:100,1))
prob_stay <- length(which(correct_door==my_pick))/n
prob_stay

#if switch

doors.prob <- data.frame()

max_doors <- 1000

for (t in 3:max_doors){
  switch_right <- 0
  for (i in 1:n){
    correct_door <- sample(1:t,1)
    my_pick <- sample(1:t,1)
    if (correct_door != my_pick){
      show_door <- setdiff(1:t,c(my_pick,correct_door))
      new_pick <- correct_door
    } else if (correct_door == my_pick){
      show_door = sample(setdiff(1:t,c(my_pick)),(t-2))
      new_pick = sample((setdiff(1:t,c(correct_door,show_door))),1)
    } 
    if (new_pick == correct_door){
      switch_right <- switch_right + 1
    }  else {
      switch_right <- switch_right
    }
    prob_switch <- switch_right/n
  }
  switch_right <- 0 
  t = t + 1
  doors.prob <- rbind(doors.prob, list(t,prob_switch))
}

plot <- ggplot(doors.prob, aes(x=X2,y=X0)) + geom_point() + geom_line(linetype="solid",color="red",size=1) +
  labs(x = "Number of People", y = "Probability") + # Axis labels
  scale_fill_viridis_d()
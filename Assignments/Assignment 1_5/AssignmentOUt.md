    #if stay
    n <- 10000
    correct_door <- replicate(n,sample(1:3,1))
    my_pick <- replicate(n,sample(1:3,1))
    prob_stay <- length(which(correct_door==my_pick))/n
    prob_stay

    ## [1] 0.3373

    #if switch
    switch_right <- 0
    for (i in 1:n){
      correct_door <- sample(1:3,1)
      my_pick <- sample(1:3,1)
      if (correct_door != my_pick){
        show_door <- setdiff(1:3,c(my_pick,correct_door))
        new_pick <- correct_door
      }
      else {
        show_door = sample((setdiff(1:3,my_pick)),1)
        new_pick = setdiff(1:3,c(my_pick,show_door))
      } 
      if (new_pick == correct_door){
        switch_right <- switch_right + 1
      }
      else {
        switch_right <- switch_right
      }
    }
    prob_switch <- switch_right/n
    prob_switch

    ## [1] 0.6709

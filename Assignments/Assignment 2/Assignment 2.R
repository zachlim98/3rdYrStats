library(dplyr)
set.seed(281020) #set seed for simulation

walk <- function(S=0, K=10){
  round_amt <- c() #create list to store sequence of bets
  while (abs(S) < abs(K)){
    choice <- sample(c(1,2),1,prob=c(0.49,0.51)) #choose win or loose
    if (choice == 1){ #if win
      S <- S + 1
      round_amt <- c(round_amt,S)
      } else { #if lose
        S <- S - 1
        round_amt <- c(round_amt,S)
      }
    }
  return(round_amt) #return list
}

sim_amt <- tibble() #create empty tibble
total_sim <- replicate(10,temp <- tibble(walk())) #create list of lists of outcomes

walks <- data_frame() #create empty data
for (i in 1:10){
  df <- data.frame(matrix(unlist(total_sim[i]), #unbind lists from lists 
                          nrow=length(total_sim[i]), byrow=T)) #add to dataframe by row 
  walks <- bind_rows(walks,df)
}

game_names <- data.frame("game" = 1:10) #df to store game number
walks <- cbind(game_names,walks) #add game number to df
walks <- walks %>% pivot_longer(!game,names_to="Game",values_to="Money")  %>%
   mutate(Round=as.integer(gsub("X","",Game))) #pivot from rows to columns

#create plot
plot <- walks %>% ggplot(aes(x=Round,y=Money,group=factor(game),color=factor(game))) +
  geom_line(size=1) +
  scale_y_continuous(breaks=seq(-10,10,2)) +
  scale_x_continuous(breaks=seq(0,220,10)) +
  labs(title="Outcome of 10 Games",x="No. of bets",y="Money",color="Game Number") +
  scale_color_viridis_d() +
  theme_bw() 

#create function to just capture win/loss
walk_win <- function(S=0, K=10){
  while (abs(S) < abs(K)){
    choice <- sample(c(1,2),1,prob=c(0.49,0.51))
    if (choice == 1){
      S <- S + 1
    } else {
      S <- S - 1
    }
  }
  return(S)
}

#simulate 10,000 games, record win/loss
r<-100000
wins <- 0
for (i in 1:r){
  outcome <- walk_win()
  if (outcome > 0){
    wins <- wins + 1
  }
}

prob_win <- wins/r #probability of winning
prob_win

#find prob of winning given lost first three bets
win_lost3 <- function(S=-3, K=10){
  while (abs(S) < abs(K)){
    choice <- sample(c(1,2),1,prob=c(0.49,0.51))
    if (choice == 1){
      S <- S + 1
    } else {
      S <- S - 1
    }
  }
  return(S)
}

#simulate 100,000 times
r<-100000
wins3 <- 0
for (i in 1:r){
  outcome <- win_lost3()
  if (outcome > 0){
    wins3 <- wins3 + 1
  }
}

prob_win3 <- wins3/r #probability of winning given lost first 3
prob_win3

###################################################################

Passcodes <- 10**5 #number of combinations 
Passcodes

PB_config <- choose(26,13)*factorial(13)/2**13 #number of plugboard configs
PB_config

Poss_set <- Passcodes*PB_config #total number of configs
Poss_set

prob_A <- 300/1000
prob_B <- 500/1000
prob_C <- 200/1000
prob_f_givA <- 0.09
prob_f_givB <- 0.04
prob_f_givC <- 0.11

prob_f <- (prob_A*prob_f_givA) + (prob_B*prob_f_givB) + (prob_C*prob_f_givC)

prob_A_givf <- (prob_A*prob_f_givA)/prob_f
prob_B_givf <- (prob_B*prob_f_givB)/prob_f
prob_C_givf <- (prob_C*prob_f_givC)/prob_f
prob_A_givf
prob_B_givf
prob_C_givf

######################################################
sample1 <- sample(letters, 13) #choose 13 random letters
sample2 <- sample(setdiff(letters,sample1),13) #shuffle the other 13 random
samples <- append(sample1,sample2)
config <- matrix(samples, nrow = 13) #create 2 columns of letters
#####################################################

#function to allow allies to generate new/input existing plugboard config
new_code <- function(){
  seed <- readline(prompt = "Choose the seed for the configuration: ")
  #set.seed to allow for different config to be generated
  set.seed(seed)
  #generate new key-pair value
  sample1 <- sample(letters, 13) 
  sample2 <- sample(setdiff(letters,sample1),13) 
  sets1 <- append(sample1,sample2)
  sets2 <- append(sample2,sample1)
  names(sets1) <- sets2
  return(sets1)
}

#function to check that message typed in is only alphabets
letters_only <- function(x) !grepl("[^A-Za-z ]", x)

#function to prompt user to enter new message
enter_message <- function(){
  message <- readline(prompt="What is your message: ")
  if (letters_only(message)==T){ #check if messages are alphabets
    store <- strsplit(tolower(message),split="") #split and convert to lower letters
  } else{
    print("ERROR: Please type in only letters") #error msg if disallowed entry
  }
  return(store)
}

#function to encode and decode
enigma <- function(){
  cyp <- new_code()
  new_msg <- enter_message() #prompt for new message
  decode <- c()
  for (i in new_msg){
    char <- cyp[i] #access PB config
    decode <- append(decode,char) #add to encoded/decoded character list
  }
  readable <- gsub("NA"," ",paste(decode, collapse = "")) #make it easier to read
  cat("Your message is: ",readable) #print out the encoded/decoded message
}


library(mapsapi)
library(tidyverse)
library(stringr)

# Google API key
key = "AIzaSyDmTh_LFt4SqVslqiwo3pFw8tMaAta6p0k"

# prepare regular expression
regexp <- "[[:digit:]]+"

data = read_csv("data.csv") #import data


times = list()

for (i in 1:nrow(data)){
  start_pt = as.character(data[i,3])
  
  doc = mp_directions(
    origin = start_pt,
    destination = "Orchard MRT, Singapore",
    alternatives = FALSE,
    mode = "transit",
    key = key, 
    quiet = TRUE
  )
  
  r = mp_get_routes(doc)
  
  time = r$duration_text
  
  times <- c(times, time)
  
}

time2 <- list()
active <- time1

for (i in 1:length(times)){
  
  if (str_detect(times[i], "hour")==TRUE) {
    
    t <- str_extract_all(list[i], regexp)
    
    converted <- (as.numeric(t[[1]][1])*60)+as.numeric(t[[1]][2])
    
    time2 <- c(time2,converted)
    
  }
  
  else {
    
  t <- as.numeric(str_extract(times[i], regexp))
  
  time2 <- c(time2, t)
    
  }

}

data <- data %>% mutate(loc2 = as.numeric(unlist(time2)))
data$loc2 <- time2
data$loc3 <- time3
data$loc4 <- time4

data <- data %>% group_by(Region) %>% arrange(desc(loc1), .by_group=TRUE)


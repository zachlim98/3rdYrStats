library(mapsapi)
library(tidyverse)
library(dplyr)
library(stringr)

# Google API key
key = "AIzaSyDmTh_LFt4SqVslqiwo3pFw8tMaAta6p0k"

# prepare regular expression
regexp <- "[[:digit:]]+"

data = read.csv("datas.csv") #import data
data$Bishan = 0
data$Tampines = 0
data$BuonaV = 0
data$HollandV = 0
data$Lakeside = 0

destination_list = list("Bishan MRT, Singapore","Tampines MRT, Singapore",
                        "Buona Vista MRT, Singapore", "Holland Village MRT, Singapore",
                        "Boon Lay MRT, Singapore")

for (z in 1:5) {
  
  for (i in 1:nrow(data)){
    start_pt = as.character(data[i,3])
    end_pt = as.character(destination_list[z])
    
    doc = mp_directions(
      origin = start_pt,
      destination = end_pt,
      alternatives = FALSE,
      mode = "transit",
      key = key, 
      quiet = TRUE
    )
    
    r = mp_get_routes(doc)
    
    time = r$duration_text
    
    if (str_detect(time, "hour")==TRUE) {
      
      t <- str_extract_all(time, regexp)
      
      converted <- (as.numeric(t[[1]][1])*60)+as.numeric(t[[1]][2])
      
    }
    
    else {
      
      converted <- as.numeric(str_extract(time, regexp))
    
    }
    
    data[i,z+3] <- converted
    
  }
  
}

minimum=apply(data, 1, function(x) names((sort(x))[1]))
minimum2=apply(data, 1, function(x) names((sort(x))[2]))
minimum3=apply(data, 1, function(x) names((sort(x))[3]))
minimum4=apply(data, 1, function(x) names((sort(x))[4]))
minimum5=apply(data, 1, function(x) names((sort(x))[5]))

data$first_choice <- minimum
data$second_choice <- minimum2
data$third_choice <- minimum3
data$fourth_choice <- minimum4
data$fifth_choice <- minimum5

datat <- data %>% 
  select(Name, Region, c(first_choice:fifth_choice)) %>%
  group_by(Region) %>%
  arrange(first_choice,second_choice,third_choice,fourth_choice,fifth_choice, .by_group=TRUE)

write.csv(data, file="choices.csv")

datat$allocation = 0 

Group1 <- datat %>% 
  select(Name, Region, c(first_choice:fifth_choice)) %>%
  filter(first_choice=="Bishan"|second_choice=="Bishan") %>%
  group_by(Region) %>% 
  slice_head(n=3) %>%
  ungroup() %>%
  slice_head(n=8)

datat %>% mutate(Allocation = )


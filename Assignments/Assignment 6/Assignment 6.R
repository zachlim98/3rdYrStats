library(dplyr)
library(tidyr)
library(ggplot2)

#import files to read
mortality <- read.csv("mortality.csv")
poverty <- read.csv("poverty.csv")

#quick explorations

#see the head
head(mortality)
head(poverty)

#get a summary
summary(mortality)
summary(poverty)

#pivot to make tidy
mortality_tidy <- mortality %>% 
  pivot_longer(!country, names_to="year", values_to = "mrate") %>%
  mutate(year = as.numeric(gsub("X","",year)))
poverty_tidy <- poverty %>% 
  pivot_longer(!country, names_to="year", values_to = "prate") %>%
  mutate(year = as.numeric(gsub("X","",year)))

#join the dataframes to form data 
data <- inner_join(mortality_tidy,poverty_tidy,by=c("country"="country","year"="year")) %>% na.omit(data)

#calculate the correlation
corr <- cor(data$mrate,data$prate)
corr 

#calculate the regression model
reg <- lm(data$mrate ~ data$prate)       
#show the coefficients 
reg$coefficients

#plot the model
data %>% ggplot(aes(prate,mrate)) +
  geom_point(aes(color=factor(country)),show.legend = F) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(title = "Linear Regression of Mortality Rate on Poverty Rate",
       x = "Poverty Rate (%)",
       y = "Infant Mortality Rate (per 1000)")

#calculate the opposite regression model
reg2 <- lm(data$prate ~ data$mrate)       
#show the coefficients 
reg2$coefficients

#get summary of regression
summary(reg)
#get summary of 2nd regression
summary(reg2)

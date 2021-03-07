library(tidyverse)
library(dplyr)
library(plotly)

# read in the data
mortality <- read_csv("mortality.csv")
poverty <- read_csv("poverty.csv")

# explore the data
head(mortality)
head(poverty)

# create tidy datasets
mortality_tidy <- mortality %>% 
  pivot_longer(cols = !country, names_to = "year", values_to = "mrate")

poverty_tidy <- poverty %>%
  pivot_longer(cols = !country, names_to = "year", values_to = "prate")

# joining datasets together
measurements <- inner_join(mortality_tidy, poverty_tidy, by = c("country", "year")) %>%
  na.omit() %>% filter(!country %in% c("Europe & Central Asia", "East Asia & Pacific", 
                                       "Middle East & North Africa", "Sub-Saharan Africa", "Latin America & Caribbean", 
                                       "Low income", "Low & middle income",
                                       "Lower middle income", "Lower middle income", "Middle income", "Upper middle income", 
                                       "Fragile and conflict affected situations", "IDA total", "IDA only", "IDA blend", 
                                       "IDA & IBRD total", "IBRD only"))

# Finding expected values
IMR_exp <- mean(measurements$mrate)
PR_exp <- mean(measurements$prate)

# Finding correlation between Y and X 

IMRPR_corr <- cor(measurements$prate,measurements$mrate)

# creating model 

model <- lm(mrate ~ prate, measurements)

summary(model)

# plotting model

plot <- measurements %>% ggplot(aes(x=prate, y=mrate)) +
  geom_point() +
  geom_smooth(method="lm") +
  

plot

# plotting using plotly 

fv <- model %>% fitted.values

p <- plot_ly() %>%
  add_trace(y = measurements$mrate, x=measurements$prate, type="scatter", 
            hovertemplate= paste('Prate: %{y: .2f}', 
                                 '<br>Mrate: %{x: .2f}</br>'), name="") %>%
  add_trace(x= measurements$prate, y=fv, mode  = "lines", 
            hovertemplate= "Regression of Poverty on Mortality",
            name="") %>%
  layout(showlegend=F, title="Relationship between Poverty and Mortality Rate")

p

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
            mode="marker", marker=list(color="rgba(1, 41, 90, 0.4)"), 
            hovertemplate= paste('Prate: %{y: .2f}', 
                                 '<br>Mrate: %{x: .2f}</br>'), name="") %>%
  add_trace(x= measurements$prate, y=fv, mode  = "lines", 
            hovertemplate= "Regression of Poverty on Mortality",
            name="") %>%
  layout(showlegend=F, title="Relationship between Poverty and Mortality Rate",
         yaxis=list(title="Infant mortality rate per 1,000 live births"),
         xaxis=list(title="Percentage of population with less than $3.10 per day"))

p

smokers <- read_csv("smoking.csv")

smokers <- smokers %>% mutate(group = cut(age, breaks=c(-Inf, 50.24962, 57.61331, 63.89274, Inf),
                                           labels = c("0 - 50", "51 - 57", "58 - 63", "64 and Over")))

# Group data
df <- smokers %>%
  group_by(group, smoker) %>%
  summarise(mean = mean(years))

df$smoker <- replace(df$smoker, df$smoker==1, "Yes")

# Plot
g <- ggplot(df, aes(smoker, mean, fill = group)) +
  geom_col() +
  facet_wrap(. ~ group) +
  xlab("Smoking status") +
  ylab("Average years to live") +
  scale_fill_viridis_d(option = "cividis") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank())
g

gplot <- ggplotly(g)

model2 <- lm(years~smoker, data=smokers)

model2_adj <- lm(years~smoker+age, data=smokers)

fig <- df %>% plot_ly(x=~group, y=~mean, type="bar", text=df$smoker,
                      transforms = list(list(type="groupby", 
                                             groups=df$smoker,
                                             styles=list(
                                               list(target="No", value=list(marker=list(color='blue'))),
                                               list(target="Yes", value=list(marker=list(color='darkred')))
                                             ))), hoverinfo='skip',
                      hovertemplate = paste("Smoker: %{text}",
                                            "<br>Years left:%{y: .2f}</br><extra></extra>")) %>%
  layout(barmode="group", 
         title="Comparison of Years Left to Live", 
         xaxis=list(title="Age Group Quartiles"), 
         yaxis=list(title="Years left to live"),
         showlegend=TRUE,
         legend=list(title=list(text="<b>Smoker?</b>")))

fig

hist(runif(10, 0, 10))
hist(runif(100, 0, 10))
hist(runif(1000000, 0, 10))

y_var <- seq(-1,2,0.01)
density <- dunif(seq(-1,2,0.01), 0, 0.5)
density_plot <- bind_cols(y_var, density)

plot(density_plot, xlab("Numbers"), ylab("Probability Density"))

vote_sample <- sample(c(1,0), 100, prob =c(0.7,0.3), replace=TRUE)
sum(vote_sample)/length(vote_sample)

rbinom(100, 1, 0.7)
rbinom(1, 100, 0.7)

Pandemics <- seq(0, 20, 1)
Density <- dpois(Pandemics, 1)
plot(Pandemics, Density, type = "l", main = "Lambda = 1")

plot(dpois(seq(0,20,1),1),xlim = c(0,20), ylim = c(0.0,0.5), ylab = "Density", xlab="Number of Pandemics", main="Lambda = 1")

kenya <- read_csv("kenya.csv")
sweden <- read_csv("sweden.csv")
world <- read_csv("world.csv")

###############################################
## B3: Calculate age-specific fertility rate ##
###############################################

asfr <- function(data) {
  data %>%
    mutate(
      asfr = births / py.women) %>%
    select(period, age, asfr) %>%
    data.frame() # Convert tibble to data frame
}

asfr(kenya)
asfr(sweden)
asfr(world)

########################################
## B4: Calculate total fertility rate ##
########################################

tfr <- function(data) {
  out <- asfr(data) 
  out %>%
    group_by(period) %>%
    summarise(
      tfr = 5 * sum(asfr))
}

tfr(kenya)
tfr(sweden)
tfr(world)

###########################################
## B5: Calculate age-specific death rate ##
###########################################

asdr <- function(data) {
  data %>%
    mutate(
      asdr = 1000 * deaths / (py.men + py.women)) %>%
    select(period, age, asdr) %>%
    data.frame() # Convert tibble to data frame
}

asdr(kenya)
asdr(sweden)
asdr(world)

###########################
## B6: Visualise results ##
###########################

# Collect ASFR and ASDR for each unit
ken <- left_join(asfr(kenya), asdr(kenya))
swe <- left_join(asfr(sweden), asdr(sweden))
wor <- left_join(asfr(world), asdr(world))

# Create one data frame with all results
df <- rbind(ken, swe, wor)
df$country <- c(rep("Kenya", 30), rep("Sweden", 30), rep("World", 30))

# Transform age groups to ordered factor
df$age <- factor(df$age, 
                 levels = c("0-4", "5-9", "10-14", 
                            "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                            "50-54", "55-59", "60-69", "70-79", "80+"))

# Age groups for reproductive age range
age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

# Visualise ASFR
g1 <- ggplot(subset(df, age %in% age_groups), # Limit age range
             aes(age, 1000 * asfr, fill = country)) + # Modify rate and colour code by country
  geom_col() + # Show as columns
  labs(x = "Age", y = "Age-specific fertility rate per 1000 women") + # Axis labels
  scale_fill_viridis_d() + # Choose a nice colour palette
  facet_grid(country ~ period) + # Stratify plot by country and period
  theme_bw() + # Remove redundant lines
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90)) 

g1

# Visualise ASDR
g2 <- ggplot(df, aes(age, asdr, fill = country)) + # Colour code by country
  geom_col() + # Show as columns
  scale_y_continuous(breaks = seq(0, 200, 50)) + # Y-axis scale
  labs(x = "Age", y = "Age-specific death rate per 1000 population") + # Axis labels
  scale_fill_viridis_d(option = "plasma") + # Choose a nice colour palette
  facet_grid(country ~ period) + # Stratify plot by country and period
  theme_bw() + # Remove redundant lines
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90))

g2

fig1 <- df %>% filter(period=="1950-1955" & country=="Kenya") %>%
  plot_ly(x=~age, y=~asdr, type="bar", name="Kenya", 
          hovertemplate= paste('ASDR: %{y: .2f}'))
fig2 <- df %>% filter(period=="1950-1955" & country=="Sweden") %>%
  plot_ly(x=~age, y=~asdr, type="bar", name="Sweden",
          hovertemplate= paste('ASDR: %{y: .2f}')) 
fig3 <- df %>% filter(period=="1950-1955" & country=="World") %>%
  plot_ly(x=~age, y=~asdr, type="bar", name="World",
          hovertemplate= paste('ASDR: %{y: .2f}'))

fig4 <- df %>% filter(period=="2005-2010" & country=="Kenya") %>%
  plot_ly(x=~age, y=~asdr, type="bar", name="Kenya", 
          hovertemplate= paste('ASDR: %{y: .2f}'))
fig5 <- df %>% filter(period=="2005-2010" & country=="Sweden") %>%
  plot_ly(x=~age, y=~asdr, type="bar", name="Sweden",
          hovertemplate= paste('ASDR: %{y: .2f}')) 
fig6 <- df %>% filter(period=="2005-2010" & country=="World") %>%
  plot_ly(x=~age, y=~asdr, type="bar", name="World",
          hovertemplate= paste('ASDR: %{y: .2f}'))

fig <- subplot(fig1, fig2, fig3, fig4, fig5, fig6, nrows = 2 ,shareX = TRUE, titleY=FALSE) %>% 
  layout(title="ASDR comparison",
         xaxis=list(title="Age"),
         yaxis=list(title="ASDR"))

fig

ggfig <- ggplotly(g1, tooltip = c("age","1000 * asfr")) 

ggfig

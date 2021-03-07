# import tidyverse lib for data manipulation
library(tidyverse) 

# import datasets
mortality <- read_csv("county_mortality.csv") %>% 
  rename(FIPS = fips) # rename from "fips" to "FIPS" for joining
covariates <- read_csv("county_covariates.csv")

# join datasets, removing duplicate columns of statename and countyname
county_data <- inner_join(mortality, covariates, by = "FIPS") %>%
  select(!c("statename", "countyname"))

# to find what the ranges of each tertile is 
unique(cut(county_data$all_cause, 
           breaks = quantile(county_data$all_cause, 
                             probs = seq(0,1, by = 0.33)))) # split into tertiles

# create new dataframe to prep data for plotting
choromap_data <- county_data %>% 
  select(c("FIPS", "all_cause")) %>% # select only the essential columns 
  rename(fips = FIPS, value = all_cause) %>% # rename for plotting
  # split the values into tertiles using the ranges found above
  mutate(tertile = as.factor(
    ifelse(value <= 800, "1", 
           ifelse(value <= 950, "2", "3"))))

# import more libraries for visualisation
library(ggpubr)
library(ggrepel)
library(usmap)

# create first graphic
g1 <- plot_usmap(regions = c("county"), 
                 data = choromap_data, 
                 values = "tertile") + 
  # set the labels and the colors to make sure it's nice! 
  scale_fill_manual(values = c('NA' = "white", "4" = "darkblue", "3" = "blue",
                               "2" = "deepskyblue", "1" = "lightblue2"), 
                    labels = c("323 - 800", 
                               "801 - 950", "951 - 1633", "Missing Data"),
                    name = "All-Cause Mortality Rate\n(per 100,000)") + 
  # adjust the legend so it doesn't block Hawaii 
  theme(legend.position = "right", 
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# create simple function to find top 3 mortality rates and bottom 3 mortality rates
is_topbottom <- function(x) {
  return(x < 400 | x > 1500)
}

# create second graphic
g2 <- county_data %>% 
  # create new topbottom value and label columns for plotting
  mutate(topbot = ifelse(is_topbottom(all_cause), paste(county,state,sep=",\n"), as.numeric(NA)),
         topbot_values = ifelse(is_topbottom(all_cause), all_cause, as.numeric(NA))) %>%
  # begin plot
  ggplot(aes(x=all_cause)) +
  # create histogram to show distribution
  geom_histogram(fill = "dodgerblue1",
                 color = "black",
                 binwidth = 50,
                 boundary = 0) +
  # use jitter to plot the top and bottom counties 
  geom_jitter(aes(x=topbot_values, y = 1), 
              na.rm = TRUE,
              color = "indianred",
              size = 3,
              alpha = 1.0,
              position = position_jitter(width = 5, height = 0, seed = 3)) +
  # use label_repel to plot their labels separate from each other
  geom_label_repel(aes(x = topbot_values, y = 1, label = topbot),
                   box.padding = 0.3,
                   color = "black",
                   position = position_jitter(width = 5, height = 0, seed = 3)) +
  # plot mean and median values as lines
  geom_segment(aes(y = 0, yend = 420, x = mean(all_cause), xend = mean(all_cause)),
               size = 1, color = "brown2") +
  geom_segment(aes(y = 0, yend = 420, x = median(all_cause), xend = median(all_cause)),
               size = 1, color = "darkorange3") +
  # label the lines, using offsets
  annotate(geom = "label", x = mean(county_data$all_cause)+60, y = 200, 
           label = paste("Mean \n",
                         round(mean(county_data$all_cause))),
           color = "brown2") +
  annotate(geom = "label", x = median(county_data$all_cause)-70, y = 200, 
           label = paste("Median \n",
                         round(median(county_data$all_cause))),
           color = "darkorange3") +
  scale_x_continuous(breaks = seq(350, 1650, 100)) +
  scale_y_continuous(breaks = seq(0, 400, 50), expand = c(0,0)) +
  labs(title = "", 
       x = "All-Cause Mortality Rate (per 100,000)",
       y = "Number of Counties") +
  theme_bw(base_size = 14)

# use ggarrange to combine both charts into a single visualisation and name them
g_combined <- ggpubr::ggarrange(g1, g2, labels = c("A. Mortality Rate Across Counties",
                                                   "B. Distribution of Mortality Rate"),
                                hjust = c(-0.5, -0.8))

g_combined

# create the model
model <- lm(all_cause ~ income_median + edu_hs + race_black 
            + race_other + pop_density, 
            data = county_data)

# view output of model
summary(model)

# create residual model by regression income_median against other variables
income_model <- lm(income_median ~ edu_hs + race_black + race_other + pop_density, 
                     data = county_data)

# add residuals to dataset
isol_data <- county_data %>% 
  mutate(residuals = income_model$residuals)

# regress residuals against mortality
residual_model <- lm(all_cause ~ residuals, data = isol_data)

summary(residual_model)

# import library in order to get data of adjusted income
library(visreg)

# use visreg to generate data of adjusted income, setting 
# conditions (cond) to use the mean of the other variables 
adjusted <- visreg(model, "income_median",
                   collapse = FALSE, 
                   plot = FALSE,
                   cond = list(edu_hs = mean(isol_data$edu_hs),
                               race_black = mean(isol_data$race_black),
                               race_other = mean(isol_data$race_other),
                               pop_density = mean(isol_data$pop_density)))

# extract the adjusted income_median from visreg output
adjusted_data <- adjusted$res$income_median

# create dataframe with adjusted data 
isol_data <- county_data %>%
  mutate(adjusted = adjusted_data)

# create plot to show variation of mortality with income, after controlling 
# for other covariates
g3 <- isol_data %>%
  ggplot(aes(y = all_cause, x = adjusted)) + 
  geom_point(color = "grey", size = 2.5, alpha = 0.4) +
  geom_smooth(method = "lm", color = "indianred", 
              se = FALSE) +
  scale_y_continuous(breaks = seq(400, 1600, 100)) +
  scale_x_continuous(breaks = seq(25000, 125000, 10000)) +
  theme_minimal(base_size = 13) +
  labs(x = "Median Income, Adjusted for Other Variables",
       y = "All-Cause Mortality Rate (per 100,000)",
       title = "Mortality Rate Against Median Income (Adjusted)")

g3

# create second model to more easily understand relationship
model_easy <- lm(all_cause ~ log(income_median) + edu_hs + race_black 
                 + race_other + pop_density, 
                 data = county_data) 

# get the estimate for log(income_median)
coef(model_easy)["log(income_median)"]

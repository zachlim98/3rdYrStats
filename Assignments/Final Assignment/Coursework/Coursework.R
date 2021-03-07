library(tidyverse)
library(usmap)
library(ggrepel)
library(ggpubr)

mortality <- read_csv("county_mortality.csv") %>% 
  rename(FIPS = fips)
covariates <- read_csv("county_covariates.csv")

county_data <- inner_join(mortality, covariates, by = "FIPS") %>%
  select(!c("statename", "countyname"))

county_mortality_data <- county_data %>% 
  select(c("FIPS", "all_cause", "county", "state")) %>%
  rename(region = FIPS, value = all_cause)

unique(cut(choromap_data$value, breaks = quantile(choromap_data$value, probs = seq(0,1, by = 0.33))))

choromap_data <- county_data %>% 
  select(c("FIPS", "all_cause")) %>%
  rename(fips = FIPS, value = all_cause) %>%
  mutate(tertile = as.factor(ifelse(value <= 800, "1", ifelse(value <= 950, "2", "3"))))

g1 <- plot_usmap(regions = c("county"), 
                 data = choromap_data, 
                 values = "tertile") + 
  scale_fill_manual(values = c('NA' = "white", "4" = "darkblue", "3" = "blue",
                               "2" = "deepskyblue", "1" = "lightblue2"), 
                    labels = c("323 - 800", 
                               "801 - 950", "951 - 1633", "Missing Data"),
                    name = "All-Cause Mortality Rate\nper 100,000") + 
  theme(legend.position = "right", 
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 15))

g1

is_topbottom <- function(x) {
  return(x < 400 | x > 1500)
}


g2 <- county_mortality_data %>% 
  mutate(outlier = ifelse(is_outlier(all_cause), paste(county,state,sep=", "), as.numeric(NA)),
         outlier_values = ifelse(is_outlier(all_cause), all_cause, as.numeric(NA))) %>%
  ggplot(aes("column", all_cause)) +
  geom_boxplot(outlier.colour = "grey",
               outlier.alpha = 0.5,
               width = 0.3) + 
  geom_jitter(aes(y=outlier_values), 
             na.rm = TRUE,
             color = "indianred3",
             alpha = 0.7,
             position = position_jitter(width = 0, height = 5, seed = 3)) +
  geom_text_repel(aes(y = outlier_values, label = outlier),
                  box.padding = 0.7,
                  position = position_jitter(width = 0, height = 5, seed = 3)) +
  scale_y_continuous(breaks = seq(400,1600,100)) +
  labs(title = "Distribution of All Cause Mortality Across US Counties",
       y = "All Cause Mortality (per 100,000)",
       x = "") +
  theme_minimal() + 
  theme(axis.ticks.x=element_blank(),
        axis.text.x=element_blank())

 
g2

g2b <- county_mortality_data %>% 
  mutate(outlier = ifelse(is_outlier(all_cause), paste(county,state,sep=",\n"), as.numeric(NA)),
         outlier_values = ifelse(is_outlier(all_cause), all_cause, as.numeric(NA))) %>%
  ggplot(aes(x=all_cause)) +
  geom_histogram(fill = "dodgerblue1") +
  geom_jitter(aes(x=outlier_values, y = 1), 
              na.rm = TRUE,
              color = "indianred",
              size = 3,
              alpha = 1.0,
              position = position_jitter(width = 5, height = 0, seed = 3)) +
  geom_label_repel(aes(x = outlier_values, y = 1, label = outlier),
                  box.padding = 0.3,
                  color = "black",
                  position = position_jitter(width = 5, height = 0, seed = 3)) +
  geom_segment(aes(y = 0, yend = 380, x = mean(all_cause), xend = mean(all_cause)),
               size = 1, color = "brown2") +
  geom_segment(aes(y = 0, yend = 380, x = median(all_cause), xend = median(all_cause)),
               size = 1, color = "darkorange3") +
  annotate(geom = "label", x = mean(county_mortality_data$all_cause)+60, y = 200, 
           label = paste("Mean \n",
                         round(mean(county_mortality_data$all_cause))),
           color = "brown2") +
  annotate(geom = "label", x = median(county_mortality_data$all_cause)-70, y = 200, 
                label = paste("Median \n",
                              round(median(county_mortality_data$all_cause))),
           color = "darkorange3") +
  scale_x_continuous(breaks = seq(350, 1650, 100)) +
  scale_y_continuous(breaks = seq(0, 400, 50), expand = c(0,0)) +
  labs(title = "", 
       x = "All-Cause Mortality Rate (per 100,000)",
       y = "Count") +
  theme_bw()


g2b

g3 <- county_data %>%
  group_by(state) %>%
  ggplot() +
  geom_boxplot(aes(y = all_cause, x = reorder(state, all_cause, FUN = median))) +
  coord_flip() +
  labs(title = "All Cause Mortality Across Counties (Grouped by State)",
       y = "All Cause Mortality (per 100,000)",
       x = "") +
  theme_minimal()

g3

county_mortality_data <- county_data %>%
  mutate(race_total = race_black + race_other,
         race_high = (ifelse(race_total >= mean(race_total), "High", "Low")),
         density_quartile = ifelse(pop_density >= mean(pop_density), "Urban", "Rural"),
         hs_quartile = ifelse(edu_hs >= mean(edu_hs), "Higher Ed Prop", "Lower Ed Prop"),
         income_quartile = ntile(income_median, 5))

g4 <- county_mortality_data %>%
  ggplot(aes(x=income_median, y=all_cause, color = income_quartile)) +
  facet_grid() +
  geom_point() +
  geom_smooth(method = "lm")

g4


adjusted_data <- adjusted_data %>%
  mutate(decile = county_mortality_data$income_quartile)


g5 <- adjusted_data %>% 
  ggplot(aes(x=as.factor(decile), y=y, group=decile)) +
  geom_boxplot(outlier.colour = "black",
               outlier.alpha = 0.7,
               color = "blue",
               fill = "blue",
               alpha = 0.1,
               lwd = 0.6) +
  scale_y_continuous(breaks = seq(200,1550, 50)) +
  scale_x_discrete(labels = c("Bottom Quintile", "21st - 40th \n Quintile", 
                              "41st - 60th \n Quintile", "61st - 80th \n Quintile", 
                              "Top Quintile"),
                   limits = rev(levels(as.factor(adjusted_data$decile)))) +
  labs(title = "Change in All Cause Mortality by Median Income (Adjusted)",
       x = "Income Bracket (Adjusted)", y = "All Cause Mortality (per 100,000)") +
  coord_flip() +
  theme_bw()

g5

decile <- county_mortality_data %>% 
  mutate(income_quartile = cut(income_median, 
                               breaks = quantile(income_median, 
                                                 probs = seq(0,1, by = 0.25)),
                               labels = 1:4))

change_mortaliy <- mean(abs(diff(decile$median)))
change_income <- mean(abs(diff(decile$income_mean)))

model_easy <- lm(all_cause ~ log(income_median) + edu_hs + race_black 
            + race_other + pop_density, 
            data = decile)

summary(model_easy)




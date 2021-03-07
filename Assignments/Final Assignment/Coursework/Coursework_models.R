library(caret)
library(MASS)

county_data <- county_data %>% mutate(urban = ifelse(pop_density>1000, 1, 0))

##############
# Using MASS #
##############

model <- lm(all_cause ~ income_median + edu_hs + race_black 
            + race_other + pop_density, 
            data = county_data)

summary(model)

step.model_BIC <- stepAIC(model, direction = "both", k = log(nrow(county_data)))

summary(step.model_BIC)

###############
# Using Caret #
###############

train.control <- trainControl(method = "cv", number = 10)

step.model_RMSE <- train(all_cause ~ income_median + edu_hs + race_black 
                    + race_other + pop_density, 
                    data = county_data,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)

step.model_RMSE$results
step.model_RMSE$bestTune

coef(step.model_RMSE$finalModel, 5)

residual.model <- lm(income_median ~ edu_hs + race_black + race_other + pop_density, 
             data = county_data)

isol_data <- county_data %>% 
  mutate(residuals = residual.model$residuals)

model2 <- lm(all_cause ~ residuals, data = isol_data)

summary(model1)
summary(model2)

plot(model2)

plot1 <- isol_data %>%
  ggplot(aes(x=residuals, y=all_cause)) +
  geom_point() + 
  geom_smooth(method = "lm")

plot1

library(car)

car::avPlots(model, 
             terms = ~income_median)

library(visreg)

adjusted <- visreg(model, "income_median",
                   collapse = FALSE, 
                   plot = FALSE,
       cond = list(edu_hs = mean(isol_data$edu_hs),
                   race_black = mean(isol_data$race_black),
                   race_other = mean(isol_data$race_other),
                   pop_density = mean(isol_data$pop_density)))

alt_model <- lm(all_cause ~ income_quartile + edu_hs + race_black 
                + race_other + pop_density, 
                data = county_mortality_data)

adjusted_data <- adjusted$res$income_median

isol_data <- isol_data %>%
  mutate(adjusted = adjusted_data)

isol_data %>%
  ggplot(aes(y = adjusted_data, x = all_cause)) + 
  geom_point(color = "grey", alpha = 0.7) +
  geom_smooth(method = "lm", color = "indianred") +
  labs(x = "Median Income, Adjusted for Other Variables",
       y = "All Cause Mortality Rate (per 100,00)")

summary(alt_model)

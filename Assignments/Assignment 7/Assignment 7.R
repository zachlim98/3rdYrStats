library(tidyverse)
library(ggplot2)

# load the csv
smoking_df <- read_csv("smoking.csv")

# explore the dataset
glimpse(smoking_df)
head(smoking_df)
summary(smoking_df)

# splitting into equal numbers
smoking_df <- smoking_df %>% mutate(age_round = (round(age,0))) %>%
  mutate(quartile = cut_number(age_round,4))

smoking_df$quartile <- factor(smoking_df$quartile, labels = c("21 - 50", "51 - 58", "59 - 64", "65 - 89"))

plot <- smoking_df %>% ggplot(aes(x=age, y=years,color=as.factor(smoker))) +
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", se=F) +
  labs(title = "Association between smoking and years to live (eq numbers)") + xlab("Age") + ylab("Years to live") +
  facet_wrap(~quartile,scales = "free") +
  scale_color_brewer(name  ="Smoker?",
                    labels=c("No", "Yes"),
                     palette = "Dark2") +
  theme_bw()

#plot the graph
plot

# splitting into equal age ranges
smoking_df_2 <- smoking_df %>% mutate(age_round = (round(age,0))) %>%
  mutate(quartile = cut_interval(age_round,4))

smoking_df_2$quartile <- factor(smoking_df_2$quartile, labels = c("21 - 38", "39 - 55", "56 - 72", "73 - 89"))

plot2 <- smoking_df_2 %>% ggplot(aes(x=age, y=years,color=as.factor(smoker))) +
  geom_point(alpha=0.3) +
  geom_smooth(method="lm", se=F) +
  labs(title = "Association between smoking and years to live (eq range)") + xlab("Age") + ylab("Years to live") +
  facet_wrap(~quartile,scales = "free") +
  scale_color_brewer(name  ="Smoker?",
                     labels=c("No", "Yes"),
                     palette = "Dark2") +
  theme_bw()

#plot the graph
plot2

#create the lm model
lm(smoking_df$years ~ smoking_df$smoker)

#create lm model, controlling for age
lm(smoking_df$years ~ smoking_df$smoker + smoking_df$age)

#saving model for summary
model <-lm(smoking_df$years ~ smoking_df$smoker + smoking_df$age)

#view summary of model
summary(model)

#construct 95% confidence interval 
std.err <- sqrt(diag(vcov(model)))
coeff <- model$coefficients

#print out confidence intervals 
for (i in 1:3){
  cat(sprintf("95%% Confidence Interval for %s: %f \u00b1 %f \n", names(coeff[i]), coeff[i], (2*std.err[i])))
}

#99% confidence intervals
for (i in 1:3){
  cat(sprintf("99%% Confidence Interval for %s: %f \u00b1 %f \n", names(coeff[i]), coeff[i], (3*std.err[i])))
}

#creating sampling list
sampling = list(20,50,100,500)

#print confint for all sample sizes
for (i in sampling){
  new_sample <- sample_n(smoking_df,i)
  model <- lm(new_sample$smoker ~ new_sample$years + new_sample$age)
  print(confint(model))
}





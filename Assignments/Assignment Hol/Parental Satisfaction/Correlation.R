library(dplyr)
library(ggplot2)
library(fuzzyjoin)
library(corrplot)

marital <- read.csv("RelationshipQuality.csv") %>% 
  rename(Name = Name.of.Parent)
  
parenting <- read.csv("SDQ.csv") %>% rename(SDQ_Score = Total)

combined  <- stringdist_join(marital, parenting,
                by = "Name",
                mode = "left",
                method = "jw",
                ignore_case = TRUE,
                distance_col = "dist",
                max_dist = 0.5) %>%
  group_by(Name.x) %>%
  slice_min(order_by = dist, n = 1) %>%
  arrange(desc(dist))

# %>%
#   filter(dist < 0.27)

write.csv(combined, file="combined.csv")
survey <- read.csv("combined.csv") %>%
  rename(SDQ_Score = Total) %>%
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.factor, as.numeric) %>%
  select(!Name)

pairs(Total~., data=survey, upper.panel = NULL)

cor.table = cor(survey)
corrplot(cor.table, type="lower", method="number")
corrplot(cor(survey[,1:11])[1:10,11, drop=FALSE], method="number", cl.pos='n')

fit = lm(Total ~ ., data=survey)
  
plot = survey %>% ggplot(aes(x= overall_hap, y= SDQ_Score)) +
  geom_smooth(method="lm") +
  labs(x="Overall Happiness", y="SDQ Score") +
  theme_bw()

plot


library(dplyr)

#import files
df <- read.csv("Stats.csv")

#mutate to create mean of Compulsory Papers and Option Papers
df_edit <- df %>% mutate("MeanComp" = rowMeans(.[,1:5])) %>% 
  mutate("MeanOpt"= rowMeans(.[,6:8]))

#Check for normality
shapiro.test(df_edit$Option_QM)
shapiro.test(df_edit$MeanComp)
shapiro.test(df_edit$MeanOpt)

#run student t-test
Comp_QM <- t.test(df_edit$Option_QM, df_edit$MeanComp, alternative="greater", paired = T) 

Option_QM <- t.test(df_edit$Option_QM, df_edit$MeanOpt, alternative="greater", paired = F) 

Comp_QM

Option_QM

#remove outlier years of 2020, 2019
df_wo <- df_edit[3:6,]

#re-run student t-test
Comp_QM_wo <- t.test(df_wo$Option_QM, df_wo$MeanComp, alternative="greater", paired = T) 

Option_QM_wo <- t.test(df_wo$Option_QM, df_wo$MeanOpt, alternative="greater", paired = F) 

Comp_QM_wo

Option_QM_wo

## Introduction

We wanted to see if the mean of the QM options was statistically significantly higher than the mean of the Compulsory Papers and the other Option Papers. We hence captured data from the last 5 years (available in the Examiners' Reports).

For the Options Papers, we referred only to papers that had more than 4 students (which allowed for mean of the grades to be noted) and papers that had data going back 5 years. This resulted in the following Option Papers being used: Quantitative Methods, Health and Disease, Evolution and Medicine, and Physical and Forensic Anthropology. While we note that this does not represent the range of options available, there was insufficient data for the other Options for statistical tests to be performed. 

The code for the test is as follows: 

```R
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
```
```
##  Shapiro-Wilk normality test
## 
## data:  df_edit$Option_QM
## W = 0.89103, p-value = 0.3236

##  Shapiro-Wilk normality test
## 
## data:  df_edit$MeanComp
## W = 0.95174, p-value = 0.7543

##  Shapiro-Wilk normality test
## 
## data:  df_edit$MeanOpt
## W = 0.95492, p-value = 0.7798
```
```R
#run student t-test
Comp_QM <- t.test(df_edit$Option_QM, df_edit$MeanComp, alternative="greater", paired = T) 

Option_QM <- t.test(df_edit$Option_QM, df_edit$MeanOpt, alternative="greater", paired = F) 

Comp_QM
Option_QM
```
```
##  Paired t-test
## 
## data:  df_edit$Option_QM and df_edit$MeanComp
## t = 2.8272, df = 5, p-value = 0.0184

##  Welch Two Sample t-test
## 
## data:  df_edit$Option_QM and df_edit$MeanOpt
## t = 2.4433, df = 5.1071, p-value = 0.02869
```

Given that the data was normally distributed (as per the Shapiro-Wilk normality test), we used a student t-test to test for the statistical difference in means. For the Compulsory papers and QM, we used a paired t-test (given that students would have to take both the Compulsory Papers and the QM)

H0: μ1−μ2=0
H1: μ1−μ2>0
Given μ1 = Mean of QM, μ2 = Mean of Compulsory Papers, α = 0.05:

We reject the null hypothesis for both tests, given p-value = 0.0184 for the first and p-value = 0.02869 for the second. This seemed to suggest that the mean of the QM paper was significantly higher. 

However, we argue that 2020 and 2019 were outliers given the use of SPSS in place of R. Removing the outliers, we performed the test again. 


```R
#remove outlier years of 2020, 2019
df_wo <- df_edit[3:6,]

#re-run student t-test
Comp_QM_wo <- t.test(df_wo$Option_QM, df_wo$MeanComp, alternative="greater", paired = T) 

Option_QM_wo <- t.test(df_wo$Option_QM, df_wo$MeanOpt, alternative="greater", paired = F) 

Comp_QM_wo
Option_QM_wo
```
```
##  Paired t-test
## 
## data:  df_wo$Option_QM and df_wo$MeanComp
## t = 1.908, df = 3, p-value = 0.07621
## alternative hypothesis: true difference in means is greater than 0

##  Welch Two Sample t-test
## 
## data:  df_wo$Option_QM and df_wo$MeanOpt
## t = 1.4335, df = 3.0667, p-value = 0.1226
## alternative hypothesis: true difference in means is greater than 0
```
H0: μ1−μ2=0
H1: μ1−μ2>0
Given μ1 = Mean of QM, μ2 = Mean of Compulsory Papers, α = 0.05:

Here, we argue that we fail to reject the null hypothesis given the **p-value = 0.1226** for the test between the mean of the QM Option score and the other Options. 
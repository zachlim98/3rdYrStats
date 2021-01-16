# Introduction

The study is designed to investigate the correlation of marital satisfaction to the degree of parental challenges parents of children aged 7-16 years old in Singapore. Based on literature review, marital satisfaction was an important predictor of
parenting experiences. A research done with parents of adolescents and adults with autism shows that parents in the general population who are in well-functioning marital relationships report less parenting burden or stress than otherwise. Research shows that it also affects quality of parent-child relationships, parents with less than average marital satisfaction experiencing more burden than those with above average satisfaction (Hartley, Barker, Seltzer &amp; Greenberg, 2012). 

Children of distressed couples often have poorer school performance, more behavioural problems, greater chances of having physical illness and on the more extreme end, higher rate of emotional problems and lower level of well-being. In a longitudinal study (Linville, Chornister, Dishion, Todahl, Miller, Shaw, Gardner, & Wilson, 2010), it shows that couple satisfaction, parenting practices and parental depression were significantly interrelated, which predicts child behavioural problems over time. Although parent depression and parenting practices were associated to problem behaviours, but only couple satisfaction predicted later child problem behaviour. 

# Design

This study obtained information on parental issues faced and relationship quality of SDQ parents with their spouses. Data was collected over 3 months. The initial sample size was targeted to be **156** parents. However, as a result of survey limitations, only 24 parents were included in the final survey. Data was collected separately with regards to parent's marital satisfaction and SDQ score.

R Version 4.0.2 was used for the statistical analysis.

# Results

An initial linear regression was performed, with the SDQ score as the response variable. All the elements of marital satisfaction as collected in the survey (e.g. frequency of arguments with spouse, stability of marriage, overall happiness etc.) were set as explanatory variables. The results of the linear regression model is shown below. 

```R
lm(formula = Total ~ ., data = survey)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.4330 -1.0772 -0.2089  0.9223  3.5670 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)   
(Intercept)    14.24258    4.36792   3.261   0.0062 **
commitment      2.16818    1.09246   1.985   0.0687 . 
spousal_commit -1.04049    0.91504  -1.137   0.2760   
stable          1.43601    1.33353   1.077   0.3011   
strong         -2.76143    1.47466  -1.873   0.0838 . 
happy          -0.13228    1.55774  -0.085   0.9336   
team            0.10950    1.06150   0.103   0.9194   
overall_hap     0.08357    0.83691   0.100   0.9220   
argue           0.76391    0.94993   0.804   0.4358   
parent_stress   0.24674    0.73125   0.337   0.7412   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.273 on 13 degrees of freedom
Multiple R-squared:  0.4962,	Adjusted R-squared:  0.1086 
F-statistic:  1.28 on 10 and 13 DF,  p-value: 0.3321
```

As can be observed, none of the specific variables were particularly correlated with SDQ score. This is also evidenced by the correlation plot shown below, where SDQ score was neither strongly nor positively correlated with any of the variables.

![image](https://user-images.githubusercontent.com/68678549/104799302-01053300-5809-11eb-86da-331e8760604b.png)

If we plot overall happiness against SDQ score, we do see a relatively negative correlation, showing that as overall marital happiness increases, SDQ score does decrease. However, note the largeness of the error bars surrounding the plot, showing the relatively large errors of the linear regression line plotted. 

![image](https://user-images.githubusercontent.com/68678549/104799349-77a23080-5809-11eb-9e8f-a4d4a6762ec8.png)

# Discussion

The results of the survey and the statistical analysis perform show that there was no significant correlation between marital satisfaction/happiness and SDQ score. While certain graphed variables might appear to give this impression, it would be dangerous to accept these results given the lack of correlation found in the linear regression model. The key limitation of this research paper is the paucity of data collected. 24 participants is too low a number to draw any significant conclusions. 




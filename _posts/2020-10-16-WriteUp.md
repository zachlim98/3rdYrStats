Problem Sheet 1
===============

Problem Set A
-------------

Imported dplyr for dataframe manipulation. Used expanded grid to create
the variable of all possible outcomes

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    #Create vector of dice values
    dice <- c(1,2,3,4,5,6)

    #use expand.grid to return all possible die combinations
    S <- expand.grid(dice,dice,dice)

    #create new variable with sum
    S <- S %>% mutate(Value=Var1+Var2+Var3)

    #Calculate probability of sum of eyes=12 
    prob_sum_12 <- nrow(S[S$Value == "12",])/nrow(S)

    #Show value
    prob_sum_12

    ## [1] 0.1157407

Wanted to compare result with `sample` function’s result. Results were
similar

    #confirm with sample
    set.seed(1510)
    N1 <- 1000000
    diceRolls <- replicate(3, sample(dice, N1, replace = TRUE))
    sum_12_2 <- sum(diceRolls[,1] + diceRolls[,2] + diceRolls[,3] == 12)
    prob_sum_12_2 <- sum_12_2/N1
    prob_sum_12_2

    ## [1] 0.11543

Next, needed to create vector of probabilities for biased die. Then
renamed the columns and binded the probabilities to the variables

    #create vector of probability
    biased_prob <- c(0.125,0.125,0.125,0.125,0.125,0.375)
    prob_grid <- expand.grid(biased_prob,biased_prob,biased_prob)
    #rename columns 
    prob_grid <- prob_grid %>% rename(Prob1=Var1, Prob2=Var2, Prob3=Var3)

    #join the dfs together
    S <- bind_cols(S, prob_grid)

Ran `sample` to estimate probability of sum being 12 for biased dice

    # Calculate probability that sum is 12 
    set.seed(1510)
    N2 <- 1000000
    BdiceRolls <- replicate(3, sample(dice, N2, replace = TRUE, prob = biased_prob))
    biased_sum_12 <- sum(BdiceRolls[,1] + BdiceRolls[,2] + BdiceRolls[,3] == 12)
    prob_biased_12 <- biased_sum_12/N2
    prob_biased_12

    ## [1] 0.107438

Problem Set B
-------------

Imported library `ggplot2` for plotting and imported data

    library(ggplot2)

    #import CSVs
    kenya_raw <- read.csv("P:/Adulting/Uni/Year 3/Papers/QM/Tutorial 1/kenya.csv")
    sweden_raw <- read.csv("P:/Adulting/Uni/Year 3/Papers/QM/Tutorial 1/sweden.csv")
    world_raw <- read.csv("P:/Adulting/Uni/Year 3/Papers/QM/Tutorial 1/world.csv")

Created functions to calculate ASFR, TFR, and ASDR

    #create asfr, tfr, asdr function 
    asfr_tfr_asdr <- function(dataset){

      dataset <- dataset %>% mutate(asfr = births/py.women)
      dataset <- dataset %>% mutate(asdr = deaths/rowSums(dataset[,c("py.men","py.women")]))

      tfr1 <- dataset %>% slice(4:10) %>% summarise(result=sum(asfr))
      tfr2 <- dataset %>% slice(19:25) %>% summarise(result=sum(asfr))

      print(paste("The TFR for the period 1950 - 1955 is: ", (tfr1[1]*5)))
      print(paste("The TFR for the period 2005 - 2010 is: ", (tfr2[1]*5)))
      print(paste("This was a ", (format(round((((tfr2[1]-tfr1[1])/tfr1[1])*100),2))), "% change between the 2 periods"))
      return(dataset)
    }

Ran the functions to create new dataframes to store the ASFR and ASDR.
Combined the dataframes for easier plotting

    #Compute asdr, asfr, tfr for datasets
    kenya_comp <- asfr_tfr_asdr(kenya_raw)

    ## [1] "The TFR for the period 1950 - 1955 is:  7.591409803262"
    ## [1] "The TFR for the period 2005 - 2010 is:  4.87956848605033"
    ## [1] "This was a  -35.72 % change between the 2 periods"

    sweden_comp <- asfr_tfr_asdr(sweden_raw)

    ## [1] "The TFR for the period 1950 - 1955 is:  2.2269172222989"
    ## [1] "The TFR for the period 2005 - 2010 is:  1.90276415584971"
    ## [1] "This was a  -14.56 % change between the 2 periods"

    world_comp <- asfr_tfr_asdr(world_raw)

    ## [1] "The TFR for the period 1950 - 1955 is:  5.00724803403158"
    ## [1] "The TFR for the period 2005 - 2010 is:  2.54362342232248"
    ## [1] "This was a  -49.2 % change between the 2 periods"

    #merge dfs
    merge_comp <- bind_rows(kenya_comp,sweden_comp,world_comp)
    merge_comp$age <- factor(merge_comp$age, levels = unique(merge_comp$age))

    #plot asfr
    asfr_plot <- merge_comp %>%
      ggplot(aes(age, asfr, fill=period)) +
      geom_col() +
      facet_grid(~country) +
      theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1),
            panel.background = element_blank()) +
      labs(title = "ASFR, segmented by Country", x = "Age Group", y = "ASFR") 
    asfr_plot

![](WriteUp_files/figure-markdown_strict/unnamed-chunk-9-1.png)

    #plot asfr with country comparison
    asfr_plot_c <- merge_comp %>%
      ggplot(aes(age, asfr, fill=country)) +
      geom_col(position="dodge") +
      facet_grid(~period) +
      theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1),
            panel.background = element_blank()) +
      labs(title = "ASFR, segmented by Time Period", x = "Age Group", y = "ASFR") 
    asfr_plot_c

![](WriteUp_files/figure-markdown_strict/unnamed-chunk-10-1.png)

    #plot asdr
    asdr_plot <- merge_comp %>%
      ggplot(aes(age, asdr, fill=period)) +
      geom_col() +
      facet_grid(~country) +
      theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1), 
            panel.background = element_blank()) +
      labs(title = "ASDR, segmented by Country", x = "Age Group", y = "ASDR") 
    asdr_plot

![](WriteUp_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    #plot asdr with country comparison
    asdr_plot_c <- merge_comp %>%
      ggplot(aes(age, asdr, fill=country)) +
      geom_col(position="dodge") +
      facet_grid(~period) +
      theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1),
            panel.background = element_blank()) +
      labs(title = "ASDR, segmented by Time Period", x = "Age Group", y = "ASDR") 
    asdr_plot_c

![](WriteUp_files/figure-markdown_strict/unnamed-chunk-12-1.png)

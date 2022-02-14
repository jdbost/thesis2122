#### ON START UP ####

# clear environment

rm(list = ls())

# set working directory to project directory

setwd("C:/Users/avata/OneDrive/Desktop/R/thesis2122")

# load libraries

library(tidyverse)

# load data

load("C:/Users/avata/OneDrive/Desktop/POL 194H/NAAS/ICPSR_37380/DS0001/NAAS-Data.rda")

# store data in a nicer frame!!

df <- da37380.0001




#### DATA CLEANING ####

# Independent Variables

df$identity <- df$Q10_21 # Q10_21 "What is your present religion, if any?" 
df$express_ex <- df$Q10_23 # Q10_23 "How often do you attend a place of worship?" 
df$express_in <- df$Q4_2E # Q4_2E "How important is your religion to your identity?" 
df$aapiethnicity <- df$S10_1 # S10_1 "Do you consider any part of your background to be Asian or Asian-American, such as Chinese, Filipino, Indian, or Pacific Islander like Native Hawaiian or Samoan?" 

# Linked fate battery - Dependent Variable

df$linkedfate_race <- df$Q4_3 # Q4_3 "Do you think what happens generally to other [RACES] in this country affects what happens in your life?" 
df$linkedfate_race_howmuch <- df$Q4_3A # Q4_3A "Will it affect you a lot, some, or not very much?"
df$linkedfate_ethn <- df$Q4_4 # Q4_4 "Do you think what happens generally to other [RETHNIC] Americans affects what happens in your life?"
df$linkedfate_ethn_howmuch <- df$Q4_4A # Q4_4A "Will it affect you a lot, some, or not very much?"
df$linkedfate_share <- df$Q4_5A # Q4_5A "What, if anything do [INSRACES] in the United States share with one another? Would you say they share...A common race"

# Just Asian Americans - 4362 observations

# # subset example code: https://stats.oarc.ucla.edu/r/modules/subsetting-data/

df2 <- subset(df, df$INSRACE == "(1) Asian American")


table(df2$identity)            
table(df2$express_ex)
table(df2$express_in)
table(df2$aapiethnicity)
table(df2$linkedfate_race)
table(df2$linkedfate_race_howmuch)
table(df2$linkedfate_ethn)
table(df2$linkedfate_ethn_howmuch)
table(df2$linkedfate_share)


#### RECODE SO NO FACTOR VARIABLES + DESCRIPTIVE STATS CODE ####

# # 01/29/2022: MAKE CATEGORIES MATCH CMPS

# IV: Religious Identity

df2$identity_num <- as.numeric(df2$identity) 

table(df2$identity_num)         # table() function checks that new coding is correct!
table(df2$identity,          
      df2$identity_num)

df2$identity_hist[df2$identity == "(01) Agnostic"] <- "Atheist or agnostic"
df2$identity_hist[df2$identity == "(02) Atheist"] <- "Atheist or agnostic"
df2$identity_hist[df2$identity == "(03) Baptist"] <- "Other"
df2$identity_hist[df2$identity == "(04) Buddhist"] <- "Buddhist"
df2$identity_hist[df2$identity == "(05) Catholic"] <- "Catholic"
df2$identity_hist[df2$identity == "(06) Christian"] <- "Christian"
df2$identity_hist[df2$identity == "(07) Christian Scientists"] <- "Other"
df2$identity_hist[df2$identity == "(08) Church of God in Christ"] <- "Other"
df2$identity_hist[df2$identity == "(09) Church of the Nazarene"] <- "Other"
df2$identity_hist[df2$identity == "(10) Congregationalist (includes United Church of Christ)"] <- "Other"
df2$identity_hist[df2$identity == "(11) Disciples of Christ, Churches of Christ"] <- "Other"
df2$identity_hist[df2$identity == "(12) Episcopalian, Anglican"] <- "Other"
df2$identity_hist[df2$identity == "(13) Falun Gong"] <- "Other"
df2$identity_hist[df2$identity == "(14) Greek Orthodox"] <- "Other"
df2$identity_hist[df2$identity == "(15) Hindu"] <- "Hindu"
df2$identity_hist[df2$identity == "(16) Jain"] <- "Other"
df2$identity_hist[df2$identity == "(17) Jehovah's Witnesses"] <- "Other"
df2$identity_hist[df2$identity == "(18) Jewish"] <- "Other"
df2$identity_hist[df2$identity == "(19) Lutheran"] <- "Other"
df2$identity_hist[df2$identity == "(20) Methodist"] <- "Other"
df2$identity_hist[df2$identity == "(21) Mormon, Church of the Latter Day Saints"] <- "Other"
df2$identity_hist[df2$identity == "(22) Muslim, Mohammedan, Islam"] <- "Muslim"
df2$identity_hist[df2$identity == "(23) Orthodox, Eastern Orthodox"] <- "Other"
df2$identity_hist[df2$identity == "(24) Pentecostal"] <- "Other"
df2$identity_hist[df2$identity == "(25) Presbyterian"] <- "Other"
df2$identity_hist[df2$identity == "(26) Protestant (no denomination given)"] <- "Protestant"
df2$identity_hist[df2$identity == "(27) Reformed, Dutch Reformed, Christian Reformed"] <- "Other"
df2$identity_hist[df2$identity == "(28) Seventh Day Adventist"] <- "Other"
df2$identity_hist[df2$identity == "(29) Sikh"] <- "Other"
df2$identity_hist[df2$identity == "(30) Unitarian, Universalist"] <- "Other"
df2$identity_hist[df2$identity == "(31) Other Non-Christian"] <- "Other"
df2$identity_hist[df2$identity == "(32) African Methodist Episcopal/AME"] <- "Other"
df2$identity_hist[df2$identity == "(77) No religion"] <- "None"
df2$identity_hist[df2$identity == "(78) Spiritual, but not religious"] <- "Other"

df2$identity_hist <- factor(df2$identity_hist,
                            levels = c("Catholic",
                                       "Protestant",
                                       "Christian",
                                       "Muslim",
                                       "Hindu",
                                       "Buddhist",
                                       "Atheist or agnostic",
                                       "Other",
                                       "None"))

table(df2$identity_hist)

df2$identity_reg <- as.numeric(df2$identity_hist)

table(df2$identity_hist,
      df2$identity_reg)


# IV: Religious Expression - External/Frequency

df2$express_ex_num <- as.numeric(df2$express_ex)
df2$express_ex_num <- (df2$express_ex_num-6)*(-1)

table(df2$express_ex_num)
table(df2$express_ex,
      df2$express_ex_num)

df2$express_ex_hist[df2$express_ex_num == 0] <- "Never"
df2$express_ex_hist[df2$express_ex_num == 1] <- "Seldom"
df2$express_ex_hist[df2$express_ex_num == 2] <- "A few times a year"
df2$express_ex_hist[df2$express_ex_num == 3] <- "Once or twice a month"
df2$express_ex_hist[df2$express_ex_num == 4] <- "Once a week"
df2$express_ex_hist[df2$express_ex_num == 5] <- "More than once a week"

df2$express_ex_hist <- factor(df2$express_ex_hist,
                                  levels = c("Never",
                                             "Seldom",
                                             "A few times a year",
                                             "Once or twice a month",
                                             "Once a week",
                                             "More than once a week"))

table(df2$express_ex_hist)
table(df2$express_ex_hist,
      df2$express_ex_num)


# IV: Religious Expression - Internal/Importance

table(df2$express_in)

df2$express_in_num <- as.numeric(df2$express_in)
df2$express_in_num <- (df2$express_in_num-1)

table(df2$express_in_num)
table(df2$express_in,
      df2$express_in_num)

df2$express_in_hist[df2$express_in_num == 0] <- "Not at all important"
df2$express_in_hist[df2$express_in_num == 1] <- "Somewhat important"
df2$express_in_hist[df2$express_in_num == 2] <- "Very important"
df2$express_in_hist[df2$express_in_num == 3] <- "Extremely important"

df2$express_in_hist <- factor(df2$express_in_hist,
                              levels = c("Not at all important",
                                         "Somewhat important",
                                         "Very important",
                                         "Extremely important"))

table(df2$express_in_hist)
table(df2$express_in_hist,
      df2$express_in_num)


# Interaction: Ethnicity

df2$aapiethnicity_num <- as.numeric(df2$aapiethnicity)

table(df2$aapiethnicity_num)
table(df2$aapiethnicity,
      df2$aapiethnicity_num)

df2$aapiethnicity_hist[df2$aapiethnicity_num == 2] <- "Other"
df2$aapiethnicity_hist[df2$aapiethnicity_num == 3] <- "Other"
df2$aapiethnicity_hist[df2$aapiethnicity_num == 4] <- "Chinese"
df2$aapiethnicity_hist[df2$aapiethnicity_num == 5] <- "Filipino"
df2$aapiethnicity_hist[df2$aapiethnicity_num == 6] <- "Other"
df2$aapiethnicity_hist[df2$aapiethnicity_num == 7] <- "Indian"
df2$aapiethnicity_hist[df2$aapiethnicity_num == 8] <- "Japanese"
df2$aapiethnicity_hist[df2$aapiethnicity_num == 9] <- "Korean"
df2$aapiethnicity_hist[df2$aapiethnicity_num == 11] <- "Other"
df2$aapiethnicity_hist[df2$aapiethnicity_num == 12] <- "Vietnamese"

df2$aapiethnicity_hist <- factor(df2$aapiethnicity_hist,
                              levels = c("Chinese",
                                         "Indian",
                                         "Korean",
                                         "Filipino",
                                         "Vietnamese",
                                         "Japanese",
                                         "Other"))

table(df2$aapiethnicity_hist)
table(df2$aapiethnicity_num,
      df2$aapiethnicity_hist)

df2$aapiethnicity_reg <- as.numeric(df2$aapiethnicity_hist)

table(df2$aapiethnicity_hist,
      df2$aapiethnicity_reg)


# DV: Racial Linked Fate

df2$linkedfate_race_yes <- ifelse(df2$linkedfate_race == "(1) Yes", 1, 0)

table(df2$linkedfate_race_yes)
table(df2$linkedfate_race,
      df2$linkedfate_race_yes)


df2$linkedfate_race_how_num <- as.numeric(df2$linkedfate_race_howmuch)
df2$linkedfate_race_how_num <- (df2$linkedfate_race_how_num-3)*(-1)

table(df2$linkedfate_race_how_num)
table(df2$linkedfate_race_howmuch,
      df2$linkedfate_race_how_num)

df2$linkedfate_race_hist[df2$linkedfate_race == "(2) No"] <- "No"
df2$linkedfate_race_hist[df2$linkedfate_race_howmuch == "(3) Not very much"] <- "Not very much"
df2$linkedfate_race_hist[df2$linkedfate_race_howmuch == "(2) Some"] <- "Some"
df2$linkedfate_race_hist[df2$linkedfate_race_howmuch == "(1) A lot"] <- "A lot"

df2$linkedfate_race_hist <- factor(df2$linkedfate_race_hist,
                                   levels = c("No",
                                              "Not very much",
                                              "Some",
                                              "A lot"))

table(df2$linkedfate_race_hist)

df2$linkedfate_race_reg[df2$linkedfate_race == "(2) No"] <- 0
df2$linkedfate_race_reg[df2$linkedfate_race_howmuch == "(3) Not very much"] <- 1
df2$linkedfate_race_reg[df2$linkedfate_race_howmuch == "(2) Some"] <- 2
df2$linkedfate_race_reg[df2$linkedfate_race_howmuch == "(1) A lot"] <- 3

table(df2$linkedfate_race_reg)

table(df2$linkedfate_race_hist,
      df2$linkedfate_race_reg)


# DV: Ethnic Linked Fate

df2$linkedfate_ethn_yes <- ifelse(df2$linkedfate_ethn == "(1) Yes", 1, 0)

table(df2$linkedfate_ethn,
      df2$linkedfate_ethn_yes)

df2$linkedfate_ethn_how_num <- as.numeric(df2$linkedfate_ethn_howmuch)
df2$linkedfate_ethn_how_num <- (df2$linkedfate_ethn_how_num-3)*(-1)

table(df2$linkedfate_ethn_how_num)
table(df2$linkedfate_ethn_howmuch,
      df2$linkedfate_ethn_how_num)

df2$linkedfate_ethn_hist[df2$linkedfate_ethn == "(2) No"] <- "No"
df2$linkedfate_ethn_hist[df2$linkedfate_ethn_howmuch == "(3) Not very much"] <- "Not very much"
df2$linkedfate_ethn_hist[df2$linkedfate_ethn_howmuch == "(2) Some"] <- "Some"
df2$linkedfate_ethn_hist[df2$linkedfate_ethn_howmuch == "(1) A lot"] <- "A lot"

df2$linkedfate_ethn_hist <- factor(df2$linkedfate_ethn_hist,
                                   levels = c("No",
                                              "Not very much",
                                              "Some",
                                              "A lot"))

table(df2$linkedfate_ethn_hist)

df2$linkedfate_ethn_reg[df2$linkedfate_ethn == "(2) No"] <- 0
df2$linkedfate_ethn_reg[df2$linkedfate_ethn_howmuch == "(3) Not very much"] <- 1
df2$linkedfate_ethn_reg[df2$linkedfate_ethn_howmuch == "(2) Some"] <- 2
df2$linkedfate_ethn_reg[df2$linkedfate_ethn_howmuch == "(1) A lot"] <- 3

table(df2$linkedfate_ethn_reg)

table(df2$linkedfate_ethn_hist,
      df2$linkedfate_ethn_reg)


# DV: Commmon race

df2$linkedfate_share_num <- ifelse(df2$linkedfate_share == "(1) Yes", 1, 0)

table(df2$linkedfate_share_num)
table(df2$linkedfate_share_num,
      df2$linkedfate_share)

df2$linkedfate_share_hist[df2$linkedfate_share == "(1) Yes"] <- "Yes"
df2$linkedfate_share_hist[df2$linkedfate_share == "(2) No"] <- "No"

table(df2$linkedfate_share_hist)
table(df2$linkedfate_share_num,
      df2$linkedfate_share_hist)




#### BREAKDOWN TABLES ####

# Use to figure out interesting jitters

# # prop.table(table(df$var1, df$var2),1)*100




#### HISTORGRAMS AND BARCHARTS ####

# Religious identity barplot - Univariate

identity_bar <- ggplot(data=subset(df2, !is.na(identity_hist)),
                       aes(x = identity_hist, 
                           y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.75) +
  labs(x = "", 
       y = "Percent", 
       title  = "What is your present religion, if any?",
       fill = "Religious identity") +
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

identity_bar


# External expression - Univariate

express_ex_bar <- ggplot(data=subset (df2, !is.na(express_ex_hist)),
                      aes(x = express_ex_hist, 
                          y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.6) +
  labs(x = "", 
       y = "Percent", 
       title  = "How often do you attend a place of worship?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

express_ex_bar


# Internal expression - Univariate

express_in_bar <- ggplot(data=subset (df2, !is.na(express_in_hist)),
                         aes(x = express_in_hist, 
                             y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.6) +
  labs(x = "", 
       y = "Percent", 
       title  = "How important is your religion to your identity?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

express_in_bar


# Ethnicity - Univariate

aapiethnicity_bar <- ggplot(df2, 
                   aes(x = aapiethnicity_hist, 
                       y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.65) +
  labs(x = "", 
       y = "Percent", 
       title  = "Do you consider any part of your background to be Asian or Asian-American, such as Chinese, Filipino, or Indian?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

aapiethnicity_bar

# # kind of a ridiculous distribution...


# Racial Linked Fate - Univariate

linkedfate_race_bar <- ggplot(data=subset (df2, !is.na(linkedfate_race_hist)),
                              aes(x = linkedfate_race_hist, 
                                  y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.5) +
  labs(x = "", 
       y = "Percent", 
       title  = "Do you think what happens generally to other [RACES] in this country affects what happens in your life?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

linkedfate_race_bar


# Ethnic Linked Fate - Univariate

linkedfate_ethn_bar <- ggplot(data=subset (df2, !is.na(linkedfate_ethn_hist)),
                              aes(x = linkedfate_ethn_hist, 
                                  y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.6) +
  labs(x = "", 
       y = "Percent", 
       title  = "Do you think what happens generally to other [RETHNIC] Americans affects what happens in your life?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

linkedfate_ethn_bar


# Common race - Univariate

linkedfate_share_bar <- ggplot(data=subset (df2, !is.na(linkedfate_share_hist)),
                              aes(x = linkedfate_share_hist, 
                                  y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.5) +
  labs(x = "", 
       y = "Percent", 
       title  = "What, if anything do [INSRACES] in the United States share with one another? Would you say they share...A common race?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

linkedfate_share_bar


#### JITTER PLOTS ####

identity_express_ex_jitter <- ggplot(data=subset (df2, !is.na(express_ex_hist)),
                                     aes(x = identity_hist, 
                                         y = express_ex_hist)) + 
  geom_jitter(color = "steelblue") +
  labs(x = "Religious Identity", 
       y = "Frequency of Religious Expression", 
       title  = "Level of Religious Expression by Religious Identity") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

identity_express_ex_jitter


identity_express_in_jitter <- ggplot(data=subset (df2, !is.na(express_in_hist)),
                                     aes(x = identity_hist, 
                                         y = express_in_hist)) + 
  geom_jitter(color = "steelblue") +
  labs(x = "Religious Identity", 
       y = "Importance of Religious Expression", 
       title  = "Level of Religious Expression by Religious Identity") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

identity_express_in_jitter

#### CORRELATION PLOT ####

library(ggcorrplot)

# try to make a matrix w/numeric variables

df2_mat <- df2[c("identity_reg", "express_ex_num", "express_in_num", "aapiethnicity_reg", "linkedfate_race_yes","linkedfate_share_num")] %>%
  group_by(identity_reg,express_ex_num,express_in_num,aapiethnicity_reg,linkedfate_race_yes,linkedfate_share_num)

# df2_mat <- df2[c("express_reg", "linkedfate_reg")] %>%
#   group_by(express_reg,linkedfate_reg)

df2_mat

# Compute a correlation matrix

corr_mat <- round(cor(df2_mat, use = "complete.obs"), 2)

corr_mat

head(corr_mat)

# Compute a matrix of correlation p-values

corr_mat_p <- cor_pmat(corr_mat)

corr_mat_p

# Visualize the correlation matrix

ggcorrplot(corr_mat, 
           hc.order = TRUE,                               # Using hierarchical clustering
           type = "lower",                                # Get the lower triangle
           insig = "blank",                               # Leave blank on no significant coefficient
           lab = TRUE,                                    # Add correlation coefficients
           ggtheme = ggplot2::theme_minimal,
           colors = c("#6D9EC1", "white", "#E46726")) +
  labs(title  = "Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))


#### CORRELATIONS - LINEAR REGRESSION ####

summary(lm(data=df2, express_ex_num~express_in_num))
summary(lm(data=df2, linkedfate_race_yes~express_in_num))

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.6042548  0.0127877  47.253   <2e-16 ***
# express_in_num 0.0009827  0.0069754   0.141    0.888    

# Residual standard error: 0.4888 on 3939 degrees of freedom
#  (421 observations deleted due to missingness)
# Multiple R-squared:  5.038e-06,	Adjusted R-squared:  -0.0002488 
# F-statistic: 0.01985 on 1 and 3939 DF,  p-value: 0.888

summary(lm(data=df2, linkedfate_race_yes~express_in_num*identity_hist))

# Coefficients:
#                                                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                      0.575060   0.042356  13.577   <2e-16 ***
# express_in_num                                   0.007305   0.020603   0.355   0.7229    
# identity_histProtestant                         -0.140042   0.156523  -0.895   0.3710    
# identity_histChristian                           0.059222   0.053666   1.104   0.2699    
# identity_histMuslim                             -0.100964   0.062424  -1.617   0.1059    
# identity_histHindu                              -0.110423   0.066873  -1.651   0.0988 .  
# identity_histBuddhist                            0.031672   0.054824   0.578   0.5635    
# identity_histAtheist or agnostic                 0.097502   0.061416   1.588   0.1125    
# identity_histOther                               0.072551   0.063981   1.134   0.2569    
# identity_histNone                                0.072530   0.048755   1.488   0.1369    
# express_in_num:identity_histProtestant           0.110023   0.083682   1.315   0.1887    
# express_in_num:identity_histChristian           -0.001151   0.027070  -0.043   0.9661    
# express_in_num:identity_histMuslim               0.060111   0.030579   1.966   0.0494 *  
# express_in_num:identity_histHindu                0.072880   0.035876   2.031   0.0423 *  
# express_in_num:identity_histBuddhist            -0.063212   0.027187  -2.325   0.0201 *  
# express_in_num:identity_histAtheist or agnostic  0.031953   0.064240   0.497   0.6189    
# express_in_num:identity_histOther                0.009842   0.030887   0.319   0.7500    
# express_in_num:identity_histNone                -0.022473   0.029950  -0.750   0.4531    

# Residual standard error: 0.4849 on 3701 degrees of freedom
#  (643 observations deleted due to missingness)
# Multiple R-squared:  0.0202,	Adjusted R-squared:  0.0157 
# F-statistic: 4.489 on 17 and 3701 DF,  p-value: 2.21e-09

summary(lm(data=df2, linkedfate_race_yes~express_ex_num))

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     0.626684   0.020466  30.621   <2e-16 ***
# express_ex_num -0.011140   0.006194  -1.798   0.0722 .  

# Residual standard error: 0.4911 on 2978 degrees of freedom
#  (1382 observations deleted due to missingness)
# Multiple R-squared:  0.001085,	Adjusted R-squared:  0.0007495 
# F-statistic: 3.234 on 1 and 2978 DF,  p-value: 0.07221

summary(lm(data=df2, linkedfate_race_yes~express_ex_num*identity_hist))

# Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                             0.5925383  0.0592357  10.003  < 2e-16 ***
# express_ex_num                         -0.0009972  0.0161393  -0.062 0.950736    
# identity_histProtestant                -0.0856187  0.2160280  -0.396 0.691889    
# identity_histChristian                  0.0962956  0.0786970   1.224 0.221191    
# identity_histMuslim                     0.0655829  0.0800274   0.820 0.412564    
# identity_histHindu                      0.0140488  0.0935277   0.150 0.880609    
# identity_histBuddhist                  -0.0818734  0.0728823  -1.123 0.261373    
# identity_histOther                      0.2509944  0.0744370   3.372 0.000756 ***
# express_ex_num:identity_histProtestant  0.0399516  0.0644274   0.620 0.535238    
# express_ex_num:identity_histChristian  -0.0125502  0.0211775  -0.593 0.553480    
# express_ex_num:identity_histMuslim     -0.0212183  0.0222602  -0.953 0.340569    
# express_ex_num:identity_histHindu      -0.0107685  0.0299440  -0.360 0.719155    
# express_ex_num:identity_histBuddhist   -0.0010796  0.0229729  -0.047 0.962522    
# express_ex_num:identity_histOther      -0.0589136  0.0219843  -2.680 0.007407 ** 

# Residual standard error: 0.487 on 2966 degrees of freedom
#  (1382 observations deleted due to missingness)
# Multiple R-squared:  0.02137,	Adjusted R-squared:  0.01708 
# F-statistic: 4.982 on 13 and 2966 DF,  p-value: 9.24e-09

summary(lm(data=df2, linkedfate_ethn_yes~express_ex_num))
summary(lm(data=df2, linkedfate_ethn_yes~express_ex_num))
summary(lm(data=df2, linkedfate_ethn_yes~express_ex_num))
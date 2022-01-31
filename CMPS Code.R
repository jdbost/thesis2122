#### ON START UP ####

# clear environment

rm(list = ls())

# set working directory to project directory

setwd("C:/Users/avata/OneDrive/Desktop/R/thesis2122")

# load libraries

library(tidyverse)
library(ggplot2)


# load data

load("C:/Users/avata/OneDrive/Desktop/POL 194H/CMPS/ICPSR_38040/DS0001/CMPS2016-Data.rda")

# store data in a nicer frame!!

df <- da38040.0001




#### DATA CLEANING ####

# Independent Variables

df$identity <- df$C129 # C129 "When it comes to religion, what do you consider yourself to be?" 
df$express <- df$C131 # C131 "Do you attend religious service or gathering: at least every week, almost every week, a few times a month, only a few times during the year, hardly ever or never?" 
df$aapiethnicity <- df$S8 # S8 "Asian Americans come from a diversity of backgrounds. What do you consider to be your primary ethnicity or family ancestry:" 
df$relig_ethn <- df$C133_4 # C133_4 "Please indicate the approximate racial/ethnic composition of your place of religious worship or gathering: Percent Asian"

# Linked fate battery - Dependent Variable

df$linkedfate <- df$C150 # C150 "Do you think what happens generally to [ANS to S2] people in this country will have something to do with what happens in your life?" 
df$linkedfate_howmuch <- df$C151 # C151 "Will it [ANS C150] affect you . a lot, some, not very much?"
df$linkedfate_positive <- df$C152 # C152 "Some people feel positively about the link they have with their racial or ethnic group members, while others feel negatively about the idea that their lives may be influenced by how well the larger group is doing. Which comes closer to your feelings?"

# Just Asian Americans - 3006 observations

df2 <- df[!is.na(df$aapiethnicity),]

table(df2$identity)            # factor
table(df2$express)             # factor
table(df2$aapiethnicity)       # factor
table(df2$relig_ethn)          # continuous, NA's included
table(df2$linkedfate)          # factor
table(df2$linkedfate_howmuch)  # factor
table(df2$linkedfate_positive) # factor



## RECODE SO NO FACTOR VARIABLES ##

# IV: Religious Identity

df2$identity_num <- as.numeric(df2$identity)

table(df2$identity,          # table() function checks that new coding is correct!
      df2$identity_num)


# IV: Religious Expression - External/Frequency

df2$express_num <- as.numeric(df2$express)
df2$express_num <- (df2$express_num-6)*(-1)

table(df2$express,
      df2$express_num)


# Interaction: Ethnicity

df2$aapiethnicity_num <- as.numeric(df2$aapiethnicity)

table(df2$aapiethnicity,
      df2$aapiethnicity_num)


# Interaction: Racial/Ethnic Composition of Place of Worship      

df2$binned_relig_ethn <- ifelse(df2$C133_4<=25,1,
                                ifelse(df2$C133_4>25&df2$C133_4<=50,2,
                                       ifelse(df2$C133_4>50&df2$C133_4<=75,3,
                                              ifelse(df2$C133_4>75&df2$C133_4<=100,4,NA))))

table(df2$binned_relig_ethn)

df2$binned_relig_ethn_hist[df2$binned_relig_ethn == 1] <- "0-25%"
df2$binned_relig_ethn_hist[df2$binned_relig_ethn == 2] <- "26-50%"
df2$binned_relig_ethn_hist[df2$binned_relig_ethn == 3] <- "51-75%"
df2$binned_relig_ethn_hist[df2$binned_relig_ethn == 4] <- "76-100%"

table(df2$binned_relig_ethn_hist)


# DV: Linked Fate

df2$linkedfate_yes <- ifelse(df2$linkedfate=="(1) Yes", 1, 0)

table(df2$linkedfate,
      df2$linkedfate_yes)


df2$linkedfate_how_num <- as.numeric(df2$linkedfate_howmuch) 
df2$linkedfate_how_num <- (df2$linkedfate_how_num-3)*(-1)

table(df2$linkedfate_howmuch,
      df2$linkedfate_how_num)


df2$linkedfate_pos_num <- as.numeric(df2$linkedfate_positive) 

table(df2$linkedfate_pos_num)
table(df2$linkedfate_pos_num,
      df2$linkedfate_positive)

# Spencer's Code

df2$linkedfate_pos_num2[df2$linkedfate_pos_num == 2] <- -1
df2$linkedfate_pos_num2[df2$linkedfate_pos_num == 1] <- 1
df2$linkedfate_pos_num2[df2$linkedfate_pos_num == 3] <- 0

table(df2$linkedfate_pos_num)
table(df2$linkedfate_pos_num2)


df2$linkedfate_pos_hist[df2$linkedfate_pos_num2 == -1] <- "Negatively"
df2$linkedfate_pos_hist[df2$linkedfate_pos_num2 == 1] <- "Positively"
df2$linkedfate_pos_hist[df2$linkedfate_pos_num2 == 0] <- "Neither positive nor negative"

df2$linkedfate_pos_hist <- factor(df2$linkedfate_pos_hist,
                                  levels = c("Positively",
                                             "Negatively",
                                             "Neither positive nor negative"))
                                  
table(df2$linkedfate_pos_hist)



#### DESCRIPTIVE STATISTICS ####

table(df2$identity)
summary(df2$identity_num)
table(df2$identity_num)


df2$identity_hist[df2$identity == "(1) Catholic"] <- "Catholic"
df2$identity_hist[df2$identity == "(2) Protestant"] <- "Protestant"
df2$identity_hist[df2$identity == "(3) Christian"] <- "Christian"
df2$identity_hist[df2$identity == "(4) Muslim"] <- "Muslim"
df2$identity_hist[df2$identity == "(5) Hindu"] <- "Hindu"
df2$identity_hist[df2$identity == "(6) Buddhist"] <- "Buddhist"
df2$identity_hist[df2$identity == "(7) Atheist or agnostic"] <- "Atheist or agnostic"
df2$identity_hist[df2$identity == "(8) Other:(SPECIFY)"] <- "Other"
df2$identity_hist[df2$identity == "(9) None"] <- "None"

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



table(df2$express)
table(df2$express_num)

df2$express_hist[df2$express_num == 0] <- "Never"
df2$express_hist[df2$express_num == 1] <- "Hardly ever"
df2$express_hist[df2$express_num == 2] <- "Only a few times during the year"
df2$express_hist[df2$express_num == 3] <- "A few times a month"
df2$express_hist[df2$express_num == 4] <- "Almost every week"
df2$express_hist[df2$express_num == 5] <- "At least every week"

df2$express_hist <- factor(df2$express_hist, 
                           levels = c("Never",
                                      "Hardly ever",
                                      "Only a few times during the year",
                                      "A few times a month",
                                      "Almost every week",
                                      "At least every week"))

table(df2$express_hist)

df2$express_reg <- as.numeric(df2$express_hist)

table(df2$express_hist,
      df2$express_reg)


table(df2$aapiethnicity)
table(df2$aapiethnicity, 
      df2$aapiethnicity_num)

df2$aapi_hist[df2$aapiethnicity_num == 1] <- "Chinese"
df2$aapi_hist[df2$aapiethnicity_num == 2] <- "Other"
df2$aapi_hist[df2$aapiethnicity_num == 3] <- "Indian"
df2$aapi_hist[df2$aapiethnicity_num == 4] <- "Korean"
df2$aapi_hist[df2$aapiethnicity_num == 5] <- "Filipino"
df2$aapi_hist[df2$aapiethnicity_num == 6] <- "Vietnamese"
df2$aapi_hist[df2$aapiethnicity_num == 7] <- "Japanese"
df2$aapi_hist[df2$aapiethnicity_num == 8] <- "Other"
df2$aapi_hist[df2$aapiethnicity_num == 9] <- "Other"
df2$aapi_hist[df2$aapiethnicity_num == 10] <- "Other"
df2$aapi_hist[df2$aapiethnicity_num == 11] <- "Other"
df2$aapi_hist[df2$aapiethnicity_num == 12] <- "Other"
df2$aapi_hist[df2$aapiethnicity_num == 13] <- "Other"
df2$aapi_hist[df2$aapiethnicity_num == 14] <- "Other"

df2$aapi_hist <- factor(df2$aapi_hist,levels = c("Chinese",
                                                 "Indian",
                                                 "Korean",
                                                 "Filipino",
                                                 "Vietnamese",
                                                 "Japanese",
                                                 "Other"))

table(df2$aapi_hist)

df2$aapi_reg <- as.numeric(df2$aapi_hist)

table(df2$aapi_hist,
      df2$aapi_reg)



summary(df2$relig_ethn)
summary(df2$binned_relig_ethn)

# linked fate variables: present df2$linkedfate_yes + df2$linkedfate_how_num

# # Spencer's Code

# for regression

df2$linkedfate_reg[df2$linkedfate == "(2) No"] <- 0
df2$linkedfate_reg[df2$linkedfate_howmuch == "(3) Not very much"] <- 1
df2$linkedfate_reg[df2$linkedfate_howmuch == "(2) Some"] <- 2
df2$linkedfate_reg[df2$linkedfate_howmuch == "(1) A lot"] <- 3

table(df2$linkedfate_reg)


# # Spencer's Code - edited

# for hist

table(df2$linkedfate_howmuch)
table(df2$linkedfate)

df2$linkedfate_hist[df2$linkedfate == "(2) No"] <- "No"
df2$linkedfate_hist[df2$linkedfate_howmuch == "(3) Not very much"] <- "Not very much"
df2$linkedfate_hist[df2$linkedfate_howmuch == "(2) Some"] <- "Some"
df2$linkedfate_hist[df2$linkedfate_howmuch == "(1) A lot"] <- "A lot"

# from https://sebastiansauer.github.io/ordering-bars/

df2$linkedfate_hist <- factor(df2$linkedfate_hist,
                              levels = c("No",
                                         "Not very much",
                                         "Some",
                                         "A lot"))

table(df2$linkedfate_hist)





#### BREAKDOWN TABLES ####

# # From Dr. B - https://www.statmethods.net/stats/frequencies.html

# # prop.table(your_table, 1)
# # prop.table(your_table, 2)
# # 1 = row percentages
# # 2 = column percentages
# # prop.table(your_table) # cell percentages
# # your_table <- table(df$var1, df$var2)
# # prop.table(df$var1, df$var2, 1)

# # your_table <- table(df$var1, df$var2)
# # prop.table(your_table)
# # prop.table(table(df$var1, df$var2))

# Breakdown by identity and express

prop.table(table(df2$identity_hist,df2$express_hist),1)*100


# Breakdown by national origin/ethnicity and religion

prop.table(table(df2$identity_hist,df2$aapi_hist),1)*100


# Breakdown by frequency of religious service attendance and linked fate

prop.table(table(df2$express_hist,df2$linkedfate_yes),1)*100

prop.table(table(df2$express_hist,df2$linkedfate_hist),1)*100



#### HISTOGRAMS/BARPLOTS ####

# Center title: https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2

theme(plot.title = element_text(hjust = 0.5))


# Linkedfate histogram/barplot - Univariate

# Example code from https://rkabacoff.github.io/datavis/Univariate.html#categorical

linkedfate_hist_bar <- ggplot(df2, 
                       aes(x = linkedfate_hist, 
                       y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.6) +
  labs(x = "", 
       y = "Percent", 
       title  = "Do you think what happens generally to Asian people in this country \nwill have something to do with what happens in your life?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


linkedfate_hist_bar


# Religious identity barplot - Univariate

identity_bar <- ggplot(df2, 
                       aes(x = identity_hist, 
                           y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.75) +
  labs(x = "", 
       y = "Percent", 
       title  = "When it comes to religion, what do you consider yourself to be?",
       fill = "Religious identity") +
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

identity_bar


# Racial composition of local religious community histogram - Bivariate

binned_relig_ethn_bar <- ggplot(data=subset(df2, !is.na(binned_relig_ethn_hist)),
                                aes(x = binned_relig_ethn_hist,
                                    y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.6) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "", 
       y = "Percent", 
       title  = "Approximate percentage of Asians attending respondents' place of religious worship or gathering") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

binned_relig_ethn_bar



# Ethnic linked fate histogram - Univariate

linkedfate_pos_bar <- ggplot(df2, 
  aes(x = linkedfate_pos_hist, 
      y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.5) +
  labs(x = "", 
       y = "Percent", 
       title  = "Do you feel positively, negatively, nor neither positive or negative \nabout the link you have with your racial or ethnic group members?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

linkedfate_pos_bar


# Ethnicity histogram - Univariate

aapi_bar <- ggplot(df2, 
                   aes(x = aapi_hist, 
                       y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.65) +
  labs(x = "", 
       y = "Percent", 
       title  = "What do you consider to be your primary ethnicity or family ancestry?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

aapi_bar


# External expression - Univariate

# # Get rid of NA column: https://stackoverflow.com/questions/17216358/eliminating-nas-from-a-ggplot#36778937

express_bar <- ggplot(data=subset (df2, !is.na(express_hist)),
                      aes(x = express_hist, 
                          y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.6) +
  labs(x = "", 
       y = "Percent", 
       title  = "How often do you attend a religious service or gathering?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30,hjust = 1),
        plot.title = element_text(hjust = 0.5))

express_bar


# Religious identity x expression stacked barplot - Bivariate

# Example code https://rkabacoff.github.io/datavis/Bivariate.html#Categorical-Categorical

identity_express_stackbar <- ggplot(df2, 
                                    aes(x = express_hist, 
                                        fill = identity_hist)) + 
  geom_bar(position = "stack",
           width = 0.75)

identity_express_stackbar <- identity_express_stackbar +
  labs(x = "", 
       y = "Percent", 
       title  = "How often do you attend a religious service or gathering?",
       fill = "Religion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30,hjust = 1),
        plot.title = element_text(hjust = 0.5))

identity_express_stackbar


# Religious identity x ethnicity stacked bar - Bivariate

identity_aapi_stackbar <- ggplot(df2, 
                                 aes(x = identity_hist, 
                                     fill = aapi_hist)) + 
  geom_bar(position = "stack",
           width = 0.75) +
  labs(x = "", 
       y = "Percent", 
       title  = "When it comes to religion, what do you consider yourself to be?",
       fill = "Ethnicity") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  coord_flip()

identity_aapi_stackbar

# sub for angled theme(axis.text.x = element_text(angle = 45,hjust = 1))



#### JITTERPLOTS ####

# Example code from Minnie

# plot(jitter(yourdata$xvariable, factor=2), jitter(yourdata$yvariable, factor=2),
#      main="your title",
#      col="cornflowerblue",
#      pch=20,
#      xlab = ("x label"),
#      ylab=("y label"))


# plot(jitter(df2$identity_reg, factor=2), jitter(df2$aapi_reg, factor=2),
#      main="your title",
#      col="cornflowerblue",
#      pch=20,
#      xlab = ("x label"),
#      ylab=("y label"))

# ggplot2 version

identity_aapi_jitter <- ggplot(df2, 
                               aes(x = identity_hist, 
                                   y = aapi_hist)) + 
  geom_jitter(color = "steelblue") +
  labs(x = "Religious Identity", 
       y = "Ethnicity", 
       title  = "Religious Identity by Ethnic Group") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

identity_aapi_jitter


identity_express_jitter <- ggplot(data=subset (df2, !is.na(express_hist)),
                               aes(x = identity_hist, 
                                   y = express_hist)) + 
  geom_jitter(color = "steelblue") +
  labs(x = "Religious Identity", 
       y = "Level of Religious Expression", 
       title  = "Level of Religious Expression by Religious Identity") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

identity_express_jitter


identity_linkedfate_jitter <- ggplot(data=subset (df2, !is.na(identity_hist)),
                                    aes(x = identity_hist, 
                                        y = linkedfate_hist)) + 
  geom_jitter(color = "steelblue") +
  labs(x = "Religious Identity", 
       y = "Level of Linked Fate Expression", 
       title  = "Level of Linked Fate Expression by Religious Identity") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

identity_linkedfate_jitter


express_linkedfate_jitter <- ggplot(data=subset (df2, !is.na(express_hist)),
                                    aes(x = express_hist, 
                                        y = linkedfate_hist)) + 
  geom_jitter(color = "steelblue") +
  labs(x = "Frequency of Religious Expression", 
       y = "Level of Linked Fate Expression", 
       title  = "Level of Linked Fate Expression by Frequency of Religious Expression") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

express_linkedfate_jitter



#### CORRELATION PLOT ####

# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

library(ggcorrplot)

# try to make a matrix w/numeric variables

df2_mat <- df2[c("identity_reg", "express_reg", "aapi_reg", "binned_relig_ethn", "linkedfate_reg")] %>%
  group_by(identity_reg,express_reg,aapi_reg,binned_relig_ethn,linkedfate_reg)

df2_mat <- df2[c("express_reg", "linkedfate_reg")] %>%
  group_by(express_reg,linkedfate_reg)

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

# change x and y labels? 

# What does this graph communicate that the jitterplot doesn't?



#### Basic correlations (we're going to use linear regression) ####

summary(lm(data=df2, linkedfate_yes~express_num))

# Output:
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.596940   0.015576  38.324  < 2e-16 ***
# express_num 0.015219   0.005611   2.712  0.00673 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.4824 on 2368 degrees of freedom
#  (636 observations deleted due to missingness)
# Multiple R-squared:  0.003097,	Adjusted R-squared:  0.002676 
# F-statistic: 7.356 on 1 and 2368 DF,  p-value: 0.006731


summary(lm(data=df2, linkedfate_yes~aapiethnicity))

# Output:
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       0.6468130  0.0157052  41.185  < 2e-16 ***
# aapiethnicity(02) Taiwanese       0.0311531  0.0474031   0.657  0.51110    
# aapiethnicity(03) Indian         -0.0898771  0.0271176  -3.314  0.00093 ***
# aapiethnicity(04) Korean         -0.0543959  0.0369508  -1.472  0.14109    
# aapiethnicity(05) Filipino       -0.0273008  0.0286771  -0.952  0.34117    
# aapiethnicity(06) Vietnamese     -0.0175447  0.0373912  -0.469  0.63895    
# aapiethnicity(07) Japanese       -0.0435072  0.0299486  -1.453  0.14640    
# aapiethnicity(08) Pakistani      -0.0570694  0.0793671  -0.719  0.47216    
# aapiethnicity(09) Thai           -0.1603265  0.0814021  -1.970  0.04898 *  
# aapiethnicity(10) Iranian        -0.6468130  0.4861003  -1.331  0.18342    
# aapiethnicity(11) Bangladeshi    -0.1731287  0.1125619  -1.538  0.12414    
# aapiethnicity(12) Laotian         0.0804598  0.0860209   0.935  0.34968    
# aapiethnicity(13) Cambodian      -0.1762247  0.0847892  -2.078  0.03776 *  
# aapiethnicity(14) Other: SPECIFY -0.0009796  0.0520142  -0.019  0.98497    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.4858 on 2992 degrees of freedom
# Multiple R-squared:  0.008258,	Adjusted R-squared:  0.003949 
# F-statistic: 1.917 on 13 and 2992 DF,  p-value: 0.0241


summary(lm(data=df2[df2$aapiethnicity=="(01) Chinese",], linkedfate_yes~express_num)) # This is a subsetting regression
summary(lm(data=df2[df2$aapiethnicity=="(07) Japanese",], linkedfate_yes~express_num))
summary(lm(data=df2, linkedfate_yes~identity_hist))
summary(lm(data=df2, linkedfate_yes~identity_reg*aapi_hist)) # This is an interaction effect
summary(lm(data=df2[df2$aapiethnicity=="(01) Chinese",], linkedfate_yes~identity_reg)) # This is an interaction effect
summary(lm(data=df2[df2$aapiethnicity=="(07) Japanese",], linkedfate_yes~identity_reg)) 

#### Task list: #####
## 1 - check to make sure all the independent variables are coded as you want
## 2 - recode your dependent variables as needed (no factors) and figure out 
## whether you want to create an index variable (average of the three variables)
## 3 - create your descriptive statistics and check against codebook to make sure
## no one has been dropped inadvertently, and that you're only including the right
## people (1 Iranian???)
## 4 - mapping relationships using linear regression. Decide which regressions to 
## run, with an eye to answering your first two research questions. 
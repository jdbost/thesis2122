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
df2$identity_reg <- (df2$identity_reg-1)

table(df2$identity_hist,
      df2$identity_reg)


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
       title = "Figure 5. Reported Feelings of Linked Fate",
       subtitle  = "Do you think what happens generally to Asian people in this country will have something to do with what happens in your life?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

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
       title  = "Figure 1. Religious Identity",
       subtitle = "When it comes to religion, what do you consider yourself to be?",
       fill = "Religious identity") +
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

identity_bar


# Racial composition of local religious community histogram - Univariate

binned_relig_ethn_bar <- ggplot(data=subset(df2, !is.na(binned_relig_ethn_hist)),
                                aes(x = binned_relig_ethn_hist,
                                    y = ..count.. / sum(..count..))) + 
  geom_bar(fill = 'steelblue',
           color = 'black',
           width = 0.6) +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "", 
       y = "Percent", 
       title = "Figure 4. Racial/Ethnic Composition of Respondents’ Religious Community",
       subtitle  = "Please indicate the approximate racial/ethnic composition of your \nplace of religious worship or gathering: Asian") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

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
       title = "Figure 6. Respondents' Quality of Feelings of Linked Fate",
       subtitle  = "Do you feel positively, negatively, or neither positive or negative \nabout the link you have with your racial or ethnic group members?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

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
       title = "Figure 3. Ethnicity",
       subtitle  = "What do you consider to be your primary ethnicity or family ancestry?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

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
       title = "Figure 2. Religious Expression, Frequency",
       subtitle  = "How often do you attend a religious service or gathering?") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30,hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

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
       title  = "Figure 7. Jitterplot of Religious Identity by Ethnic Group") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

identity_aapi_jitter


identity_express_jitter <- ggplot(data=subset(df2, !is.na(express_hist)),
                               aes(x = identity_hist, 
                                   y = express_hist)) + 
  geom_jitter(color = "steelblue") +
  labs(x = "Religious Identity", 
       y = "Level of Religious Expression", 
       title  = "Figure 8. Jitterplot of Religious Expression by Religious Identity") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

identity_express_jitter


identity_linkedfate_jitter <- ggplot(data=subset (df2, !is.na(identity_hist)),
                                    aes(x = identity_hist, 
                                        y = linkedfate_hist)) + 
  geom_jitter(color = "steelblue") +
  labs(x = "Religious Identity", 
       y = "Level of Linked Fate Expression", 
       title  = "Figure 9. Jitterplot of Linked Fate by Religious Identity") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

identity_linkedfate_jitter


express_linkedfate_jitter <- ggplot(data=subset (df2, !is.na(express_hist)),
                                    aes(x = express_hist, 
                                        y = linkedfate_hist)) + 
  geom_jitter(color = "steelblue") +
  labs(x = "Religious Expression", 
       y = "Level of Linked Fate Expression", 
       title  = "Figure 10. Jitterplot of Linked Fate by Religious Expression") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 30,hjust = 1),
        plot.title = element_text(hjust = 0.5))

express_linkedfate_jitter



#### CORRELATION PLOT ####

# http://www.sthda.com/english/wiki/ggcorrplot-visualization-of-a-correlation-matrix-using-ggplot2

library(ggcorrplot)

# try to make a matrix w/numeric variables

df2_mat <- df2[c("identity_reg", "express_reg", "aapi_reg", "binned_relig_ethn", "linkedfate_reg")] %>%
  group_by(identity_reg,express_reg,aapi_reg,binned_relig_ethn,linkedfate_reg)

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

# change x and y labels? 

# What does this graph communicate that the jitterplot doesn't?


#### HYPOTHESIS TESTS ####

# NOTES FROM DR.B
# summary(lm(data=df, y~x1+x2)) # basic regression
# summary(lm(data=df,y~x1*x2)) # interaction regression = summary(lm(data=df,y~x1+x2+x1*x2))
# x1+x2 == x1=1 and x2=1, intercept when x1=x2=0
# x1*x2 = x1+x2+x1*x2 == x1=1 while x2=0, x2=1 while x1=0, and both x1=x2=1, and intercept when x1=0 and x2=0
# use subsetting >> interaction for categorical variables -- calculates slope for category

# H1: Religious Asian Americans belonging to Catholic or Christian traditions 
# will likely express lower levels of panethnic linked fate than religious Asian Americans 
# belonging to a non-Christian religion, such as Buddhism or Hinduism.

summary(lm(data=df2, linkedfate_yes~identity_hist))

mod.1 <- lm(data=df2, linkedfate_yes~identity_hist)

summary(mod.1)

# H1 ANSWER: no real difference between "Christian" (Catholic, Protestant, Christian) 
## and non-Christian religions (except maybe for Hindus) 
## Significant results for Atheist/agnostic group and None group, perhaps
## suggesting some key ideological/???? difference in beliefs between the two groups maybe?
## or at the very least that it's not appropriate to group them together.

# INTERACTION: How does this vary across ethnicity?

int.1.1 <- lm(data=df2, linkedfate_yes~identity_hist*aapi_hist) # Interaction: identity DEPENDS ON ethnicity

summary(int.1.1)

summary(lm(data=df2[df2$identity_hist=="Atheist or agnostic",], linkedfate_yes~aapi_hist)) # This is a subsetting regression
summary(lm(data=df2[df2$identity_hist=="None",], linkedfate_yes~aapi_hist)) # This is a subsetting regression


# NOTE: only significance for:
# Protestant Indian
# Christian Indian
# Buddhist Indian
# None Indian

# H1 ANSWER pt. 2: When interacting identity and ethnicity, nothing much changes tbh.
# Essentially, religious identity doesn't have a strong influence on Asian Americans' expression of linked fate.

# H2a: Asian Americans with higher levels of external religious expression 
# (or the frequency measure) will be less likely to express higher levels of 
# linked fate than Asian Americans with lower levels of external religious expression.

summary(lm(data=df2, linkedfate_yes~express_num))

mod.2 <- lm(data=df2, linkedfate_yes~express_num)

summary(mod.2)

# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# Intercept = "Never"   0.596940   0.015576  38.324  < 2e-16 ***
# express_num           0.015219   0.005611   2.712  0.00673 ** 

# Residual standard error: 0.4824 on 2368 degrees of freedom
#  (636 observations deleted due to missingness)
# Multiple R-squared:  0.003097,	Adjusted R-squared:  0.002676 
# F-statistic: 7.356 on 1 and 2368 DF,  p-value: 0.006731

# H2a ANSWER: Generally, people who are "more religious" (i.e. attend more religious services),
# are more likely to express linked fate -- to answer "Yes"
## Although, notably, baseline (i.e. non-religious people) likelihood of expressing linked fate
## is pretty high -- about 60%!

# INTERACTION[?]: How does this vary by religion?

int.2.1 <- lm(data=df2, linkedfate_yes~express_num*identity_hist)

summary(int.2.1)

## The results from the atheist/agnostic group are really interesting --
## among those identifying as atheist/agnostic, the more frequently someone 

# H2a ANSWER pt.2: Overall, there doesn't seem to be a big difference across religious 
# identities, and the association of more frequent religious service attendance 
# is still significant even when controlling for identity.

int.2.2 <- lm(data=df2, linkedfate_yes~express_num*aapi_hist)

summary(int.2.2)

# H2a ANSWER pt.3: Overall, more frequent attendance at a religious service is associated with
# a higher likelihood of expressing linked fate. Neither variance in religious identity nor
# variance in ethnicity help to explain this trend/have much effect on this trend.

# CONTROLLING IVs

int.3 <- lm(data=df2, linkedfate_yes~identity_hist+express_num)

summary(int.3)

# Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       0.521124   0.028619  18.209  < 2e-16 ***
# identity_histProtestant           0.012631   0.044229   0.286  0.77522    
# identity_histChristian           -0.015106   0.029901  -0.505  0.61348    
# identity_histMuslim              -0.024515   0.061647  -0.398  0.69090    
# identity_histHindu               -0.045099   0.033927  -1.329  0.18389    
# identity_histBuddhist             0.103765   0.035434   2.928  0.00344 ** 
# identity_histAtheist or agnostic  0.151202   0.035310   4.282 1.93e-05 ***
# express_num                       0.035442   0.006668   5.315 1.16e-07 ***

# Residual standard error: 0.4791 on 2362 degrees of freedom
#  (636 observations deleted due to missingness)
# Multiple R-squared:  0.01928,	Adjusted R-squared:  0.01637 
# F-statistic: 6.632 on 7 and 2362 DF,  p-value: 8.655e-08

# Robustness checks w/linkedfate_reg (ordinal)

rob_mod.1.1 <- lm(data=df2, linkedfate_reg~identity_hist)

rob_mod.1.2 <- lm(data=df2, linkedfate_reg~identity_hist*aapi_hist)

rob_mod.2.1 <- lm(data=df2, linkedfate_reg~express_num)

rob_mod.2.2 <- lm(data=df2, linkedfate_reg~express_num*identity_hist)

summary(rob_mod.1.1)
summary(rob_mod.1.2)
summary(rob_mod.2.1)
summary(rob_mod.2.2)

# Robustness checks w/linkedfate_pos (binary)

rob2_mod.1.1 <- lm(data=df2, linkedfate_pos_num2~identity_reg)

rob2_mod.1.2 <- lm(data=df2, linkedfate_pos_num2~identity_reg*aapi_reg)

rob2_mod.2.1 <- lm(data=df2, linkedfate_pos_num2~express_num)

rob2_mod.2.2 <- lm(data=df2, linkedfate_pos_num2~express_num*identity_reg)

summary(rob2_mod.1.1)
summary(rob2_mod.1.2)
summary(rob2_mod.2.1)
summary(rob2_mod.2.2)

# Misc code (simply cannot murder my darlings)

summary(lm(data=df2, linkedfate_yes~aapiethnicity))
summary(lm(data=df2, linkedfate_yes~identity_hist*aapi_hist)) # This is an interaction effect
summary(lm(data=df2[df2$aapiethnicity=="(01) Chinese",], linkedfate_yes~express_num)) # This is a subsetting regression
summary(lm(data=df2[df2$aapiethnicity=="(07) Japanese",], linkedfate_yes~express_num))
summary(lm(data=df2, linkedfate_yes~identity_hist))
summary(lm(data=df2, linkedfate_yes~identity_hist*aapi_hist)) # This is an interaction effect
summary(lm(data=df2[df2$aapiethnicity=="(01) Chinese",], linkedfate_yes~identity_hist)) # This is an interaction effect
summary(lm(data=df2[df2$aapiethnicity=="(07) Japanese",], linkedfate_yes~identity_hist)) 

summary(lm(data=df2[df2$identity_hist=="Protestant",], linkedfate_yes~aapiethnicity)) # This is a subsetting regression



#### REGRESSION TABLES - STARGAZER ####

library(stargazer)

# Example code from Queen Minnie - MODEL 1 AND MODEL 2 AND CONTROLS

stargazer(mod.1,mod.2,int.3,
          type = "text",
          title = "Table 1: Regression Results",
          align = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),  # or you can do the default levels
          notes = c("*p<0.05; **p<0.01; ***p<0.001"), # if you decide to change p levels
          notes.append = FALSE,
          dep.var.labels = c("Linked Fate"),
          covariate.labels = c("Protestant", "Christian", "Muslim", "Hindu", "Buddhist", "Atheist or agnostic", "Other", "None",
                               "Religious Expression"),    # put in order that you list the ind. levels
          keep.stat = c("n","rsq"),
          out = "regresults.html")   # saves as html; I converted this to PDF          

# MODEL 1 INTERACTIONS

stargazer(int.1.1,
          type = "text",
          title = "Appendix A: Religious Identity and Ethnicity Model",
          align = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),  
          notes = c("*p<0.05; **p<0.01; ***p<0.001"), # if you decide to change p levels
          notes.append = FALSE,
          dep.var.labels = c("Linked Fate"),
          covariate.labels = c("Protestant", "Christian", "Muslim", "Hindu", "Buddhist", "Atheist or agnostic", "Other", "None",
                               "Indian", "Korean", "Filipino", "Vietnamese","Japanese","Other",
                               "Protestant Indian", "Christian Indian", "Muslim Indian", "Hindu Indian", "Buddhist Indian", "Atheist or agnostic Indian", "Other Indian", "None Indian",
                               "Protestant Korean", "Christian Korean", "Muslim Korean", "Hindu Korean", "Buddhist Korean", "Atheist or agnostic Korean", "Other Korean", "None Korean",
                               "Protestant Filipino", "Christian Filipino", "Muslim Filipino", "Hindu Filipino", "Buddhist Filipino", "Atheist or agnostic Filipino", "Other Filipino", "None Filipino",
                               "Protestant Vietnamese", "Christian Vietnamese", "Muslim Vietnamese", "Hindu Vietnamese", "Buddhist Vietnamese", "Atheist or agnostic Vietnamese", "Other Vietnamese", "None Vietnamese",
                               "Protestant Japanese", "Christian Japanese", "Muslim Japanese", "Hindu Japanese", "Buddhist Japanese", "Atheist or agnostic Japanese", "Other Japanese", "None Japanese",
                               "Protestant Other", "Christian Other", "Muslim Other", "Hindu Other", "Buddhist Other", "Atheist or agnostic Other", "Other Other", "None Other"),
          keep.stat = c("n","rsq"),
          out = "mod1int.html")   # saves as html; I converted this to PDF  

# MODEL 2 INTERACTIONS

stargazer(int.2.1,int.2.2,
          type = "text",
          title = "Appendix B: Religious Expression Interaction Effects",
          align = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),  
          notes = c("*p<0.05; **p<0.01; ***p<0.001"), # if you decide to change p levels
          notes.append = FALSE,
          dep.var.labels = c("Linked Fate"),
          covariate.labels = c("Religious Expression",
                               "Protestant", "Christian", "Muslim", "Hindu", "Buddhist", "Atheist or agnostic",
                               "Protestant Expression","Christian Expression","Muslim Expression","Hindu Expression","Buddhist Expression","Atheist or agnostic Expression",
                               "Indian", "Korean", "Filipino", "Vietnamese","Japanese","Other",
                               "Indian Expression", "Korean Expression", "Filipino Expression", "Vietnamese Expression","Japanese Expression","Other Expression"),
          keep.stat = c("n","rsq"),
          out = "mod2int.html")   # saves as html; I converted this to PDF          

# IV CONTROLS

stargazer(int.3,
          type = "text",
          title = "Table 1: Regression Results",
          align = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),  # or you can do the default levels
          notes = c("*p<0.05; **p<0.01; ***p<0.001"), # if you decide to change p levels
          notes.append = FALSE,
          dep.var.labels = c("Linked Fate"),
          covariate.labels = c("Protestant", "Christian", "Muslim", "Hindu", "Buddhist", "Atheist or agnostic",
                               "Religious Expression"),    # put in order that you list the ind. levels
          keep.stat = c("n","rsq"),
          out = "mod1mod2con.html")   # saves as html; I converted this to PDF          

# MODEL 1 ROBUSTNESS CHECKS

stargazer(rob_mod.1.1,rob_mod.1.2,
          type = "text",
          title = "Appendix C: Model 1 Robustness Check",
          align = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),  
          notes = c("*p<0.05; **p<0.01; ***p<0.001"), # if you decide to change p levels
          notes.append = FALSE,
          dep.var.labels = c("Linked Fate Scale"),
          covariate.labels = c("Protestant", "Christian", "Muslim", "Hindu", "Buddhist", "Atheist or agnostic", "Other", "None",
                               "Indian", "Korean", "Filipino", "Vietnamese","Japanese","Other",
                               "Protestant Indian", "Christian Indian", "Muslim Indian", "Hindu Indian", "Buddhist Indian", "Atheist or agnostic Indian", "Other Indian", "None Indian",
                               "Protestant Korean", "Christian Korean", "Muslim Korean", "Hindu Korean", "Buddhist Korean", "Atheist or agnostic Korean", "Other Korean", "None Korean",
                               "Protestant Filipino", "Christian Filipino", "Muslim Filipino", "Hindu Filipino", "Buddhist Filipino", "Atheist or agnostic Filipino", "Other Filipino", "None Filipino",
                               "Protestant Vietnamese", "Christian Vietnamese", "Muslim Vietnamese", "Hindu Vietnamese", "Buddhist Vietnamese", "Atheist or agnostic Vietnamese", "Other Vietnamese", "None Vietnamese",
                               "Protestant Japanese", "Christian Japanese", "Muslim Japanese", "Hindu Japanese", "Buddhist Japanese", "Atheist or agnostic Japanese", "Other Japanese", "None Japanese",
                               "Protestant Other", "Christian Other", "Muslim Other", "Hindu Other", "Buddhist Other", "Atheist or agnostic Other", "Other Other", "None Other"),
          keep.stat = c("n","rsq"),
          out = "mod1rob.html")   # saves as html; I converted this to PDF  

# MODEL 2 ROBUSTNESS CHECK

stargazer(rob_mod.1.1,rob_mod.1.2,
          type = "text",
          title = "Appendix D: Model 2 Robustness Check",
          align = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),  
          notes = c("*p<0.05; **p<0.01; ***p<0.001"), # if you decide to change p levels
          notes.append = FALSE,
          dep.var.labels = c("Linked Fate Scale"),
          covariate.labels = c("Protestant", "Christian", "Muslim", "Hindu", "Buddhist", "Atheist or agnostic", "Other", "None",
                               "Indian", "Korean", "Filipino", "Vietnamese","Japanese","Other",
                               "Protestant Indian", "Christian Indian", "Muslim Indian", "Hindu Indian", "Buddhist Indian", "Atheist or agnostic Indian", "Other Indian", "None Indian",
                               "Protestant Korean", "Christian Korean", "Muslim Korean", "Hindu Korean", "Buddhist Korean", "Atheist or agnostic Korean", "Other Korean", "None Korean",
                               "Protestant Filipino", "Christian Filipino", "Muslim Filipino", "Hindu Filipino", "Buddhist Filipino", "Atheist or agnostic Filipino", "Other Filipino", "None Filipino",
                               "Protestant Vietnamese", "Christian Vietnamese", "Muslim Vietnamese", "Hindu Vietnamese", "Buddhist Vietnamese", "Atheist or agnostic Vietnamese", "Other Vietnamese", "None Vietnamese",
                               "Protestant Japanese", "Christian Japanese", "Muslim Japanese", "Hindu Japanese", "Buddhist Japanese", "Atheist or agnostic Japanese", "Other Japanese", "None Japanese",
                               "Protestant Other", "Christian Other", "Muslim Other", "Hindu Other", "Buddhist Other", "Atheist or agnostic Other", "Other Other", "None Other"),
          keep.stat = c("n","rsq"),
          out = "mod2rob.html")   # saves as html; I converted this to PDF          

#### REGRESSION PLOT ####

plot_model(mod.1, type = "pred", terms = c("no"), 
           ci.lvl = .95,
           title="Figure 1b: Affect towards linked fate does not substantially \n differ by country/region-of-origin",
           axis.title=c("Country/region-of-origin", "Asian linked fate "),
           colors=c("black"),
           legend.title="Status") +  geom_line() +
  ylim(2,3) 



#### Task list: #####
## 1 - check to make sure all the independent variables are coded as you want
## 2 - recode your dependent variables as needed (no factors) and figure out 
## whether you want to create an index variable (average of the three variables)
## 3 - create your descriptive statistics and check against codebook to make sure
## no one has been dropped inadvertently, and that you're only including the right
## people (1 Iranian???)
## 4 - mapping relationships using linear regression. Decide which regressions to 
## run, with an eye to answering your first two research questions. 
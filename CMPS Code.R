#### ON START UP ####

# clear environment

rm(list = ls())

# set working directory to project directory

setwd("C:/Users/avata/OneDrive/Desktop/R/thesis2122")

# load libraries

library(tidyverse)

# load data

load("C:/Users/avata/OneDrive/Desktop/POL 194H/CMPS/ICPSR_38040/DS0001/CMPS2016-Data.rda")

# store data in a nicer frame!!

df <- da38040.0001




#### DATA CLEANING - INDEPENDENT VARIABLES ####

df$identity <- df$C129 # C129 "When it comes to religion, what do you consider yourself to be?" 
df$express <- df$C131 # C131 "Do you attend religious service or gathering: at least every week, almost every week, a few times a month, only a few times during the year, hardly ever or never?" 
df$aapiethnicity <- df$S8 # S8 "Asian Americans come from a diversity of backgrounds. What do you consider to be your primary ethnicity or family ancestry:" 
df$relig_ethn <- df$C133_4 # C133_4 "Please indicate the approximate racial/ethnic composition of your place of religious worship or gathering: Percent Asian"

# Linked fate battery - DEPENDENT VARIABLE

df$linkedfate <- df$C150 # C150 "Do you think what happens generally to [ANS to S2] people in this country will have something to do with what happens in your life?" 
df$linkedfate_howmuch <- df$C151 # C151 "Will it [ANS C150] affect you . a lot, some, not very much?"
df$linkedfate_positive <- df$C152 # C152 "Some people feel positively about the link they have with their racial or ethnic group members, while others feel negatively about the idea that their lives may be influenced by how well the larger group is doing. Which comes closer to your feelings?"

# Just Asian Americans (3006 observations)

df2 <- df[!is.na(df$aapiethnicity),]

table(df2$identity)            # factor
table(df2$express)             # factor
table(df2$aapiethnicity)       # factor
table(df2$relig_ethn)          # continuous, NA's included
table(df2$linkedfate)          # factor
table(df2$linkedfate_howmuch)  # factor
table(df2$linkedfate_positive) # factor



## RECODE SO NO FACTOR VARIABLES ##

df2$identity_num <- as.numeric(df2$identity)

table(df2$identity,          # table() function checks that new coding is correct!
      df2$identity_num)

df2$express_num <- as.numeric(df2$express)
df2$express_num <- (df2$express_num-6)*(-1)

table(df2$express,
      df2$express_num)

df2$aapiethnicity_num <- as.numeric(df2$aapiethnicity)

table(df2$aapiethnicity,
      df2$aapiethnicity_num)

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




#### DESCRIPTIVE STATISTICS ####

# These include things like mean, median, mode, quartiles, min, and max
# For categorical variables, write general summary, including histogram, 
# talk about distribution, and anything else?

# df2$identity

table(df2$identity)
summary(df2$identity_num)
table(df2$identity_num)

df2$identity_reg <- droplevels.factor(df2$identity, exclude = "(8) Other:(SPECIFY)") %>%
                    as.numeric()

table(df2$identity_reg,
      df2$identity_num)


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


# df2$express: makes sure analysis uses express_num;
# # might want to recode the variable in its factor form too

summary(df2$express)
summary(df2$express_num)

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


# df2$aapiethnicity: exclude the 1 Iranian (sorry), think about narrowing discussion >100/Top 7 groups
# # which means creating "Other" category

summary(df2$aapiethnicity)
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


# df2$relig_ethn: use binned_relig_ethn for all discussion and analysis

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

# Spencer's example - Proportion/percent table

# # table <- table(df2$linkedfate_reg)
# # prop.table(table)*100

# Breakdown by identity and express

# # Spencer's example - Proportion/percent table

# # Also from Dr. B - https://www.statmethods.net/stats/frequencies.html

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

prop.table(table(df2$identity_hist,df2$express_hist),1)*100


# Breakdown by national origin/ethnicity and religion

prop.table(table(df2$identity_hist,df2$aapi_hist),1)*100


# Breakdown by identity and ethnic makeup of place of worship
breakdown_table_relgn_ethwor <- df2[c("identity", "relig_ethn")] %>%
  group_by(identity,relig_ethn) %>%
  summarize(Freq=n())





#### HISTOGRAMS/BARPLOTS ####

library(ggplot2)

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

binned_relig_ethn_bar <- ggplot(data=subset (df2, !is.na(binned_relig_ethn_hist)),
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

plot(jitter(yourdata$xvariable, factor=2), jitter(yourdata$yvariable, factor=2),
     main="your title",
     col="cornflowerblue",
     pch=20,
     xlab = ("x label"),
     ylab=("y label"))


plot(jitter(df2$identity_reg, factor=2), jitter(df2$aapi_reg, factor=2),
     main="your title",
     col="cornflowerblue",
     pch=20,
     xlab = ("x label"),
     ylab=("y label"))

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



#### Basic correlations (we're going to use linear regression) ####

summary(lm(data=df2, linkedfate_yes~express_num))
summary(lm(data=df2[df2$aapiethnicity=="(01) Chinese",], linkedfate_yes~express_num)) # This is a subsetting regression
summary(lm(data=df2[df2$aapiethnicity=="(07) Japanese",], linkedfate_yes~express_num))
summary(lm(data=df2, linkedfate_yes~identity))
summary(lm(data=df2, linkedfate_yes~religion*aapiethnicity)) # This is an interaction effect
summary(lm(data=df2[df2$aapiethnicity=="(01) Chinese",], linkedfate_yes~religion)) # This is an interaction effect
summary(lm(data=df2[df2$aapiethnicity=="(07) Japanese",], linkedfate_yes~religion)) 

#### Task list: #####
## 1 - check to make sure all the independent variables are coded as you want
## 2 - recode your dependent variables as needed (no factors) and figure out 
## whether you want to create an index variable (average of the three variables)
## 3 - create your descriptive statistics and check against codebook to make sure
## no one has been dropped inadvertently, and that you're only including the right
## people (1 Iranian???)
## 4 - mapping relationships using linear regression. Decide which regressions to 
## run, with an eye to answering your first two research questions. 


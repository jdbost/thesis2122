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

df$identity <- df$Q10_21 # Q10_21 "What is your presnt religion, if any?" 
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

# # 01/26/2022: FIGURE OUT HOW TO WINNOW DOWN CATEGORIES - IDENTITY AND AAPIETHNICITY

df2$identity_num <- as.numeric(df2$identity) 

table(df2$identity_num)         # table() function checks that new coding is correct!
table(df2$identity,          
      df2$identity_num)


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


df2$aapiethnicity_num <- as.numeric(df2$aapiethnicity)

table(df2$aapiethnicity_num)
table(df2$aapiethnicity,
      df2$aapiethnicity_num)



df2$linkedfate_race_yes <- ifelse(df2$linkedfate_race == "(1) Yes", 1, 0)

table(df2$linkedfate_race_yes)
table(df2$linkedfate_race,
      df2$linkedfate_race_yes)


df2$linkedfate_race_how_num <- as.numeric(df2$linkedfate_race_howmuch) 

table(df2$linkedfate_race_how_num)
table(df2$linkedfate_race_howmuch,
      df2$linkedfate_race_how_num)


df2$linkedfate_ethn_yes <- ifelse(df2$linkedfate_ethn == "(1) Yes", 1, 0)

table(linkedfate_ethn_yes)
table(df2$linkedfate_ethn,
      df2$linkedfate_ethn_yes)


df2$linkedfate_ethn_how_num <- as.numeric(df2$linkedfate_ethn_howmuch) 

table(df2$linkedfate_ethn_how_num)
table(df2$linkedfate_ethn_howmuch,
      df2$linkedfate_ethn_how_num)


df2$linkedfate_share_num <- ifelse(df2$linkedfate_share == "(1) Yes", 1, 0)

table(df2$linkedfate_share_num)
table(df2$linkedfate_share_num,
      df2$linkedfate_share)




##### BREAKDOWN TABLES ####

# Use to figure out interesting jitters

# # prop.table(table(df$var1, df$var2),1/2)*100




#### HISTORGRAMS AND BARCHARTS ####





#### JITTER PLOTS ####




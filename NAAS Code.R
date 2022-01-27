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

df2 <- df[!is.na(df$INSRACE == "(1) Asian American"),]

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



## RECODE SO NO FACTOR VARIABLES ##

df2$identity_num <- as.numeric(df2$identity)

table(df2$identity,          # table() function checks that new coding is correct!
      df2$identity_num)
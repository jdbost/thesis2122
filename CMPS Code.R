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

#### Data cleaning - INDEPENDENT VARIABLES ####

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

# View() function to check recoding for df2 variables

View(df2$identity)            # factor
View(df2$express)             # factor
View(df2$aapiethnicity)       # factor
View(df2$relig_ethn)          # continuous, NA's included
View(df2$linkedfate)          # factor
View(df2$linkedfate_howmuch)  # factor
View(df2$linkedfate_positive) # factor

# If all data matches, proceed!


#### RECODE SO NO FACTOR VARIABLES #####

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

# Spencer's Code

df2$linkedfate_pos_num2[df2$linkedfate_pos_num == 2] <- -1
df2$linkedfate_pos_num2[df2$linkedfate_pos_num == 1] <- 1
df2$linkedfate_pos_num2[df2$linkedfate_pos_num == 3] <- 0

table(df2$linkedfate_pos_num)
table(df2$linkedfate_pos_num2)

df2$binned_relig_ethn <- ifelse(df2$C133_4<=25,1,
                              ifelse(df2$C133_4>25&df2$C133_4<=50,2,
                                   ifelse(df2$C133_4>50&df2$C133_4<=75,3,
                                        ifelse(df2$C133_4>75&df2$C133_4<=100,4,NA))))

df2$binned_relig_ethn
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

# df2$identity: figure out what "Other" is (likely omit)
# # 01/09/2022: omit "Other" category -- only 72 -- in analysis only (identity_num)?

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

table(df2$identity_hist)

# Spencer's example

identity_num_pct <- df2 %>% 
    group_by(identity_num) %>% 
    tally() %>% 
    mutate(pct = n/sum(n))

identity_num_pct <- identity_num_pct * 100

table(identity_num_pct)

# Breakdown by identity and express

breakdown_table_identity_express <- df2[c("identity", "express")] %>%
  group_by(identity,express) %>%
  summarize(Freq=n())

# Breakdown by identity and express

breakdown_table_identity_express <- df2[c("identity_reg", "express_num")] %>%
  group_by(identity_reg,express_num) %>%
  summarize(Freq=n())

breakdown_table_identity_express_pct <- breakdown_table_identity_express / length(breakdown_table_identity_express)

View(breakdown_table_identity_express)

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

table(df2$express_hist)


# df2$aapiethnicity: exclude the 1 Iranian (sorry), think about narrowing discussion >100/Top 7 groups
# # which means creating "Other" category

summary(df2$aapiethnicity)
table(df2$aapiethnicity)

# # 01/11/2022: try collapsing small groups using fct_collapse() https://stackoverflow.com/questions/36568758/combining-factor-level-in-r

df2$aapi <- fct_collapse(df2$aapiethnicity, Other = c("(08) Pakistani",
                                                      "(09) Thai",
                                                      "(10) Iranian",
                                                      "(11) Bangladeshi",
                                                      "(12) Laotian",
                                                      "(13) Cambodian")) %>%
              droplevels.factor(df2$aapiethnicity, exclude = "(14) Other:(SPECIFY)") %>%
                as.numeric()

summary(df2$aapi)
table(df2$aapi,
      df2$aapiethnicity)

# # 01/12/2022: success I think! depends on how the histogram turns out?

# # 01/14/2022: consider using Spencer's method to manually reassign values?

# df2$relig_ethn: use binned_relig_ethn for all discussion and analysis

summary(df2$relig_ethn)
summary(df2$binned_relig_ethn)

# # 01/12/2022: this also looks good!

# linked fate variables: present df2$linkedfate_yes + df2$linkedfate_how_num

# # Spencer's Code

# for regression

df2$linkedfate_reg[df2$linkedfate == "(2) No"] <- 0
df2$linkedfate_reg[df2$linkedfate_howmuch == "(3) Not very much"] <- 1
df2$linkedfate_reg[df2$linkedfate_howmuch == "(2) Some"] <- 2
df2$linkedfate_reg[df2$linkedfate_howmuch == "(1) A lot"] <- 3

table(df2$linkedfate_reg)
class(df2$linkedfate_reg)

# # Spencer's Code - edited

# for hist

table(df2$linkedfate_howmuch)
table(df2$linkedfate)

df2$linkedfate_hist[df2$linkedfate == "(2) No"] <- "No"
df2$linkedfate_hist[df2$linkedfate_howmuch == "(3) Not very much"] <- "Not very much"
df2$linkedfate_hist[df2$linkedfate_howmuch == "(2) Some"] <- "Some"
df2$linkedfate_hist[df2$linkedfate_howmuch == "(1) A lot"] <- "A lot"

table(df2$linkedfate_hist)
class(df2$linkedfate_hist)


summary(df2$linkedfate_yes)
summary(df2$linkedfate_how_num)
View(df2$linkedfate_pos_num2)


#### BREAKDOWN TABLES ####

# Breakdown by national origin/ethnicity and religion
breakdown_table_ethn_identity <- df2[c("aapiethnicity", "identity")] %>%
  group_by(aapiethnicity,identity) %>%
  summarize(Freq=n())

View(breakdown_table_ethn_identity)

# Breakdown by identity and express
breakdown_table_identity_express <- df2[c("identity", "express_num")] %>%
  group_by(identity,express_num) %>%
  summarize(Freq=n())

View(breakdown_table_identity_express)

# Breakdown by identity and ethnic makeup of place of worship
breakdown_table_relgn_ethwor <- df2[c("identity", "relig_ethn")] %>%
  group_by(identity,relig_ethn) %>%
  summarize(Freq=n())

#### HISTOGRAMS/BARPLOTS ####

library(ggplot2)

# Example code from https://rkabacoff.github.io/datavis/Univariate.html#categorical

linkedfate_hist_bar <- ggplot(df2, 
                       aes(x = linkedfate_hist, 
                       y = ..count.. / sum(..count..))) + 
              geom_bar() +
              labs(x = "", 
                   y = "Percent", 
                   title  = "Do you think what happens generally to Asian people in this country will have something to do with \nwhat happens in your life?") +
              scale_y_continuous(labels = scales::percent)

# # Example code https://www.delftstack.com/howto/r/ggplot-axis-tick-labels-in-r/

linkedfate_hist_bar <- linkedfate_hist_bar + 
  scale_x_discrete(limits = c("No","Not Very Much","Some","A lot"))

linkedfate_hist_bar


binned_relig_ethn_bar <- ggplot(df2,aes(x = df2$binned_relig_ethn_hist,y = ..count.. / sum(..count..))) + 
  geom_bar() +
  labs(x = "", 
       y = "Percent", 
       title  = "What is the approximate racial/ethnic composition of your place of religious worship or gathering?")

binned_relig_ethn_bar


# xlabel reordering w/limits() http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/#changing-the-order-of-items

identity_bar <- ggplot(df2, 
                aes(x = identity_hist, 
                y = ..count.. / sum(..count..))) + 
        geom_bar(fill = 'steelblue',
                 color = 'black') +
        labs(x = "", 
             y = "Percent", 
             title  = "When it comes to religion, what do you consider yourself to be?") +
        scale_y_continuous(labels = scales::percent) +
        scale_x_discrete(limits = c("Catholic",
                                    "Protestant",
                                    "Christian",
                                    "Muslim",
                                    "Hindu",
                                    "Buddhist",
                                    "Atheist or agnostic",
                                    "Other",
                                    "None"))

# axis label rotation https://rkabacoff.github.io/datavis/Univariate.html#categorical
# # axis label rotation troubleshoot!!!!!!!!

identity_bar + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                theme_minimal()

identity_bar


# Example code https://rkabacoff.github.io/datavis/Bivariate.html#Categorical-Categorical

identity_express_stackbar <- ggplot(df2, 
                                    aes(x = express_hist, 
                                    fill = identity_hist)) + 
                geom_bar(position = "stack")

identity_express_stackbar <- identity_express_stackbar +
                labs(x = "", 
                     y = "Percent", 
                     title  = "How often do you attend a religious service or gathering") +
                scale_y_continuous(labels = scales::percent) +
                theme_minimal() +
                coord_flip()
                                            
identity_express_stackbar + scale_x_discrete(limits = c("Never",
                                                        "Hardly ever",
                                                        "Only a few times during the year",
                                                        "A few times a month",
                                                        "Almost every week",
                                                        "At least every week"))

# Example code http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/#reversing-the-direction-of-an-axis

bp + scale_x_discrete(limits = rev(levels(PlantGrowth$group)))

identity_express_stackbar + scale_x_discrete(limits = rev(levels(df2$express_hist)))

                                             
identity_express_stackbar


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


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


identity_express_jitter <- ggplot(data=subset(df2, !is.na(express_hist)),
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



#### Basic correlations (we're going to use linear regression) ####

# H1: Religious Asian Americans belonging to Catholic or Christian traditions 
# will likely express lower levels of panethnic linked fate than religious Asian Americans 
# belonging to a non-Christian religion, such as Buddhism or Hinduism.

summary(lm(data=df2, linkedfate_yes~identity))

# Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)    
# Intercept = Catholics            0.619835   0.022047  28.115   <2e-16 ***
# identity(2) Protestant           0.018875   0.044764   0.422   0.6733    
# identity(3) Christian           -0.007813   0.030242  -0.258   0.7962    
# identity(4) Muslim              -0.025632   0.062414  -0.411   0.6813    
# identity(5) Hindu               -0.061695   0.034204  -1.804   0.0714 .  
# identity(6) Buddhist             0.053881   0.034595   1.558   0.1195    
# identity(7) Atheist or agnostic  0.067380   0.031987   2.106   0.0352 *  
# identity(8) Other:(SPECIFY)      0.005165   0.061265   0.084   0.9328    
# identity(9) None                -0.070189   0.030053  -2.336   0.0196 *  

# Residual standard error: 0.485 on 2997 degrees of freedom
# Multiple R-squared:  0.00995,	Adjusted R-squared:  0.007307 
# F-statistic: 3.765 on 8 and 2997 DF,  p-value: 0.0002114

# How does this vary across ethnicity?

summary(lm(data=df2, linkedfate_yes~identity*aapi_hist)) # Interaction: identity DEPENDS ON ethnicity

# Coefficients: (5 not defined because of singularities)
#                                                       Estimate Std. Error t value Pr(>|t|)    
# Intercept = Catholics + "Never"                      0.5975610  0.0535216  11.165  < 2e-16 ***
# identity(2) Protestant                               0.1421651  0.0779890   1.823  0.06842 .  
# identity(3) Christian                                0.0346229  0.0649194   0.533  0.59385    
# identity(4) Muslim                                   0.1524390  0.2481695   0.614  0.53910    
# identity(5) Hindu                                   -0.2642276  0.2848905  -0.927  0.35376    
# identity(6) Buddhist                                 0.0985175  0.0718850   1.370  0.17064    
# identity(7) Atheist or agnostic                      0.0987007  0.0629460   1.568  0.11698    
# identity(8) Other:(SPECIFY)                          0.0024390  0.1623391   0.015  0.98801    
# identity(9) None                                    -0.0009508  0.0605047  -0.016  0.98746    
# aapi_histIndian                                      0.1224390  0.1107264   1.106  0.26891    
# aapi_histKorean                                     -0.1808943  0.1124803  -1.608  0.10789    
# aapi_histFilipino                                    0.0432554  0.0618330   0.700  0.48426    
# aapi_histVietnamese                                  0.0539542  0.0801471   0.673  0.50088    
# aapi_histJapanese                                   -0.0186136  0.1233994  -0.151  0.88011    
# aapi_histOther                                      -0.0758218  0.1143562  -0.663  0.50736    
# identity(2) Protestant:aapi_histIndian              -0.5764508  0.2214370  -2.603  0.00928 ** 
# identity(3) Christian:aapi_histIndian               -0.2731415  0.1493656  -1.829  0.06755 .  
# identity(4) Muslim:aapi_histIndian                  -0.2613279  0.2898854  -0.901  0.36740    
# identity(5) Hindu:aapi_histIndian                    0.1156562  0.3021388   0.383  0.70190    
# identity(6) Buddhist:aapi_histIndian                -0.5685175  0.2707152  -2.100  0.03581 *  
# identity(7) Atheist or agnostic:aapi_histIndian     -0.2075896  0.1410064  -1.472  0.14107    
# identity(8) Other:(SPECIFY):aapi_histIndian         -0.2224390  0.2209059  -1.007  0.31405    
# identity(9) None:aapi_histIndian                    -0.3728953  0.1486303  -2.509  0.01216 *  
# identity(2) Protestant:aapi_histKorean              -0.0388317  0.1589507  -0.244  0.80702    
# identity(3) Christian:aapi_histKorean                0.1860730  0.1287751   1.445  0.14858    
# identity(4) Muslim:aapi_histKorean                          NA         NA      NA       NA    
# identity(5) Hindu:aapi_histKorean                    0.3475610  0.4565051   0.761  0.44651    
# identity(6) Buddhist:aapi_histKorean                -0.0151841  0.3638705  -0.042  0.96672    
# identity(7) Atheist or agnostic:aapi_histKorean      0.1816023  0.1444557   1.257  0.20880    
# identity(8) Other:(SPECIFY):aapi_histKorean          0.3308943  0.2559355   1.293  0.19615    
# identity(9) None:aapi_histKorean                     0.0842841  0.1499415   0.562  0.57408    
# identity(2) Protestant:aapi_histFilipino            -0.2375268  0.1685083  -1.410  0.15877    
# identity(3) Christian:aapi_histFilipino             -0.0631944  0.0870067  -0.726  0.46770    
# identity(4) Muslim:aapi_histFilipino                        NA         NA      NA       NA    
# identity(5) Hindu:aapi_histFilipino                  0.6234113  0.5630413   1.107  0.26829    
# identity(7) Atheist or agnostic:aapi_histFilipino   -0.1395170  0.1196525  -1.166  0.24370    
# identity(9) None:aapi_histFilipino                  -0.1853201  0.1236793  -1.498  0.13414    
# identity(2) Protestant:aapi_histVietnamese           0.2063198  0.3564945   0.579  0.56280    
# identity(3) Christian:aapi_histVietnamese           -0.2155498  0.1469382  -1.467  0.14250    
# identity(4) Muslim:aapi_histVietnamese                      NA         NA      NA       NA    
# identity(5) Hindu:aapi_histVietnamese                       NA         NA      NA       NA    
# identity(6) Buddhist:aapi_histVietnamese            -0.0519194  0.1147101  -0.453  0.65086    
# identity(7) Atheist or agnostic:aapi_histVietnamese -0.0579082  0.1286685  -0.450  0.65270    
# identity(8) Other:(SPECIFY):aapi_histVietnamese     -0.2539542  0.2772937  -0.916  0.35983    
# identity(9) None:aapi_histVietnamese                -0.1227866  0.1172376  -1.047  0.29503    
# identity(2) Protestant:aapi_histJapanese            -0.1611124  0.1668559  -0.966  0.33433    
# identity(3) Christian:aapi_histJapanese             -0.0282044  0.1394344  -0.202  0.83971    
# identity(4) Muslim:aapi_histJapanese                 0.2686136  0.5557383   0.483  0.62889    
# identity(5) Hindu:aapi_histJapanese                         NA         NA      NA       NA    
# identity(6) Buddhist:aapi_histJapanese              -0.0186413  0.1424562  -0.131  0.89590    
# identity(7) Atheist or agnostic:aapi_histJapanese   -0.0055169  0.1420414  -0.039  0.96902    
# identity(8) Other:(SPECIFY):aapi_histJapanese        0.2519469  0.2414360   1.044  0.29679    
# identity(9) None:aapi_histJapanese                  -0.0908171  0.1379678  -0.658  0.51043    
# identity(2) Protestant:aapi_histOther                0.0027625  0.1893927   0.015  0.98836    
# identity(3) Christian:aapi_histOther                 0.0936379  0.1354334   0.691  0.48937    
# identity(4) Muslim:aapi_histOther                   -0.1089608  0.2773216  -0.393  0.69442    
# identity(5) Hindu:aapi_histOther                     0.0549885  0.3256628   0.169  0.86593    
# identity(6) Buddhist:aapi_histOther                  0.0583148  0.1348206   0.433  0.66538    
# identity(7) Atheist or agnostic:aapi_histOther       0.1470020  0.1401344   1.049  0.29426    
# identity(8) Other:(SPECIFY):aapi_histOther           0.0591552  0.2369415   0.250  0.80287    
# identity(9) None:aapi_histOther                      0.0347672  0.1295127   0.268  0.78837    

# Residual standard error: 0.4847 on 2948 degrees of freedom
# Multiple R-squared:  0.02761,	Adjusted R-squared:  0.008814 
# F-statistic: 1.469 on 57 and 2948 DF,  p-value: 0.01308

summary(lm(data=df2, linkedfate_yes~identity+aapi_hist)) # Control: look at variation of identity as ethnicity stays the same

# Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)    
# Intercept = Catholics + Chinese  0.651054   0.030282  21.500   <2e-16 ***
# identity(2) Protestant           0.015605   0.047210   0.331   0.7410    
# identity(3) Christian           -0.006078   0.032399  -0.188   0.8512    
# identity(4) Muslim              -0.007153   0.067014  -0.107   0.9150    
# identity(5) Hindu               -0.003552   0.049552  -0.072   0.9429    
# identity(6) Buddhist             0.052285   0.038444   1.360   0.1739    
# identity(7) Atheist or agnostic  0.062981   0.035140   1.792   0.0732 .  
# identity(8) Other:(SPECIFY)      0.024071   0.063036   0.382   0.7026    
# identity(9) None                -0.078485   0.033740  -2.326   0.0201 *  
# aapi_histIndian                 -0.093170   0.040311  -2.311   0.0209 *  
# aapi_histKorean                 -0.059418   0.037701  -1.576   0.1151    
# aapi_histFilipino               -0.030667   0.032622  -0.940   0.3473    
# aapi_histVietnamese             -0.029744   0.038360  -0.775   0.4382    
# aapi_histJapanese               -0.054188   0.030237  -1.792   0.0732 .  
# aapi_histOther                  -0.036912   0.030910  -1.194   0.2325    

# Residual standard error: 0.4849 on 2991 degrees of freedom
# Multiple R-squared:  0.01261,	Adjusted R-squared:  0.007989 
# F-statistic: 2.729 on 14 and 2991 DF,  p-value: 0.0005132


# H2a: Asian Americans with higher levels of external religious expression 
# (or the frequency measure) will be less likely to express higher levels of 
# linked fate than Asian Americans with lower levels of external religious expression.

summary(lm(data=df2, linkedfate_yes~express_num))

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.596940   0.015576  38.324  < 2e-16 ***
# Intercept = "Never"
# express_num 0.015219   0.005611   2.712  0.00673 ** 

# Residual standard error: 0.4824 on 2368 degrees of freedom
#  (636 observations deleted due to missingness)
# Multiple R-squared:  0.003097,	Adjusted R-squared:  0.002676 
# F-statistic: 7.356 on 1 and 2368 DF,  p-value: 0.006731

## Generally, people who are "more religious" (i.e. attend more religious services),
## are more likely to express linked fate -- to answer "Yes"
### Although, notably, baseline (i.e. non-religious people) likelihood of expressing linked fate is pretty high
### -- about 60%!

# How does this vary by religion?

summary(lm(data=df2, linkedfate_yes~express_num*identity))

# Coefficients:
#                                               Estimate Std. Error t value Pr(>|t|)
# Intercept = Catholics + "Never"               0.536530   0.041527  12.920  < 2e-16 ***
# express_num                                   0.029911   0.012701   2.355  0.01861 *
# identity_histProtestant                       0.132437   0.084693   1.564  0.11801
# identity_histChristian                       -0.040665   0.057846  -0.703  0.48213
# identity_histMuslim                          -0.228053   0.124820  -1.827  0.06782 .
# identity_histHindu                           -0.052018   0.065787  -0.791  0.42919
# identity_histBuddhist                         0.037655   0.058088   0.648  0.51689
# identity_histAtheist or agnostic              0.136490   0.048891   2.792  0.00529 **
# express_num:identity_histProtestant          -0.040128   0.024782  -1.619  0.10552
# express_num:identity_histChristian            0.008926   0.017204   0.519  0.60390
# express_num:identity_histMuslim               0.073853   0.039381   1.875  0.06087 .
# express_num:identity_histHindu                0.001869   0.022854   0.082  0.93484
# express_num:identity_histBuddhist             0.042337   0.025807   1.641  0.10103
# express_num:identity_histAtheist or agnostic  0.003879   0.031168   0.124  0.90096

# Residual standard error: 0.4786 on 2356 degrees of freedom
#  (636 observations deleted due to missingness)
# Multiple R-squared:  0.02382,	Adjusted R-squared:  0.01843 
# F-statistic: 4.422 on 13 and 2356 DF,  p-value: 1.849e-07

summary(lm(data=df2, linkedfate_yes~express_num+identity))

# Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)    
# Intercept = Catholics + "Never"  0.521124   0.028619  18.209  < 2e-16 ***
# express_num                      0.035442   0.006668   5.315 1.16e-07 ***
# identity(2) Protestant           0.012631   0.044229   0.286  0.77522    
# identity(3) Christian           -0.015106   0.029901  -0.505  0.61348    
# identity(4) Muslim              -0.024515   0.061647  -0.398  0.69090    
# identity(5) Hindu               -0.045099   0.033927  -1.329  0.18389    
# identity(6) Buddhist             0.103765   0.035434   2.928  0.00344 ** 
# identity(7) Atheist or agnostic  0.151202   0.035310   4.282 1.93e-05 ***

# Residual standard error: 0.4791 on 2362 degrees of freedom
#  (636 observations deleted due to missingness)
# Multiple R-squared:  0.01928,	Adjusted R-squared:  0.01637 
# F-statistic: 6.632 on 7 and 2362 DF,  p-value: 8.655e-08



summary(lm(data=df2, linkedfate_yes~aapiethnicity))
summary(lm(data=df2, linkedfate_yes~identity_hist*aapi_hist)) # This is an interaction effect
summary(lm(data=df2[df2$aapiethnicity=="(01) Chinese",], linkedfate_yes~express_num)) # This is a subsetting regression
summary(lm(data=df2[df2$aapiethnicity=="(07) Japanese",], linkedfate_yes~express_num))
summary(lm(data=df2, linkedfate_yes~identity_hist))
summary(lm(data=df2, linkedfate_yes~identity_reg*aapi_hist)) # This is an interaction effect
summary(lm(data=df2[df2$aapiethnicity=="(01) Chinese",], linkedfate_yes~identity_hist)) # This is an interaction effect
summary(lm(data=df2[df2$aapiethnicity=="(07) Japanese",], linkedfate_yes~identity_hist)) 


#### REGRESSION PLOT ####

# Code from https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/

identity_linkedfate_reg <- ggplot(df2, 
                              aes(x = identity, y = linkedfate_yes)) + 
                            geom_point() +
                            stat_smooth(method = "lm", col = "red")

identity_linkedfate_reg


#### Task list: #####
## 1 - check to make sure all the independent variables are coded as you want
## 2 - recode your dependent variables as needed (no factors) and figure out 
## whether you want to create an index variable (average of the three variables)
## 3 - create your descriptive statistics and check against codebook to make sure
## no one has been dropped inadvertently, and that you're only including the right
## people (1 Iranian???)
## 4 - mapping relationships using linear regression. Decide which regressions to 
## run, with an eye to answering your first two research questions. 
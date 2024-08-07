####################################################
## Week 11 Lab: Interactions (continued)          ##
##  Friday, December 1st, 2023                    ##
####################################################


################################################################################

## Load packages and function --------------------------------------------------

library(tidyverse)
library(psych) 
library(car) 
library(effects)
library(effectsize)
library(ggplot2)

source("610710_functions.R")

################################################################################

# Dichotomous x Continuous Interaction Exercise (30 min)

## Suppose you sampled 400 participants and asked them to report whether
## or not they read for pleasure (a score of -0.5 indicates they do not read
## for pleasure and a score of 0.5 indicates they do read for pleasure), the 
## number of hours spent outside per week (ranging from 0 to 20), and happiness
## (measured on a 50-point scale with greater numbers indicating greater 
## happiness). There are the same number of people who read for pleasure as there
## are people who do not read for pleasure. On average, participants in your 
## sample spend 10 hours outside per week. You obtain the following results:

## happiness = 10 + 3 * read_c + 2 * outside_c + 0.5 * read_c * outside_c


# 1.
# What is the magnitude of the simple effect of time spent outside on happiness 
# for participants who read for pleasure?





# 2.
# What is the magnitude of the simple effect of time spent outside on happiness 
# for participants who do not read for pleasure?





# 3.
# What is the effect of reading on happiness for participants with an average
# number of hours spent outside? Is this a simple or main effect?





# 4.
# Describe in words how the effect of reading on happiness varies by the number
# of hours spent outside.



# 5.
# How many hours does a person spend outside if they have a score of -3 on the 
# outside_c predictor?




# 6. 
# What is the predicted happiness for a person who spends 14 hours outside per 
# week and does not read for pleasure?




# 7. 
# Change the coding of reading to 0 = does not read and 1 = reads for pleasure
# and fill in the resulting equation:

# happiness = ___ + ___ * read + ___ * outside_c + ___ * read * outside_c






# 8. 
# How (if at all) will the standard errors for the parameter estimates for the 
# interaction terms of the two models (one with read_c and one with read) differ?






# 9.
# A lab member suggests to you that you can test whether you have a significant
# interaction by examining the simple effects of spending time outside at each
# level of reading for pleasure. That lab member tells you that if one is significant
# and the other is not, you will have a significant interaction. Do you agree
# with this lab member? Why or why not?






# Write the augmented and compact model for the model comparison 
# which tests whether the read_c * outside_c interaction effect is significant.



################################################################################

# R Code For Interactions (Continuous x Dichotomous and 2 x 2 ANOVA) 

# A researcher is testing whether watching a video about the benefits of a new 
# tax policy moderates the relationship between political affiliation and support
# for that policy. The researcher recruits participants and randomly assigns them
# to either watch or not watch a short video. The researcher than measures whether
# they support the policy on a scale from 0 (do not support at all) to 10 (strongly
# support). The researcher then determines the participant's political affiliation.
# Political affiliation was measured both continuously (1 = Strong Republican, 
# 7 = Strong Democrat) and dichotomously (0 = Republican, 1 = Democrat). The 
# researcher records condition (0 = no video, 1 = video).

# Run the following code:

set.seed(123)
d <- data.frame(
  cond = sample(0:1, 600, replace = TRUE),
  political_cont = sample(1:7, 600, replace = TRUE)
)
d$political_dichot <- ifelse(
  d$political_cont %in% c(1, 2, 3), 0,
  ifelse(d$political_cont %in% c(5, 6, 7), 1, sample(0:1, 600, replace = TRUE))
)
d$interact <- d$political_cont * d$cond
d$support <- round(pmax(0, pmin(10, rnorm(600, mean = 5, sd = 2) + 0.2 * d$interact)))
d <- d[, !(names(d) %in% c('interact'))]


# Examine the univariate and bi-variate summary statistics for the variables in
# the dataframe, d.

describe(d)
GGally::ggpairs(d)

# create a table reporting the number of people at each level of the three 
# predictor variables (condition and both measures of political affiliation)

table(d$cond)
table(d$political_cont)
table(d$political_dichot)



# create a table reporting the average support score for each level of the three
# predictor variables

describeBy(d$support, d$cond)
describeBy(d$support, d$political_cont)
describeBy(d$support, d$political_dichot)

# Create a table reporting the average support score for each level of condition 
# and each level of political affiliation (measured dichotomously). 

# Note: you may need to consult previous labs, HWs, or other resources like chat
# GPT to help find an answer.







# Based on the table you created, are the means consistent with a model in which
# the interaction between cond political affiliation (measured dichotomously)
# is significant? How can you tell?






# Fit a linear model to test whether condition and political affiliation
# (measured dichotomously) interact to predict support for the tax policy. In 
# addition to the parameter estimates, also determine partial eta squared for
# the interaction term.







# Create a publication quality graph bar graph depicting the cell means with error 
# bars representing the standard error of the point estimates from the model
# you just tested.
























# Provide a 1-2 sentence description of the model you tested and comment on 
# whether the interaction was significant.






# Re-test this same model, but instead of using the centered predictors, use
# the uncentered predictors. What changed between the two models?






# Fit a new model in which you regress support for the tax policy on condition,
# political affiliation (measured continuously), and their interaction.
# In addition to the parameter estimates, also determine partial eta squared for
# the interaction term.







# Provide a 1 - 2 sentence description of the model you tested and whether the
# interaction you tested was significant.







################################################################################

### Together as a group ###

# Introducing ANCOVA

# ANCOVA is an acronym for analysis of covariance. This is a term that was coined
# in the ANOVA literature before the GLM became more popular in psychology. ANCOVA
# is just a fancy term for an interactive model in which we're also controlling
# for a variable (and the covariate is not involved in the interaction).

# Why might we add such a covariate?


# Let's suppose that the researchers who were testing the interaction between
# condition and political affiliation were worried that participant's 
# support would be influenced by whether they had seen news coverage about 
# the proposed policy.

# Run the following code:
set.seed(123)
correlation_factor <- 0.5
d$news <- round(correlation_factor * d$support + sqrt(1 - correlation_factor^2) *
                  rnorm(600, mean = 0, sd = 1))
d$news <- ifelse(d$news >= 3, 0, 1)

# Adding news as a covariate

# Let's add news to our model with a continuous predictor for political affiliation.




# The * symbol indicates the variables that are being included in the
# interaction. The + symbol tells R to estimate a parameter for that variable but
# not allow it to interact with anything.

# What is another way we could write this same model to be more explicit about
# what is being included or not included in our interaction?




# Now let's look at the output of this model:





# Did including our covariate increase our power (i.e., decrease our standard 
# error)?

summary(m3)
summary(m4)
## Yes! 

# Did we find a significant interaction effect in our ANCOVA model?







# Yzerbyt and colleagues identify that when we manipulate one of the variables in
# the interaction term of an ANCOVA we must include the manipulated variable by
# covariate interaction in our model in order obtain an unbiased estimate of
# the interaction term.

# How do we set up this new verion of our model?

# First let's center our covariate. While normally covariates are not involved 
# in any interactions, it is not necessary to center them (unless we care about 
# the intercept or if we want to graph the model predictions). That is, in most 
# cases, the parameter estimate for the covariate represents the predicted effect 
# of the covariate for the sample, regardless of participants' scores on other 
# predictors. However, since we have included it in an interaction term to ensure
# unbaised estimates of our interaction term, we must center it.



# Now we can set up our model:






# There's no fundamental difference between controlling for a variable in an 
# additive model and controlling for a variable in an interactive model. In 
# both cases, to the extent that our control variable (news) accounts for unique 
# variance in the dependent variable (and, preferably, to the extent that our 
# control variable is uncorrelated with our other predictor variables), 
# that control variable will increase our power to test our focal effects.






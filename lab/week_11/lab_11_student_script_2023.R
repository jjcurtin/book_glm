####################################################
## Week 11 Lab: Intro to Interactions             ##
##  Friday, November 17th, 2023                   ##
####################################################


################################################################################

## Load packages and function --------------------------------------------------

################################################################################
Sys.setLanguage("en") # Show output, warnings, and error messages in English.
library(tidyverse)
library(psych) 
library(car) 
library(effects)
library(effectsize)
library(ggplot2)

source("610710_functions.r")

################################################################################

## Graphing models using transformed data --------------------------------------

################################################################################

# Just run through the commands below
d_raw <- read_tsv('lab_10_prestige.dat')
d <- d_raw[d_raw$education > 0, ]
d <- d %>% filter(name != "bartenders", name != "commercial.travellers")
d$income_log_t <- log2(d$income)
m_income_log2 <- lm(income_log_t ~ education + women, data = d)

# Create d_graph 
## Focal predictor = education; Outcome variable = log-transformed income.
d_graph <- data.frame(education = seq(min(d$education, na.rm= T), max(d$education, na.rm= T), length = 100), 
                      women = mean(d$women, na.rm= T))
d_graph <- ggplotPredict(m_income_log2, d_graph)

## Graphing log-transformed income on the y-axis
scatterplot <- ggplot() + 
  geom_point(data = d, aes(x = education, y = income_log_t), color = "black", 
             position = position_jitter(width = .2, height = 0),
             alpha = .6) + 
  geom_smooth(data = d_graph, aes(x = education, y = Predicted, ymin = CILo, ymax = CIHi), 
              stat = "identity", color="purple") +
  theme_bw(base_size = 14) + 
  theme(axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        panel.border = element_blank()) +
  scale_x_continuous("Years of Education") +   
  scale_y_continuous("Income (log 2 transformed)",
        breaks = seq(floor(min(d$income_log_t, na.rm=T)), ceiling(max(d$income_log_t, na.rm=T)), by = 1)) + 
  # y-axis label and tick mark location. Use scale_y_continuous() because y is specified as 
  # a continuous variable in default aes settings
  labs(title = 'Effect of % Women in a field on Income')
scatterplot


## Graphing raw income on the y axis: curved regression line
d_graph$Predicted_ut <- 
d_graph$CILo_ut <- 
d_graph$CIHi_ut <- 

  # "un-transforms" the predicted values into original units

scatterplot_curved <- ggplot() + 
  geom_point(data = d, aes(x = education, y = income), color = "black", 
             position = position_jitter(width = .2, height = 0),
             alpha = .6) + 
  geom_smooth(aes(x = education, y = Predicted_ut, ymin = CILo_ut, ymax = CIHi_ut), 
              data = d_graph, stat = "identity", color = "purple") +
  theme_bw(base_size = 14) + 
  theme(axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        panel.border = element_blank()) +
  scale_x_continuous("Years of Education") +   
  scale_y_continuous("Income", breaks = seq(500, 26000, by = 1500)) + 
  # y-axis label and tick mark location. Use scale_y_continuous() because y is specified as 
  # a continuous variable in default aes settings
  labs(title = 'Effect of % Women in a field on Income')
scatterplot_curved


################################################################################

## Review exercise: bulging rule ----------------------------------------------

################################################################################
# TAs run the code on TA script and show graphs


# According to the Bulging Rule:
## How do we transform this?



################################################################################

## Interpreting coefficients when data are transformed -------------------------

################################################################################

# Researchers recently regressed happiness (0-100) on a measurement of 
# participants' self-confidence. After checking their model assumptions, the 
# researchers decide that happiness needed to be transformed down the ladder. 
# Specifically, a log2 transformation. After transforming their outcome and 
# fitting a new model, they get the following estimate for b1: .084

# The research team knows that saying a "every additional unit of self-
# confidence increases log2(happiness) by .084" is not very informative. What is the 
# interpretion b1 in raw units of happiness?

# One unit increase in log2(happiness) = a doubling in happiness
# The inverse of log2 is the second power.

# Method 1: 
# How many times does the coefficient "go into" one?

1/.084 #11.90

# Scoring ~12 points higher on the measure of self-confidence is associated with 
# being twice as happy (happiness score doubles).


# Method 2: 
# Can raise 2 to the power of our coefficient to find out how much y is
# multiplied by for every 1 unit increase in x.

2^.084 # 1.059

# Scoring 1 point higher on the measure of self-confidence is associated with 
# being 1.059 times as happy (happiness score changes by a factor of 1.059).



## Try it yourself -------------------------------------------------------------
# You try to replicate this study and find that you still
# need to transform happiness by log2. You fit a model regressing log2(happiness)
# on self-confidence and find that b1 = .341

# Method 1



# Method 2



################################################################################

## Introduction to Interactions ------------------------------------------------

################################################################################

# Imagine a hypothetical study by a graduate student Andrew. In this study, Andrew
# examined whether preschoolers benefit from collaborative learning (vs. passive 
# learning) when they are learning about the external world.

# Often, teachers believe that working actively in a group to learn is better  
# than learning by passively observing. However, Andrew believes that there’s a 
# developmental difference in preschoolers ability to learn from collaboration, such that 
# only older preschoolers would benefit from and learn better in collaboration, 
# whereas this benefit would be considerably smaller for younger preschoolers.

# To test this hypothesis, Andrew had preschoolers learn about the physical phenomenon 
# that a sponge soaks up water with an adult experimenter. In the Observation 
# condition, the preschoolers watched the adult experimenter demonstrate using 
# a sponge to soak up water. In the Collaboration condition, the child and adult
# played with a sponge and water together to learn about this phenomenon. 

# The preschoolers' learning was assessed 5 times within a 2 hour-block after the  
# learning period. The experimenter would "accidentally" pour water on the desk  
# and ask the preschooler if they can help. A sponge was placed on the desk 
# along with other toys. The percentage of times that the preschoolers used the
# sponge to soak up water was recorded as the indicator of the preschooler's learning.

# code book:
# 1. age (continuous)
# 2. condition (either Observe [0] or Collaborate [1])
# 3. learning (sponge uses in percentage)
# 4. prosociality (don't worry about this now)

# Read in the data.


d <- read_tsv("lab_11_data_2023.dat") %>% janitor::clean_names()
glimpse(d)
describe(d)

# What type of interaction is this? 



# What should we do first before testing interactions?
## Center the predictors

# Why do we need to center the predictors in an interactive model?



# Center the predictors
d$age_c <- d$age - mean(d$age, na.rm = TRUE)
d$condition_c <- dplyr::recode(d$condition, "0" = -0.5, "1" = 0.5)



# Additive model: no interaction term --------------------------------------

# First, let's fit a model without the interaction term for pedagogical purposes.
# Use centered predictors.

m1 <- lm()
summary(m1)

# Interpret the coefficients

## Intercept

## b1: condition

## b2: age


# Model with an interaction term -----------------------------------------------

# We allow the slope of condition to be a function of age:
# y = b0 + b2*age_c + (b1 + b3*age_c)*condition_c

# Note that this is statistically equivalent to:
# y = b0 + b1*condition_c + (b2 + b3*condition_c)*age_c

# Either way, here is the model we fit:
# y = b0 + b1*condition_c + b2*age_c + b3*condition_c*age_c

# So this model will allow us to test if the effect of condition is a function of
# age. To test it we need to create a product term of age*condition.

# "Dumb" method that works for mostly all software or calculation by hand:
## Manually take the product of condition_c and age_c and assign it to a new variable



## Then fit the model including condition_c, age_c and the interaction term.
m_int_1 <- lm()
summary(m_int_1)

# Easier method in R (same result)
m_int_2 <- lm()
summary(m_int_2)

# Much easier method in R (same result)
m_int <- lm()
summary(m_int)

# Why is the following model NOT the correct model to test our question?
m_wrong <- lm(learning ~ condition_c:age_c, data = d)
summary(m_wrong)
# It doesn't include simple effects (lower-order terms)
# So the results will be sensitive to measurement/scale of the variables.

# What has changed from the additive model?
summary(m_int)
summary(m1)

# To test the interaction effect, m_int is compared to the additive model m1
car::Anova(m_int, type = 3)
anova(m1, m_int)
# Use of Anova Type 3 is IMPORTANT. Significance tests of the simple effects are 
# handled differently (arguably less ideally) if you use Type 2.

#################
#               #
# Exercise Time #
#               #
#################

# Interpret coefficients in the interaction model-------------------------------
summary(m_int)

# Intercept:



# condition_c:



# age_c:



# interaction:


## OR:



# Obtain the effect size of the interaction effect------------------------------


# Hand-calculate predictions and simple effects---------------------------------

# y = 0.386 + 0.013*age_c + (0.113 + 0.025*age_c)*condition_c
# y = 0.386 + 0.113*condition_c + (0.013 + 0.025*condition_c)*age_c
# y = 0.386 + 0.113*condition_c + 0.013*age_c + 0.025*condition_c*age_c
summary(m_int)

# What is the effect (regression coefficient) of age in the Observation condition?

## For the Observation condition, condition_c = -0.5
## beta_age = (0.013 + 0.025*condition_c) = 


## What does this number tell us? 



# What is the effect (regression coefficient) of age in the Collaborate condition?
## For the Observation condition, condition_c = 0.5
## beta_age = (0.013 + 0.025*condition_c) = 


## What does this number tell us? 



# What is the effect (regression coefficient) of condition among 40-mos-old preschoolers?
## For 40-mos-old preschoolers, age_c = 
## beta_condition = (0.113 + 0.025*age_c) = 


# What is the effect (regression coefficient) of condition among 60-mos-old preschoolers?
## For 60-mos-old preschoolers, age_c = 
## beta_condition = (0.113 + 0.025*age_c) = 


# What is the model predicted value for a 42-mos-old preschooler in the collaboration condition?

## For this preschooler, 
## age_c = 
## condition_c = 
## Predicted value = 

#################
#               #
# End Exercise  #
#               #
#################
# Is the effect of condition significant among younger preschoolers (40-mos-old)?
# How do we test that??
## Recentering!!
d$age_40 <- d$age - 40
m_int_40 <- lm(learning ~ condition_c * age_40, data = d)
summary(m_int_40)
# The effect of condition is non-significant for 40-mos-old preschoolers.

# Is the effect of condition significant among older preschoolers (60-mos-old)?
# How do we test that??
## Recentering!!
d$age_60 <- d$age - 60
m_int_60 <- lm(learning ~ condition_c * age_60, data = d)
summary(m_int_60)
# The effect of condition is significant for 60-mos-old preschoolers.

# Graphing a continuous x dichotomous interaction -------------------------

# A graph is more informative w/ uncentered predictors, as we are not concerned 
# with the interpretation of regression coefficients of the simple effects.

m_graph <- lm(learning ~ age * condition, data = d)
d_graph <- expand.grid(
  
)
# Because we are plotting interactions, we need to sample a range of possible values 
# for both age and condition. We will NOT hold either one at its average this time.

# Generate predictions!
d_graph <- ggplotPredict(m_graph, d_graph)


# Let's recode condition and give it labels to be used for plotting:
d_graph$condition_str <- dplyr::recode(d_graph$condition,
                                       "0" = "Observation", 
                                       "1" = "Collaboration")

# We will use raw data "d" in geom_point(), so we create the same condition_str 
# variable in raw data for plotting.
d$condition_str <- dplyr::recode(d$condition,
                                       "0" = "Observation", 
                                       "1" = "Collaboration")


ggplot(d, aes(x = age, y = learning, color = condition_str, fill = condition_str)) +
  geom_point(aes(), alpha = .2,size=3) +
  geom_smooth(data = d_graph, stat = "identity", 
              aes(y = Predicted, ymin = CILo, ymax = CIHi))+ 
  labs(x = "Age (months)", y = "Learning (percentage)", main = "Collaborative learning is beneficial in older preschoolers",
       color = "Condition", fill = "Condition") + 
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), # remove gridlines
    panel.grid.minor = element_blank(), # remove gridlines
    legend.position = c(.2, .93), # positioning legend
    legend.background = element_blank(), # removing background of legend
  ) + # removing title of legend
  scale_x_continuous(breaks = seq(min(d$age), max(d$age),by = 2),expand = c(0,0))+
  scale_y_continuous(breaks = c(0.0,0.2,0.4,0.6,0.8,1.0),
                     labels = scales::percent, # make it percentage scale
                       expand = c(0,0)) + 
  scale_color_brewer(palette = "Set1")+ 
  scale_fill_brewer(palette = "Set1") # don't like R's default colors? 
# you can use other color sets.
# see http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/figure/unnamed-chunk-14-1.png



##  ########################################  ##
##       ANCOVA and Polynomial Regression     ##
##   Week 14 Lab, Friday, December 8th, 2023  ##
##  ########################################  ##

library(data.table)
library(tidyverse)
library(psych)
library(car)
library(effectsize)

source("/Users/nicholasvest/Documents/PSYCH 610/Week 14/Lab 14/610710_functions.R")

######## PART 1: ANCOVA ########

# ANCOVA is an acronym for analysis of covariance. This is a term that was coined
# in the ANOVA literature before the GLM became more popular in psychology. ANCOVA
# is just a fancy term for an interactive model in which we're also controlling
# for one or more variables (and the covariate(s) is (are) not involved in the interaction).

# Why might we add a covariate?


# For this exercise, we are working with the nicotine withdrawal dataset. 
# We are interested in the effect of the type of cigarette usually used by
# participants in addition to whether participants were in a nicotine 
# deprivation condition or not, on their startle response (anx_stl).

# Coding for smoking condition: deprived = -0.5, non_deprived = 0.5
# Coding for cigarette type: standard = -0.5, e-cig = 0.5

d <- read_tsv("lab_14_data_1.dat") %>% janitor::clean_names()
describe(d)

# Change the names of the variables to reflect the fact that they're already 
# centered



# Our question of interest is: 
# Is there an interactive effect between nicotine deprivation and 
# cigarette type on startle response?
# Let's write out the model that answers this question


# Before we run this model, Is there a covariate we can add that might increase 
# our statistical power from our set of variables we collected?



# What features will a covariate that reduces error variance in the model
# and increases statistical power to detect an interaction effect have?
# It will correlate with the DV (anx_stl) but not with the focal predictor (or its
# correlation should be much lower).
# By this definition, is base_stl likely to increase our power?
d %>%
  select(anx_stl, base_stl, smoke_cond_c, cig_type_c) %>%
  cor(use = "pairwise.complete")
# Yep!

# Adding base_stl as a covariate

# The * symbol indicates the variables that are being included in the
# interaction. The + symbol tells R to estimate a parameter for that variable but
# not allow it to interact with anything.

# Now, let's look at the output of this model:



# Did we find a significant interaction effect?

# Yes! Controlling for baseline startle, itself a significant predictor of 
# posttest startle, F(1,55) = 81.18, p < .001, both the parameters for
# condition, F(1,55) = 5.57, p = .022, and the interaction between condition 
# and cigarette type became significant, F(1,55) = 5.43, p < .05.

# There's no fundamental difference between controlling for a variable in an 
# additive model and controlling for a variable in an interactive model. In 
# both cases, to the extent that our control variable accounts for unique 
# variance in the dependent variable (and, preferably, to the extent that our 
# control variable is uncorrelated with our other predictor variables), 
# that control variable will increase our power to test our focal effects.

# NOTE: Because covariates are not involved in any interactions, it is not 
# necessary to center them (unless we care about the intercept or, as we will 
# see below, if we want to graph the model predictions). The parameter 
# estimate for the covariate represents the predicted effect of the covariate 
# for the sample, regardless of participants' scores on other predictors.

##  ##################################################################  ##
##   Graphing: bar plot with adjusted means (Adjusted for a covariate)  ##
##  ##################################################################  ##

# In this example, we're most interested in the cigarette type by condition 
# interaction. So, we're going to create a barplot of this interaction 
# while adjusting for the covariate, base_stl. To do so, we will set the
# covariate at its mean (just like we did when graphing one-predictor models 
# with a covariate).

# Re-run the model, for reference!
m_graph_1 <- lm(anx_stl ~ smoke_cond_c * cig_type_c + base_stl, data = d)
(summary(m_graph_1))

# We want to make 4 predictions, one for standard smokers in the deprived 
# condition, one for standard smokers in the non-deprived condition, one for
# e-cig smokers in the deprived condition, and one for e-cig smokers in the non-
# deprived condition. Let's use expand grid to vary both cig type and smoke
# condition. We'll hold our covariate constant at its mean.




# Now we'll make predictions!



# Now we'll re-label everything.

# Recode from numeric to actual names
d$cig_type_str <- factor(d$cig_type_c, c(-0.5, 0.5), c("Standard", "e-cig"))
d$smoke_cond_str <- factor(d$smoke_cond_c, c(-0.5, 0.5), c("Deprived", "Nondeprived"))
d_graph_1$cig_type_str <- factor(d_graph_1$cig_type_c, c(-0.5, 0.5), c("Standard", "e-cig"))
d_graph_1$smoke_cond_str <- factor(d_graph_1$smoke_cond_c, c(-0.5, 0.5), c("Deprived", "Nondeprived"))

# Bar plot!



##  #####################################  ##
##   Computing two separate interactions   ##
##  ###################################### ##

# Here, smoke_cond_c * cig_type_c AND smoke_cond_c * base_stl

# Why would we want to do this? (hard question)
# Yzerbyt and colleagues say this is the model we should run when we have both 
# a manipulated and a measured independent variable. That could be the case 
# here: let's say we recruited both standard and e-cig users, rather than 
# assigning them to cigarette type.

# to get an unbiased estimate of the interaction term in an ANCOVA requires 
# you to include the interaction between the manipulated predictor and the
# covariate

# In that case, this would be the correct analysis!

# Center baseline startle


# You write this model out pretty much exactly as you'd expect:


# Interpret the coefficients!!
# b0 = estimated post-test startle for someone with mean baseline startle, 
#      neutral with respect to both condition and cigarette type.
# b1 = The condition effect for someone with average baseline startle and 
#      neutral with respect to cigarette type.
# b2 = The effect of cigarette type for someone neutral with respect to condition,
#      while statistically controlling for the other predictors in the model.
# b3 = The effect of baseline startle for someone neutral with respect to condition.
#      while statistically controlling for the other predictors in the model.
# b4 = ???
# b5 = The extent to which the baseline startle effect changes by condition
#      (and vice versa), while statistically controlling for the other predictors in the model.


######## PART 2: Polynomial Regression ########

rm(list = ls())
source("/Users/nicholasvest/Documents/PSYCH 610/Week 14/Lab 14/610710_functions.R")

# We'll be using a new (fake) dataset: The productivity dataset.  
# We want to know whether an academic's productivity is related to the number of 
# years since PhD. There are lots of ways to measure productivity. The ideal 
# measure is probably some combination of publications, grants, and teaching 
# outcomes. 
# For the present dataset, it's just a z-score, such that higher --> more 
# productive. Furthermore, we want to know if an academic's level of laziness 
# moderates this effect. We will use Emma's laziness Scale. 


# What are your hypotheses? Should years since receiving a PhD be related to 
# productivity? 

# Do you think this is a linear trend (constant slope)? 

# Should the relationship depend on laziness?  

# Could there be an optimal point in one's career?


d <- read.csv("productivity.csv") %>% janitor::clean_names()
# It has three variables: years, productivity, and laziness scores 



# Let's predict that the relationship between years and productivity changes 
# across years since PhD. In particular, we're predicting rapid growth that will 
# peak at some optimal year, and then decrease. 

# We need polynomial regression! 


# Polynomial Regression in R --------------------------------------------------- 




# Run the model with centered years 
# On the right side of the ~ you need to embed arithmetic functions (+, -, ^, 
# etc) within the "I" function 


# What does the linear effect represent?  

# The years_c coefficient represents the predicted effect of an extra year of 
# experience on productivity at an average number of years since PhD. 

# What does the quadratic effect represent?  

# The fact that the coefficient for years_c^2 is different from 0 tells us that 
# the predicted relation between years and productivity changes across the range 
# of years since PhD. In this case, the relation becomes negative with time. As a result, 
# there may be some values for years since PhD where an extra year since PhD 
# leads to a drop in productivity. 

# quick and dirty graph of this model: 



# Effect Sizes in Polynomial Regression  --------------------------------------- 

# How do we find the variance accounted for by the quadratic component and the 
# linear component separately? 
eta_squared(car::Anova(m_poly_c)) 
# Note: the variance accounted for by the linear component only applies to the linear 
# component at that specific value of the predictor 

# Where do we find the overall effect of years since PhD? 
# How can we test the significance of this effect? 

# We can briefly compare the SSE from this model (the augmented model) to the SSE from 
# the relevant compact model. 

# What is the relevant compact model? 


# Calculating point slopes ----------------------------------------------------- 

m_poly <- lm(productivity ~ years + I(years^2), data = d)
coef(m_poly) 

# -1.2330 + 0.1086*years -0.0016*years^2 

# Take the derivative: 


# Now we can plug different years in to see what the point slope is at those points 

# What's the slope at 20 years? 


# What's the interpretation of this predicted value? 


# What's the slope at 50 years since getting a PhD? 


# What's the interpretation of this predicted value? 


# What's the turning point year? When does the relationship between years and productivity 
# go from positive to negative? 

# This is really a question about when the slope will be equal to 0. 
 

# More complex models  --------------------------------------------------------- 

# What if we hypothesized that the relationship between years and productivity 
# depended on laziness scores? In other words, what if the curvilinear relationship 
# (in this case, an inverted U) between years since PhD were more severe for 
# people high in laziness? Conceptually, that would mean that lazy people slow 
# down more quickly in their productivity. How do we test this? 



# How do we interpret these findings?  
# For people with an average number of years since their PhD, there is an effect of 
# laziness on productivity, such that a one unit increase in laziness is related 
# to a .02 unit decrease in productivity (when controlling for everything else 
# in the model). 


# For people average on laziness and number of years since PhD, every additional  
# year since PhD productivity increases .03 units (when controlling for everything  
# else in the model). Once we control for laziness (and the interaction between  
# laziness and years),there is no longer a significant quadratic relation between  
# years since PhD and productivity (p = .294).  


# There is an interaction between laziness and years (controlling for everything 
# else in the model) for participants at 0 on years_c (average number of years 
#  since PhD). This means that the relation between years since PhD and  
# productivity changes depending on the person's laziness.  
# There is not a significant interaction between years^2 and laziness. 
# This means that the decline in the increase in productivity as people have 
# more years since PhD does not depend on laziness. 


# Graphing Polynomial Regression ----------------------------------------------- 

# We'll graph the uncentered quadratic model. 
d_input <- data.frame(years = seq(min(d$years), max(d$years), length = 60)) 
d_predictions <- ggplotPredict(m_poly, d_input) 


scatterplot <- ggplot() +  
  geom_point(data = d, aes(x = years, y = productivity), color = "black") + # Raw adjusted Data 
  geom_smooth(data = d_predictions, aes(ymin = CILo, ymax = CIHi,  
                                        x = years, y = Predicted), # model data 
              stat = "identity", color = "blue") + 
  scale_x_continuous("Years since PhD",  
                     breaks = seq(0, 60, by=10)) +   # x-axis label and tick mark location 
  scale_y_continuous("Productivity (Standardized)",  # y-axis label and tick mark location 
                     breaks = seq(-2, 4, by = 0.5)) + 
  theme_bw(base_size = 14) +
  theme(axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        panel.border = element_blank()) 

scatterplot 

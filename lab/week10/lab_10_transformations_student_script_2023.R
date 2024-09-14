##################################################
## Week 10 Lab: Assumptions and transformations ##
##       Friday, November 10th, 2023            ##
##################################################


# Setup: Load libraries, set working directory ---------------------------------
library(tidyverse) 
library(psych) 
library(car) 
library(effectsize) 
library(rockchalk) 

#source in up-to-date 610710 functions
source("")


## Review Exercise: Mediation --------------------------------------------------

# In pairs or on your own please complete the following exercise.

# Recent research indicates that people prefer watching rebooted shows like 
# "Fuller House," "iCarly," "Dexter," etc., over new, original series. While 
# it could suggest that the rebooted shows boast higher quality, the research 
# team suspects that this preference is more likely influenced (i.e., mediated) 
# by feelings of nostalgia rather than the inherent quality of the shows. To 
# investigate this, the team conducted a study with 200 participants. Each 
# participant was randomly assigned to either watch one season of an original 
# series or one season of a rebooted show. Before diving into their assigned 
# series, participants filled out a "Nostalgia" scale. Following the viewing 
# experience, participants rated their series on various factors, which were 
# then averaged into a single "enjoyment" measure.

# Participants then watched their assigned series, and rated it on multiple factors
# which were then averaged into one composite "enjoyment" measure.

# So what is X, M, and Y?
# >> X: 
# >> M: 
# >> Y: 

# Remember this? Let's try to visualize the hypothesized mediation model.

   #############                          #############
   ##---------##                          ##---------##
   ##-???????-##**************************##-???????-##
   ##-???????-##                          ##-???????-##
   ##---------##                          ##---------##
   #############                          #############

                      #############
                      ##---------##
                      ##-???????-##
#                   **##-???????-## **
#                 **  ##---------##  **
#               **    #############    **
   ###########**                         **############
   ##---------##                          ##---------##
   ##-???????-##**************************##-???????-##
   ##-???????-##                          ##-???????-##
   ##---------##                          ##---------##
   #############                          #############

### Where is X? 
#### 

### Where is Y?
#### 

### Where is M?
#### 

### Where is path a?
#### 

### Where is path b?
#### 

### Where is the total effect labeled c?
#### 

### Where is the direct effect labeled c'?
#### 

### Where is the indirect effect labeled ab?
#### 

# Okay, now, the following models have been fit based on the above mentioned study.
# Please identify from which model you can calculate one or more of the following
# pathways C, A, B, C', and AB

# Nostalgia ~ b0 + b1(Condition)  
#### 

# Enjoyment ~ b0 + b1(Condition)  
#### 

# Enjoyment ~ b0 + b1(Condition) + b2(Notalgia)  
#### 

# You're in the middle of testing each of the four conditions, but your computer and, 
# well, all computers break (don't worry, they will start working after you finish this 
# question), forcing you calculate the remaining pathway coefficients by hand. 

# You know that the total effect = 2.88 
# You know that the a pathway = .68 
# You Know that the c' pathway = 1.996 

# What is the coefficient for the b pathway? # 
# What is the coefficient for the ab pathway? # 

# Luckily, all the computers in the world start working again right after you figure out  
# the coefficient for the last pathway. You check your work and find that you calculated 
# all the coefficients correctly. What’s even more exciting, you find that each pathway is 
# significant. Your collaborator is very excited, and they want to write in the manuscript that 
# the data is consistent with the hypothesized mediation model: nostalgia mediates the 
# effect of Condition (reboots vs. originals) on Enjoyment.

# Is your collaborator right? Why or why not?


# What proportion of the total effect is mediated by the mediator?


# Messy Data: Transformations --------------------------------------------------

# Load and examine the data
# We are going to return to the modified Prestige dataset. 
d_raw <- read_tsv('lab_10_prestige.dat')
describe(d_raw)

# Remember we had a value that was clearly a data entry error 
# So, let's remove it!


# Univariate exploration

# Note: This is NOT how we assess normality.

# Consider this with your neighbor: What does univariate and bivariate mean?


# Bivariate exploration
# Generate a correlation table between income, education, and women. 
# Do the correlations match your intuitions?


# From last week, we had 2 cases that we determined we would exclude due to high
# influence. Let's remove them.
d_new <- d %>% filter(name != "general.managers", name != "physicians")

# Remember our model from lab 9? Let's keep the high influence points for now.


# The instances we encountered exhibited extreme values in income, and our 
# recent analysis reveals that both income and the percentage of women follow 
# non-normal distributions. Given this scenario, we may question whether the 
# anomalies in our two problematic cases arise not from peculiar instances but 
# rather from the inherent nature of our variables' distributions, potentially 
# leading to systematic violations of the assumptions underlying the General 
# Linear Model's expectations for our data structure.


# Checking Model Assumptions ---------------------------------------------------

# Review from lecture: What are the 5 primary assumptions the GLM makes about
# the structure of the data?
# 1. 
# 2. 
# 3. 
# 4. 
# 5. 

# Violations of assumptions can cause...?
# A. 
# B.
# C. 

# Normality, constant variance, and linearity can be evaluated by using 
# the modelAssumptions function from our sourced function file.

# No measurement error in the predictors is always violated to some extent, but 
# essentially demands that you use the most reliable measures at your disposal.

# Independence of residuals can be fixed by a variety of approaches (e.g., 
# repeated measures analysis; see Psych 710)


# Assumption 3: Normality of errors --------------------------------------------

# Remember that "normality" in this case refers to the model residuals, NOT the
# distribution of any single variable in the model. 

# A quantile-comparison (Q-Q) plot compares two distributions against each other
# to see if they’re similar. GLMs assume that residuals are normally distributed,
# so we compare the actual distribution of model residuals to the theoretical
# normal distribution. The closer the two distributions are, the more normal the
# residuals are. modelAssumptions() will give you this and more.


 
# The function inside the function above:
qqPlot(m1, labels = FALSE, sim = TRUE, main = "Quantile-Comparison Plot to Assess Normality", 
       xlab = "SDs away from the mean (Normal Distribution)", ylab = "Studentized Residuals")

# What's on the X-axis here? 

# What about the Y-axis?


# What are we looking for here? 
# The points to fall along the diagonal -- which would mean the theoretical 
# normal distribution matches our actual distribution from our data (= our 
# residuals are normally distributed). 

# How are we doing for normality of errors?

# What is the problem?


# ASIDE: The logic of the QQ plot ----------------------------------------------
# These are difficult to understand. You should think of the X-axis as
# representing "where I'm looking for it" and the Y-axis as representing "where
# I found it." 
# For example, look at the lower end of the X-axis on this QQ plot.
# We're going to look for the error we'd expect to occur at a t-value of -2 (-2
# on X-axis). As we move up the Y-axis, we encounter that error at something
# more like -2.5. Put differently, this case is farther from the center of the
# data (i.e., t of 0) than we would expect it to be if the distribution of
# errors were normal. Because we're encountering data farther from the center of
# the distribution than we should be, we can infer that we have a thin lower
# tail, which indeed seems to be the case based on the figure on the right. 
# For another example, say we want to look for the error we'd expect to occur at
# a t-value of 1 (1 on the X-axis). Moving up the Y-axis, we "find" this error
# at about 0.5, closer to the center of the distribution than we'd expect.
# Because we found this error closer to a t of 0 than we'd expect in a perfectly
# normal distribution, we can infer we have a steep, skinny peak of our error
# distribution, which indeed we do! I encourage practicing this!!

# So, should we be concerned at this point? Well, we're not doing very well on
# normality, but let's not make any brash decisions yet.


# Assumption 4: Constant variance ----------------------------------------------

# The constant variance assumption implies that, for any given predicted value
# of the dependent variable, the residuals will have the same variance.  

# One easy way to assess the viability of this assumption is to plot the predicted
# values against the observed values and assess visually whether the residuals
# have more scatter at some predicted values than at others. 

# modelAssumptions() will generate that figure, as well as a plot of the predicted 
# values versus the absolute value of the residuals ("spread-level plot").


# If your plots don't show immediately, are they minimized/hidden in a different window?
# If not, try to clear your plots with the "broom" icon and rerun the function.

# The functions inside the function above:
plot(rstudent(m1) ~ fitted.values(m1), 
     main = "Studentized Residuals vs. Fitted Values", 
     xlab = "Fitted Values", ylab = "Studentized Residuals") + abline(h = 0, lty = 2, col = "blue")
spreadLevelPlot(m1, data = d)

# ASIDE: the logic of these plots ----------------------------------------------
# These two plots differ in 2 important ways.
# 1. The Y-axis on the right-sided plot has the absolute value of the studentized 
#    residuals. (Imagine you took the plot on the left and folded it bottom to 
#    top along the y = 0 line. If the variance is not constant, the regression
#    line plotted on the right graph will deviate more from a slope of 0.
# 2. Look at the tick marks on the y-axis of these plots. The one on the left is 
#    linear, while the one on the right is logarithmic. This is why the two 
#    don't match up perfectly based on the "folding" idea. 
# What are we looking for here?
# A. On the left plot, you want the spread of the data at a given value of x to be
# similar to that at other values of x. I.e., Is the vertical width of the
# scatter consistent across the range of fitted values?
# B. On the right plot, you want to see a regression line with a slope near 0 
# (i.e., flat!)

# So... are we concerned now? 


# Assumption 5: Linearity ------------------------------------------------------

# If the residuals are linear, then the slope of the best fitting line should be
# approximately the same no matter what sub-sample of the data you consider.
# Imagine sorting your dataframe on one of your predictor variables, and sliding
# a window down the dataframe and fitting little models for each window. If the
# relationship is linear, the solutions for the sub models should be very
# similar, deviating from each other randomly. On the other hand, if the
# relationship of X and Y is non-linear, then the estimate of the slope will
# differ systematically at different points along X. modelAssumptions() will
# plot such lines (summarized in pink), overlayed on the residual scores and in 
# addition to the best linear fit (blue-dashed).



# function inside the above function
crPlots(m1, ask = TRUE)

# This plot is referred to as a component residual / partial residual plots. It
# shows you the residuals for a certain predictor, plotted against the DV. If
# the relationship isn’t a line, then we are concerned about the assumption of
# linearity of predictors (i.e., their relationship with Y). All that detail aside, 
# if the dotted blue line and the pink line look similar, then the relation between 
# that X and Y is relatively linear. Otherwise, there may be a non-linear relationship.

# So, we're actually doing pretty well in terms of the linearity assumption. 
# There aren't any places where the blue and pink lines radically diverge.


# So, what now? -----------------------------------------------------------

# We have discovered that our data violate the constant variance and normality
# of errors assumptions.  We will attempt to correct some of these issues by
# applying transformations. But here is an interesting point: we detected our
# outliers in a model with the raw scores. The outliers may be different in the
# model based on transformed data.

# To be thorough, it might be worth testing whether adding back these points
# correct any of the violations. That is, we apply the transformation to all
# observations (including the outliers), and then check one more time if the
# outliers are still outliers. Spoiler alert for the sake of our lab time: they
# aren't.

# Let's consider why the assumptions were violated.
# Both income and percentage women are strongly skewed. Remember that this
# in and of itself may not violate the model assumption... but it is
# possible to transform the skewed variables to be more normal, and this
# might influence the distribution of residuals and satisfy the assumptions.
hist(d$income)
hist(d$women)
hist(d$education)

# One place to start is by eye-balling the univarite distribution and noting
# the direction of the skew. Log transformations will spread out the lower values
# and condense the higher values--in other words, it will impose a negative skew.

# Raising to a power between 0 and 1 (where raising to 1/2 is the square root, 
# 1/3 is the cubed root, etc.) will impose a similar transformation, but may be 
# a little less intuitive.

# Powers greater than 1 will skew in the opposite direction.

# Now that we have a better understanding of what transforming data can do,
# let's return to our dataset and use modelBoxCox to help us figure out what
# transformatation to use.

# modelBoxCox finds a power to raise your dependent variable to that is most
# likely to correct model assumption problems.


# These results recommend raising income to the .51 power.

## Before we apply a transformation what do we need to check about the values of income?  
# ARE MOST OF THE VALUES BETWEEN 0 and 100?  


# We therefore need to change the variable before applying a power transformation. 
# Will adding a negative start (e.g., -610) help? No, because even with this 
# negative start most values will be in the thousands. 
# >> Solution: 

# Creating a new variable:
# Note we are using the Box-Cox power transformation function:
## the bcPower function computes the scaled power transformation of x = U + gamma
## where gamma is set by the user so U + gamma is strictly positive for 
## these transformations to make sense.


# A square root transformation on the predicted variable is not the easiest 
# thing to interpret.

# Every additional year of education leads a 9.54 unit increase in the square 
# root of the income... Great... I still have no idea what this means...

# But, clearly we want to spread out the lower values and contract the high
# values, because that is what powers between 0 and 1 do. Logs do this too, so
# we might transform by "log base 2" using log2().

# What is the logic of log (base 2)? What does a 1-unit increase in a log2 
# variable represent?



# Creating a new variable and model with an adjusted and transformed version
# of our outcome variable "income" (dividing by 100 to bring values into 
# operable range for transformation)


# How do we interpret the coefficients of this model? 
# Every year of additional education leads to a 0.17 unit increase in log2(income/100).

# But again, this isn't very intuitive. What could we do to make it make more
# sense? We could make sure we phrase the result such that it relates to a
# one-unit (interpretable) increase in log2 income.

1/.17 # about 6. so...
# 

# NOTE: There are other ways to convert logs to more interpretable units, and I
# encourage you to look into it if this is something you encounter in your own
# research.

# So, did this fix our model assumptions?
modelAssumptions(m_income_log2, "NORMAL")
modelAssumptions(m_income_log2, "CONSTANT")
modelAssumptions(m_income_log2, "LINEAR")
# ...a little bit, but we still have some problems.

# Let's think of the bulging rule. We can also transform x down to help solve 
# some issues.

# Let's try transforming percent women instead 


modelAssumptions(m_women_sqrt, "NORMAL")
modelAssumptions(m_women_sqrt, "CONSTANT")
modelAssumptions(m_women_sqrt, "LINEAR")
# This transformation doesn't really improve the viability of our assumptions 
# and complicates the interpretation of our regression coefficients.  
# Probably not worthwhile.
## Remember we do not assume that predictors have a normal distribution.

# Remember that violating the assumptions of linearity, equal variance, and
# normality do not mean a model is nonredeemable. Normality is the least concerning.
# They inflate standard error, and violating linearity may bias the parameter
# estimates, but all three can often be handled through transformations.

# Our next step might be to consider models of the data that make different
# assumptions about the distribution of residuals. It may be that the GLM is not
# the right tool for the job. That said, remember that GLM is quite robust to
# violations of normality and constant variance. Linearity is a little more
# troublesome, but even so small violations are not worth panicking and running
# to find a more exotic method. Take a measured approach and seek advice from
# those with relevant expertise if you feel your data violate the GLM
# assumptions in an irreparable way. We will explore some options later in the
# course (including looking at quadratic and other non-linear effects).

# But, let us be satisfied with the model with log2(income)
# ...after we conduct a case analysis on it! All the threats of influential data
# points still exist in models with transformed variables. We'll want to check
# if there are any points worth removing.

hist(rstudent(m_income_log2))
which(rstudent(m_income_log2) < -5.5)
d[d$job_id == 51, ] # commercial travelers

hist(cooks.distance(m_income_log2))
which(cooks.distance(m_income_log2) > .12)
# we know that 51 is commercial travelers, let's see what 61 is
d[d$job_id == 61, ] # bartenders

# What did we learn?


# Let's go ahead and remove these problem cases, even if it's a bit of a liberal 
# choice

d <- d %>% filter(!name %in% c('bartenders', 'commercial.travellers'))

# and let's re-fit our model
m_income_log2_b <- lm(income_log_t ~ education + women, data = d)
summary(m_income_log2_b)
summary(m_income_log2)
# Did our parameter estimates change when we removed these outliers? Standard errors?
# F statistics?


# Questions? Good luck on CA2!
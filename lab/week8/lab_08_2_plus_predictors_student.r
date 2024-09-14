# Week 08 Lab
# Spooky three predictor models & VIF
# Friday, October 27, 2023

# Introducing the data ---------------------------------------------------------

# We're going to use some spectacularly sPoOkY data this lab, it's on:
# Halloween consumer behavior.

# Imagine a hypothetical study by a local newspaper. 
# Earlier this year, 79 community members responded to a survey about how they 
# have celebrated Halloween in the past and how they plan to celebrate 
# Halloween this (coming) October. 
# The survey included many items, including the followings:

# In the past, you have often celebrated Halloween by wearing a costume.
# In the past, you have often celebrated Halloween by decorating your home.
# For Halloween this year, it is likely that you will buy some costumes.
# For Halloween this year, it is likely that you will go shopping before September.

# The participants responded to a Likert scale, where -2 = strongly disagree, 
# 0 = neither agree nor disagree, and 2 = strongly agree.


# Set up -----------------------------------------------------------------------
Sys.setLanguage("en")
library(tidyverse)
library(psych)
library(car)
library(effectsize) 
library(rockchalk) 
library(data.table)
source("610710_functions.R")

# Data Wrangling Techniques -----------------------------------------------------


# Data files in real life are often a disaster when you receive them: no variable
# names, weird coding schemes, and separate data files that need to be combined. 

# The data we are using today are split into three data files. The three data 
# files each contain different variables we need, and they share a column of ID 
# variable. We can use the ID variable to merge the three data files into one dataframe. 

# Let's read in all csv files first! Proudly introducing the fread() function

# Now figure out what fread() does!


# Read in the data using fread()



# Left, right, and full join ---------------------------------------------------
# We will use left_join(), right_join(), and full_join() to merge dataframes.
# These functions combine dataframes column-wise using a shared variable (e.g., participant ID)
?left_join # This is from dplyr

# Let's do a quick demonstration here.
# Here we create 2 dataframes
d_demo_1 <- data.frame(id = c(1,2,3,5), # Demo 1 has participant 1, 2, 3, 5
                       happy = c(3,3,1,4) # Happiness Score
                       )
d_demo_1
d_demo_2 <- data.frame(id = c(1,3,4,5), # Demo 2 has participant 1, 3, 4, 5
                       sad = c(1,4,2,1) # Sadness Score
                       )
d_demo_2
# Woo... Participants do not match across dataframes

left_join(d_demo_1,d_demo_2)
# left_join() retains all rows in the dataframe on the left side
right_join(d_demo_1,d_demo_2)
# right_join() retains all rows in the dataframe on the right side
full_join(d_demo_1,d_demo_2)
# full_join() retains all unique rows in the dataframe from both dataframes


# Let's combine our raw dataframes into one ------------------------------------
# Note left_join(), right_join(), and full_join() can only deal with 2 dataframes
# at a time. 
# Remember to assign the output back to an R object



# Force the output to be a dataframe. This is because the default output is 
# data.table, which will cause complications if used with other functions (i.e., varScore).




# Clean up the data ------------------------------------------------------------
# Use some functions to inspect our data.



# Is there anything weird about the data we need to adjust? 



# We'll need to clean up these variables them before analyzing them.

# sub() and gsub() can be handy for finding and replacing patterns in strings
?sapply
?sub

# Here we're using sub to find "response:" and replace it with "" (nothing!)
var_names <- names(d_raw)[-1] # create a vector of variable names
d <- d_raw %>% mutate_at(
  var_names, 
  # input the names of the variables we want to manipulate
  function(x) as.numeric(sub("response:", "", x))
  ) 
  # apply to each variable a function that replaces "response:" with "", and 
  # then force that variable to be numeric

str(d) ## Looks like that fixed it! All the values are numeric now!

# Let's shorten the names of the variables we'll be using since they're a little 
# unwieldy.
names(d)
# Just do that for the 4 variables we are going to use
old_names <- c("celebrate_wear_costume", "celebrate_decorate_house", 
             "plan_to_buy_costume", "go_shopping_before_september")
new_names <- c("costume", "decorate", "plan_buy_costume", "shop_early")
setnames(d, old_names, new_names) 
# very important you have the same order for old names and new names


# Analyses --------------------------------------------------------------------

# You are a small-scale Halloween retailer. To prepare for the early Halloween 
# market (pre-september), you hope to gain some insights on what the market will 
# look like using these data. 

# Run some commands to get a general sense of the data. 
str(d)
head(d)
describe(d)


# let's view correlations, distributions, and scatterplots all in one step.
GGally::ggpairs(d[,-1])

# Which 2 variables are the most weakly correlated here? 
# plan_buy_costume and decorate.


# Most strongly correlated?
# decorate and costume


###################
# Exercise Time!! #
###################

# Regression with 1 predictor --------------------------------------------------
# Note: We do not encourage progressively adding predictors when analyzing data
# (think about researcher degrees of freedom!). We're doing it for pedagogical 
# purposes in lab so you can see what adding predictors can do to results. 

# You wonder if people who often celebrate by wearing a costume tend to shop early for Halloween.
# Run a model to see if costume predicts shopping before September (shop_early). 




# 1a. What's the result? 



# 1b. What is the interpretation of the coefficient for 'costume'? 



# Regression with 2 predictors -------------------------------------------------
# Why might we add another predictor(s)? (two main situations apart from mediation)




# New question: 
# You wonder celebrating by wearing a costume predicts shopping before September 
# when statistically controlling for celebrating by decorating one's home.

# I.e., Do people who often celebrate by wearing a costume tend to shop early for 
# Halloween, after accounting for how often they celebrate by decorating their home.

# Run a model to see if costume predicts shopping before September (shop_early). 




# How did the regression coefficient, its standard error, F statistic, and p  
# value associated with 'costume' change?




# 2a. What's the interpretation of the coefficient for 'costume'?




# 2b. What's the interpretation of the coefficient for 'decorate'?




# 2c. How much of the variance in 'shop_early' is uniquely explained by each of 
#     the two predictors?




# 2d. How does the delta-R^2 of 'costume' in m_2 compare to the R squared of 
#     'costume' in m_1?





###################
#  End Exercise!! #
###################

# There is an increase in the SE for 'costume' from m_1 to m_2. 
# It increases from 0.12 (in the 1 predictor model) to 0.24 (in the 2 
# predictor model). The coefficient also decreased. Why?

# We can find a partial answer by revisiting the correlation between the variables.
GGally::ggpairs(d)

# 'costume' and 'decorate' are strongly correlated (r = .866)! Why might that be?
## People who tend to celebrate Halloween with costumes also tend to be into
## decorating the house. 

m_costume <- lm(costume ~ decorate, data = d)
(m_costume_sum <- summary(m_costume))
# In fact, 'decorate' accounts for 75% of the variance in 'costume'!

# Because of the strong correlation between 'costume' and 'decorate', it's 
# likely that a lot of the variance in 'shop_early' explained by 
# 'costume' overlaps with the variance explained by 'decorate'.

## Draw a pie chart.

# When variables in the model are very highly correlated, this is called
# "multicollinearity." When multicollinearity occurs, it inflates standard
# errors and changes the magnitude of regression coefficients.

# We can directly test the multicollinearity of each predictor in our model 
# using this function:

vif(m_2)

# vif stands for "variance inflation factor"
# A conventional cutoff for vif is 5, such that if you have a vif value >= 5 
# for your focal predictor, then multicollinearity is a problem. 

# Good-to-know knowledge:
# VIFj = 1 / (1–R_j^2)
# What is R_j^2?
## R_j^2: percent variance in predictor j that is explained by all other predictors

# What level of R_j^2 is problematic, given the conventional cutoff for vif = 5?
## When R_j^2 > 0.8

# Regression with 3 predictors -------------------------------------------------
# As a Halloween retailer, you want to know if people who actually plan to purchase 
# costumes are more likely to shop for Halloween early on (before September), 
# when controlling for the degree to which they celebrate Halloween by wearing 
# costumes and by decorating their home.





# Which is the focal predictor?


# 3a. What is the interpretation of the intercept for m_2?

# The predicted score of self-reported likelihood to shop for Halloween before 
# September, for people who responded with exactly ratings of 0 to the 
# 'plan_buy_costume', 'costume', and 'decorate' items on the survey.



# 3b. What if we want the intercept to reflect likelihood to shop for Halloween before 
# September for an average person (in terms of 'plan_buy_costume', 'costume', and 'decorate')? 





# What is the interpretation of the intercept now?
# The predicted score of self-reported likelihood to shop for Halloween before 
# September, for people who responded with average ratings to the 
# 'plan_buy_costume', 'costume', and 'decorate' items on the survey.

# Compare the model summary output between m_3 and m_3_c. What changed? What
# hasn't changed?
m_3_sum
m_3_c_sum
# Intercept changed, none of the slopes changed.


# 3c. What is the interpretation of the coefficient for 'plan_buy_costume' and
# what does the coefficient tell us?





# 3e. After removing the variance explained by ‘costume’ and ‘decorate', how much
# of the residual variance in 'shop_early' does 'plan_buy_costume' account for?
Anova(m_3) %>% eta_squared() # about 18%




# Publication-quality graphing -------------------------------------------------

# Create a plot showing the effect of 'plan_buy_costume' on 'shop_early' when 
# controlling for 'costume' and 'decorate'

# Create m_graph (aka the model we will use in our graph) -- in this case it is 
# the same model as m_3 but we like to keep them separate for clarity








d_graph <- ggplotPredict(m_graph, d_graph)

plot <- ggplot(data = d, aes(x = plan_buy_costume, y = shop_early)) +
  geom_point() + # raw data points... there's only 6 so we don't need anything fancy
  geom_smooth(data = d_graph, aes(y = Predicted, ymin = CILo, ymax = CIHi,
                                  color = "Multiple Regression", fill = "Multiple Regression"),
    stat = "identity", alpha = .2) +
  theme_bw(base_size = 14) +# remove the background, increase axis font size
  labs(x = "Plans to Buy Costumes", y = "Shop for Halloween Early")
plot + scale_color_manual(name = "Regression Model",
                     labels = c("Multiple Regression"),
                     values = c("purple"))+
  scale_fill_manual(name = "Regression Model",
                    labels = c("Multiple Regression"),
                    values = c("purple"))

# This line looks slightly "off" from the data, because we're controlling for
# (unseen) variables but still plotting raw data points. Let's look at what would
# happen if we add the regression line from the one-predictor model.
m_graph_2 <- lm(shop_early ~ plan_buy_costume, data = d)

d_graph_2 <- expand.grid(
  plan_buy_costume = seq(from = min(d$plan_buy_costume, na.rm = T), 
                         to = max(d$plan_buy_costume, na.rm = T), 
                         length.out = max(d$plan_buy_costume, na.rm = T) - 
                           min(d$plan_buy_costume, na.rm = T) + 1)) 
d_graph_2 <- ggplotPredict(m_graph_2, d_graph_2)

plot + geom_smooth(
  data = d_graph_2, aes(y = Predicted, ymin = CILo, ymax = CIHi,
                        color = "Simple Regression", fill = "Simple Regression"),
  stat = "identity",  alpha = .2) +
  scale_color_manual(name = "Regression Model",
                       labels = c("Multiple Regression","Simple Regression"),
                       values = c("purple", "orange"))+
  scale_fill_manual(name = "Regression Model",
                       labels = c("Multiple Regression","Simple Regression"),
                       values = c("purple", "orange"))

## Now we are seeing the effects from all the predictors not just 
## our focal predictor and it is telling a different story!

# Spooky! 


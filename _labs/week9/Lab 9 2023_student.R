##  ##############################################  ## 
##  Week 9 Lab: Dealing With Messy Data - Outliers 	## 
##           Friday, November 3, 2023               ## 
##  ##############################################  ## 

## Case Analysis and multiple predictor models review! 
# Prep R and Source Functions ------------------------------ 

# We will need the following packages loaded 
library(tidyverse) 
library(psych) 
library(car) 
library(effectsize) 
library(rockchalk)

# Using source(), specify the location of the file "610710_functions.R" 
# Note: we have updated the 610710_functions file, you must download the new version
source("610710_functions.R") 
# This directory is specific to my computer, yours will be different. 

# Case Analysis with the prestige data set --------------------------------------
# We're going to work with the Prestige data set again, but we've modified it, 
# so download the file from the course website. 

# Now let's import the data. It's a dat file (tab delimited), so we can 
# use "read_tsv" for import. 
# When you import these data, you'll see that read_tsv creates a column name  
# called SubID. Rename this to something more sensible, given that the  
# observations aren't subjects, but rather occupations. 

d <- read_tsv("lab_09_prestige.dat") %>% rename(job_id = SubID)


# 1. Univariate Statistics ------------------------------------------------ 

# Let's start with some basic data exploration. This should be the first  
# step in any analysis. 

describe(d) 

# Do the means look appropriate? - yes
# Do the SDs make sense?  - yes
# Is this the range we would expect for these variables? - education  = -10.7 


# We can create a scatter plot with education on the y-axis and job_id on
# the x-axis. We see that one case is far away from all others. 

plot(d$job_id, d$education)

# Identify which case has this anomalous value 

d[d$education == -10.7,]
tail(d)
d[d$education < 0,]

# Remove it. 

d <- d[d$education >= 0, ]
describe(d)

# What other logical statements could I put in the bracket notation to achieve 
# the same goal? 

d <- d[d$education != -10.7,]

# 2. Simple plots ----------------------------------------------------- 

# Let's say that we're interested in predicting income from years of education  
# and percent women in that occupation, with education as our focal predictor.
# So, those are the variables we're going to focus on. Let's plot education, 
# income, and pct women, the variables we'll be working with, one at a time with 
# each of the varaibles on the y axis and job_id on the x-axis. 

plot(d$job_id, d$education)
plot(d$job_id, d$income)
plot(d$job_id, d$women)

# should we remove the "outliers" here?  
# Not yet

 

# This is an extremely informal and simplistic way to detect outliers.  
# All you're going to find here are highly anomalous values, and you'll only  
# be able to remove things based on impossibility. 

# 3.Correlations ----------------------------------------------- 

# Let's determine how our variables education, income, and women are correlated.

d %>% GGally::ggpairs(c("education", "income", "women"))



# Here, we are getting a sense of how our variables are related to each other.
# We wouldn't make decisions based on these plots, but they might suggest some
# problematic cases. 

# 4. Data visualization --------------------------------------------------- 

# It's helpful to visualize the model that you're fitting. 
# What sort of graph can we use to show the relationship between education 
# and income? 
# A scatterplot will work! Let's also use color to represent percent women. 

ggplot(data = d, aes(x = education, y = income, color = women, label = job_id)) +  
  geom_point() + geom_text(vjust = -.5) 

# This graph shows us that we have some high leverage points and regression 
# outliers. 

# Which are both high leverage and regression outliers? --- 24, 17
# Which occupations are high leverage but not regression outliers? --- 25, 21
# Which are regression outliers but not high leverage? --- 2

# Let's use bracket notation to get the specific occupations of each point 
d[d$job_id == 2 | d$job_id == 24 | d$job_id == 17 | d$job_id == 21 |
    d$job_id == 25, ]

# It's always important to visually inspect your data 
# but before taking action we must look at some diagnostic functions. 

# Diagnostic functions --------------------------------------------------------- 

# modelCaseAnalysis() is one of the functions we sourced.

# Remember, we're interested in predicting income from education and pct women. 
# Let's fit our model: 

m1 <- lm(income ~ education + women, data = d)
(m1_sum <- summary(m1))

# What did we find?

# for every 1 year increase in education, income is predicted to increase
# by $944.88, when controlling for the perctage of women in that occupation

# Education has a significant positive (partial) relationship with income. 
# The percentage of women in the occupation has a significant negative (partial) 
# relationship with income.

# 4. Leverage ------------------------------------------------------------- 

# Leverage is indicated by hat values 
# What is the hat value an index of? 

# It's how far a given value on a predictor variable is from the centroid  
# of the predictor variables. 

## Let's look at some simple plots that show the hat values in histogram form






which(hatvalues(m1) > .06) # gives you the IDs of the observations with hat
# values over .06 (which we identified visually on the graph as the gap in the 
# distribution)

# You can then identify which observations these job_ids correspond to using 
# this code:
d[84, ] 

# Visually identify which of these buckets might have high leverage in our model
### a good rule of thumb is to look for observations on the graph that are
### separated by a gap in the histogram buckets

## Now we are going to use a function from our sourced functions file that makes 
## this basic graphic, but also provides the IDs for each Hat value.

x_hats <- modelCaseAnalysis(m1, Type = "HATVALUES") 

# Looking at our graph we see that ID 84 is far beyond the red line plotted on
# our histogram. Points far beyond this line have high leverage. We can use the
# same code from above to confirm which occupation corresponds to this id.

d[84, ] # sewing machine operators


# Sewing machine operators have high leverage. 
# Why might this be? 

# A job that has relatively few years of education and high percentage of women


# So, are we ready to remove this occupation from our data? 
# no

# 5. Regression outliers -------------------------------------------------- 

# Regression outliers have large residuals 
# What is this an index of? 
# It indicates how dramatically a given observation deviates from the model 
# predictions of the model we've estimated. 



## Let's look at some simple plots that show the studentized residuals in 
## histogram form





which(rstudent(m1) > 3) # gives you the IDs of the observations with hat
# values over 3 (which we identified visually on the graph as the gap in the 
# distribution)

# Visually identify which of these buckets might have problematic residuals in 
# our model
### a good rule of thumb is to look for observations on the graph that are 
### separated by a gap in the histogram buckets

# Let's identify which observations these job_ids correspond to using  
# this code:

d[c(2,24), ] # 2: general managers, 4: physicans


## Once again, we can source a function that provides this basic graph, 
## but also provides the IDs.

# The RESIDUALS graph plots a histogram of the (studentized) residuals for an  
# individual model. There's a vertical line so you can compare the residuals 
# to a Bonferroni-corrected cutoff point. 

x_resids <- modelCaseAnalysis(m1, Type = "RESIDUALS") 


# What does it mean for these studentized residuals to be large? How could they  
# affect our model estimates?  

# It means the observation differs significantly  
# from the predictions made by our model. Regression outliers can (but don't  
# always) change our parameter estimates, but they nearly always inflate the standard 
# error of our parameter estimates. 


# What are the two occupations?
d[c(2,24), ] # 2: general managers, 4: physicans


# Two cases are beyond the red cutoff and are pretty separated from the bulk of  
# the data: 
# The next step is to look the other variables for these regression outliers:

describe(d$income) 



# Should we remove these observations from our data? 
# we do not have enough information to remove them

# 6. Influence ------------------------------------------------------------ 

# Influence: Cook's distance 
# What two characteristics does influence take into account? 

# Leverage and studentized residuals. Observations with a large Cook's D are  
# often extreme on both of these. Note: an observation can have a small residual 
# and a large Cook's D 





# Is it necessary for an influential point to have high leverage?  
# A large studentized residual? 

# Not necessarily, but it will have to be high on at least one of these  
# dimensions. Remember, Cook's D is basically indicating how much our parameter  
# estimates would change if we removed a given observation. 



## Let's look at some simple plots that show the Cook's D values in histogram form




# Visually identify which of these buckets might have problematic Cook's D values in our model
### a good rule of thumb is to look for observations on the graph that are separated
### by a gap in the histogram buckets

which(cooks.distance(m1) > .15) # gives you the IDs of the observations with hat
# values over .15 (which we identified visually on the graph as the gap in the 
# distribution






## Let's use our sourced function for a faster visualization of the points with
## high influence in our model.

x_cooks <- modelCaseAnalysis(m1, Type = "COOKSD") 


# Here, several points exceed the cutoff. This cutoff is only a rough  
# rule-of-thumb, though. What's more important is whether or not the points are  
# part of our distribution of Cook's distance. 

# How many points have a large gap? 

2


# But what are we missing here? 

# We don't know which parameters are being affected by these influential points. 
# Influence: df betas 
# DF Betas calculate influence on a given parameter. It helps us sort out which  
# points affect which parameters. 





# For this one we will need to look at the graph of each of the parameters 
# df beta values individually

# First we can save the dfbetas function output as an object to look at:


# Then we can look at the influence on the intercept:







# Next we can look at our focal predictor: years of education



# Then we can look at our covariate: percentage of women in the field



## Now, as before, we can source a function that allows us to see 
## the IDs of these high influence observations

modelCaseAnalysis(m1, Type = "DFBETAS") 

# you'll notice this works somewhat differently than the others. Here, we get a  
# figure for each of the parameters. You also want to record the row numbers of  
# the observations as you go. 

# 24 on intercept (sort of), 24 on education (sort of), no real problem with 
# percent women in the scatterplots, but we also identify point 002. 


# Influence: The influence plot 

x_infs <- modelCaseAnalysis(m1, Type = "INFLUENCEPLOT") 


# What's going on here? What's the circle size? What are the axes? 
# The size of the circle indicates the Cook's D value. 
# The X axis is the hat value 
# The Y axis is the studentized residual.

# From our Cook's d plot, we already saw that general managers and physicians  
# had high influence. 

# Notice: Sewing operators have high leverage, but this point is not a  
# regression outlier. While we might want to investigate this case further,  
# it seems less problematic. 




# So..... now what?? ------------------------------------------------------ 

# Through our analyses, we found physicians and general managers are ill-fit by 
# our model, have extreme values on the outcome variable (income), and have a  
# large influence on our parameter estimates. Furthermore, including them  
# inflates the standard error of our parameter estimates. 
# We also identified one case that has high leverage but is well-fit by our model. 

# Key Questions ----------------------------------------------------------- 

# 1. Can we explain this result from some kind of error in coding? Can we correct  
# the error(s)? No, we did not collect these data.



# 2. What is the impact of removing the cases? 

d_new <- d[d$name != "general.managers" & d$name != "physicians",] 

m2 <- lm(income ~ education + women, data = d_new) 

summary(m1) # including problem observations 
summary(m2) # removing problem observations 




# What changes? Conclusions? Precision? 
# In this case, our conclusions do not change, but our precision improves. 
# In a real-world setting, if we did choose to remove them, we would have to report  
# this in our Results section, and we'd have to provide some reason for doing so  
# (see below). 


# Reasons for exclusion (Read this independently then start the exercise) --------------- 

# Here are a couple questions you might ask before you start checking for 
# outliers: 

# What would we consider too extreme of an observation on a predictor variable? 
# You should try to answer this question in terms of practical raw values as  
# opposed to hat values (e.g., if someone reports they are over 150 years old 
# on an online survey, they should be removed from our data). 

# Can we identify some other variable that might contribute to outliers? 
# One example is recording a subject's time spent completing a questionnaire and  
# setting cutoff points for what 'acceptable' times are based on pilot-testing 
# (e.g., throwing out anyone who completes the survey in under 2 mins). 

# What is the process we'll use to identify outliers, and what will we do with  
# them when we find them? It's very difficult to accuse you of p-hacking if you 
# can point to your pre-registration and show that all of the actions you took 
# to deal with outliers are wholly consistent with what you said you were  
# going to do. 

# Once you have collected your data and identified outliers, there are a few  
# more questions you might ask about these observations that can help give  
# justification to your decision to exclude them: 

# Was there human error in entering this value? 
# In certain cases, you can go back to the source and see whether this  
# observation is simply due to data entry error. We love situations like this 
# (because they're pretty easy to fix). 

# Was anything about that participant's experimental session weird? 
# Maybe the power went out, they got a phone call, or they fell asleep. If this 
# is a relevant concern, someone should keep a notebook where they jot down  
# observations of strange occurrences when running a given participant. 

# Does it make sense that their value is extreme? 
# If we ran the Prestige study in America in 2023, we would have an even more 
# skewed income graph: people in top positions are paid astronomically more  
# than working-class people. We might be able to tell a compelling story about  
# why these extreme observations have so much influence, and our (theory-based)  
# argument could give us reason to exclude them.  

# Based on the claims we're trying to make with our research, can we exclude  
# these observations? 
# Removing extreme observations can, in certain cases, affect the external  
# validity of our research. For example, if we exclude top earners when we  
# analyze the prestige data, we can't claim that our conclusions apply to those 
# professions. However, we will have more precise parameter estimates for the  
# large majority of occupations we've included in our sample. We have to note  
# these shortcomings by being precise with our conclusions. 

# Exercise (3+ predictor models) -----------------------------------------------

# Your friend Mitchell did a classroom climate intervention study. Classrooms were
# randomly assigned to either receive a brief prejudice reduction intervention 
# on the first day of class or not. Then, two months later, students in these 
# classes were invited by their instructors to participate in a campus climate 
# survey, which comprised a number of outcome measures of interest. Today, we’ll 
# be looking at only one of these outcomes: concern about discrimination. 
# It is argued that concern about discrimination is a good indicator of someone’s
# positivity towards and familiarity with members of different social groups, 
# as well as their tendency to engage in inclusive behaviors. Concern was 
# measured using a 4-item scale, and the items were written generally 
# (i.e., they were not specific to one target of discrimination). 
# Mitchell also recorded if the classroom was a STEM classroom. Participants
# completed a simple two-item measure of political affiliation, already averaged
# for you.

########################## Code Book ####################################
####### Items of the concern scale --> 1 = low concern, 7 = high
####### political orientation (pol) --> 1 = very liberal, 5 = very conservative
####### STEM --> -0.5 = non-STEM, 0.5 = STEM
####### Condition --> intervention = 0.5, control = -0.5

# Mitchell expects the intervention to have a positive effect on individuals’ 
# concern about discrimination.

# 1. Read in the data from "lab_09_classroom_climate_data.csv" and explore it







# 2. Check the reliability of the concern scale and create a composite score for 
# concern





# 3. Do students who received the intervention have higher concern scores than 
# those who didn’t? Test this question using a linear model




# 4. Interpret the model coefficients (betas) for the intercept and the 
# predictor variable (condition).

# Note: Condition is already centered (-.5 and .5), this will affect interpretations
# b0: 

# b1: 


# 5. Mitchell has learned that both being in a STEM course and political 
# orientation are strong predictors of certain outcomes in the prejudice domain. 
# Rerun the model from question 3, this time controlling for both STEM and 
# political orientation. 





# 6. Interpret the model coefficients (betas) for the intercept  
# and the three predictor variables (condition, stem, and political orientation).

# Note: Condition is already centered (-.5 and .5), this will affect interpretations

# b0: 

# b1: 

# b2: 

# b3: 



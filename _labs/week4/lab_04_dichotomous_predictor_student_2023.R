###############################################
#### Week 4 Lab: One Dichotomous predictor ####
####     Friday, September 29, 2023       ####
###############################################

# 1. Packages --------------------------------------------------------------

# install the "readxl", "effectsize", and "gridExtra" packages.
install.packages('readxl')
install.packages('effectsize')
install.packages('gridExtra')

# We'll need readxl for data import.
# We'll also need tidyverse, psych, and car. 
# Let's load them all now.

library(tidyverse)
library(readxl)
library(psych)
library(car)
library(effectsize)
library(gridExtra)

source("610710_functions.R")

# 2. Mental Paper Folding Revisited

# Recall our study from last week...

# This study examines how well 5-year-old children can mentally visualize paper 
# folding and how accurately they fold paper physically.
# For the mental paper folding part, children were shown a fold and had to imagine 
# how the paper would look after being folded. They did this 10 times, and we have 
# their performance recorded as either 1 (correct) or 0 (incorrect) for each of 
# these 10 trials, labeled as mpf_01 to mpf_10.

# For the physical paper folding task, children were given a piece of paper with 
# a line and were asked to fold it as closely as possible to that line. 
# The "distance" column measures how far their actual fold was from the expected 
# fold in millimeters. If they folded over the line, the number is positive, 
# and if they folded under the line, the number is negative.

# When we ended last week, we had produced a group in which we regressed distance,
# our outcome variable, on the number of correct mental folds, our continuous 
# predictor variable. However, we have not yet answered the question: is this 
# model a better predictor of distance than our basic (mean only) model.

# Let's read in our data


# As a review, go ahead and take a few minutes to get a sense of the data (confirm
# that the dataframe is how you expect it to appear). When you have completed this
# step, create your composite score of mental paper folding (mpf_m). Confirm that
# mpf_m has been correctly added to your dataframe.








# Let's re-run our 0 and 1 predictor models:



# As a reminder, we compared these two models using the Anova function



### Models with 1 Predictor ###

# Unlike last week where we "bought" 1 piece of information, the mean distance
# score, now we'll "buy" two pieces of information, the mean distance score and 
# each participant's mental paper folding score
# distance = b0 + b1 * mental_paper_folding

# Mental_paper_folding is one of the additional pieces of information we'll add 
# to the right of the ~


# What's that 1 tell R, again?
# Here, the "1" says that we want to estimate the intercept for the model.


# We can again compare model 2 and model 1 explicitly:



# we can also compare model 2 with the mean only model using summary:



# Note: the statistics shown in the line labeled “mpf_m” are in fact the result
# of a model comparison between the mod_2 and a model that is identical to mod_2 
# but does not contain mpf_m.

# We can also call specific parts of the summary object in order to calculate F.







# How would you look at the effect sizes for the model?



# eta_squared provides our partial eta squared.

# If we want a 95% confidence interval for our slope we can use confint()



# How could we summarize our results from this output?








### 3. Introducing our new (fictional) study ###

# Researchers aimed to determine whether a perspective-taking intervention could 
# enhance  children's attitudes towards inclusive behavior (i.e., the extent to 
# which children think including others in a group is important). The children 
# were divided into two conditions: one condition engaged in perspective-taking 
# related  to emotions when they saw a child being excluded from a group, 
# while the other condition did so in response to a different situation that 
# could upset a child but didn't involve social exclusion.

# In the intervention condition, participants watched a video depicting social 
# exclusion (i.e., a child was excluded from a group of other children playing).
# In the control condition, participants watched a video without any social 
# exclusion (i.e., a child is playing alone in an otherwise empty classroom). 
# All participants answered a question designed to assess their attitude towards 
# inclusive behavior. A score of -3 indicated they thought inclusion was "very 
# unimportant", a score of 0 indicated they thought inclusion was "neither
# important nor unimportant", and a score of 3 indicated they though inclusion
# was "very important".


# What is the independent variable? 

# What is the dependent variable? 



# For the sake of clear labels, in the dataframe we will refer to participants'
# attitudes towards inclusive behavior as "Inclusion".

# Prediction: Participants who take part in an perspective-taking intervention 
# are more likely to have higher inclusion scores (i.e., more positive attitudes 
# towards inclusive behavior) than participants who do not participate in an 
# a perspective-taking intervention.


# Read in the data:


# Note: You have to specify the sheet of the excel document if there are 
# multiple sheets

# Check out the dataframe



# We can also determine all of the column names with the names() function



# To test this prediction, which variables will we need from the dataframe?
# We'll need the IV and DV. 


# Will we need to create or modify any variables?
# We do want to create a numeric version of the IV (condition). 
# No need to center anything.

# 4. Prepare/Wrangle Data -------------------------------------------------

# Let's first make a new dataframe with all our 
# variables in snake case using 'janitor'.





 #did it work?

# Today we are only interested in the variables 'condition' and 'inclusion'
# We can use bracket notation to create a dataframe with only those variables.
# Let's give that dataframe a new name because we do not want to overwrite 
# an existing dataframe.



 # did it work?

# Glimpse() shows us a small sample of our dataframe including a variable's type. 

# Let's get some descriptive statistics.
# Find the average inclusion score for each condition




####################################################################
#### Fitting a model with a dichotomous predictor              ####
####################################################################

# The variable "condition" is currently a character variable. 
# Although you can make a linear model with a character variable, numeric
# variables make centering and interpretation easier. 
# So, let's recode our character variable into a numeric variable.




# Note, we could also center this variable as 0 and 1, 1 and 2, etc. As long as 
# our two numbers have a difference of 1, our interpretation of b1 will not be
# affected in the model we are going to run. The only part of our model that will
# change is b0 (and thus our interpretation of b0).

# Did it work? How could we check?



# we could also use the table function to see the one variable



# Let's see if the intervention affects children's inclusion scores
# How do we do this?
# We regress inclusion scores on condition. Remember: We always regress the 
# outcome variable on the predictor variable (or variables).




# Even though "1" (the intercept) is not explicitly stated in our model,
# R will always estimate it automatically.

# Let's get a summary of the model.



# Note: All of the inferential statistics associated with the “condition_c” 
# predictor are the result of a model comparison between the model written in 
# the R script (mod_3_sum) and the mean-only model.


# We can also call specific parts of the summary object in order to calculate F.



# How many degrees of freedom would we report with our F stat?



# How would you look at the effect sizes for the model?



# eta_squared provides our partial eta squared.

# If we want a 95% confidence interval for our slope we can use confint()



# Does our CI for condition_c contain zero? 

# Why do we care if our CI for condition_c contain zero?



# How would you report the stats for our predictor variable from the previous 
# analyses?



# How would you interpret b1 and b0? What conclusions can we draw?












# Real-world interpretation of b1?



# Real-world interpretation of b0?






#################################
####        Plotting         ####
################################# 

# Last week we started reviewing the basics of ggplot. We are going to continue
# that review in today's lab with an introduction to graphing bar charts, which
# are often used when depicting the results of studies with categorical predictors


# Let's start the same way we did last lab by specifying the model we want R
# to graph for us using ggplot.

# Here's our model: 




# Next we'll create a dataframe with just our inputs for condition_c. Last week
# we used 'd_graph <- data.frame(mpf_m = seq(0, 10, length = 200))' for our 
# continuous variable. What might we do this week with a dichotomous predictor?



# Next we can use the ggplotPredict function to generate error bars. What do
# these error bars represent?





# let's look at the predictions:



# We want a graph with meaningful labels. Right now, the values
# of condition are -.5 and .5. 
# We'll add a column to the dataframe that has labels so we can call that
# column in ggplot later on.




# check it!
d_graph # Now we can make a graph!

# First, we specify the dataframe we're using (d_graph)
# Then, we establish our x and y variables (condition and our predicted
# values)
# Then we use fill to tell R we want the colors of each bar to represent the 
# condition.



# Now we'll graph the bars. We are graphing the predicted values straight out 
# of the dataframe we generated with ggplotPredict. What are the points? 
# We can check by calling d_graph.





 # stat = identity tells r that we are
# going to provide the data points directly. Otherwise, R would need to calculate
# them on its own. 


# Now we add error bars.
# What will our error bars represent?







# Again, we can provide r directly with the data we want it to use for the error
# bars. ymin and ymax will specifcy the lower and upper bounds of the error bars



# Here is a new step. Adding our raw data to the graph. 



# Here we use geom_point. We specify where the data points will come from. We can't
# use d_graph because it only has our predictions. So, we must use d. Next, we specify
# the variables for the x and y axes. 
# position tells R we want to adjust the location of our data points. Specifically,
# we want to jitter our points horizontally to improve visibility and decrease 
# clutter. DO NOT jitter vertically. This will misrepresent your data. 
# Alpha determines how opaque our data points are. 




# Why do show our raw data?
# 1) So we can see the range of our data
# 2) So we can get a sense for effect sizes/within vs. between group differences
# 3) To show if there are outliers
# 4) Transparency in the sharing of our findings and data

# Looking good. Let's add finishing touches now.
# We will add labels, add a theme, emphasize the x axis, and remove the legend
# because it's redundant. 
# Note that this is done with multiple functions that we are adding all at once.
# Each new function is separated by a "+"







# Let's see what we have now!



# Lookin' Good!










# In the future you probably would not want to write out each step of the code
# adding onto the plot. Instead, we can create the the graph in 3 steps.

# Step 1 - define the model
m_graph <- lm(inclusion ~ condition_c, data = d3) 

# Step 2 - create the information for d_graph
d_graph <- data.frame(condition_c = c(-.5, .5))
d_graph <- ggplotPredict(m_graph, d_graph)
d_graph$condition <- dplyr::recode(d_graph$condition_c, 
                                   "-.5" = "Control", ".5" = "Intervention")

# Step 3 - format the graph
plot1 <- ggplot(d_graph, aes(x = condition, y = Predicted, fill = condition)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = CILo, ymax = CIHi, width = .25)) +
  geom_point(data = d3, aes(x = condition, y = inclusion),
             position = position_jitter(width = .2, height = 0),
             alpha = .3) +
  labs(title = "Perspective Taking and Inclusive Behavior", 
       x = "Condition", y = "Attitudes Towards Inclusive Behavior") + 
  theme_bw() + 
  geom_hline(yintercept = 0, color = "red") + 
  theme(legend.position = "none") 

plot1

# You can add this code to your "plotting toolbox" which we started building 
# in the last lab. In the future, if you are asked to create a bar graph for a 
# model with a dichotomous predictor and a continuous outcome variable, you can 
# edit this code.


# Begin ggplot exercise in small groups! --------------------------------------



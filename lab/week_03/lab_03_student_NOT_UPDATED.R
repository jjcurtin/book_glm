## # # # # # # # # # # # # # # # # # # # # ##
##    Week 3 Lab: General Linear Model     ##
##      Friday, September 22nd, 2023       ##
## # # # # # # # # # # # # # # # # # # # # ##

#### Style guide -------------------------------------------------

# We've created a style guide and included it in today's lab files. 
# Let's spend a couple minutes to go over it together.


#### Beep beep! --------------------------------------------------

#install.packages("beepr")


# beepr can actually be quite useful in serious work. While running code that 
# takes a long time to execute, you may work on something else while waiting 
# for it to finish. Calling beep at the end of your script signals that the 
# long process is done.


#### Setting up your workspace --------------------------------------------

# Load packages -- we need tidyverse, psych, and car today


# Set your working directory
# Session > Set Working Directory > To Source File Location


########## Paper Folding Study
# Read in and view the data.


# Learn about the variables present in the dataframe.


# Study Description:
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

# Research Question: Does mental paper folding accuracy predict actual paper folding?
# *** What is the predictor?
# *** What is the dependent variable?

# Prediction: Children with higher mental paper folding accuracy will have 
# lower distance scores (i.e., more accurate physical folding).

# Restate the question we are asking: Do mental paper folding scores explain 
# variance in actual paper folding ability?


#### Prepare data for analysis -------------------------------------------------

# Create a variable called "mpf_m" that represents the average of a child's 
# mental folding accuracy on the mental paper folding task.



# Use varScore from our sourced package.
# But first, we must source it in.




# Now learn a little about this variable.


# View the distribution of scores.


# Let's add x and y-axis labels.



#### The null (stupid) model ---------------------------------------------------

# Specification of the null model:
# No piece of information, so we're estimating no parameters (P = 0)
# Distance = B0 (B0 = 0)



# Step 2: Compute the total prediction error
# by adding up all of the error terms (= squared errors)


# Sum of squared errors (SSE):


# Is this a good model? 



#### The mean-only (basic) model -----------------------------------------------

# Specification of the mean-only (basic) model:
# Buy one piece of information, the mean, so we're estimating one parameter.
# (P = 1)
# Distance = b0  

# Step 1: Make the best predictions, given the information you have
# Since we have the mean, we'll guess that for everyone.

d$predict_mean = mean(d$distance)

# Step 2: compute the total prediction error and SSE


# Is this a good model? 


# By "better" we mean: Is the increase in complexity worth it?
# Another way to put it is to say "by estimating an additional parameter, are 
# we getting a model that has a drastically lower SSE?"

# Is the mean-only model a better model than the null model?
# Just because the sum of squared errors is smaller doesn't necessarily mean the 
# model is better.
# To answer this question, we need to explicitly compare these models!


#### Model comparison ----------------------------------------------------------

# Is Model 1 significantly better than Model 0?
# This question is answered using the F statistic. 
# Roughly speaking, F is the amount of error reduction (by adding the additional 
# parameter(s) to the model) divided by the average prediction error of the 
# augmented model. ["average" because we are dividing the SSE(A) by the number 
# of parameters that we have not yet used up]
# Generally, when this ratio is > 4 (i.e., when the error reduction is at least 
# 4 times larger than the average prediction error) we conclude that it's worth 
# it to add that additional parameter. 

# First we are going to calculate the F-value by hand.

# In order to calculate F, we need to know how many observations (n) were in the 
# data set. 
n <- 200


# F formula --------------------------------------------------------------------

# f = 
# [(sse_c - sse_a) / (n_parameters_a - n_parameters_c)] / 
# (sse_a / (n - n_parameters_a))

# The F stat is a ratio comparing two quantities: 

# 1. The numerator: (sse_c - sse_a) / (n_parameters_a - n_parameters_c) 
# is the average reduction in SSE from the compact to the augmented model, per 
# each parameter added to the augmented model that was not in the compact model. 
# So, we calculate the difference in SSE between the models and divide that by 
# the difference in the number of parameters between models. 

# 2. The denominator: (sse_a / (n - n_parameters_a)) 
# takes the SSE from the augmented model and says: "of the ERROR left after we 
# fit this model, how much error might we expect to explain with the addition of 
# any given new parameter?"
# We calculate this value by taking the SSE from the augmented model and dividing it 
# by the number of parameters we could still estimate. Since we can never 
# estimate more parameters than we have observations (i.e., n_parameters_a 
# cannot be greater than n, the sample size) the number of parameters we still 
# could have estimated in the augmented model is the total number of available 
# parameters (n) minus the number of parameters we estimated in the augmented 
# model (n_parameters_a). 
# See also, Judd et al., "Data Analysis" 3rd ed. pg 54

# In the present case:


# Let's compute the F stat.

f_stat <- ((sse_0 - sse_1) / (n_parameters_a - n_parameters_c)) / (sse_1 / (n - n_parameters_a))
f_stat

df_n <- 1 - 0 # How many more parameters are in model 1 than model 0?
df_d <- n - 1 # How many additional parameters could we theoretically add to our model?


# pf function gives you the p-value from the F-table that you're familiar with.
# lower.tail = FALSE means you want the probability of getting an F-value LARGER 
# than your f_stat value, under the assumption that the population mean is, 
# in fact, zero. 

# Because p = .002, which is < .05, the additional precision was "worth" the addition of the parameter.
# The mean of the sample is a better predictor than 0.


#### Models & model comparison the easy way ------------------------------------

# Let's make a model where we predict scores on our variable of interest 
# (distance) from the mean of that variable. Everyone's score is being predicted 
# by a constant when using the mean-only model. This is something we need to 
# keep in mind when we specify our model.


# lm: this is a (general) linear model
# data = d: we need to specify which dataframe R should use to find these 
# variables
# distance ~ 1: we predict the thing on the left side of the ~ using the 
# variable(s) on the right side. 
# The "1" on the right hand side of the ~ means that we want to estimate a 
# constant intercept in this model. Note that this intercept will be the overall 
# mean and will not necessarily be equal to 1. 
# In fact, mod1 is equivalent to getting the mean of distance 
# and predicting that for each observation. We will talk more about this in 
# future units.

# To specify our null (stupid) model using lm:


# We can use the anova function from the stats package to do a model comparison


# The p-value should be the same as what we calculated above.
pf(f_stat, df_n, df_d, lower.tail = FALSE)


# Questions?


#### Graphing our data and model ----------------------------------------------

# You will encounter us using the terms "quick and dirty" plot and 
# "publication quality" plot.
# Generally speaking, when we use the latter, we mean a plot that is unfinished
# and not ready for publication. It may not be labeled clearly or even include
# error bars.
# Quick and dirty plots are more for your understanding and ability to look at 
# the data visually (e.g., when you're meeting with your advisor and you don't 
# want to specify everything for ggplot in the moment).

# Quick & dirty


# Now, let's add a best fitting line



########## Publication quality

# Load ggplot2
library(ggplot2)
?ggplot2
# From Help: ggplot() is typically used to construct a plot incrementally, 
# using the + operator to add layers to the existing ggplot object. This is 
# advantageous in that the code is explicit about which layers are added and 
# the order in which they are added. For complex graphics with multiple layers, 
# initialization with ggplot is recommended.

# Tip: Open a new (blank) file now to begin a "plotting toolbox" for yourself. 
# Whenever we create a different type of plot in lab, add it with notes
# to your plotting toolbox, so you have a one-stop shop to copy and paste from 
# for creating plots in the future. Use section headings to keep it organized.

# Specify the model we are using for our graph


# Generating predicted data (necessary for confidence interval bands).

# Here we are creating a dataframe for our confidence interval bands.
# To start, our new dataframe has a single column, called 'mental_folding,' 
# the name of our predictor (our X variable).
# seq() creates a sequence of numbers within the range of the first two numbers 
# you give it, then we specify with length = 100 that we want 100 different 
# values in that range. We're setting the output of seq() (which is 100 values 
# between 0 and 10) to be what's contained in the column "mental_folding."
d_graph <- data.frame(mpf_m = seq(0, 10, length = 200))

# Let's check it out. 
View(d_graph)
# So far we've got a dataframe containing just one variable, mental_folding, 
# representing many of the possible values of mental_folding.

# This function is one we loaded in the beginning of lab (ggplotPredict)
# ggplotPredict plugs our inputs from d_graph into our model, m_graph. 
# The result is a dataframe with predictions based on our model as well as 
# lower and upper standard error bounds for each condition.



# Graph a scatterplot of the data with mental_paper_folding on the x-axis 
# and distance on the y-axis
plot_a <- ggplot(d, aes(x = mpf_m, y = distance)) # set the general parameters for the plot
# ggplot interprets things in aes("...") as column names, so you don't need quotes

plot_a

# now we add layers to this plot as we go:
# overwrite plot_a with a new object that has all of the same stuff as plot_a 
# *and* points.
plot_a <- plot_a + geom_point() 
# Now what does our plot look like?
plot_a 

# Now let's add another layer to "plot" that will graph the regression line.
# Can we just use the default parameters? Nope. The data for the regression line 
# must come from the d_ci dataframe so we can include confidence bands.
plot_a <- plot_a + geom_smooth(data = d_graph,            # where to find these variables
                                  aes(ymin = CILo, ymax = CIHi, x = mpf_m, y = Predicted), # all these are just columns we created in d_graph
                                  stat = "identity",
                                  color="red")  # we can make it a different color to stand out

plot_a


# Finally, let's do just a couple things to make it look nice:
plot_a  <-  plot_a + theme_bw(base_size = 14) + # remove the background grey color
  labs(x = 'Number of correct mental folds', y = 'Distance from actual fold (mm)') +
  xlim(0,1)+
  ylim(-100,110)

plot_a

# Everything at once (combining previous code into single plot)
plot_a_complete <- ggplot(d, aes(x = mpf_m, y = distance)) +
  geom_point() +
  geom_smooth(data = d_graph, aes(ymin = CILo, ymax = CIHi, x = mpf_m, y =Predicted),
              stat = "identity", color="red") +
  theme_bw(base_size = 14) + 
  labs(x = 'Number of correct mental folds', y = 'Distance from actual fold (mm)') +
  xlim(0,1)+
  ylim(-100,110)

plot_a_complete

# If you were prepping this graph for publication, what else would you want to do?
# Possibly get rid of the grid lines
# Improve the tick marks on the x and y axis. (There are no .5 scores on the MPF 
# task, so make the x axis only whole numbers)
# Other things?

# We will learn how to do all these things later!

# Why use ggplot instead of other graphing alternatives like excel or Tableau?
# ggplot allows you to graph model predictions, rather than just raw data. 
# For simple models, like the one from today, there is very little difference. 
# However, once you start working with more complex models (ones where we have 
# several interactions and covariates), plotting model predictions will allow us 
# to plot the relation between one (or more variables) and the DV after taking 
# into account the effects of the other variables in the model. The learning curve
# might be steep, but once you have mastered ggplot, you can easily graph any 
# model you want without using any other packages or software. 
# Very powerful, very customizable, extremely useful.

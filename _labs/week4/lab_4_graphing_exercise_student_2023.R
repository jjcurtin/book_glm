###############################################
####    Week 4 Lab: Graphing Exercise      ####
####     Friday, September 29, 2023        ####
###############################################

# Now that we have practiced making a bar graph together, try it in small groups! 

# We will be using the free Crime dataframe from MASH

#### 1 #### Prep
# Read in your data and look through it briefly with a line of code or two




#### 2 #### The potential relationship between region and crime rates in the US.

# There is a common belief that more crime occurs in hotter locations.
# Based on this belief, we might predict that southern states have greater crime
# rates than northern states. 
# Using the Crime dataframe, we can test this prediction.


# First, let's make it clear what each of level of d_e$Southern represents. 
# I will tell you d_e$Southern is coded such that 0 = Northern and 1 = Southern.
# Use recode to reflect that in a new variable called d_e$Southern_char




# Next, let's center our newly recoded d_e$Southern



# Now create a linear model in which crime rate is regressed upon southern


# Get a summary of the model and get an F stat



# What does the summary tell us? 
# Is there a relationship between the number of offenses per million (d_e$CrimeRate)
# and whether or not a state is southern (d_e$Southern)?







#### 3 #### Let's get graphing. 

# First create a quick and dirty plot of the relationship between the number of
# offenses per million (d_e$CrimeRate) and whether the state is southern (d_e$Southern).




# In just three steps, set up a new dataframe called d_e_graph.
# d_e_graph should contain predicted values,
# lower and upper confidence intervals,
# standard error, centered numeric values for northern and southern states, and
# character values that can serve as lables for northern and southern states 







#### 4 ####
# Build a bar graph in ggplot using the dataframe d_e_graph. Call this graph:
# "plot_crime"

# Build the grid




# Add the bars



# Add error bars




# Add raw data points





# Lets add the finishing touches





# Congrats! All done!


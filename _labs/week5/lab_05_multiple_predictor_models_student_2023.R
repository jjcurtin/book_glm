##########################################################
#### Week 5 Lab: Models with 2 Predictors – TA Script #### 
####            Friday, October 6th, 2023             #### 
##########################################################


#########
##
## Set up
##
#########

######## Questions about the comprehensive assignment or other material? #######

######## Conventions for decimals (sig. Figs) when reporting statistics ######## 
# b and F values: 
#          1) always include the digit(s) preceding the decimal. 
#          2) if the number is less than one, report 0.xxx (e.g., 0.852) 
#          3) otherwise, report two decimal places after the decimal (e.g., 2.85) 
# p values: 
#          1) because p values are bounded by 0 and 1, do not report the digit 
#             before the decimal place. 
#          2) report 3 digits after the decimal
#          3) although you can just report the most stringent level of significance 
#             which the p value falls under (e.g., p = .003 --> p < .005), it is 
#             good practice to report exact p values to the 3rd decimal place,
#             for transparency of your results 
#          4) except for p values smaller than .001, just report p < .001

# Before lab! Install 4 new packages -------------------------------------------- 
#install.packages("kableExtra")
# If you're interested, cool stuff you can do with it here: 
# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html 

#install.packages("GGally")
# GGally helps create quick yet nice plots of descriptive statistics.

#install.packages("rockchalk") 
# rockchalk helps calculate R^2 of regression models.

#install.packages("pacman")
# Pacman helps you load multiple packages using a single line of command.
# Lets try that now!!

# Set up working directory to source file location 
# Load up tidyverse, car, psych, kableExtra, effectsize, effects, ggplot2, GGally, and rockchalk
library(pacman)
pacman::p_load(tidyverse, car, psych, kableExtra, 
               effectsize, effects, ggplot2, GGally,rockchalk)

# Source 610710_functions.
source("610710_functions.R")


################
##
## Data Analysis
##
################


# Prestige dataset ---------------------------------------------------------- 

# Today we're going to use a dataset called "Prestige", which comes with the
# car package.

# How do we get the documentation for the Prestige dataset?


# How do we load data that are included in an R package? 


# We'll drop one of the three categories in the "type" variable so we have a  
# dichotomous predictor. We'll learn how to deal with categorical predictors with  
# three or more levels later in the class. After the subset command, our type  
# variable should only have blue and white collar jobs. 


# Remember our boolean logical operators: ! is for "not," so != is "not equal to" 

table(d$type) 
# Wait... Why is 'prof' still in the output of table() function?

# What is the data type of the "type" variable?
class(d$type)

# Factor! A factor variable is basically a categorical variable.
# A factor variable can be ordered, or unordered. We won't get into this for now.
# The values stored in a factor variable can be either numeric or character values.
factor(c(1,3,2,3,1),levels = c(3,2,1))
factor(c(1,3,2,3,1),levels = c(3,2,1),labels = c("Low","Medium","High"))
factor(c("High","Low","Medium","Low","High"), levels = c("Low","Medium","High"))

# Because "type" is a factor, R assumes that 'prof' is still a possible response, 
# although no observation in the dataframe d belongs to that group.
# So, if we ask R to make a table of "type", it still includes 'prof' (professional jobs) 

# How do we fix this? 



# You could also force "type" into a character variable.



# What happen if you force a factor variable into a numeric variable?
d$type_num <- as.numeric(d$type)
table(d$type_num) 

#------------------------------------------------------------------------------#
#                                 Start Exercise                               #
#------------------------------------------------------------------------------#
# Now it's your turn! Complete the following section in pairs.

# Take a quick look at the data using several of the functions we learned.


# Here is a new function, ggpairs(), from the package GGally. What does it do? 
# Use it on our data. 
?ggpairs


# ggpairs() is for broad overview of the data including correlations!

# What does each row in our data represent? Use rownames() on our data.
?rownames



# Recode the "type" variable to a numeric variable (-0.5 and 0.5).   



# Use recode() to create a character version (d$type_named) of the "type" 
# variable with sensible names for each type of job (what does bc and wc each stand for?)



# Create a correlation table listing all of the pairwise correlations between  
# the 4 variables we care about: "type_c", "income", "education", "women" 




# How can we compute the partial-eta^2 for a simple linear relationship between
# a pair of variables, using only the information from the correlation table? 



#------------------------------------------------------------------------------#
#                                 End of Exercise                              #
#------------------------------------------------------------------------------#

# What if we also want p values for these correlations? (Is the correlation
# between two variables significantly different from 0?)
?corr.test 


# Well that's kind of annoying to not have them next to each other, especially 
# if we had more variables.  

corr_results <- 

# If we want to see just the correlation magnitudes (r) 
corr_results$r 

# If we want to see just the p-values 
corr_results$p 

# Let's write a function to create a table with the values next to each other. 
# This will also reduce human error introduced by filling in the table by hand.

# You can read through this later on your own to learn how it works. For now just 
# run the lines of code.  

# First we declare a function that takes two arguments, a list of variables, and  
# the 'use' method for corr.test 
corr_table <- function(vars, use_method) { 
  # Run the corr.test function 
  corr_results <- corr.test(d[, vars], use = use_method) 
  # Make a dataframe called d_corrs that's empty other than row.names that are our vars 
  d_corrs <- data.frame(row.names = vars) 
  # Our first 'for' loop!! This loops through the code between the two {} once  
  # for every item in 'vars,' calling each one 'v' in turn.  
  # Once it runs everything between the {} for that item, then it loops back up  
  # to do it all again for the next item, until it runs out of items.  
  for (v in vars) { 
    print(v) 
    for (pair_v in vars) { # a nested for loop 
      # Obtain the r value from the correlation table where row = v, column = pair_v 
      # and round it to 3 digits 
      r <- round(corr_results$r[v, pair_v], digits = 3)  
      # Obtain the p value from the correlation table where row = v, column = pair_v 
      # and round it to 3 digits 
      p <- round(corr_results$p[v, pair_v], digits = 3) 
      # paste() puts it all together 
      v_r_p <- paste("r = ", r, ", (p = ", p, ")", sep = "") 
      # Now, in our d_corrs dataframe where row = v, column = pair_v, put our  
      # pasted together result 
      d_corrs[v, pair_v] <- v_r_p 
    }
  }
  return(d_corrs) 
} 

# To save this customized function for future use, potentially in other scripts, 
# you need to put it in a separate R script and source that R script so you can
# use the function. Sounds familiar? That was basically what we were doing the
# whole time with 610710_functions.R

# Now copy and paste the code above to the bottom of 610710_functions.R and save
# the file. Then source 610710_functions.R again.
source("610710_functions.R")

# Eventually for your own research, you may create a toolbox R script where 
# you save all useful customized functions. E.g., you can write and save a 
# function that makes plot for a particular kind of analysis that you conduct. 

# Customized functions won't automatically "run" by themselves. You need to 
# give input and run the code to use them, just like any other functions.
corr_table(vars, "pairwise.complete")  

# Let's save the correlation table as an object so we can easily use it later: 
d_corrs <- corr_table(vars, "pairwise.complete") 

# We can also write it to a .csv file to make it easy to format fonts in 
# Excel and drop it into a manuscript/poster 

# First, we must convert the row names of d_corrs into an actual column
rownames(d_corrs) # these are variables with which we calculated correlations
d_corrs <- rownames_to_column(d_corrs, var = "variable") 
d_corrs$variable # the row names now become a column
write_csv(d_corrs, "lab_05_correlations.csv") # this will save to your working directory 

# Or you can use the kableExtra package to make the table in html format. 
d_corrs %>% 
  kbl(caption = "Table 1") %>% 
  kable_classic(full_width = F, html_font = "Cambria") 

# Lastly, we can also look at these correlations visually, by using spm() 
# function to create all the bivariate scatterplots 
spm(d[, vars]) # this spm() command lives in the car package 

#------------------------------------------------------------------------------#
#                                 Start Exercise                               #
#------------------------------------------------------------------------------#

########          Models with a single continuous predictor             ######## 
# Suppose we are interested in whether jobs that require more years of  
# education are also the ones in which people earn, on average, a higher income

# Write out the two models we will be comparing in formula form. 
# What are the parameters estimated in each of the two models?






# Now fit the model in R. Explain how this model answers our research question above 


# Obtain and report t statistic, degrees of freedom, and p value for each parameter



# How do we convert t statistic to F statistic?


# Interpret the intercept 



# Is the intercept meaningful?



# Interpret the regression coefficient for education 



# Is the association between education and income statistically significant? 


# How large is the effect? Obtain the effect size for education.



# How much variance of 'income' is explained by 'education'?



# let's graph! 
# First make a new version of our model for graphing. (good practice)
# This is more important for, say, models with interaction terms.


# Make a dataframe for plotting that contains the predictor  






## Use seq() to create a sequence of values for continuous predictors
## We want the sequence of values to sit between the minimum and maximum 
## possible values of the predictor in our sample. Use "from =" and "to ="
## to set the start point and end point of the sequence to generate.

## By default, seq() will create the sequence of values by increment of 1.
## Use length.out to specify the length of the sequence to generate.
## Combined with "from =" and "to =", length.out adjusts the increment between each value.
## We want length.out to be at least the distance between the min and max values + 1.

# Generate predictions for d_graph_1 based on m_graph_1 using ggplotPredict


# Make the plot in ggplot. Add raw data points.








########          Models with a single dichotomous predictor            ######## 
# Suppose we are interested in whether white collar jobs on average require more 
# years of education than blue collar jobs

# Now fit the model in R. Explain how this model answers our research question above 


# Obtain and report t statistic, degrees of freedom, and p value for each parameter



# Interpret the intercept 



# Interpret the regression coefficient for type_c



# let's graph! 


# Make a dataframe for plotting that contains the predictor  


# Generate predictions for d_graph_2 based on m_graph_2 


# Recode type_c to a character variable with meaningful labels.


# Make the bar plot in ggplot. Add raw data points.








#------------------------------------------------------------------------------#
#                                 End of Exercise                              #
#------------------------------------------------------------------------------#


# Models with two predictors  ----------------------------------------------- 

# Women representation of a job is measured by 'women', a variable in the 
# Prestige dataset that records the percentage of a job's incumbents who are women.

# Consider adding women representation as a covariate to model 1.
# i.e., use both the 'education' variable and the 'women' variable to predict income.

# In general, why might it make sense to add a covariate? (2 reasons)


# Now we will fit a model, m3, to predict 'income' from 'education' and 'women'

# Note that education is still the focal predictor 





# How does adding women representation as a covariate influence the (variance-based)
# effect size of education? 

# Compare partial-eta^2 in the models with and without the covariate. 
# Describe that comparison



# What does the partial-eta^2 for education represent? 



# Now let's calculate *delta* R squared using a function from package rockchalk.



# What does the delta R squared for education represent? 



# Confidence interval bands narrow after adding women representation as a 
# covariate to the model: 
plot(effect("education", m1)) 
plot(effect("education", m3)) 

# Did our parameter estimate for education change? Is that surprising? 
coef(m1) 
coef(m3) 

# What is the interpretation of the intercept for the model where women 
# representation is included as a covariate? 



# This isn't particularly meaningful. One way to make the parameter estimate for  
# the intercept more meaningful is to mean-center our predictor variables, and  
# refit the model. Go ahead and do that now!





# What changed and what didn't? Why?


# What is the interpretation of the intercept after mean-centering? 



# Why is the standard error for the intercept so much larger for the model with  
# uncentered predictors as compared to the model with centered predictors?  



# Do any of the effect sizes change between the centered and uncentered models? 
eta_squared(car::Anova(m3_c, type = 3)) 
eta_squared(car::Anova(m3, type = 3)) 


# What is the interpretation of the coefficient for education and what does the  
# coefficient tell us?  



# The regression coefficient tells you about the effect of the focal predictor 
# independent of the effects of all other covariates.

# What does the total R-squared for model m3_c mean?  
summary(m3_c) 


# For MODEL R-squared given by summary(), the augmented model is ALWAYS compared
# to the mean-only model. In other words, model R-squared is the proportion of 
# variance of the DV explained by ALL predictors in the model.




##  ##############################################  ## 
##           Week 7 Lab: Exercise Week!             ## 
##           Friday, October 20th, 2023             ## 
##  ##############################################  ## 


# Prep R and Source Functions ------------------------------ 

# Set your working directory


# We will need the following packages loaded 
library(tidyverse) 
library(psych) 
library(car) 
library(effectsize) 
library(rockchalk) 
library(lavaan)

source("610710_functions.R")

## Exercise 1: Calculating parameter estimates ------------------------
# Abdul is running a study to see if listening to classical music before going to  
# sleep leads to more restful sleep. He assigns one group of participants to  
# listen to classical music before bed and another group to listen to no music  
# before bed. He then asks participants to report how restful their sleep was  
# each night. Abdul runs a simple regression analysis and reports the following  
# results:  
# b0 = 3.01, SE = .08, b1 = 1.15, SE = .16, F(1, 197) = 53.01, p < .001,  
# partial eta squared = .21 
# The results above were obtained when music condition was coded 0.5 (music) and  
# -0.5 (no music). 


# a. What would the value of b0, the SE for b0, b1, and the SE for b1 be if 
# music condition had been coded as 0 (no music) and 1 (music).  
# Note, if it is not possible to calculate the value, simply write "not  
# possible to calculate" 

# b0 = 3.01 * -.5 * 1.15 = 2.44
# b0 SE = "not possible to calculate"
# b1 = 1.15
# b1 SE = .16


# b. What would the value of b0, the SE for b0, b1, and the SE for b1 be if 
# music condition had been coded as 1 (no music) and 0 (music).  
# Note, if it is not possible to calculate the value, simply write "it is not 
# possible to calculate" 

# b0 = 3.59 = 3.01 + 1.15 * music = 3.01 + 1.15 * .5
# b0 SE = Not possible to calculate
# b1 = -1.15
# b1 SE = .16

# c. What would the value of b0, the SE for b0, b1, and the SE for b1 be if 
# music condition had been coded as -1 (no music) and 1 (music).  
# Note, if it is not possible to calculate the value, simply write "it is not 
# possible to calculate" 

# b0 = 3.01
# b0 SE = .08
# b1 = 0.58 = slope/number of units apart = 1.15/2
# b1 SE = .16/2 = .08
# Note that the df numerator and denominator do not change and so F and p do not change. 
# pEta2 does not change either 
# The fact that F and t do not change illustrates why the new SE for b1 is half 
# of the old SE. Remember that t = b1/SE_b1. If the new b1 is half of the old b1, 
# then the new SE for b1 needs to be half of the old SE for the t to remain the same. 


# d. Using the original values provided above, calculate by hand the t-value for b1.  
sqrt(53.01)

# e. What was the sample size in this study?
199


## Exercise 2: model predictions ------------------------------

# Let's import the data. It's a .dat file (tab delimited), so we can 
# use "read_tsv" for import. 
# When you import these data, you'll see that read_tsv creates a column name  
# called SubID. Rename this column to something more sensible, given that the  
# observations aren't subjects, but rather occupations. 

d_raw <- read_tsv("lab_07_prestige.dat") %>% 
  rename(job_id = SubID) 

# Note: we have not learned how to deal with categorical variables with more  
# than 2 levels. Please subset your dataframe so that our variable "type" will  
# only have blue and white collar jobs.  



# The variables education and income were calculated based on averages from the 
# year 1971 in Canada. The average income in Canada 1970 was estimated at $5,000 
# exactly. Conduct a test to determine if the average income in 1971 is
# significantly different from the average income in 1970.

# Remove NAs from the income variable

# Calculate sum of squared errors for our average income in 1970


# Calculate sum of squared errors for our average income in 1971





















# You want to know if there is a relationship between income and job type (blue
# versus white collar). Specifically, you hypothesize that blue collar jobs will 
# have a lower income than white collar jobs. However, you suspect that the 
# prestige of an occupation also has a relationship with income and should be 
# controlled for.  

# Recode type so that bc is 0 and wc is 1. Call this variable type_bc 

# Recode type so that bc is 1 and wc is 0. Call this variable type_wc 

# Center type so that bc is -.5 and wc is .5. Call this variable type_c 

# Mean center prestige. Call this variable prestige_c 



# Now, with your subsetted dataframe, fit a linear model in which income is  
# regressed on type_bc while statistically controlling for prestige (uncentered).  

 


# What do the estimates for b0, b1, and b2 represent? 

# b0 = 

# b1 = 

# b2 = 


# Provide a conceptual interpretation for each parameter estimate. 

# b0 = 

# b1 = 

# b2 =  


# Fit a linear model in which income is regressed on type_wc while  
# statistically controlling for prestige (uncentered).  




# What do the estimates for b0, b1, and b2 represent?

# b0 = 


# b1 = 


# b2 = 




# Provide a conceptual interpretation for each parameter estimate. 

# b0 = 


# b1 = 


# b2 = 


# Fit a linear model in which income is regressed on type_c while  
# statistically controlling for prestige_c  





# What do the estimates for b0, b1, and b2 represent?

# b0 = 


# b1 = 


# b2 = 



# Provide a conceptual interpretation for each parameter estimate. 

# b0 = 


# b1 = 


# b2 = 



# If you wanted b0 to reflect the predicted average income for a white-collar job 
# that has an average level of prestige, what model would you fit? Run that model. 




# Using code, calculate both partial eta squared and delta R squared for this model. 

# Interpret partial eta squared and delta R squared for each parameter 










# Which parameter estimate(s) change across all four of these models? 






## Exercise 3: Mediation -----------------------------------------------------

# A retail store has developed a new financial incentive program to increase the 
# amount each person working in the store sells. Half of the workers 
# receive this intervention (a financial incentive to sell more), 
# while the other half of the workers receive an active control 
# (a written message of encouragement but no financial incentive). One week after 
# the incentive program has begun, the company surveys each worker 
# and assess how excited they are about the incentive (or message) they received.
# The workers report their excitement on a Likert scale ranging 
# from (0) “not at all excited” to (4) “very excited.” 1 year after the incentive
# program begins, the store sends you a data file including the workers’ assigned 
# conditions, their excitement 1 week into the program, and how much they made 
# that year (earnings). Note that earnings at this company are based on commission  
# with each sale, thus they vary from worker to worker.

# id = worker id
# intervention = intervention condition. Condition 2 = financial incentive,
#                 condition 1 = active control 
# excitement = worker excitement about the intervention (higher values = more excitement)
# earned = earnings that year

# Fit the appropriate models to test the hypothesis that the effect of the 
# intervention condition on earnings is mediated by the worker’s excitement
# about the incentive they received.

# Load the sales.csv data file.



# Examine your data file, find correlations between variables, means, and standard
# deviations for each variable.






# Determine if each of the 4 conditions for mediation are satisfied:

# 1. 







# 2. 







# 3. 









# 4. 
















# Provide a written summary of your results. (Hint: it might be helpful to look
# at the Unit 8 slides).





































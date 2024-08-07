## # # # # # # # # # # # # # # # # # # # # # ## 
##   Week 3 Lab: Validity and Reliability    ## 
##      Friday, September 15th, 2023         ## 
## # # # # # # # # # # # # # # # # # # # # # ## 


#### This week's "fun with R" -------------------------------------------------- 


# install.packages("RXKCD",repos = "http://cran.us.r-project.org") 
# Remove the "#" in front of install.packages and then run line 10

# Now let's load it. 
library(RXKCD) # this command loads the 'RXKCD' package 

# What's this package for? 


# getting XKCD comic strips (any XKCD comic!), like this one! 
getXKCD(552)
# Hit zoom for a better view of the comic 


# What is the statistical issue that this comic raises?



#### General questions about class or discussion ----------------------------- 




#### Loading packages for today ---------------------------------------------- 

library(tidyverse) 
library(psych) 
library(car) 


#### The personality dataframe ------------------------------------------------- 

# Previously we have used the read_csv() command to read in a .csv file 
# containing the data we would use during the lab. 
# Today we are doing something a bit different.  
# Some R packages come with open source dataframes already. 
# This week, we are using a dataframe that is stored in a package that you already have. 

# The "bfi" dataframe is in the "psych" package. Let's look at the documentation 
# for bfi. 

?bfi

# So what is going on? 
# Participants completed a 25-item Big Five personality measure. Who knows the 
# five constructs that make up the "Big Five"?
# Conscientiousness, Agreeableness, Neuroticism, Openness, Extraversion
# AKA C.A.N.O.E.

# Each construct was measured with five questions (example: a1, a2, a3, a4, a5).
# A higher score indicates more displays of that particular trait.
# Participants also reported their gender, level of education, and age.
# Note: This is an older dataset which gender was coded male or female. A new 
# dataset should capture the multiple possible gender identities.

# Now let's put the data into our global environment, and let's rename it "d".


# Note that there is no particular advantage to renaming the dataframe bfi to d. 
# If we wanted, we could simply call bfi in place of d. However, for the sake of 
# consistency, we will rename it to d (the same name we give all our dataframes) 

# See if you can answer the following questions (without running code): 
# How many observations are in the dataframe? 
# How many variables are in the dataframe? 

# What are 3 different functions we can use to look at the dataframe?

# or  

# or 

# Glimpse gives an overview over the structure of our dataframe, 
# including the number of observations and variables. 
# We have 2800 observations for each of 28 different variables.

# Now get some random rows of data to check things out.


# Wait, what do these variables mean? The names are unclear. 
# What can we do to find out?


# Why does typing '?d' not work? 
# Because 'd' is only a temporary object in our global environment to which 
# we assigned the values from bfi; 'bfi' is part of the psych package.

# Another thing I'm noticing is that these variables all use capital letters. 
# To keep things simple and neat, let's use a function from the janitor package 
# to clean up our variable names.


#### Tidy tip: the "clean_names" function -------------------------------------- 

# install.packages("janitor",repos = "http://cran.us.r-project.org") 
# Remove the "#" in front of install.packages and then run line 102

# Now that you've installed the package, how should you load it?


d <- clean_names(d, case = "snake") #snake will replace caps with lower case 
# and change spaces and periods to underscores 

# Now, how can we check if we've successfully changed the names? 
names(d)


#### Exploring the dataframe ----------------------------------------------------- 

# What are some theoretical questions these data could allow us to answer? 
# 1. Is openness negatively related to age? 
# 2. Are women higher or lower in neuroticism than men? 
# 3. Are people high in agreeableness also higher in extraversion? 
# etc. 

#### Getting descriptives review #### 

# Time to put your skills to the test:

# 1. Get the Mean, Median, Standard deviation, and Range for every variable.


# 2. Get the mean of only the first item in the dataframe A1 (without out using describe()).
mean(d$a1) # Why does this not work?


# 3. Get the SD of only the first item A1 (without out using describe()).
sd(d$a1, na.rm = T) 

# 4. Recode gender so that it is centered and store this centered variable as "gender_c".
# But wait, what are the current values? 
table(d$gender)



# Did it work? 


# 5. Is this the same as mean-centering? Why or why not?


# 6. Create a new variable, a1_c, by mean-centering the first variable, a1.


# 7. What do we need to do to create a standardized / z-scored a1 variable (a1_z)? 
# You can write out the mathematical equation manually, or you can use the 
# function 'scale.' If you use the 'scale' function, you specify whether you 
# would like R to center the variable, and whether you would like R to scale 
# the variable to create a Z score.

# Let's do it by hand:


# And let's use the function.

# Note that scale and center have their defaults as true. You could omit those. 

# It's nice to standardize if you're trying to compare or combine 
# variables that are on different scales. However, if you have a meaningful 
# variable (like a grade), it's good to avoid standardizing as it helps with 
# interpretation. We won't standardize often in this class. 


#### Evaluating reliability -------------------------------------------------- 

# If we would like to know whether our scale is reliable, what can we do? 
# 1. Test-retest reliability: administer the same test twice at two different
# time points, then correlate them.
# 2. Split-half reliability: divide total set of items into halves (e.g., odd-
# numbered and even-numbered questions), then correlate them.
# 3. Compute Cronbach's alpha. What is Cronbach's alpha?

# How can we figure out how to compute Cronbach's alpha in R? 


# To calculate Cronbach's alpha, we'll use ...
?alpha

# What kind of information does alpha() expect? 



#### Subsetting data with bracket notation ------------------------------------- 

# Remember, in this dataframe, there are 5 constructs (each measured with 5 questions). 

# Problem: What if we want to look at just the variables that represent 
# a single construct (e.g., Agreeableness)? 

# We can use bracket notation to grab columns and rows out of a dataframe: 




# So, to review: 
# Items before the comma refers to what? 
# Stuff after the comma refers to what? 
# What was the third participant's response to item O2? 


# How can we use bracket notation to get all of the columns belonging 
# to a single construct (e.g. Agreeableness (a1, a2, a3, a4, a5))? 
d[, c("a1", "a2", "a3", "a4", "a5")] 

# But writing this out every time is a pain. Let's make objects that are assigned 
# a vector of character values (for each trait).
agreeableness <- c("a1", "a2", "a3", "a4", "a5") 
conscientiousness <- c("c1", "c2", "c3", "c4", "c5") 
extraversion <- c("e1", "e2", "e3", "e4", "e5") 
neuroticism <- c("n1", "n2", "n3", "n4", "n5") 
openness <- c("o1", "o2", "o3", "o4", "o5") 

# Now call the columns representing agreeableness again.

# Here, we are using bracket notation to subset our dataframe just to 
# the columns specified in the vector 'Agreeableness' 

# If we only want to print the first few rows so it doesn't 
# overwhelm our console, what can we do?

# We can use the "head" command! 
d[, agreeableness] %>% head() # remember the pipe (%>%) from the first week? 


#### Cronbach's Alpha and item-total correlations #### 

# Now, we are interested in getting a measure of internal reliability for 
# each of our constructs (e.g, C.A.N.O.E). 

# First, it's important to note whether there are any reverse-coded items.
# For now, I'm going to tell you that items 2 and 5 are reverse coded. 
# With that information, we are going to create Cronbach's alpha for Openness. 
# Now subset the data for only questions associated with Openness.
d[, openness] 

# After subsetting the data, we can directly tell alpha the 
# reverse-coded items using "keys =" and run the command 
# I'm making sure to refer to the right "alpha" function (there are many) 
# by prefacing it with psych:: 
psych::alpha(d[, openness], keys = c("o2", "o5")) 

#### Interpreting the output #### 
# This is a lot of output. Focus on the 'raw_alpha' at the top left. 
# How reliable is this scale? 

# Does anyone know at what alpha threshold a measure is considered acceptable? 


# What is the easiest way we could increase the reliability?

# Let's look at the conscientiousness scale. 
# I'll tell you that items C4 and C5 are “keyed” (i.e., reverse-coded). 
# Calculate the reliability of the Conscientiousness scale on your own. 


# How reliable is this scale? 


# How do we interpret the rest of this output? 

# 'Reliability if dropped' tells you how the alpha value would change 
# if that item were removed. 
# If removing an item would greatly improve alpha, it might be worth doing so
# (but only during scale development). 

# Item statistics tells you about each item, and how it relates to the rest 
# of the scale. If you were constructing a scale, you might want to see how 
# each item correlates to the rest of the scale. 
# raw.r = the correlation of each item with the total score. 

# Note that you might be capitalizing on random noise when you remove an item 
# It's possible the scale becomes more reliable in this sample, but not in future  
# samples. 

# Three RULES for dropping variables! 
# 1. You CANNOT remove items if you are using an established (i.e., published) scale. 
# 2. You SHOULD NOT remove items if you have not preregistered the criteria for 
# their removal. 
# 3. If you do remove one or more items, you must provide adequate justification. 

# Consider the agreeableness scale. Calculate the alpha of the agreeableness scale. 
# a1 is reverse scored. 


# Pretend you're in the scale development phase of research. 
# What items, if any, would you want to drop? 

# 2 arguments for removing A1? 
# It has the lowest item-total correlation ('raw.r'). 
# It would also lead to the greatest increase in alpha. 

# Arguments against removal? 
# The increase in alpha would be really small, 
# and we've met the arbitrary criterion anyway. 
# A1 is also reverse-coded, and we should be more reticent to remove these 
# items than others.
# Agreeableness is an established scale! 


#### Calculating average scores -------------------------------------------- 

# Let's create average scores for each of the 5 traits. What function have we
# already learned that can do this?
# rowMeans(), with cbind()
# However, we're going to teach you a new function with greater flexibility: varScore()
# This function is part of an older package that has deprecated. We will source
# this function into R so that we can use it here.

# Now, let's source the function varScore into R. We will source other functions
# throughout the semester.



# Notice where the function is stored in your environment.

# What information do we need to supply varScore()?
# - Need to know which data frame to look in for the item names
# - Need to know which items are forward-coded and reverse-coded
# - Need to know high and low anchor points of scale range
# - Prorate: Fills in missing values based on existing data
# - MaxMiss: Maximum acceptable percentage of missing data before total score will
#            be set to 'NA'

# Why are we using varScore() and not just rowMeans()?
# varScore more capably deals with missing data (there is no equivalent to MaxMiss)

# Let's do it! 
# At this point I'm just going to tell you the reverse-coded items: 
# A1, C4, C5, E1, E2, O2, O5

# Openness
d$open_m = varScore(d,                       # data we're working from
                    Forward= c('o1','o3','o4'),  # forward-coded items
                    Reverse= c('o2','o5'),       # reverse-coded items
                    Range=c(1, 6),               # scale range (i.e., the range of possible values)
                    MaxMiss=0.25,                # exclude observations if missing ‚> 25% of items
                    Prorate=T                 # fill in missing data based on existing data
) / 5                             # divide by the number of items (adds by default)

# Now do conscientiousness, agreeableness, and extraversion on your own! 
# Copy-paste liberally. 
# You'll notice that it doesn't matter if there are extra lines added as 
# long as you haven't closed the function. 

# Conscientiousness 


# Agreeableness


# Extraversion


# Neuroticism has no reverse-coded items. This makes this code MUCH cleaner.
# We can remove 2 things and simplify another. Thoughts?

# You don't need range if all items are forward-coded, nor do we need "Reverse = "
# We can also use the object we created earlier instead of writing the items out

# Let's look at them all!



#### Working with these data --------------------------------------------------- 

# How can we get a quick overview of the distribution of Agreeableness values? 
# Anyone have a suggestion for a function to use? 
hist(d$agree_m) 
# or 
qplot(d$agree_m) # this "quick plot" function comes from the ggplot2 package. 

# What if we want to see a plot of Openness as a function of age? 
plot(d$age, d$open_m) 
qplot(d$age, d$open_m) 

# Now we want to see the relationship between these variables as a line 
plot(d$age, d$open_m) + abline(lm(d$open_m ~ d$age)) 
qplot(d$age, d$open_m) + geom_smooth(method = "lm") 

# It appears the older participants score slightly higher on openness. 

# Are women more extraverted than men? How might we explore this? 
describeBy(d$extr_m, d$gender) 

# How can we find out which is women and which is men? 
?bfi 

# It would appear so! 

# Also, it can be tedious to check your codebook. It's often a good idea 
# to recode categorical variables so you can understand them more easily 
# when you're summarizing and visualizing your data: 

d$gender_char <- dplyr::recode(d$gender, '1' = 'Male', '2' = 'Female') 
table(d$gender, d$gender_char) # Crosstab to check if it worked. Looks good 
describeBy(d$extr_m, d$gender_char) 

# What if we want to see how the 5 traits are correlated? 
cor(d[, c("cons_m", "agree_m", "neur_m", "open_m", "extr_m")],  
    use = "pairwise.complete.obs") # pairwise.complete.obs handles functions 
# in the same way as na.rm =T . It removes missing values. 


# What is the strongest relationship?  
# Does this surprise you? Why or why not? 


#### QUESTIONS?? #### 

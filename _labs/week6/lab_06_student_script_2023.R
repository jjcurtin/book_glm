#####################################
####    Lab Week 6: Mediation     ####
####         Oct 13, 2023       	####
#####################################

#### Congratulations on finishing your first CA!
#### Questions or comments about the course?


## Let's carve a pumpkin! Run lines 11-18 of code.
par(bg=1)
plot(c(0,0), cex=0, xlim=c(-1,1), ylim=c(-1,1))
X <- runif(100,-1,1)
Y <- runif(100,0,1)
M <- rchisq(100,1)/20
points(Y~X, cex=M, pch=19, col=colors()[1])
points(0,-0.5, pch=19, col=colors()[498],cex=20)
for (i in 1:4){polygon(locator(3),col=7)}
# Click on 3 points on the graphics window to specify the corners of a triangle. 
# Nothing will happen in the graphics window until you have clicked on 3 points. 
# This will remove the first piece from the pumpkin. You get a total of 4 
# triangles to carve your pumpkin.

dev.off()
# Bye bye pumpkin!
rm(list = ls())
# Clear global environment!


## Some new packages to install:
#install.packages("lavaan")
#install.packages("multilevel")

library(car)
library(tidyverse)
library(stats)
library(psych)
library(lavaan)
library(dplyr)
library(rockchalk)
library(effectsize)
library(beepr)

## Set working directory
source("610710_functions.R")

#### Review Exercise: Models with 2 Predictors ####
# For this exercise we will work with the ToothGrowth data set which is 
# built into R. ToothGrowth data set contains the results from an experiment 
# studying the effect of vitamin C on tooth growth (in mm) in 60 Guinea pigs. 
# Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) 
# by one of two delivery methods, (orange juice coded OJ or ascorbic acid coded as VC).



# First please explore your data to get basic descriptive statistics and the levels 
# of each variable



# Create a model in which you test the prediction that when controlling for the
# effect of delivery method, the dose levels of vitamin C will predict tooth growth.

# Center supp for interpretation



# Using plain language (use as little statistical jargon as possible), interpret 
# the coefficients (estimates) in words that refer to the variables in the model.

summary(m)

## >> b0: 

## >> b1: 

## >> b2: 

# Now, let's review partial-eta squared and delta-r squared.

# What is our focal predictor? 


# Partial-eta squared compares the variance "explained" by the focal predictor 
# (dose) to the variance that is left after the variance explained by the other 
# predictor (supp) in the regression model has been removed.

## Compact model


sse_c <- sum((fitted(m_c) - d$len)^2) # SSE of compact model
sse_c

## Augmented model


sse_a <- sum((fitted(m_a) - d$len)^2) # SSE of augmented model
sse_a

# Remember how we calculate partial-eta squared?


# Let's check!
car::Anova(m_a, type = 3) %>% eta_squared()

# Delta-r squared compares the variance "explained" by the focal predictor (dose)
# to the total variance. Sometimes referred to as "the part of the variance that 
# is uniquely explained by the predictor".

(sse_c - sse_a) / 3452.21 # I am giving you the SSE of mean-only model

# Let's check!
getDeltaRsquare(m_a)


# Last, create a publication quality graph depicting the model you just ran in the
# previous question. Be sure to include a title for your graph, clear labels for
# the x any axes, either error bars or error bands (as appropriate for you model),
# and jittered data points.

# set up the model:
m_graph <- lm(len ~ dose + supp, data = d)

# Make dataframe/define your range of values as needed for ggplotPRedict


# use ggplotPRedict to create error bands


# create the plot
plot <- ggplot(data = d, aes(x = dose, y = len, color = supp)) +
  geom_smooth(data = d_graph, aes(ymin = CILo, ymax = CIHi, y = Predicted),
              stat = "identity") +
  geom_point(position = position_jitter(width = 0.05), alpha = .5)+
  theme_bw(base_size = 14) +
  labs(x = 'Dosage (mg/day)', 
       y = 'Tooth Length', 
       title = 'Guinea Pig Tooth Length') +
  theme(plot.title = element_text(hjust = 0.5)) # center title!
plot

# We can fix this by changing the min and max values by hand. 
# Let's do everything from defining the range of values again:

# define your range of values as needed for ggplotPRedict
d_graph <- expand.grid(
  dose = seq(min(0), 
             max(2)),
  supp = c("OJ", "VC"))

# use ggplotPRedict to create error bands
d_graph <- ggplotPredict(m_graph, d_graph)


# create the plot
plot <- ggplot(data = d, aes(x = dose, y = len, color = supp)) +
  geom_smooth(data = d_graph, aes(ymin = CILo, ymax = CIHi, y = Predicted),
              stat = "identity") +
  geom_point(position = position_jitter(width = 0.05), alpha = .5)+
  theme_bw(base_size = 14) +
  labs(x = 'Dosage (mg/day)', 
       y = 'Tooth Length', 
       title = 'Guinea Pig Tooth Length') +
  theme(plot.title = element_text(hjust = 0.5))
plot
## Great! Looks much better now!


# Indicate where each of the parameter estimates (b0, b1, b2) in the model are 
# on the graph you just made. OJ is coded 0 and VC is coded 1.

## Where is b0?

## Where is b1?

## Where is b2?


#### Additional Review Exercise: Models with 2 Predictors ####
## Open the lab 6 exercise and complete this in groups of 2-3. Open a new RMarkdown 
## file and save it as "lab_06_exercise", write your code there.

## The key will be posted on Canvas and, for the sake of time, not reviewed in lab.


#### Mediation ####
# We're going to work with a new (fake) data set called STEM_data.csv. 
# Researchers contacted the families of high school students at the start of their
# first year in high school. Researchers assessed the students' interest in STEM
# courses on a 7-point Likert scale (1 = not at all interested; 7 = very interested).
# Researchers also obtained the students' parents' transcripts from when the parents
# were in high school. The researchers calculated parents' GPA in STEM courses.
# Over the next four years, researchers counted the number of elective STEM courses the
# students took.

rm(list = ls())



# let's use a new function to take a quick look at our data. Let's include all 
# values except for ID.


# Researchers aim to examine whether the GPA of parents in STEM fields can be 
# used to predict the number of STEM courses their children enroll in during 
# high school. The researchers hypothesize that parental STEM GPA influences 
# their children's interest in STEM courses, which in turn impacts the number 
# of elective STEM courses that students take. In short, they predict that
# the (statistical) effect of parents' GPA on the number of STEM courses taken by students 
# is mediated by the students' interest in STEM subjects.

# Let's verbally describe our hypothesized mediation model.



# So what is X, M, and Y?
# >> X: 
# >> M: 
# >> Y: 

# Now, let's try to visualize the hypothesized mediation model.

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


#### What are the 4 conditions that must be met in a mediation analysis? ####
# 1. 
# 2. 
# 3. 
#### 
# 4. 
#### 

# Note: Remember that a*b + c' = c. This means that if path a*b is significant, 
# then c' is significantly smaller than c.

## Now we will check each of the 4 conditions:

# 1. There is a relationship between X and Y. Path c is significant.
# Here, does parent gpa predict stem courses?


# 2. There is a relationship between X and M. Path a is significant.
# Here, does parent gpa predict student interest?


# 3. There is a relationship between M and Y, controlling for X. 
#### Path b is significant in a model including X.
# Here, does student interest predict courses when controlling for parent gpa?


# The effect of student interest is significant in this model. 
# Have we satisfied all the conditions of mediation?
 


#### lavaan ####
require(lavaan)  ## This command tells R to default to the lavaan package functions
# Lavaan is a package that we can use for testing mediation.
# The notation is a little different, but the logic is the same.
# Y ~ X.

# In lavaan we need to specify the model first, and then fit the model to the data.

# We can test all conditions of mediation in one model

# Let's first specify the model (it looks a little different but stay with me!)
medmodel <- ' 

# This works like regular R. You can add comments to make your code more interpretable
# Lets first specify the models, and we can calculate other effects.
# We want to predict interest from parent gpa. AND
# We want to predict stem courses from parent gpa and interest.

student_interest ~ A * parent_gpa
courses ~ CPRIME * parent_gpa + B * student_interest

# By adding "A" and "CPRIME" we are labeling the coefficients.
# Unless you are doing a mediation, you do not need to specify a label for the betas.

# Define effects
AB:= A*B # Indirect effect
Total := CPRIME + A*B # Total effect
'

# Now, let's fit the model to the data.


# the sem function might take a minute to complete, it could also make Knitting slower. 
# This is normal. It is running many bootstraps which takes a bit of time.

# This shows us the estimates for the mediation model



# Where have we seen these estimates before?
# "Total" is the same parameter as that from step 1. This is path c.
# It's value might be slightly different because of the bootstrap.
# "A" is the same parameter as that from step 2. This is... well, path a. Likewise,
# "B" is the same parameter as that from step 3. This is path b.
# Note, that even though lavaan will give us estimates for paths c, a, and b,
# we still will set up three models using lm to test the first three conditions.

# "AB" is the indirect effect. This is path a*b. We can get this same estimate
# from m2 and m3.
summary(m2) # coefficient is 1.025
summary(m3) # relevant coefficient is 0.366


# From the parameterEstimates function, we can see that the AB CI doesn't include 0. 
# So ab is significant, therefore there seems to be an indirect effect
# of gpa on courses, through interest. Condition 4 is satisfied.

# Proportion Mediated = Proportion via mediation = 
# (AB)/Total = Proportion of mediation effect over the total effect.



# Note! R takes random draws when it conducts bootstrapping, which means your results 
# will change a little every time you run the mediate command (usually the 'seed' 
# that provides your random numbers is set based on your machine's internal clock). 
# What can you do to get reproducible results?



# The 'seed' is the key your computer uses as a jumping off point for coming up 
# with 'random numbers'.
# If you specify the seed explicitly, you'll always get the same random numbers.

# The same applies to lavaan!
set.seed(1234)
fit1 <- sem(medmodel, data = d, se = "bootstrap", bootstrap = 100)
parameterEstimates(fit1, ci = TRUE, level = 0.95, boot.ci.type = "perc", zstat = F)

set.seed(1234)
fit1 <- sem(medmodel, data = d, se = "bootstrap", bootstrap = 100)
parameterEstimates(fit1, ci = TRUE, level = 0.95, boot.ci.type = "perc", zstat = F)

# Same results in both! And your values should match mine!

## How does bootstrap work? ##

# Bootstrapping works by sampling with replacement from your sample.
# In this dataset we have a sample size of 102. With bootstrapping you  
# randomly take 102 observations from your sample. Because you are sampling with 
# replacement, one observation can be chosen multiple times.

# After you draw 102 observations, then R computes all the parameter estimates.
# The estimate of the indirect effect is created by multiplying a * b.
# R repeats this 1,000 (or however many times we specified).
# R saves these 1,000 values of the indirect effect, and creates a distribution.
# A 95% confidence interval is calculated by finding the exact values at the 
# 2.5th and 97.5th percentiles of this distribution of 1,000.

#### Quick and dirty graphs of effects ####

# Use quick plots to illustrate these analyses (are all 4 conditions satisfied?)
library(effects)

# the effect function takes the focal predictor and the model and puts them in 
# a format for plot() to easily read
# ?effect --> check it out if you're interested

# 1.


# 2.


# 3. 


# 4. 


#### GENERAL LINEAR MODEL
####      PSYCH  610      
####  Week 1: Intro to R  

# TAs:
# 2013: Chris Cox & Pooja Sidney
# 2014: Xiaming Ma
# 2015: Tammi Kral
# 2016: Adrienne Wood
# 2017: Mitchell Campbell
# 2018: Mitchell Campbell
# 2019: David Menendez and Katharine Scott
# 2020: Michael Asher and Sasha Sommerfeldt
# 2021: Ethan Harrod and Ben Douglas 
# 2022: Ivette Colón and Emma Cunningham
# 2023: Ben Douglas, LiChen Dong, and Nick Vest
 
# Note for future TA's: this script uses anonymous data collected via Google Forms
# from the students in the course prior to the first lab. Reach out to Ben, LiChen, 
# or Nick so they can share the survey with you.




# 0. Setup ----------------------------------------------------------------


# A. Download the lab_1_student_script and psych610_survey_data files from the 
#    course canvas page

# Navigate to the week 1 lab section in modules and then download both files. 
# Save these files to wherever you plan to keep course materials. Save both the 
# R script and the .csv file in the same folder for now (e.g. folder "Lab 1").

# Open lab_1_student_script.R file with R Studio. Your student script should 
# look mostly the same as the script I am showing now on my screen.

# You'll have access to the "student_script" for lab and then I will publish the
# "ta_script" which is a more complete version with answers to any of the exercises 
# we did during lab so you can refer back to it and check your work

# B. Adjust your R preferences. Here's what we recommend: 
# Tools > Global Options (same as RStudio > Preferences)
# General : 
  # Uncheck 
      # Restore .RData into workspace at startup, 
      # Restore most recently opened project at startup, 
      # Restore previously opened source documents at startup, 
      # Always save history (even when not saving .RData)
      # Set Save .RData to workspace on exit: Never
# Code: Display tab, Check show margin (80 characters)




# 1. Basics ---------------------------------------------------------------


# R is a language you use to communicate with your computer. 
# RStudio is an integrated development environment (IDE) for R. 

# IDE is a fancy word for any software application that helps programmers 
# develop software code more efficiently.

# RStudio makes writing R code easier using a Graphical User Interface (GUI, 
# pronounced "gooey"). A GUI is a form of user interface that allows users to 
# interact with computers through graphical icons. So basically, RStudio gives
# you buttons and menus to write code or work with data more conveniently.


# In RStudio, you type your code into the top left window. 
# When you "run" (=submit) your code, it appears, along with any output or 
# error messages, in the Console (below). 

print("Hello World!")

# Let's run some code together!


# 2. Arithmetic - R can be used as a fancy calculator ---------------------

# But here you use "control" + "enter" (if you use a PC) to tell the 
# computer to run a line of code, or "command" + "return" (if you use a Mac)

2 + 3   # Addition
2 - 3   # Subtraction
2 * 3   # Multiplication
2 / 3   # Division
2 ^ 3   # Exponentiation

# Use spaces to clarify
-2--3
-2 - -3 # This is identical, but easier to read 

# Inequalities return Boolean Logic values ("TRUE" or "FALSE")
2 < 3
2 > 3
2 <= 3
2 == 2  # two = signs together asks R if a value is equal to another. 
        # one = sign on its own will set the value of something. 
2 != 2

# Common rules for R language (make a note for these):
# 1. R doesn't read spaces, they're just there for easy reading
# 2. R DOES differentiate capitalized vs. lower case, so "A" and "a" are two 
#    separate letters in R language (this can cause problems in your code so 
#    watch out!)


# 3. Writing R code ----------------------------------------------------------

# R works by storing information by object names. To store something in an 
# object, use the assignment operator, "<-"

# You may also see some scripts that use "=" for assignment. For our purposes 
# we will use "<-" 

# Let's do some assignment. Store the value 1 in a, and the value 4 in b
#---------------#

#---------------#

# Check the workspace under environment tab (on the upper right in RStudio).  
# a and b, along with their stored values, are there. You can think in this way: 
# every time you run a command, you're asking R "what is [the something that 
# you just typed]?". 

# Now let's check what's inside the object.

#---------------#

#---------------#

whatever # Why do we get an error?

# Errors are R's way of telling you it can't do something. 
# If you don't know what an error means, it is useful to copy and paste the 
# error into Google for help.

# What else can we do with these objects?
# Arithmetic using objects:
#---------------#


#---------------#

# Use the previous objects to create a new object:
#---------------#


#---------------#

# What's the value of c?
#---------------#


#---------------#

# If we want to save words/sentences as objects, we can make it a string using
# quotes (note that I can over-write my original object "a")

# We will use double quotes in this course, though single quotes typically work
# too.
#---------------#


#---------------#

# How do we know it is a string?
#---------------#


#---------------#

# This object type tells R what kinds of things you can do with this object.
# (e.g., you can't do arithmetic with strings or with character objects)

# Types of data in R
class(3)
class(3L)
class(a)
class(TRUE)
class(factor(c("R","RStudio","RMarkdown")))

# Vectors

# Vector is a sequence of data elements of the same type.
vec_1 <- c(1,3,5)

# We use the concatenate function c() to link/combine data elements.

# What if we concatenate elements of different data types?
vec_2 <- c(1,"text",5)
# What is the data type of vec_2?
#---------------#


#---------------#

# Making elements of different types a vector forces its elements to the less 
# stringent data type. (i.e., character is less stringent than numeric because
# numbers can be seen as characters but not vice-versa)

# Arithmetic/logical operations with vectors
#---------------#



#---------------#


# 4. Writing "comments" in R ---------------------------------------------

# In R, the hashtag (aka the pound sign: #) is used for commenting. 
# All text on a line after the pound sign is a "comment" that R will not read.

# a comment

# Write some comments directly below this line now!

# You can comment out a chunk of code using ctrl + shift + c on Windows
# or command + shift + c on Macs


# 5. Packages in R --------------------------------------------------------

# R is an open-source software.
# Anyone in the world can write R code and share it with others in so-called 
# "packages", which you can download and use in your own R scripts.

# If you have not already, run this command to get some of the packages that you 
# will need for this course.

list_of_packages_to_install <- c("tidyverse","car", "foreign", "MASS", 
                                 "sciplot", "gplots", "pwr", "gvlma", "relimp", 
                                 "lmSupport", "effects", "mediation", "Hmisc", 
                                 "lme4", "AICcmodavg", "pbkrtest", "reshape2", 
                                 "rmarkdown", "tinytex", "psych")

# You'll notice we've hit "enter" after commas to keep everything inside of the 
# 80-character margin.

# quick in-lab review: What did we just do in that command? What did we do with 
# the function c()? What is the object type of list_of_packages_to_install?


# At this point, though, R doesn't care whether these are real package names or 
# not. Think of this as a grocery list you're giving to an extremely rigid 
# person. This person will go through the store and only buy items that exactly 
# match the names of items provided on the list. If you put "apples" on the 
# list but the only apples at the store are a specific varietal (e.g., 
# "Granny Smith"), your rigid grocery helper will return and tell you they 
# couldn't find "apples" at the store.

# If you want to actually install the packages, you can now use that handy 
# vector which contains your list of packages, and plug that into the 
# install.packages() command (AKA function):
install.packages(list_of_packages_to_install)

# Now the package has been "installed". But they are not ready to use yet. 
# You have to tell R to "load" a specific package to be able to use its functions.

# This is because packages may have overlaps/conflicts among themselves. You do 
# not want to have everything you "installed" running at the same time. 
# Instead, load the packages you plan to use for a specific R script.

# For the script (and for your first homework assignment), you'll need two
# packages: tidyverse and psych. Let's all load them now.
#---------------#


#---------------#

# Note: Always load packages at the TOP of R scripts to prevent errors. 
# Packages need to be loaded every time you reopen an R script. It is good to 
# make sure they are loaded before you run code that use their functions.

# 6. Reading data into R! -------------------------------------------------

# When you're reading in data, you need to tell R where the data file 
# is located on your computer. You'll do this in two steps.

# 1. Set a working directory. 

# A working directory is a location/folder on your computer where R will read or
# write files. So to read in your data, you will need to set the working 
# directory to where your data file is located. 

# Remember, at the beginning of the lab, we saved both the R script and datafile
# (the .csv file) into the same folder?

# Now, navigate to "session" in the menu at the top of your screen, and then 
# select "set working directory" -> "to source file location." 
# This tells R to set working directory to where the current R script is located,
# which is also where our csv data file is located (because we saved it there).

# Set your working directory! You'll see the filepath print to your console
# when you do.
getwd() # Use getwd() to get the current working directory and double check.


# Or we can use R code to set working directory:

# Go to where your script and data file are located in Finder/Windows File 
# Explorer. Copy & paste the file directory into the command below.
setwd("FILE DIRECTORY")


# 2. Read your data into R.

# Data come in many different formats.  Most of these formats are  
# spreadsheets with a special character that separates each cell of the 
# spreadsheet.  This special character is called a "delimiter".  Two of the most 
# common formats are "tab-delimited" data (which are separated by a tab; they 
# usually end with ".dat", as in "data.dat") and "comma-delimited" (which 
# usually end with ".csv", as in "data.csv"). We will cover reading data into R 
# in different formats (e.g., .xlsx) later in the semester. 

# We're using a function called "read_csv" from the tidyverse package to read 
# in our comma-delimited data file. 

# Now let's read the data and store them as "d" for data:
d <- read_csv("psych610_survey_data.csv")

# "d" should now magically appear in your workspace under the environment tab.
# That .csv data file is now loaded into R. Once it's in R we'll refer to it
# as a "dataframe"

# NOTE: we often refer to dataframes with the object name "d". This is, however, 
# arbitrary. Our dataframe could also be called "Archie" and it would work:

Archie <- read_csv("psych610_survey_data.csv")

# It will be extremely important for you to name things in R in a clear, logical 
# way (not just for you but also for others who will see your scripts, including 
# your TAs, advisors, people who request your data files, etc.).

# So even though you can name anything whatever you want, it would be very 
# unwise to do so.

# let's delete Archie from the workspace under environment tab now.

rm("Archie")


# 7. Viewing your data -------------------------------------------------------

# Let's view our dataframe. You can also click the name of the dataframe 
# in workspace under environment tab (to the right) or enter the following:

#---------------#


#---------------#

# As you can see it's basically a spreadsheet in R.

# Now let's get some descriptive statistics! 
# Descriptive statistics describe features of a dataframe by generating 
# summaries (e.g., mean, standard deviation) about each of the variables.
#---------------#


#---------------#

# By convention, each row in a dataframe is usually a participant, while each 
# column is typically a variable.

# Like anything else, if you enter the name of a dataframe in the console, 
# you'll see its contents.
d

# Fortunately, it is easy to summarize and get snapshots of your data in R.
#---------------#


#---------------#

# What if we wanted the first ten rows instead of the first 6?
# We can get more information about a function by placing a ? in front of it
?head

# "n = 6" is an argument with a default value. Many R commands require certain 
# bits of information (in this case, "x", which is the name of the object) and 
# have other arguments that have some default value that can be overwritten per
# your needs and preferences (in this case, "n", which is set to 6 by default
# but can be changed as we like). Let's try it!
#---------------#


#---------------#

# What if you want to look at only one variable?  
#---------------#


#---------------#

# Now let's get some descriptive statistics:

# get descriptive statistics for the variable called "r_conf"
#---------------#


#---------------#
# (this function is in the psych package)


# What about just the mean?
# Get just the mean for r_conf
#---------------#


#---------------#

# Standard Deviation?
# standard deviation of r_conf
#---------------#


#---------------#

# You may want to "describe by" the levels of a second variable. Simple!
#---------------#


#---------------#
# Here you see descriptive stats for the variable r_conf as a function of 
# participant responses to r_before



# 8. Creating centered variables ---------------------------------------------

# Sometimes we want to transform our variable so that they have a mean of 0.
# This is because a mean of 0 typically aids the interpretation of results in 
# more complex regression models (interaction effects, etc.).
# You will learn more about this in the lectures.

# A variable in a dataframe (e.g., d$r_conf) is essentially a vector.
class(d$r_conf)
# Therefore, we can create new variables from our current variables.

# When you do something (e.g., add 3) to a variable like d$r_conf,
# R will perform the addition on the r_conf in every row of our dataframe.
#---------------#


#---------------#

# However, transformations like this are temporary and don't affect our variable 
# (d$r_conf) unless we use the "assign" symbol to create a new variable. 
# Let's do that:
#---------------#


#---------------#


# Now this new variable is in our dataframe.
d$adjusted_r_conf

# How do we mean-center a variable?
# Create a new variable, subtracting the mean r_conf from the raw data
#---------------#


#---------------#

# IMPORTANT NOTE:
scores <- c(10, 6, NA) # Here we only have data for 2 of 3 participants
mean(scores) # we try to take the average. What's going on? How do we fix it?
#---------------#


#---------------#
# na.rm tells R to remove missing data from calculations
# It is always critical to look at your data to make sure they make sense! 

# Now let's confirm that there's a new variable in the dataframe


# How can we check our work?
mean(d$r_conf_c) # The mean should be 0 (or very nearly)
describe(d$r_conf_c)

# 9. Creating standardized variables -----------------------------------------

# A standardized variable (AKA a z-scored variable) has a mean of zero
# AND a standard deviation of 1.

# When you have two variables on different scales but you want to compare them, 
# standardizing may help. By making both variables M = 0 and SD = 1, we are 
# essentially putting them on the same "unit" (e.g. Kg to lbs).

# What do we have to do to standardize a variable? 

# Let's do that together for r_conf.
#---------------#


#---------------#

# How can we check that it's standardized?
#---------------#


#---------------#
# should have mean 0 and sd 1


# 10. "Recoding" variables ----------------------------------------------------

# View values of d$r_before.
#---------------#


#---------------#

# View all possible values of d$r_before and respective frequency.
#---------------#


#---------------#
# First, we want to change these "Yes" and "No" values to 1s and 0s. 
# This is called "recoding"

# For this function, we say what dataframe we need, then old_variable_name = 
# new_variable_name
d$r_before_numeric <- dplyr::recode(d$r_before, 'Yes' = 1, 'No' = 0)

# Now we want to change r_before so that all the 0s become -.5 and all the 1s 
# become .5. Try it yourself!
#---------------#


#---------------#
# This is saying: create a new variable in d called "r_before_c", and its value 
# is based on the variable "r_before". What was 0 in r_before will be -0.5 in 
# r_before_c, and what was 1 in r_before will be 0.5

# Let's look at the new variable d$r_before_c
d$r_before_c

# 11. Creating average variables (AKA composite scores) ----------------------------

# We have 2 different variables about confidence in various things. What if
# we wanted to combine them into a single, general confidence score? 
# First, let's select the variables we want to average.
select(d, stats_conf, r_conf) 

# Then we need to take the average of each row (accounting for potential missing
# data)
# We will introduce you to "pipes" here.
#---------------#


#---------------#
# %>% (pipe) is telling R to take whatever came before this symbol and use it with 
# whatever function follows that symbol. 
# In this case, it is taking the variables we selected and computing the average
# of each participant’s values on the two variables (stats_conf & r_conf). 

# %>% belongs to the tidyverse package.

# Now let's store this score as a new variable
#---------------#


#---------------#
# NOTE: The convention in this class is to attach suffixes with underscores,
# in accordance with tidy style.

## Conventions:
# 1. We use "_z" for standardized (i.e. z-scored) variables
# 2. and "_c" for mean-centered variables
# 3. we often use "_m" for mean variables (= composite scores)

# Now we can check our new variable out
describe(d$conf_m)


# 12. Basic types of plots ------------------------------------------------

# Histograms
hist(d$r_conf) 
# Generates a histogram where the x axis represents all possible values at which
# a participant could rate their confidence about learning R, and the y axis
# represents the number of participants who rated their confidence at
# that specific value.

# Basic scatter plot (x-axis data, y-axis data)
plot(d$r_conf, d$prog_exp,
     xlab = "Confidence in learning R", ylab = "Programming experience")

# Label graph title (main), x-axis (xlab), and y-axis (ylab)
#---------------#


#---------------#

# 13. Removing objects from the environment tab -----------------------------

# You're probably going to have the experience where you are either working on 
# multiple R files one right after the other or opening and using multiple
# different dataframes within a single script. 
# If you try to use a dataframe that isn't loaded, you'll get an error.
describe(e)

# However, the much bigger problem occurs when you accidentally start mixing 
# together dataframes. For example, if you're working from dataframe "d2" but 
# previously used "d1", you might accidentally call a d1 variable and get your
# dataframes confusingly mucked up (especially if you're copy-pasting). For 
# these reasons, we suggest 2 best practices:

# 1. When you open a new R script, either hit the little broom icon in the 
# environment tab of the top right pane (GUI method), or include and run this 
# line (code method): rm(list=ls()) 

# Either of these methods will remove all of the objects that have previously 
# been created throughout your script.

# 2. When you're done using a dataframe within a given script, remove it using 
# the command rm(). This will prevent you from accidentally calling it later.
rm()

# Note: Even if you're really good about doing this, you should still name your
# dataframes different things within a given script.
# For example, you could specify them by their download date (e.g., "d_905" and 
# "d_906")

# 14. R Markdown --------------------------------------------------------------

# We will often have you turn in your homework using R Markdown. 

# RMarkdown is an alternative way to display your code and results that is 
# easier to read (like your collaborators, perhaps). RMarkdown allows us to 
# combine plain text and code: you can write plain text just like in a word 
# file, and also run "code chunks" and show their outputs alongside the text. 
# This function makes it easier to: 1) annotate and explain the code you wrote 
# using plain text, and 2) run code and show the output to support an argument
# you made in plain text.

# You should have already installed the RMarkdown package, and you don't need to 
# load in RMarkdown when you want to use it like you do with other packages. 
# Instead, go to File > New File > R Markdown to open a new R Markdown file. 
# So, go ahead and do that. When you turn in homework using RMarkdown, submit 
# your homework in HTML format or PDF - we will show you how.


# 15. Chat GPT --------------------------------------------------------------

# Coding is really hard. Large language models (LLMs) are meant to help.
# You are allowed to utilize Chat GPT or other LLMs in your assignments.
# However, these tools DO NOT always provide the code that does exactly what 
# you want. It is IMPORTANT that you establish fundamental understanding of 
# R coding, so you can at minimum spot errors in code generated by LLMs and  
# debug accordingly. At times, we may ask you to explain the code you wrote in 
# your homework assignments.

# 16. Closing --------------------------------------------------------------

# This was a lot of information, it's okay if you're lost on some points! 
# We'll have you practice what you learned today in your homework assignment, 
# and you'll gain more confidence as you try things out and see for yourself
# what does and doesn't work. We believe that lots of learning happens as you 
# work through the homework and troubleshoot (pay attention to small details!). 
# You should be able to complete the homework by using your notes in this script
# from today's lab.
# Ask lots of questions, keep trying things, and feel free to email your TAs 
# (after trying something to resolve it yourself!). 
# And drop by our office hours! See the syllabus for our contact info.

# 17. In class assignment: Begin HW1 ------------------------------------------

# You can find the assignment on Canvas. 
# Feel free to work in groups, and ask your TAs questions. 
# HOWEVER, try to work through the questions INDEPENDENTLY, as this is for your 
# own learning experience!! The purpose of this first assignment is not to stress 
# you out, but to get your feet wet. Don't forget to do the assigned readings: 
# there are questions on the HW about them.













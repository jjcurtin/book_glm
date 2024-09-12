# Psych 610/710 style guide

#### Stylish code  -------------------------------------------------------------

# There will be a number of naming conventions we will ask you to follow in this
# course. What you do after this class is totally up to you, but your life will 
# be made easier by  using consistent naming schemes that are not only clear to
# you, but also to other individuals.

# Different programming languages and organizations that use them have developed 
# style guides for their code. "Good coding style is like correct punctuation: 
# you can manage without it, butitsuremakesthingseasiertoread." We've based ours 
# for class off of the tidyverse style guide: https://style.tidyverse.org

# 1. Variable and function names should use only lowercase letters, numbers, 
# and _. Use underscores (_) (so called "snake case") to separate words within 
# a name.
# Good: day_one or day_01
# Bad: DayOne or dayone

# 2. Strive for names that are concise and meaningful (this is not easy!).
# Good: day_01
# Bad: first_day_of_the_month, djm1

# 3. Where possible, avoid re-using names of common functions and variables. 
# This will cause confusion for the readers of your code. (That's why we didn't 
# use just "file" as a variable name above! "file" is a function in base R.)

# 4. Do not put spaces inside or outside parentheses for regular function calls.
# Good: mean(x, na.rm = TRUE)
# Bad: mean (x, na.rm = TRUE) or mean(x, na.rm = TRUE )

# 5. Start comments with a single # followed by a space

# 6. Use commented lines of - and = to break up your file into easily readable 
# chunks or sections. You can see in this script we've created a section at the 
# start called "Stylish code" by typing # Stylish code followed by a series of -.
# Sections add a "fold," which you can then collapse (see the little arrow on 
# the left next to the line number for the section heading?)
# RStudio will also add all folds to the dropdown menu at the bottom-left of the 
# script window (HANDY!). You can collapse all folds at once with 
# mac: cmd + option + o
# pc: alt + o on pc. 
# Check Tools > Keyboard shortcuts to view all shortcuts.

# 7. If your script uses packages, load them all at once at the very beginning 
# of the file. This is more transparent than sprinkling library() calls 
# throughout your code or having hidden dependencies that are loaded in a 
# startup file, such as .Rprofile. Similarly, include any stem filepaths and 
# reading in data files at the beginning. That way when someone else picks up 
# your file, they can easily find which files and packages they need in order to 
# use your code. 

# 8. Most infix operators (==, +, -, <-, etc.) should always be surrounded by 
# spaces
# Good:
# height <- (feet * 12) + inches
# mean(x, na.rm = TRUE)
# Bad:
# height<-feet*12+inches
# mean(x, na.rm=TRUE)

# 9. Just as when writing sentences in English, add a space after a comma, but 
# not before. 

# 10. Keep your comment lines 80 characters or less. 
# Good: Notice how we start a new line when we reach that 80 character margin. 
# This helps readability. 
# Bad: Notice how if we did not start a new line when we reach that 80 character margin. Then the text just continues to go to the right without limit! You could set up text wrapping in your RStudio settings, but it's better to not assume anyone picking up your code has wrapping turned on. Plus, when you have wrapping turned on, it can be confusing what's an *actual* new line in your code. 

# 11. In your homeworks, please label each part of your answer, e.g., 
# 1a. 
# 1b.
# 1c. 
# etc!
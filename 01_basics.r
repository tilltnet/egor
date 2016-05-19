# title: "R Basics"
# author: "Andreas Herz, Till Krenz"
# date: '2016'

# 1. Calculate and Assigning Values
# 2. Working Directory
# 3. Read Data
# 4. Object Classes
#     - Vector
#     - Dataframe
#     - List
#     - Graph
# 5. Base Functions and Packages
# 6. Save Data

## 1. Calculate and Assigning Values -----------------------------------------

4+5+2.5
12*12

# The arrow <- operator assigns a value to a variable.
a <- 4 + 5 + 2.5
b <- 12 * 12


# The variable name returns the value.
a


# We can use variables in calculations.
a + b


## 2. Working Directory ------------------------------------------------------
# The working directory is a folder on your computer where all the data you are
# using in your R session is loaded from and stored to.

# *setwd()* sets the working directory, *getwd()* returns the current working directory.

# **Excercise:** Set your working directory to a folder were you want to save the data of this
# workshop. Check if the working directory was set correctly.

# A setwd() command could look like this
#setwd(c:\\Users\\Username\\Documents\\workshop)

# Caution for Windows-Users: In the path to the folder, use one forward slash "/" 
# or two backward slashes "\\"

# **R-Studio:** You can also use the 'File' dialogue in the down right pane to
# navigate to any folder on your computer, click the 'More' button and select
# 'Set As Working Directory'.
# 
# **R-GUI:** Click on 'File' - 'Change Dir' in order to open a dialogue for
# selecting any folder on your computer as your working directory.

## 3. Read Data --------------------------------------------------------------
# Basic R provides commands for reading common file formats used to store data for
# statistical analysis. *read.csv()* can be used to read data
# that is stored as comma seperated values, if the values are seperated by 
# semicolons use *read.csv2()*.
df <- read.csv2("01_egos.csv")
df

# Data stored in R's native format 'Rda' can be read with *load()*.
load("01_stans_network.Rda")

# **R-Studio:** An object named 'stans.network' should appear in the 'Evnironment'
# Tab.

## 4. Object Classes: vector, dataframe, list, graph ----------------------
# Objects can be of different classes. The most important ones for us are: vector, 
# dataframe, list and graph.

### Vector  --------------------------------------------------------------
# *c()* generates vectors:
c(1, 4, 7, 2)
vec <- c("Hello", "you", "!")
vec

# All entries in a vector need to be of the same class.
# 
# We can access a single value in a vector with squared brackets.
vec[2]



### Dataframe ----------------------
# The CSV file we loaded before is a dataframe
class(df)

# A dataframe is R's version of a dataset. It consits of rows and colums, which 
# represent cases and variables.
#
# We can address each 'variable' in a dataframe either by using '$'
df$age

# or by using squared brackets and quotation marks
df[["netsize"]]

# The variables in a dataframe are vectors.
is.vector(df$netsize)

# All vectors in a dataframe need to be of the same length.
# We can replace variables in a dataframe:
df$sex <- c("w", "m", "w")

# Or add new vairables:
df$exact.age <- c(39, 67, 60)
class(df$exact.age)

new.case <- c(4,"m","36 - 45", 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 44)
df <- rbind(df, new.case)
class(df$exact.age)


# Due to the mixed classes in the 'new.case' vector all the variables in df, are
# changed to character/ string. We can transform the 'exact.age' variable to
# numeric.
df$exact.age <- as.numeric(df$exact.age)


# If we work a lot with a certain dataframe, we can enclose our command in a 
# *with()*, so that we don't need to write down the name of the dataframe for each
# variable.
with(df, data.frame(sex, exact.age, age))

### List ----------------------
# Lists are like vectors, but with less limitations. Each entry of list can be of
# a different class. 
lst <- list(df, vec)
lst

# List entries are also accessed by squared brackets
lst[[2]]

# Caution: Always use double brackets [[]] to access list entries, otherwise you
# will end up with a single-entry list instead of the actual object.

# **Excercise:** Bundle up all the previouly created objects in a list.


### Graph ----------------------
# The igraph package provides support for storing, analysing and visualising 
# network data. The object class used to store network data is 'igraph'.
library(igraph)
class(stans.network)
stans.network
plot(stans.network)


## 5. Base Functions and Packages -------------------------------------------
# R Base provides a wide range of functions for calculating statistical 
# measurments, as *mean()*, *median()*, *sd()*, *anova()*.

# There are functions to do basic calculations easily, like *sum()* and
# *rowSums()*.

# The *aggregate()* command is very useful, in order to calculate measurements for
# subgroups of a dataframe.
aggregate(df$exact.age, by = df["sex"], FUN = mean)

# *Packages* add all kinds of functions to R. Packages stored on CRAN, the 
# Comprehensive R Archive Network, can be installed with *install.packages()*.

#install.packages("descr")

# *descr* is a small package that provides functions for the display of univariat
# and bivariat frequency distributions.

# Once a package is installed it can be loaded with *library()*, to make its 
# functions available in our R session.
library(descr)
freq(df$sex)


# There are 8192 (April 2016) packages on CRAN providing all kinds of functions 
# and procedures. Some packages that might be interesting for people adapting 
# from another statistical software or language, are packages that faciltate the
# import of data from SPSS, Stata, SAS etc.
#   - i.e. readr, haven, foreign

# There is a set of packages that attempt to make R code more straight forward
# and better fitted for everyday business of a data scientist, colloquially these
# are called the 'Hadleyverse' since Hadley Wickham is the creator of most of
# these packages
#   - i.e. plyr, dplyr, margrittr, ggplot2, ggvis, tidyr

## 6. Save Data ------------------------------------------------------------
# Of course we can save our data on to the harddrive. *write.csv()* saves the data 
# as a CSV file, making it easily available to use in other sofware. *save()* uses
# the Rda format.
write.csv2(x = df, file = "df2.csv")
save(stans.network, df, lst, file = "saved_objects.Rda")
#---------------------R for Political Research: Lesson I-----------------------
#-Author: A. Jordan Nafa-----------------------------Created: August 19, 2022-#
#-R Version: 4.2.1-----------------------------------Revised: August 22, 2022-#

# Set Session Options, you could also declare these in .Rprofile
options(
  digits = 4, # Significant figures output
  scipen = 999, # Disable scientific notation
  repos = getOption("repos")["CRAN"] # repo to install packages from
)

# Load Required Libraries, run install.packages("pacman") first
pacman::p_load(
  "tidyverse", # Suite of packages for tidy data management 
  "data.table", # Package for high-performance data management 
  "dtplyr", # Package to interface between dplyr and data.table
  install = FALSE # Set this to TRUE to install missing packages
)

# Load the function for creating working directories in projects
source(str_c(here::here(), "/functions/project-subdirectories.R"))

# Add data, scripts, functions, documents, figures, and models folders
make_project_dirs(project_dir = NULL)

#-----------------------------------------------------------------------------#
#---------------------Lesson 1.1: Basic Calculations in R----------------------
#-----------------------------------------------------------------------------#

# Using R for Addition
2 + 2

# We can also find the sum of a sequence of numbers
sum(2, 4, 6)

# Using R for Subtraction
6 - 2

# Using R for Multiplication
9 * 12

# We can also find the product of a sequence of numbers
prod(9, 12, 36)

# Using R for Division
4/2

# Using R for Square Roots
sqrt(26 + 4)

# Using R for Exponents
exp24 <- 2^4

# Using R for more complex calculations
sqrt(sum(2, 4, 6) / exp24)

#-----------------------------------------------------------------------------#
#----------------------Lesson 1.2: Data Structures-----------------------------
#-----------------------------------------------------------------------------#

## Objects and Assignment----

# Assign numeric values to two objects, x and y
x <- 10^2; y <- 6^2

# Calculate the sum of the objects
sum_xy <- sum(x, y)

# Print the value contained in the sum_xy object
print(sum_xy)

## Vectors----

# Create a vector of numeric values 
xy <- c(10^2, 6^2)

# Calculate the sum of the values in xy
sum_xy <- sum(xy)

# Print the value contained in the sum_xy object
print(sum_xy)

# Print the first value in xy
print(xy[1])

# Print the second value in xy
print(xy[2])

# Define a mixed numeric-character vector
num_char <- c(10, 12, 2, "apple", "pair")

# Check the type of num_char
class(num_char)

# Define a mixed numeric-logical vector
num_logi <- c(10, 12, 2, TRUE, FALSE)

# Check the type of num_logi
class(num_logi)

## Data Frames and Tibbles----

# Create a data frame of fruit
df_fruit <- data.frame(
  flavor = c("sour", "bitter", "sweet", "sour"),
  fruit = c("lemon", "grapefruit", "pineapple", "lime"),
  stock = c(10, 2, 13, 4)
)

# head prints the first n rows of the data
head(df_fruit, n = 4)

# get the names of fruit using the $ approach
fruit_a <- df_fruit$fruit

# get the names of fruit by referencing the column name
fruit_b <- df_fruit[, "fruit"]

# get the names of fruit by referencing the column position
fruit_c <- df_fruit[, 2] # fruit is the second column

# check that all of the outputs produce the same result
isTRUE(all.equal(fruit_a, fruit_b, fruit_c))

# print the second column of fruit
print(df_fruit[, 2])

# print the second row of each column
print(df_fruit[2, ])

# print rows 1 and 3 of fruit and stock
print(df_fruit[c(1, 3), 2:3])

# Create a tibble of fruit
tbl_fruit <- tibble(
  flavor = c("sour", "bitter", "sweet", "sour"),
  fruit = c("lemon", "grapefruit", "pineapple", "lime"),
  stock = c(10, 2, 13, 4)
)

# glimpse prints the data frame by row
glimpse(tbl_fruit)

# get the names of fruit using the $ approach
fruit_d <- tbl_fruit$fruit

# get the names of fruit by referencing the column name
fruit_e <- tbl_fruit[, "fruit"]

# get the names of fruit by referencing the column position
fruit_f <- tbl_fruit[, 2] # fruit is the second column

# check that all of the outputs produce the same result
isTRUE(all.equal(fruit_d, fruit_e, fruit_f))

# get the mean of each value in stock using $ approach
mean(tbl_fruit$stock)

# get the mean of stock by referencing the column name
mean(tbl_fruit[, "stock"])

## Functions----

# Define a function to divide two numbers x and y
quotient <- function(x, y) {
  
  ## Divide x by y and store the result in out
  out <- x/y
  
  ## Return the number stored in the object out
  return(out)
}

# Find the quotient of 10 and 3
quotient(x = 10, y = 3)

#-----------------------------------------------------------------------------#
#----------------------Lesson 1.3: Libraries and Packages----------------------
#-----------------------------------------------------------------------------#

# Install a single package from the CRAN repository
install.packages("remotes")

# Installing multiple packages from the CRAN repository
install.packages(c(
  "downlit",
  "xml2", 
  "rmarkdown", 
  "sessioninfo"
))

# Installing packages from a non-CRAN repository
install.packages(
  "easystats", 
  repos = "https://easystats.r-universe.dev"
)

# Easy stats also provides a way to installed its dependencies
easystats::install_suggested()

# Load the remotes package
library(remotes)

# Installing a package from github using the remotes package
install_github("vdeminstitute/vdemdata")

# Getting the citation information for a package in R
citation("dplyr")

# View the help documentation for dplyr::mutate
?dplyr::mutate

# You can also use the help function
help(topic = "mutate", package = "dplyr")

## Copyright and Licensing Notice----
# 
# All R, HTML, and CSS code for this course is provided for public use 
# under a BSD 3-Clause License. For licensing terms and restrictions 
# see https://github.com/ajnafa/psci-3300-political-science-research

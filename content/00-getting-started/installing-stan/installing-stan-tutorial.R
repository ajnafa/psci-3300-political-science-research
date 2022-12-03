#-------------Setup: Getting Started with Stan, cmdstanr, and brms--------------
#-Author: A. Jordan Nafa-------------------------------Created: August 4, 2022-#
#-R Version: 4.2.1------------------------------------Revised: August 22, 2022-#

## Step 1: Preliminaries----

# Set Session Options
options(
  digits = 6, # Significant figures output
  scipen = 999, # Disable scientific notation
  repos = getOption("repos")["CRAN"] # Install packagess from CRAN
)

# Set the makeflags to use multiple cores for faster compilation
Sys.setenv(
  MAKEFLAGS = paste0(
    "-j", 
    parallel::detectCores(logical = FALSE)
  ))

# Check if any existing Stan packages are installed
{
  ## Check for existing installations
  stan_packages <- installed.packages()[
    grepl("cmdstanr|rstan$|StanHeaders|brms$", 
          installed.packages()[, 1]), 1]
  
  ## Remove any existing Stan packages
  if (length(stan_packages) > 0) {
    remove.packages(c("StanHeaders", "rstan", "brms"))
  }
  
  ## Delete any pre-existing RData file
  if (file.exists(".RData")) {
    file.remove(".RData")
  }
}

# Check if packages necessary for later installation steps are installed
{
  ## Retrieve installed packages
  pkgs <- installed.packages()[, 1]
  
  ## Check if rstudioapi is installed
  if (isTRUE(all.equal(grep("rstudioapi", pkgs), integer(0)))) {
    print("Installing the {rstudioapi} package")
    install.packages("rstudioapi")
  }
  
  ## Check if remotes is installed
  if (isTRUE(all.equal(grep("remotes", pkgs), integer(0)))) {
    print("Installing the {remotes} package")
    install.packages("remotes")
  }
  
  ## Else print a message
  else {
    print("{remotes} and {rstudioapi} packages are already installed")
  }
}

## Step 2: Installing rstan and brms----

# Install the development versions of rstan and StanHeaders
install.packages(
  pkgs = "rstan",
  repos = c(
    "https://mc-stan.org/r-packages/", 
    getOption("repos")
  ))

# This will fit a simple example model to check that the Stan compiler is working
example(stan_model, package = "rstan", run.dontrun = TRUE)

# You can either manually restart your R session via RStudio's GUI or run this code
rstudioapi::restartSession()

# Install the latest development version of brms from github
remotes::install_github("paul-buerkner/brms")

## Step 3: Installing cmdstanr and cmdstan----

# Install cmdstanr from github
remotes::install_github("stan-dev/cmdstanr")

# Check that the C++ Toolchain is Configured
cmdstanr::check_cmdstan_toolchain(fix = TRUE)

# Install cmdstan version 2.30.1
cmdstanr::install_cmdstan(
  cores = parallel::detectCores(logical = FALSE),
  overwrite = TRUE,
  cpp_options = list("STAN_THREADS" = TRUE),
  check_toolchain = TRUE
)

# You can either manually restart your R session via RStudio's GUI or run this code
rstudioapi::restartSession()

# Verify that cmdstan installed successfully
(cmdstan.version <- cmdstanr::cmdstan_version())

# Ensure cmdstan path is set properly
cmdstanr::set_cmdstan_path(
  path = paste(
    Sys.getenv("HOME"), 
    "/.cmdstan/cmdstan-", 
    cmdstan.version,
    sep = ""
  ))

## Step 4: WINDOWS USERS ONLY----

# Execute `mingw32-make install-tbb` in the terminal
rstudioapi::terminalExecute(
  command = "mingw32-make install-tbb",
  workingDir = cmdstanr::cmdstan_path()
)

# Reset the terminal
rstudioapi::terminalKill(id = rstudioapi::terminalList())

## Note: You need to close RStudio entirely and reopen it at the 
## end of step 4 here

## Step 5: Verifying the Installation----

# Load the brms library
library(brms)

# Load the built-in mtcars data
data("mtcars")

## Take the log of horsepower
mtcars$log_hp <- log(mtcars$hp)

## Specify some weakly informative priors for the model parameters
mpg_priors <- prior(student_t(10, 20.09, 2), class = Intercept) +
  prior(normal(0, 9.24), class = b, coef = wt) +
  prior(normal(0, 12.68), class = b, coef = log_hp) +
  prior(exponential(1), class = sigma)

## Fit the model
bayes_mpg_fit <- brm(
  formula = mpg ~ wt + log_hp, # Formula describing the model
  family = gaussian(), # Linear regression
  data = mtcars, # Data for the model
  prior = mpg_priors,
  cores = parallel::detectCores(logical = FALSE), # Number of cores to use
  chains = 4, # Number of chains, should be at least 4
  iter = 2000, # Total iterations = Warm-Up + Sampling
  warmup = 1000, # Warm-Up Iterations
  refresh = 0, # Disable printing progress
  save_pars = save_pars(all = TRUE),
  backend = "cmdstanr", # Requires cmdstanr and cmdstan be installed
  silent = 2
)

## Print a summary of the output
summary(bayes_mpg_fit)

## Copyright and Licensing Notice----
# 
# All R, HTML, and CSS code for this course is provided for public use 
# under a BSD 3-Clause License. For licensing terms and restrictions 
# see https://github.com/ajnafa/psci-3300-political-science-research

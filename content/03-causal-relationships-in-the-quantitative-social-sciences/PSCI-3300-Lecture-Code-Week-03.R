#-----PSCI 3300: Causal Relationships in the Quantitative Social Sciences-------
#-Author: A. Jordan Nafa---------------------------Created: September 13, 2022-#
#-R Version: 4.1.2---------------------------Last Modified: September 15, 2022-#

# Set Session Options
options(
  digits = 6, # Significant figures output
  scipen = 999, # Disable scientific notation
  repos = getOption("repos")["CRAN"],
  knitr.kable.NA = ''
)

# Load the required libraries
pacman::p_load(
  "tidyverse",
  "data.table",
  "dtplyr",
  install = FALSE
)

#------------------------------------------------------------------------------#
#------------------Democracy and Gender Equality Scatter Plot-------------------
#------------------------------------------------------------------------------#

# Install the vdemdata packafge if you haven't already
# remotes::install_github("vdeminstitute/vdemdata")

# Load the vdemdata library
data(vdem, package = "vdemdata")

gender_data <- vdem %>% 
  # Use just the year 2018
  filter(year == 2018) %>%  
  # Transmute a subset of the data for plotting
  select(
    country_name,
    v2x_polyarchy, 
    v2x_gender
  )

# Initiate the plot object
ggplot(gender_data, aes(x = v2x_gender, y = v2x_polyarchy)) +
  # Add the data points
  geom_point(
    aes(fill = country_name), 
    show.legend = FALSE, 
    shape = 21,
    size = 3
  ) +
  # Color for the fill
  scale_fill_viridis_d() +
  # Labels for the plot
  labs(
    x = "Women's Political Empowerment Index",
    y = "Procedural Democracy"
  ) +
  # Adjust the x axis scales
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  # Adjust the y axis scales
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  # Add annotation
  annotate(
    "text",
    x = 0.3,
    y = 0.7,
    size = 12,
    family = "serif",
    label = latex2exp::TeX(r'($\rho_{x,y} = 0.7954$)'),
    parse = TRUE
  ) +
  # Twek the theme settings
  theme_bw(base_family = "serif", base_size = 24)

#------------------------------------------------------------------------------#
#------------------------What is Correlation Good For?--------------------------
#------------------------------------------------------------------------------#

# Get the data we need from the vdemdata package
vdem_df <- vdem %>% 
  # We'll use just the year 2018 here for simplicity
  filter(year == 2018) %>%  
  # Transmute a subset of the data for plotting
  transmute(
    country_name, 
    v2x_polyarchy = v2x_polyarchy*10, 
    v2x_gender = v2x_gender*10
  )

# Estimate the linear relationship
lm_democ_gender <- lm(v2x_polyarchy ~ v2x_gender, data = vdem_df)

# Print a summary of the result
broom::tidy(lm_democ_gender)

# Initiate the plot object
ggplot(vdem_df, aes(x = v2x_gender, y = v2x_polyarchy)) +
  # Add the data points
  geom_point(
    aes(fill = country_name), 
    show.legend = FALSE, 
    shape = 21,
    size = 3
  ) +
  # Color for the fill
  scale_fill_manual(values = viridisLite::turbo(n = length(vdem_df$country_name))) +
  # Labels for the plot
  labs(
    x = "Women's Political Empowerment Index",
    y = "Procedural Democracy",
    title = latex2exp::TeX(r'($E\[Y_{i} | X_{i}\] = -3.03 + 1.12 \cdot X_{i}$ or $E\[Y_{i} | X_{i}\] = \alpha + \beta_{1}X_{i}$)')
  ) +
  # Adjust the x axis scales
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  # Adjust the y axis scales
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  # Plot the linear regression line
  geom_abline(
    intercept = -3.03, 
    slope = 1.12,
    lty = 2,
    size = 2
  ) +
  # Twek the theme settings
  theme_bw(base_family = "serif", base_size = 24)

#------------------------------------------------------------------------------#
#-------------------Univariate Descriptive Statistics in R----------------------
#------------------------------------------------------------------------------#

# Mean of women's political empowerment in 2018
mean(vdem_df$v2x_gender, na.rm = TRUE)

# Standard deviation of women's political empowerment in 2018
sd(vdem_df$v2x_gender, na.rm = TRUE)

# Variance of women's political empowerment in 2018
var(vdem_df$v2x_gender, na.rm = TRUE)

#------------------------------------------------------------------------------#
#--------------------Bivariate Descriptive Statistics in R----------------------
#------------------------------------------------------------------------------#

# Covariance for gender and democracy
cov(x = vdem_df$v2x_gender, y = vdem_df$v2x_polyarchy)

# Correlation coefficient for gender and democracy
cor(x = vdem_df$v2x_gender, y = vdem_df$v2x_polyarchy)

# (x_{i} - mu_{x})
cov_x <- vdem_df$v2x_gender - mean(vdem_df$v2x_gender)

# (y_{i} - mu_{y})
cov_y <- vdem_df$v2x_polyarchy - mean(vdem_df$v2x_polyarchy)

# (x_{i} - mu_{x})(y_{i} - mu_{y})
cov_xy <- cov_x*cov_y

# Sum of (x_{i} - mu_{x})(y_{i} - mu_{y})
sum_cov_xy <- sum(cov_xy)

# Covariance of x and y
(cov_result <- sum_cov_xy/length(cov_xy))

# sigma_{x} sigma_{y}
sigma_xy <- sd(vdem_df$v2x_gender)*(vdem_df$v2x_polyarchy)

# Correlation
cov_result/(sd(vdem_df$v2x_gender)*sd(vdem_df$v2x_polyarchy))

#------------------------------------------------------------------------------#
#-------------------------Non-Linear Relationships------------------------------
#------------------------------------------------------------------------------#

# Simulate some non-linear relationships
nonlinear_sims <- tibble(
  x = runif(n = 2e3, min = -10, max = 10),
  y_posquad = x + x^2 + rnorm(2e3, 0, 2),
  y_negquad = x - x^2 + rnorm(2e3, 0, 2),
  y_sin = sin(x*3.14) + rnorm(2e3, 0, 0.5),
  y_linear = x + rnorm(2e3, 0, 2)
)

# Initiate the plot object
ggplot(nonlinear_sims, aes(x = x, y = y_posquad, fill = x)) +
  # Add the data points
  geom_point(shape = 21, size = 3, show.legend = FALSE) +
  # Add the linear fit
  geom_smooth(
    method = "lm", 
    size = 2, 
    se = FALSE, 
    lty = 2, 
    color = "black"
    ) +
  # Tweak the fill color scheme
  scale_fill_viridis_c() +
  # Labels for the plot
  labs(
    x = "X",
    y = "Y",
    title = latex2exp::TeX(r'($Y_{i} = X_{i} + X_{i}^{2} + \epsilon_{i}$)')
  ) +
  # Adjust the x axis scales
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  # Adjust the y axis scales
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  # Twek the theme settings
  theme_bw(base_family = "serif", base_size = 24)

# Initiate the plot object
ggplot(nonlinear_sims, aes(x = x, y = y_negquad, fill = x)) +
  # Add the data points
  geom_point(shape = 21, size = 3, show.legend = FALSE) +
  # Add the linear fit
  geom_smooth(
    method = "lm", 
    size = 2, 
    se = FALSE, 
    lty = 2,
    color = "black"
  ) +
  # Tweak the fill color scheme
  scale_fill_viridis_c() +
  # Labels for the plot
  labs(
    x = "X",
    y = "Y",
    title = latex2exp::TeX(r'($Y_{i} = X_{i} - X_{i}^{2} + \epsilon_{i}$)')
  ) +
  # Adjust the x axis scales
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  # Adjust the y axis scales
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  # Twek the theme settings
  theme_bw(base_family = "serif", base_size = 24)

# Initiate the plot object
ggplot(nonlinear_sims, aes(x = x, y = y_sin, fill = x)) +
  # Add the data points
  geom_point(shape = 21, size = 3, show.legend = FALSE) +
  # Add the linear fit
  geom_smooth(
    method = "lm", 
    size = 2, 
    se = FALSE, 
    lty = 2, 
    color = "black"
  ) +
  # Tweak the fill color scheme
  scale_fill_viridis_c() +
  # Labels for the plot
  labs(
    x = "X",
    y = "Y",
    title = latex2exp::TeX(r'($Y_{i} = Sin(X_{i}\cdot \pi) + \epsilon_{i}$)')
  ) +
  # Adjust the x axis scales
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  # Adjust the y axis scales
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  # Twek the theme settings
  theme_bw(base_family = "serif", base_size = 24)

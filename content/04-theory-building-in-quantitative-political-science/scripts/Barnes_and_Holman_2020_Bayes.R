#-------------Replicating Barnes & Holman (2020): Bayesian Edition--------------
#-Author: A. Jordan Nafa---------------------------Created: September 21, 2022-#
#-R Version: 4.2.1---------------------------------Revised: September 26, 2022-#

# Set Session Options
options(
  digits = 6, # Significant figures output
  scipen = 999, # Disable scientific notation
  repos = getOption("repos")["CRAN"],
  knitr.kable.NA = '',
  brms.backend = "cmdstanr",
  modelsummary_get = "broom"
)

# Load the necessary libraries
pacman::p_load(
  "sjlabelled", # Package for working with labeled data
  "tidyverse", # Suite of packages for data management 
  "haven", # Reading in data from proprietary software (Stata, SPSS, SAS) 
  "brms", # Bayesian regression models with Stan
  "tidybayes", # Functions for wrangling posteriors tidy-style
  "modelsummary", # Package for making tables of model output
  "kableExtra", # Package for customizing html and latex tables
  "patchwork", # Combining multiple plots into one
  "future", # Package for parallel computation
  install = FALSE # Set this to TRUE to install missing packages
)

# Source the helper functions, assumes you're working in the course's R project
.helpers <- map(
  .x = list.files(
    path = "functions/", 
    pattern = ".*R", 
    full.names = TRUE
  ), 
  .f = ~ source(.x)
)

# Update the ggplot2 theme
theme_set(theme_tidybayes())

#------------------------------------------------------------------------------#
#-----------------------------Data Processing-----------------------------------
#------------------------------------------------------------------------------#

# Load the replication data, need to use the read_dta function from {haven}
# since the replication data is in Stata's proprietary file format
jop_data <- read_dta("data/Barnes_and_Holman_JOP2020.dta")

# Extract the data for the models
model_data <- jop_data %>% 
  # Select will return a subset with just the specified variables
  select(chamber_year:senate, diversity_person:logdm) %>% 
  # Get the time index
  mutate(
    # Apply value labels from chamber year
    chamber_year = factor(
      chamber_year,
      levels = get_values(chamber_year),
      labels = get_labels(chamber_year)
    ),
    # Get the year from chamber_year
    year = str_extract(chamber_year, "20[0-9][0-9]") %>% as.integer()
  )

# you can use glimpse to preview the data
glimpse(model_data)

#------------------------------------------------------------------------------#
#------------------------Building the Model Formulas----------------------------
#------------------------------------------------------------------------------#

# Character vector of response variables for each model in table 1 on pp. 1279
responses <- c(
  "prof_diversity", 
  "diversity_person", 
  "prof_diversity_f", 
  "diversity_person_f",
  "prof_diversity_m",
  "diversity_person_m"
)

# Formula for the LHS which are the same across models in table 1
rhs_form <- "~female + quotayear + logdm + senate + unemployment + gdi"

# Build the list of formulas
model_forms <- formula_builder(lhs = responses, rhs = rhs_form)

#------------------------------------------------------------------------------#
#--------------------Estimating the Full Models from Table 1--------------------
#------------------------------------------------------------------------------#

# Specify the priors object to pass to brm
model_priors <- prior(normal(0, 1.5), class = "b") +
  prior(normal(mean(Y), 2*sd(Y)), class = "Intercept") +
  prior(exponential(1/sd(Y)), class = "sigma")

# Build a list of file paths to save the models to
model_paths <- str_c(
  "models/Barnes-and-Holman-2020/gaussian_", 
  responses, 
  "_full"
)

# Fit each of the models (6 chains, 5k iterations)
bayes_gaussian_fits <- map(
  .x = seq_along(model_forms),
  .f = ~ brm(
    formula = bf(model_forms[[.x]], decomp = "QR"),
    family = brmsfamily("gaussian", link = "identity"),
    prior = model_priors,
    data = model_data,
    cores = 6,
    chains = 6,
    iter = 5000,
    warmup = 3000,
    refresh = 1000,
    control = list(max_treedepth = 12),
    save_pars = save_pars(all = TRUE),
    seed = 12345,
    backend = "cmdstanr",
    file = model_paths[.x]
  )
)

#------------------------------------------------------------------------------#
#-------------Fit Statistics and Cross-Validation for Table 1 Models------------
#------------------------------------------------------------------------------#

# Add additional model fit criteria
bayes_gaussian_fits <- map(
  .x = bayes_gaussian_fits,
  .f = ~ add_criterion(
    .x,
    criterion = c("loo", "loo_R2"),
    seed = 12345,
    moment_match = TRUE
  ))

# Use future to parallelize cross validation
plan(multisession(workers = 6))

# Perform exact leave-one-out cross validation for the full models. Be aware
# that this can be computationally intensive and is mainly for illustrative
# purposes. In most cases the approximate loo criteria will be fine
bayes_gaussian_fits <- map(
  .x = bayes_gaussian_fits,
  .f = ~ add_criterion(
    .x,
    criterion = "kfold",
    folds = "loo",
    save_fits = TRUE,
    chains = 6,
    seed = 12345,
    iter = 5000,
    warmup = 3000,
    control = list(
      max_treedepth = 12,
      adapt_delta = 0.90
    )
  )
)

# Close Future Sessions
plan(sequential)

#------------------------------------------------------------------------------#
#---------------------Reproducing the Results from Table 1----------------------
#------------------------------------------------------------------------------#

## Reproduce table 1 on pp. 1279----
jop_table_1 <- modelsummary(
  bayes_gaussian_fits,
  output = "data.frame",
  fmt = 3,
  conf_level = 0.95,
  statistic = "conf.int",
  kfold = TRUE, # Use looic = TRUE here instead if you didn't do k-fold CV
  looR2 = TRUE,
  signif = TRUE,
  estimate = "{estimate}{stars}",
  stars = c('*' = .05),
  coef_map = c(
    "b_female" = "% Women",
    "b_quotayear" = "Quota Years",
    "b_logdm" = "Log District Magnitude",
    "b_senate" = "Senate (Ref = House)",
    "b_unemployment" = "Unemployment",
    "b_gdi" = "GDI",
    "b_Intercept" = "Intercept",
    "sigma" = "Residual Variance"
  ),
  gof_map = tribble(
    ~ raw, ~ clean, ~ fmt,
    "n", "N Chamber-Years", 0,
    "elpd.kfold", "LOO-CV ELPD", 2, # Set the first argument to elpd.loo
    "kfold.ic", "LOO-CV IC", 2, # Set the first argument to loo.ic
    "loo_R2.mean", "Adj. Bayes R<sup>2</sup>", 2
  )
) %>% 
  ## Tweaking term names
  mutate(
    term = case_when(
      statistic == "conf.int" ~ NA_character_,
      TRUE ~ term
    )) %>% 
  ## Select a subset of the data
  select(-c(statistic, part))

# Generate a kable
kbl(
  jop_table_1,
  format = "html",
  row.names = F,
  col.names = c("", rep(c("Professional", "Personal"), 3)),
  align = "lcccccc",
  caption = "Table 1 from Barnes and Holman (2020)",
  booktabs = T,
  escape = FALSE,
  linesep = ""
) %>%
  # Grouping header for the models
  add_header_above(c(" ", "Chamber Wide" = 2, "Women" = 2, "Men" = 2)) %>% 
  # Group the table rows
  group_rows(index = c(
    "Population-Level Effects" = 14, 
    "Distributional Parameters" = 2
  ),
  label_row_css = ""
  ) %>% 
  # Set the header to repeat at the top of each page
  kable_classic(html_font = "serif") %>% 
  ## Add a footnote
  footnote(number  = "Asterisks indicate that 95% of the posterior density falls in a given direction. 95% Bayesian credible intervals are shown in brackets.")

#------------------------------------------------------------------------------#
#---------------------Reproducing the Results from Figure 3---------------------
#------------------------------------------------------------------------------#

## Make the data frame to generate expectations for % women
female_preds_data <- map(
  .x = bayes_gaussian_fits[1:2],
  ~ .x$data %>% 
    ## Hold all of the covariates at their means
    summarise(across(quotayear:gdi, ~ mean(.x))) %>% 
    ## Expand the data by values for % female holding all else constant
    expand_grid(female = seq(0, 0.5, 0.001))
)

## Generate posterior expectations
full_female_epreds <- map2(
  .x = bayes_gaussian_fits[1:2],
  .y = female_preds_data,
  ~ add_predicted_draws(
    newdata = .y,
    object = .x
  )
)

## Upper quadrant plot for Figure 3 on page 1280
fig3_ab <- map2(
  .x = full_female_epreds,
  .y = c("Professional Diversity", "Personal Diversity"),
  ~ ggplot(.x, aes(x = female, y = .prediction)) +
    ## Add a lineribbon geom
    stat_lineribbon(
      aes(fill_ramp = stat(.width)),
      .width = ppoints(100),
      fill = "royalblue1",
      color = "white",
      point_interval = "mean_qi",
    ) +
    ## Generate the color gradients
    ggdist::scale_fill_ramp_continuous(range = c(1, 0)) +
    ## Adjust the scale of the y axis
    scale_y_continuous(
      breaks = seq(0.2, 0.55, by = 0.05)
    ) +
    ## Adjust the scale of the x axis
    scale_x_continuous(
      breaks = seq(0, 0.5, by = 0.1), 
      expand = c(0.001, 0.005),
      labels = scales::percent
    ) +
    ## Add labels to the plot
    labs(
      x = "% Female Legislators",
      y = .y,
      fill_ramp = "Credible\nInterval"
    )
)

# Save the generated plot objects as .jpeg files----
map2(
  .x = c("Fig3A_Barnes_and_Holman_2020.jpeg", "Fig3B_Barnes_and_Holman_2020.jpeg"),
  .y = fig3_ab,
  .f = ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = "models/Barnes-and-Holman-2020",
    width = 14,
    height = 12,
    units = "in",
    type = "cairo",
    dpi = "retina",
    limitsize = F
  )
)

## Make the data frame to generate expectations for quota years
quotayear_preds_data <- map(
  .x = bayes_gaussian_fits[1:2],
  ~ .x$data %>% 
    ## Hold all of the covariates at their means
    summarise(across(c(female, logdm:gdi), ~ mean(.x))) %>% 
    ## Expand the data by values for % female holding all else constant
    expand_grid(quotayear = seq(0, 20, 1))
)

## Generate posterior expectations for quota years
full_quotayear_epreds <- map2(
  .x = bayes_gaussian_fits[1:2],
  .y = quotayear_preds_data,
  ~ add_epred_draws(
    newdata = .y,
    object = .x
  )
)

## Lower quadrant plot for Figure 3 on page 1280
fig3_cd <- map2(
  .x = full_quotayear_epreds,
  .y = c("Professional Diversity", "Personal Diversity"),
  ~ ggplot(.x, aes(x = quotayear, y = .epred)) +
    ## Add a lineribbon geom
    stat_lineribbon(
      aes(fill_ramp = stat(.width)),
      .width = ppoints(100),
      fill = "#32CD32",
      point_interval = "mean_qi",
    ) +
    ## Generate the color gradients
    ggdist::scale_fill_ramp_continuous(range = c(1, 0)) +
    ## Adjust the scale of the y axis
    scale_y_continuous(
      breaks = seq(0.1, 0.55, by = 0.05), 
      limits = c(0.1, 0.55)
    ) +
    ## Adjust the scale of the x axis
    scale_x_continuous(
      breaks = seq(0, 20, by = 5), 
      expand = c(0.001, 0.005)
    ) +
    ## Add labels to the plot
    labs(
      x = "Time in Years Since Quota Adoption",
      y = .y,
      fill_ramp = "Credible\nInterval"
    )
)

# Save the generated plot objects as .jpeg files----
map2(
  .x = c("Fig3C_Barnes_and_Holman_2020.jpeg", "Fig3D_Barnes_and_Holman_2020.jpeg"),
  .y = fig3_cd,
  .f = ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = "models/Barnes-and-Holman-2020",
    width = 14,
    height = 12,
    units = "in",
    type = "cairo",
    dpi = "retina",
    limitsize = F
  )
)

## Figure 3 Combined Version
fig3_combo <- (fig3_ab[[1]] | fig3_ab[[2]])/(fig3_cd[[1]] | fig3_cd[[2]]) + 
  plot_layout(guides = 'collect') & plot_annotation(
    title = "Figure 3. Chamber-Wide Professional (Left) and Personal (Right) Legislative Diversity from Barnes and Holman (2020)",
    subtitle = str_wrap("Expectations are estimated based on the mean of 42,000 draws from the posterior distributions of models 1
                        and 2 holding all covariates constant at their respective means. Gradients represent Bayesian credible 
                        intervals for the posterior expectations", width = 180))

# Save the combined plot object as a .jpeg file----
ggsave(
  filename = "Fig3_Barnes_and_Holman_2020.jpeg",
  plot = fig3_combo,
  device = "jpeg",
  path = "models/Barnes-and-Holman-2020",
  width = 14,
  height = 10,
  units = "in",
  type = "cairo",
  dpi = "retina",
  limitsize = F
)

#------------------------------------------------------------------------------#
#---------------------Reproducing the Results from Figure 5---------------------
#------------------------------------------------------------------------------#

## Make the data frame to generate expectations for % women in subgroups
female_preds_data_subgroups <- map(
  .x = bayes_gaussian_fits[3:6],
  ~ .x$data %>% 
    ## Hold all of the covariates at their means
    summarise(across(quotayear:gdi, ~ mean(.x))) %>% 
    ## Expand the data by values for % female holding all else constant
    expand_grid(female = seq(0, 0.5, 0.001))
)

## Generate posterior expectations for the subgroup models
subgroups_female_epreds <- map2(
  .x = bayes_gaussian_fits[3:6],
  .y = female_preds_data_subgroups,
  ~ add_epred_draws(
    newdata = .y,
    object = .x
  )
)

## Upper half plot for Figure 5 on page 1282----
fig5_upper <- map2(
  .x = subgroups_female_epreds,
  .y = rep(c("Professional Diversity", "Personal Diversity"), 2),
  ~ ggplot(.x, aes(x = female, y = .epred)) +
    ## Add a lineribbon geom
    stat_lineribbon(
      aes(fill_ramp = stat(.width)),
      .width = ppoints(100),
      fill = "firebrick",
      point_interval = "mean_qi",
    ) +
    ## Generate the color gradients
    ggdist::scale_fill_ramp_continuous(range = c(1, 0)) +
    ## Adjust the scale of the y axis
    scale_y_continuous(
      breaks = seq(0.1, 0.5, by = 0.1), 
      limits = c(0.05, 0.53)
    ) +
    ## Adjust the scale of the x axis
    scale_x_continuous(
      breaks = seq(0, 0.5, by = 0.1), 
      expand = c(0.001, 0.005),
      labels = scales::percent
    ) +
    ## Add labels to the plot
    labs(
      x = "% Female Legislators",
      y = .y,
      title = .y,
      fill_ramp = "Credible\nInterval"
    )
)

# Save the generated plot objects as .jpeg files----
map2(
  .x = c("Fig5A_Barnes_and_Holman_2020.jpeg", 
         "Fig5B_Barnes_and_Holman_2020.jpeg", 
         "Fig5C_Barnes_and_Holman_2020.jpeg", 
         "Fig5D_Barnes_and_Holman_2020.jpeg"),
  .y = fig5_upper,
  .f = ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = "models/Barnes-and-Holman-2020",
    width = 14,
    height = 12,
    units = "in",
    type = "cairo",
    dpi = "retina",
    limitsize = F
  )
)

## Make the data frame to generate expectations for quota years in the subgroups
quotayear_preds_data_subgroups <- map(
  .x = bayes_gaussian_fits[3:6],
  ~ .x$data %>% 
    ## Hold all of the covariates at their means
    summarise(across(c(female, logdm:gdi), ~ mean(.x))) %>% 
    ## Expand the data by values for % female holding all else constant
    expand_grid(quotayear = seq(0, 20, 1))
)

## Generate posterior expectations for quota years in the subgroups
subgroups_quotayear_epreds <- map2(
  .x = bayes_gaussian_fits[3:6],
  .y = quotayear_preds_data_subgroups,
  ~ add_epred_draws(
    newdata = .y,
    object = .x
  )
)

## Lower half plot for Figure 5 on page 1282----
fig5_lower <- map2(
  .x = subgroups_quotayear_epreds,
  .y = rep(c("Professional Diversity", "Personal Diversity"), 2),
  ~ ggplot(.x, aes(x = quotayear, y = .epred)) +
    ## Add a lineribbon geom
    stat_lineribbon(
      aes(fill_ramp = stat(.width)),
      .width = ppoints(100),
      fill = "darkviolet",
      point_interval = "mean_qi",
    ) +
    ## Generate the color gradients
    ggdist::scale_fill_ramp_continuous(range = c(1, 0)) +
    ## Adjust the scale of the y axis
    scale_y_continuous(
      breaks = seq(0.1, 0.55, by = 0.05), 
      limits = c(0.05, 0.55)
    ) +
    ## Adjust the scale of the x axis
    scale_x_continuous(
      breaks = seq(0, 20, by = 5),
      expand = c(0.001, 0.005),
      limits = c(-0.1, 20.5),
    ) +
    ## Add labels to the plot
    labs(
      x = "Time in Years Since Quota Adoption",
      y = .y,
      fill_ramp = "Credible\nInterval"
    )
)

# Save the generated plot objects as .jpeg files----
map2(
  .x = c("Fig5E_Barnes_and_Holman_2020.jpeg", 
         "Fig5F_Barnes_and_Holman_2020.jpeg", 
         "Fig5G_Barnes_and_Holman_2020.jpeg", 
         "Fig5H_Barnes_and_Holman_2020.jpeg"),
  .y = fig5_lower,
  .f = ~ ggsave(
    filename = .x,
    plot = .y,
    device = "jpeg",
    path = "models/Barnes-and-Holman-2020",
    width = 14,
    height = 12,
    units = "in",
    type = "cairo",
    dpi = "retina",
    limitsize = F
  )
)

## Figure 5 Combined Version
fig5_combo <- (fig5_upper[[1]] | fig5_upper[[2]] | fig5_upper[[3]] | fig5_upper[[4]])/(fig5_lower[[1]] | fig5_lower[[2]] | fig5_lower[[3]] | fig5_lower[[4]]) + 
  plot_layout(guides = 'collect') & plot_annotation(
    title = "Figure 5. Women’s (Left Half) and Men’s (Right Half) Professional and Personal Legislative Diversity from Barnes and Holman (2020)",
    subtitle = str_wrap("Expectations are estimated based on the mean of 12,000 draws from the posterior distributions of models 3-6
                        in table 1 holding all covariates constant at their respective means. Gradients represent Bayesian credible 
                        intervals for the posterior expectations", width = 220))

# Save the combined plot object as a .jpeg file----
ggsave(
  filename = "Fig5_Barnes_and_Holman_2020.jpeg",
  plot = fig5_combo,
  device = "jpeg",
  path = str_c(base_dir, "output/"),
  width = 18,
  height = 10,
  units = "in",
  type = "cairo",
  dpi = "retina",
  limitsize = F
)

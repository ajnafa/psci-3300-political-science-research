# A Function for building combinations of formulas----
formula_builder <- function(lhs, rhs, ...) {
  
  ## Validate the formula arguments
  stopifnot("lhs and rhs should be character vectors" = (is.character(lhs) & is.character(rhs)))
  
  ## Make a list of all unique combinations of lhs ~ rhs 
  formulas <- expand.grid(lhs, rhs, stringsAsFactors = FALSE)
  
  ## Initialize a list to store the formulas in
  out <- list()
  
  ## Build a list of formulas for each pair formulas
  for (i in 1:nrow(formulas)) {
    out[[i]] <- glue::glue_collapse(formulas[i, ]) 
  }
  
  ## Return the output
  return(out)
}

# Arguments
##
##  lhs   A character vector specifying the names of 
##        response variables to use on the left hand 
##        side of the formulas
##
##  rhs   A character vector specifying the formulas 
##        to use on the right hand  side of the model 
##        equation

# Returns
##
##  A list of model formulas containing all unique
##  combinations of lhs ~ rhs

# Usage Example
##
##  # Specify the inputs for the lhs argument
##  responses <- c("sex", "drugs", "rock")
##
##  # Specify the inputs for the rhs argument
##  covariates <- c(
##    " ~ age + education + party_id",
##    " ~ age + education",
##    )
##
##  # Build the formulas
##  model_forms <- formula_builder(lhs = responses, rhs = covariates)


# Functions for Bayesian Model Presentation Using {modelsummary}----
glance.brmsfit <- function(x, 
                           bayes_R2 = NULL, 
                           looic = NULL, 
                           marglik = NULL,
                           kfold = NULL,
                           ...) {
  
  ## Retrive the names of the already stored criteria
  criteria <- names(x$criteria)
  
  ## Retreive Bayes R2
  if(isTRUE(bayes_R2)) {
    R2 <- .extract_R2(x, criteria)
  } else {
    R2 <- data.frame(R2 = NA_real_)
  }
  
  ## Retreive LOO-IC
  if(isTRUE(looic)) {
    loo_ic <- .extract_looic(x, criteria)
  } else {
    loo_ic <- data.frame(loo_ic = NA_real_)
  }
  
  ## Retreive K-Fold IC
  if(isTRUE(kfold)) {
    kfold_elpd <- .extract_kfold(x, criteria)
  } else {
    kfold_elpd <- data.frame(kfold_elpd = NA_real_)
  }
  
  ## Marginal Likelihood
  if(isTRUE(marglik)) {
    marglik <- .extract_marglik(x, criteria)
  } else {
    marglik <- data.frame(marglik = NA_real_)
  }
  
  ## Get the number of groups for multilevel models
  if(!is.null(brms::ngrps(x))) {
    ngroups <- brms::ngrps(x)
  } else {
    ngroups <- data.frame(ngroups = NA_real_)
  }
  
  ## Number of observations
  obs <- data.frame(n = nobs(x))
  
  ## Bind everything into a single data frame
  out <- do.call(cbind, list(R2, loo_ic, kfold_elpd, marglik, ngroups, obs))
  out <- out[, !is.na(out)]
  return(out)
}

## LOO-IC Extraction Helper
.extract_looic <- function(x, criteria) {
  
  criteria_pos <- match("loo", criteria)
  
  loo_df <- data.frame(
    elpd.loo = x$criteria[[criteria_pos]]$estimates[1,1],
    elpd.loo.se = x$criteria[[criteria_pos]]$estimates[1,2]
  )
  
  return(loo_df)
}

## K-Fold CV Extraction Helper
.extract_kfold <- function(x, criteria) {
  
  criteria_pos <- match("kfold", criteria)
  
  kfold_df <- data.frame(
    elpd.kfold = x$criteria[[criteria_pos]]$estimates[1,1],
    elpd.kfold.se = x$criteria[[criteria_pos]]$estimates[1,2],
    kfold.ic = x$criteria[[criteria_pos]]$estimates[3,1],
    kfold.ic.se = x$criteria[[criteria_pos]]$estimates[3,2]
  )
  
  return(kfold_df)
}


## Bayes R2 Extraction Helper
.extract_R2 <- function(x, criteria) {
  
  criteria_pos <- match("bayes_R2", criteria)
  
  if (!is.null(x$criteria[[criteria_pos]][["conditional_R2"]])) {
    conditional_R2 <- as.data.frame(x$criteria[[criteria_pos]][["conditional_R2"]])[c(1,3:4)]
    colnames(conditional_R2) <- c("cond.R2", "cond.R2.lower", "cond.R2.upper")
  } 
  
  else {
    conditional_R2 <-  NA_real_
  }
  
  if (!is.null(x$criteria[[criteria_pos]][["marginal_R2"]])) {
    marginal_R2 <- as.data.frame(x$criteria[[criteria_pos]][["marginal_R2"]])[c(1,3:4)]
    colnames(marginal_R2) <- c("marg.R2", "marg.R2.lower", "marg.R2.upper")
  }
  
  else {
    marginal_R2 <-  NA_real_
  }
  
  R2 <- do.call(cbind, list(conditional_R2, marginal_R2))
  
  return(R2)
}

## Marginal Likelihood Extraction Helper
.extract_marglik <- function(x, criteria) {
  
  criteria_pos <- match("marglik", criteria)
  
  marglik <- data.frame(
    marglik.med = do.call("median", list(x$criteria[[criteria_pos]]$logml)),
    marglik.iqr = do.call("IQR", list(x$criteria[[criteria_pos]]$logml))
  )
  
  return(marglik)
}
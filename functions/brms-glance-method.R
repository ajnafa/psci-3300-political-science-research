# Custom glance Method for Bayesian Model Presentation Using {modelsummary}
#'
#' @param x A model object of class `brmsfit`.
#' 
#' @param bayes_R2 `Logical` argument indicating whether to include
#' Bayesian R2. Defaults to `NULL`
#' 
#' @param looR2 `Logical` argument indicating whether to include
#' the LOO adjusted Bayesian R2. Defaults to `NULL`
#' 
#' @param looic `Logical` argument indicating whether to include
#' the expected log posterior density and LOO information criteria. 
#' Defaults to `NULL`
#' 
#' @param marglik `Logical` argument indicating whether to include
#' the median of the log marginal likelihood and its interqartile
#' range. Defaults to `NULL`
#' 
#' @param kfold `Logical` argument indicating whether to include
#' the k-fold expected log posterior density and information criteria.
#' Defaults to `NULL`
#' 
#' @param ... Reserved for future development, currently unused
#'
#' @return Returns a tibble with the specified model information and
#' the number of observations in the data
#' 
#' @export glance.brmsfit
#'
glance.brmsfit <- function(x,
                           bayes_R2 = NULL,
                           looR2 = NULL,
                           looic = NULL,
                           marglik = NULL,
                           kfold = NULL,
                           ...) {
  
  ## Retrive the names of the already stored criteria
  criteria <- names(x$criteria)
  
  ## Get the number of groups for multilevel models
  if (!is.null(brms::ngrps(x))) {
    ngroups <- brms::ngrps(x)
  } else {
    ngroups <- data.frame(ngroups = NA_real_)
  }
  
  ## Number of observations
  obs <- data.frame(n = nobs(x))
  
  ## Retreive Bayes R2
  if (isTRUE(bayes_R2)) {
    R2 <- .extract_R2(x, criteria)
  } else {
    R2 <- data.frame(R2 = NA_real_)
  }
  
  ## Retrieve LOO-IC
  if (isTRUE(looic)) {
    loo_ic <- .extract_looic(x, criteria)
  } else {
    loo_ic <- data.frame(loo_ic = NA_real_)
  }
  
  ## Retrieve LOO Adjusted R2
  if (isTRUE(looR2)) {
    looR2_df <- .extract_loo_R2(x, criteria)
  } else {
    looR2_df <- data.frame(loo_R2.mean = NA_real_)
  }
  
  ## Retrieve K-Fold IC
  if (isTRUE(kfold)) {
    kfold_elpd <- .extract_kfold(x, criteria)
  } else {
    kfold_elpd <- data.frame(kfold_elpd = NA_real_)
  }
  
  ## Marginal Likelihood
  if (isTRUE(marglik)) {
    marglik <- .extract_marglik(x, criteria)
  } else {
    marglik <- data.frame(marglik = NA_real_)
  }
  
  ## Bind everything into a single data frame
  out <- do.call(cbind, list(R2, loo_ic, kfold_elpd, looR2_df, marglik, ngroups, obs))
  out <- out[, !is.na(out)]
  return(out)
}

## LOO-IC Extraction Helper
.extract_looic <- function(x, criteria) {
  criteria_pos <- match("loo", criteria)
  
  loo_df <- data.frame(
    elpd.loo = x$criteria[[criteria_pos]]$estimates[1, 1],
    elpd.loo.se = x$criteria[[criteria_pos]]$estimates[1, 2],
    loo.ic = x$criteria[[criteria_pos]]$estimates[3, 1],
    loo.ic.se = x$criteria[[criteria_pos]]$estimates[3, 2]
  )
  
  return(loo_df)
}

## K-Fold CV Extraction Helper
.extract_kfold <- function(x, criteria) {
  criteria_pos <- match("kfold", criteria)
  
  kfold_df <- data.frame(
    elpd.kfold = x$criteria[[criteria_pos]]$estimates[1, 1],
    elpd.kfold.se = x$criteria[[criteria_pos]]$estimates[1, 2],
    kfold.ic = x$criteria[[criteria_pos]]$estimates[3, 1],
    kfold.ic.se = x$criteria[[criteria_pos]]$estimates[3, 2]
  )
  
  return(kfold_df)
}

## LOO Adusted R2 Extraction Helper
.extract_loo_R2 <- function(x, criteria) {
  criteria_pos <- match("loo_R2", criteria)
  
  loo_R2_df <- data.frame(loo_R2.mean = mean(x$criteria[[criteria_pos]]))
  
  return(loo_R2_df)
}

## Bayes R2 Extraction Helper
.extract_R2 <- function(x, criteria) {
  
  criteria_pos <- match("bayes_R2", criteria)
  
  if (!is.null(x$criteria[[criteria_pos]][["conditional_R2"]])) {
    conditional_R2 <- as.data.frame(x$criteria[[criteria_pos]][["conditional_R2"]])[c(1, 3:4)]
    colnames(conditional_R2) <- c("cond.R2", "cond.R2.lower", "cond.R2.upper")
  } else {
    conditional_R2 <- NA_real_
  }
  
  if (!is.null(x$criteria[[criteria_pos]][["marginal_R2"]])) {
    marginal_R2 <- as.data.frame(x$criteria[[criteria_pos]][["marginal_R2"]])[c(1, 3:4)]
    colnames(marginal_R2) <- c("marg.R2", "marg.R2.lower", "marg.R2.upper")
  } else {
    marginal_R2 <- NA_real_
  }
  
  if (is.matrix(x$criteria[[criteria_pos]])) {
    bayes.R2 <- data.frame(
      bayes.R2 = mean(x$criteria[[criteria_pos]]),
      bayes.R2.lower = qi(x$criteria[[criteria_pos]])[,1],
      bayes.R2.upper = qi(x$criteria[[criteria_pos]])[,2],
    )
  }
  
  R2 <- do.call(cbind, list(conditional_R2, marginal_R2, bayes.R2))
  
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
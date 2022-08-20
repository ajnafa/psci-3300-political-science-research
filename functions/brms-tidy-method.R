#' Custom tidy Method for Bayesian Model Presentation Using {modelsummary}
#' 
#' This function provides a custom tidy method for objects of class brmsfit
#' that, among other things, correctly handles models multinomial logistic
#' regression models.
#'
#' @param x An object of class brmsfit
#' 
#' @param conf_level 1 - alpha level to use for the uncertainty estimates
#' 
#' @param parameters A string representing a regular expression for the
#' parameters to include from the model. Defaults to `"^b_|^sd_|^cor_|^sigma"`
#' which should be appropriate for most applications
#' 
#' @param signif `Logical` argument indicating whether to include a column 
#' for p-values. This is calculated based on the percentage of the posterior
#' outside the region of practical equivalence which can be set using
#' `rope_range = c(lower, upper)` where `lower` and `upper` are numeric
#' values. Defaults to `NULL`.
#' 
#' @param ... Additional arguments passed down to `describe_posterior`.
#' See `?bayestestR::describe_posterior` for possible options
#'
#' @return A data frame containing the relevant model information to 
#' be used internally by {modelsummary}
#' 
#' @importFrom stringr str_extract_all str_remove_all
#' @importFrom bayestestR describe_posterior
#' @importFrom brms ngrps
#' 
#' @export tidy.brmsfit
#'
tidy.brmsfit <- function(x,
                         conf_level = 0.95,
                         parameters = "^b_|^sd_|^cor_|^sigma",
                         signif = NULL,
                         ...) {

  ## Extract the model information
  posterior <- bayestestR::describe_posterior(
    x,
    ci = conf_level,
    parameters = parameters,
    effects = "all",
    ...
  )

  ## Build the data frame for modelsummary
  out <- data.frame(
    term = posterior$Parameter,
    estimate = posterior$Median,
    conf.level = posterior$CI,
    conf.low = posterior$CI_low,
    conf.high = posterior$CI_high,
    pd = posterior$pd,
    rope.percentage = posterior$ROPE_Percentage,
    rhat = posterior$Rhat,
    ess = posterior$ESS
  )

  # Compatibility with multilevel and fixed effects only models
  if (!is.null(brms::ngrps(x))) {
    out$effect <- posterior$Effects
  } else {
    out$effect <- "fixed"
  }

  # If model family is of class categorical needs to be handled differently
  if (isTRUE(grepl(x$family[1], "categorical"))) {
    ## Decompose the term column into a response category identifier
    ## and a term name identifier
    out$response <- as.character(stringr::str_extract_all(out$term, "mu.*?(?=_)"))

    ## Ovewrwrite the original term column
    out$term <- stringr::str_remove_all(out$term, "_mu.*?(?=_)")
  }

  # Option to return stars if reviewer 2 is a dick and demands you report
  # "statistical significance" for some absurd reason. Since this is based
  # on the ROPE though, it captures substantive significance because we
  # aren't intellectually bankrupt people who just p everywhere
  if (isTRUE(signif)) {
    ## Use rope.percentage for this, though could also use bayestestR::pd_to_p
    out$p.value <- ifelse(out$effect == "fixed", out$rope.percentage, 1)
  }

  # Return the data frame object
  return(out)
}
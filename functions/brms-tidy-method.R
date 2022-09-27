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
#' parameters to include from the model. Defaults to `"^b_|^sd_|^cor_|^sigma|^rescor|^phi|^ar"`
#' which should be appropriate for most applications
#' 
#' @param signif `Logical` argument indicating whether to include a column 
#' for p-values. This is calculated based on the percentage of the posterior
#' distribution that falls in a specific direction. Defaults to `NULL`.
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
                         parameters = "^b_|^sd_|^cor_|^sigma|^rescor|^phi|^ar",
                         signif = NULL,
                         ...) {

  ## Extract the model information
  posterior <- bayestestR::describe_posterior(
    x,
    ci = conf_level,
    parameters = parameters,
    effects = "all",
    component = "all",
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
    rhat = posterior$Rhat,
    ess = posterior$ESS,
    effect = posterior$Component
  )

  # If model family is of class categorical needs to be handled differently
  if (isTRUE(grepl(x$family[1], "categorical"))) {
    ## Decompose the term column into a response category identifier
    ## and a term name identifier
    out$response <- as.character(stringr::str_extract_all(out$term, "mu.*?(?=_)"))

    ## Ovewrwrite the original term column
    out$term <- stringr::str_remove_all(out$term, "_mu.*?(?=_)")
  }

  # Option to return stars if reviewer 2 is a dick and demands you report
  # "statistical significance" for some absurd reason.
  if (isTRUE(signif)) {
    ## Use rope.percentage for this, though could also use bayestestR::pd_to_p
    out$p.value <- ifelse(out$effect == "conditional", 1 - out$pd, 1)
  }

  # Return the data frame object
  return(out)
}

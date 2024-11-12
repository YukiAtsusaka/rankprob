#' Convert a multinomial regression to a `rankprob` object
#'
#' `convert_rankprob()` converts a supplied multinomial or ordinal regression model to a model that supports computation of probabilities of ranked choices.
#'
#' @param fit the output of a multinomial or ordinal regression model, e.g., form `nnet::multinom()`, `mclogit::mblogit()`, `MASS::polr()`, `WeightIt::multinom_weightit()`, or `WeightIt::ordinal_weightit()`.
#'
#' @details
#' `convert_rankprobs()` first checks to make sure the model is ordinal or multinomial as determined by [insight::model_info()]. Next, it adds `"rankprob"` to `options("marginaleffects_model_classes")` to facilitate use with `marginaleffects` and packages that depend on it, such as `clarify`. Finally, it assigned to the supplied object the class `"rankprob"`.
#'
#' To use `rankprob`, the supplied model must have a `predict()` method with a `type` that corresponds to predicted probabilities. This `type` is stored in the `"rank_probs_prob_type"` attribute of the converted `rankprob` object. You can manually set this attribute if `convert_rankprob()` guesses it incorrectly. Typically this value is set to `"probs"` or `"response"` depending on the object.
#'
#' @returns
#' A `rankprobs` object, which also has the original model classes; this simply has the new class and `"rank_probs_prob_type"` attribute added. Otherwise the supplied object is unchanged.
#'
#' @seealso [predict.rankprob()]
#'
#' @note
#' When using `marginaleffects` functions, `type = "rank_probs"` must be specified for the functions to work correctly. You may see a warning beginning with something like "These arguments are not known to be supported for models of class `rankprob`: rankings." This can be ignored.
#'
#' @examples
#' ## See exampels at `help("predict.rankprob")`

#' @export
convert_rankprob <- function(fit) {
  if (inherits(fit, "rankprob")) {
    return(fit)
  }

  multi_okay <- isTRUE(insight::model_info(fit)$is_ordinal) || isTRUE(insight::model_info(fit)$is_multinomial)

  if (multi_okay) {
    test_pred <- insight::get_predicted(fit, predict = "expectation", verbose = FALSE)
    if (is.null(dim(test_pred)) || !all(c("Response", "Predicted") %in% names(test_pred))) {
      multi_okay <- FALSE
    }
  }

  if (!multi_okay) {
    chk::err("`fit` must be an ordinal or multinomial regression model")
  }

  if (!"rankprob" %in% getOption("marginaleffects_model_classes")) {
    options("marginaleffects_model_classes" = c(getOption("marginaleffects_model_classes"), "rankprob"))
  }

  attr(fit, "rank_probs_prob_type") <- {
    if (inherits(fit, c("multinom", "polr", "brmultinom", "bracl"))) "probs"
    else "response"
  }

  class(fit) <- c("rankprob", class(fit))

  fit
}

#' Convert a multinomial regression to a `rankprob` object
#'
#' `convert_rankprob()` converts a supplied multinomial or ordinal regression mdoel to a model tha tsupports computation of probabilities of ranked choices.
#'
#' @param fit the output of a multinomial or ordinal regression model, e.g., form `nnet::multinom()`, `mclogit::mblogit()`, `MASS::polr()`, `WeightIt::multinom_weightit()`, or `WeightIt::ordinal_weightit()`.
#' @param object a `rankprob` object; the output of a call to `convert_rankprob()`.
#' @param newdata the new data used to compute predictions. If `NULL`, the original data used to fit the model will be used.
#' @param type the type of prediction desired. If `"rank_probs"`, the probability of the ranking identified in `rankings` will be computed. Otherwise passed the `predict()` method for the original model fit. Note `"rank_probs"` cannot be abbreviated, but as the default, it also doesn't need to be specified.
#' @param rankings when `type = "rank_probs"`, a list of character vectors that identify the rankings for which predicted probabilities are to be computed. Can also be a single character vector for a single ranking. If `NULL`, will compute the ranking of the outcome levels in their original order. Each ranking must contain all levels of the original outcome.
#' @param \dots further arguments pass to `predict()` when `type` is not `"rank_probs"`.
#'
#' @details
#' `convert_rankprobs()` first checks to make sure the model is ordinal or multinomial as determined by [insight::model_info()]. Next, it adds `"rankprob"` to `options("marginaleffects_model_classes")` to facilitate use with `marginaleffects` and packages that depend on it, such as `clarify`. Finally, it assigned to the supplied object the class `"rankprob"`.
#'
#' `predict()` produces the predicted probability of each supplied ranking when `type = "rank_probs"`.
#'
#' @returns
#' For `convert_rankprobs()`, a `rankprobs` object, which also has the original model classes. Otherwise the supplied object is unchanged.
#'
#' For `predict()`, a matrix with a row for each unit and a column for each ranking, with each cell equal to the predicted probability of that ranking being chosen for that unit.
#'
#' @note
#' When using `marginaleffects` functions, `type = "rank_probs"` must be specified for the functions to work correctly.
#'
#' @examplesIf requireNamespace("nnet", quietly = TRUE)
#'
#' data <- data.frame(x1 = rnorm(500),
#'                    x2 = rnorm(500),
#'                    x3 = rnorm(500),
#'                    y = sample(LETTERS[1:4], 500, TRUE))
#'
#' fit <- nnet::multinom(y ~ x1 + x2 + x3, data = data, Hess = TRUE)
#'
#' fit <- convert_rankprob(fit)
#'
#' p <- predict(fit, rankings = list(c("A", "B", "C", "D"),
#'                                   c("A", "C", "B", "D"),
#'                                   c("B", "A", "D", "C")))
#'
#' head(p)

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

  class(fit) <- c("rankprob", class(fit))
  fit
}

#' @exportS3Method stats::predict rankprob
#' @rdname convert_rankprob
predict.rankprob <- function(object, newdata = NULL, type = "rank_probs", rankings = NULL, ...) {
  chk::chk_string(type)

  if (type != "rank_probs") {
    return(NextMethod("predict"))
  }

  prob_type <- {
    if (inherits(object, c("mblogit", "mclogit", "mmblogit", "mmclogit"))) "response"
    else if (inherits(object, c("multinom", "polr", "brmultinom", "bracl"))) "probs"
    else if (inherits(object, c("multinom_weightit", "ordinal_weightit"))) "response"
  }

  args <- list(object = object, type = prob_type, ...)

  if (!is.null(newdata)) {
    args[["newdata"]] <- newdata
  }

  p <- do.call("predict", args)
  k <- ncol(p)

  if (is.null(rankings)) {
    rankings <- list(colnames(p))
  }
  else if (!is.list(rankings)) {
    rankings <- list(rankings)
  }

  for (i in seq_along(rankings)) {

    rankings[[i]] <- as.character(rankings[[i]])

    if (length(rankings[[i]]) != k || !all(rankings[[i]] %in% colnames(p))  || !all(colnames(p) %in% rankings[[i]])) {
      chk::err("`rankings` must have a value for each level of the outcome")
    }
  }

  out <- do.call("cbind", lapply(rankings, function(o) {
    pred <- p[,o[1]]

    for (i in 2:k) {
      pred <- pred * p[,o[i]] / rowSums(p[,o[i:k], drop = FALSE])
    }

    pred
  }))

  colnames(out) <- unlist(lapply(rankings, paste, collapse = " > "))
  rownames(out) <- rownames(p)

  out
}

#' @importFrom marginaleffects get_coef
#' @exportS3Method marginaleffects::get_coef rankprob
get_coef.rankprob <- function(model, ...) {
  NextMethod("get_coef")
}

#' @importFrom marginaleffects set_coef
#' @exportS3Method marginaleffects::set_coef rankprob
set_coef.rankprob <- function(model, coefs, ...) {
  NextMethod("set_coef")
}

#' @importFrom marginaleffects get_vcov
#' @exportS3Method marginaleffects::get_vcov rankprob
get_vcov.rankprob <- function(model, ...) {
  NextMethod("get_vcov")
}

#' @importFrom marginaleffects get_predict
#' @exportS3Method marginaleffects::get_predict rankprob
get_predict.rankprob <- function(model, newdata = NULL, type = "rank_probs", ...) {
  if (type != "rank_probs") {
    return(NextMethod("get_predict"))
  }

  pred <- stats::predict(model, newdata = newdata, type = type,
                         ...)

  out <- data.frame(group = rep(colnames(pred), each = nrow(pred)),
                    estimate = c(pred))

  if ("rowid" %in% colnames(newdata)) {
    out$rowid <- rep(newdata$rowid, times = ncol(pred))
  }
  else {
    out$rowid <- rep(seq_len(nrow(pred)), times = ncol(pred))
  }

  out
}

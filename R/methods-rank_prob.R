# Methods the make `rankprob` objects usable in `marginaleffects` and `clarify`

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

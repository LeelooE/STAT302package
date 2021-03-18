#' T-test
#'
#' This function runs specified t-test type on given data using
#' the given hypothesized mu.
#'
#' @param x is a numeric input vector representing the data for
#'   the t-test.
#' @param alternative is a string input used to distinguish what
#'   t-test to run, a two-sided, less, or greater, defaults to
#'   \code{"two.sided"}
#' @param mu is a numeric input indicating the given hypothesized
#'   mu value.
#' @keywords inference
#'
#' @return A list of t-test values that have been calculated, this
#'   includes the t-statistic, degrees of freedom, the
#'   \code{alternative}, and the p-value.
#'
#' @example
#' my_t.test(c(1,2,3,4,1,2), mu=2)
#' my_t.test(c(1,2,3,4,1,2), alternative="greater", mu=2)
#' y_t.test(c(1,2,3,4,1,2), alternative="less", mu=2)
#'
#' @export
my_t.test <- function(x, alternative="two.sided", mu) {
  est <- mean(x)
  df <- length(x) - 1
  t_obs <- (est - mu) / (sd(x) / sqrt(length(x)))
  p_value <- 0
  if (alternative == "less") {
    p_value <- 1 - pt(-t_obs, df=df, lower.tail = TRUE)
  } else if (alternative == "greater") {
    p_value <- pt(-t_obs, df=df, lower.tail = TRUE)
  } else {
    p_value <- 2 * pt(-abs(t_obs), df=df)
  }
  result <- list("test_stat" = t_obs,
                 "df" = df,
                 "alternative" = alternative,
                 "p_val" = p_value)
  return(result)
}

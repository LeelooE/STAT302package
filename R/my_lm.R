#' Linear Model Fitting
#'
#' This function fits linear models.
#'
#' @param formula is an input object that represents the describes
#'   the model which needs to be fitted.
#' @param data is an input numeric vector which contains the
#'   variables in the model.
#' @keywords inference, prediction
#'
#' @return A table containing all coefficients created, which
#'   include the error, estimate, t-statistic, and probability
#'   of that coefficient occurring given the t-statistic.
#'
#' @example
#' x <- c(3, 4, 5, 6, 7, 2, 3)
#' y <- c(10, 14, 50, 16, 27, 32, 13)
#' z <- c(3, 4, 4, 3, 4, 2, 1)
#' data <- data.frame(x, y, z)
#' my_lm(y ~ x + 5, data)
#' my_lm(y ~ x + z, data)
#'
#' @export
my_lm <- function(formula, data) {
  X <- model.matrix(formula, data)
  frame <- model.frame(formula, data)
  Y <- model.response(frame)
  B_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
  df <- nrow(X) - ncol(X)
  sigma_sqr <- sum((Y - (X %*% B_hat))^2 / df)
  standard_err <- diag(sqrt(sigma_sqr * solve(t(X) %*% X)))
  variable_names <- colnames(X)
  coeffs <- numeric(0)
  for (i in 1:length(standard_err)) {
    est <- B_hat[i]
    err <- standard_err[[i]]
    n <- variable_names[i]
    vals <- X[,n]
    t_obs <- est / err
    p <- 2 * pt(abs(t_obs), df=df, lower.tail = FALSE)
    coeffs <- c(coeffs, est, err, t_obs, p)
  }
  values <- matrix(coeffs,
                   ncol=4,
                   byrow=TRUE)
  colnames(values) <- c("Estimate", "Std. Error", "t value",
                        "Pr(>|t|)")
  rownames(values) <- variable_names
  result <- as.table(values)
  return(result)
}

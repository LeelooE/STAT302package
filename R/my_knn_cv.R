#' K-Nearest Neighbors Cross-Validation
#'
#' This function runs the K-Nearest Neighbors Cross-Validation algorithm on the
#' the given data using the given \code{k_nn} and \code{k_cv} value.
#'
#' @param train is an input data frame containing the columns of data that are
#'   used for the training part of the algorithm.
#' @param cl is a data frame input containing the column of predictions that
#'   the function should reference for Mean Squared Error calculations.
#' @param k_nn is a numeric input indicating amount of neighbors the algorithm
#'   should be using.
#' @param k_cv is a numeric input indicating amount of folds the function
#'   should be using.
#' @keywords prediction
#'
#' @return A list where the first element is a vector of predictions created
#'   for each \code{k_cv} folds, and the second element is the cross-validation
#'   estimate calculated using Means Squared Errors taken from each fold.
#'
#'
#'
#' @examples
#' x <- c(3, 4, 5, 6, 7, 2, 3)
#' y <- c(10, 14, 50, 16, 27, 32, 13)
#' z <- c(3, 4, 4, 3, 4, 2, 1)
#' train <- data.frame(x, z)
#' cl <- data.frame(y)
#' my_knn_cv(train, cl, 1, 5)
#' my_knn_cv(train, cl, 2, 2)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # initialize all the variables that will be needed later on
  n = nrow(train)
  # randomly assigns elements in the train data to k_cv different groups
  fold <- sample(rep(1:k_cv, length=n))
  train$split <- fold
  preds <- data.frame(matrix(ncol = 1, nrow = n))
  preds$split <- fold
  cl$split <- fold
  mses <- numeric(0)
  # loops through each k_cv group and creates predictions for it
  # and calculates the MSE for those predicitons
  for(i in 1:k_cv) {
    tr <- train[train$split != i,]
    te <- train[train$split == i,]

    cl_filter <- cl[cl$split != i,][[1]]
    cl_true <- cl[cl$split == i,][[1]]

    pred <- class::knn(tr, te, cl_filter, k = k_nn)
    pred_str <-levels(pred)[pred]
    preds[preds$split == i, "pred"] <- pred_str

    bool_vec <- pred_str == cl_true
    wrong <- length(bool_vec[bool_vec == FALSE])

    m <- length(pred_str)

    mse <- (wrong) / m
    mses <- c(mses, mse)
  }
  p = preds$pred
  # cross-validation estimate
  cv_err = (sum(mses)) / length(mses)
  ret <- list(p,cv_err)
  return(ret)
}

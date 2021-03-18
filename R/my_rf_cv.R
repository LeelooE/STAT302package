#' Random Forest Cross-Validation
#'
#' This function runs the Random Forest K-fold Cross-Validation algorithm on the
#' the given data using the given \code{k} value.
#'
#' @param k is a numeric input used to specify the amount of folds the function
#'   will use for the algorithm.
#' @param cl is a data frame input containing the column of predictions that
#'   the function should reference for Mean Squared Error calculations.
#' @param train is an input data frame containing the columns of data that are
#'   used for the training part of the algorithm.
#' @keywords prediction
#'
#' @return A numeric value indicating the cross-validation estimate calculated
#'  while running the random forest cross-validation on the given data.
#'
#' @example
#' x <- c(3, 4, 5, 6, 7, 2, 3)
#' y <- c(10, 14, 50, 16, 27, 32, 13)
#' z <- c(3, 4, 4, 3, 4, 2, 1)
#' train <- data.frame(x, z)
#' cl <- data.frame(y)
#' my_rf_cv(4, cl, train)
#' my_rf_cv(2, cl, train)
#'
#' @import randomForest
#'
#' @export
my_rf_cv <- function(k, cl, train) {

  # initialize all the variables that will be needed later on
  n = nrow(train)

  #create formula for randomForest function
  cols <- colnames(train)
  #predCol <- colnames(cl)[1]

  formula <- as.formula(paste(predCol, paste(cols, collapse = " + "), sep = " ~ "))

  # randomly assigns elements in the train data to k_cv different groups
  fold <- sample(rep(1:k, length=n))
  train$split <- fold
  preds <- data.frame(matrix(ncol = 1, nrow = n))
  preds$split <- fold
  cl$split <- fold
  mses <- numeric(0)

  # loops through each k group and creates predictions for it
  # and calculates the MSE for those predicitons

  for(i in 1:k) {
    trCl = cl[cl$split != i,]
    teCl = cl[cl$split == i,]
    tr <- train[train$split != i,]
    te <- train[train$split == i,]
    tr$predCol <- trCl
    te$predCol <- teCl
    model = randomForest(formula,data = tr, ntree = 100)
    pred <- predict(model, te)

    true <- te$predCol
    preds[preds$split == i, "pred"] <- pred

    m <- length(pred)

    mse <- (sum((true - pred)^2)) / m
    mses <- c(mses, mse)
  }
  p = preds$pred
  # cross-validation estimate
  cv_err = (sum(mses)) / length(mses)
  return(cv_err)
}

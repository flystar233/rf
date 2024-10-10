#' @title Random Forest
#' @description
#' This function implements a random forest algorithm.
#' @param X A data frame of predictor variables.
#' @param y A vector of response variables.
#' @param n_trees The number of trees to grow.
#' @param max_depth The maximum depth of each tree.
#' @param min_samples_split The minimum number of samples required to split an internal node.
#' @param min_samples_leaf The minimum number of samples required to be at a leaf node.
#' @param mtry The number of variables to consider when looking for the best split.
#' @param subsample The fraction of samples to use for each tree.
#' @param replace Whether to sample with replacement.
#' @param seed A seed for the random number generator.
#' @param type The type of random forest to grow. Options are "classification", "regression", and "extratrees".
#' @param n_cores The number of cores to use.
#' @param data A data frame of predictor and response variables.
#' @param formula A formula specifying the response and predictor variables.
#' @return A random forest object.
#' @examples
#' # Load the iris dataset
#' data(iris)
#' # Split the data into training and testing sets
#' set.seed(123)
#' train_index <- sample(1:nrow(iris), 0.7*nrow(iris))
#' train_data <- iris[train_index, ]
#' test_data <- iris[-train_index, ]
#' # Train a random forest classifier
#' rf_model <- random_forest(X = train_data[, -5], 
#' y = train_data[, 5], n_trees = 100, max_depth = 5, 
#' min_samples_split = 2, min_samples_leaf = 1, mtry = 2, 
#' subsample = 0.632, replace = TRUE, seed = 123, type = "classification", n_cores = 1)
#' # Make predictions on the test data
#' predictions <- predict(rf_model, test_data[, -5])
#' # Evaluate the accuracy of the model
#' accuracy <- sum(predictions == test_data[, 5]) / nrow(test_data)
#' print(accuracy)
#' @export
random_forest <- function(X = NULL,
                          y = NULL,
                          n_trees = 100,
                          max_depth = NULL,
                          min_samples_split = 2,
                          min_samples_leaf = 1,
                          mtry = NULL,
                          subsample = ifelse(replace, 1, 0.632),
                          replace = TRUE,
                          seed = NULL,
                          type = c("classification", "regression", "extratrees"),
                          n_cores = future::availableCores() - 1,
                          data = NULL,
                          formula = NULL) {
  if (is.null(data) && is.null(formula)) {
    X <- X
    y <- y
  } else if (!is.null(formula)) {
    formula <- as.character.default(formula)
    elevantVars <- lapply(
      formula[2:3],
      function(z) {
        attr(
          stats::terms.formula(stats::reformulate(z), data = data[1, ]), "term.labels"
        )
      }
    )
    X <- data[setdiff(elevantVars[[2]], elevantVars[[1]])]
    y <- data[elevantVars[[1]]]
  } else {
    stop("Either data or formula must be specified.")
  }
  na_cols <- sapply(X, anyNA)
  if (any(na_cols)) {
    stop(paste(
      "以下列包含 NA 值:",
      paste(names(X)[na_cols], collapse = ", ")
    ))
  }
  type <- match.arg(type)
  n_features <- ncol(X)
  n_samples <- nrow(X)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (is.null(max_depth)) {
    max_depth <- Inf
  }
  if (is.null(mtry)) {
    mtry <- floor(sqrt(n_features))
  } else if (mtry < 1) {
    mtry <- 1
  } else if (mtry > n_features) {
    mtry <- n_features
  }
  if (!is.data.frame(y)) {
    y <- as.data.frame(y)
    colnames(y) <- "target"
  }
  samples_per_tree <- floor(n_samples * subsample)

  # 设置并行计算
  future::plan(future::multisession,workers = n_cores)

  # 使用future_lapply并行构建树，并设置future.seed
  forest_and_oob <- future.apply::future_lapply(1:n_trees, function(i) {
    sample_indices <- sample(1:n_samples, samples_per_tree, replace = replace)
    oob_indices <- setdiff(1:n_samples, unique(sample_indices))

    X_sample <- X[sample_indices, ]
    y_sample <- y[sample_indices, , drop = FALSE]

    # 随机选择特征子集
    feature_indices <- sample(1:n_features, mtry)
    X_sample_subset <- X_sample[, feature_indices, drop = FALSE]
    tree <- build_decision_tree(X_sample_subset, y_sample, max_depth, min_samples_split, min_samples_leaf, type)

    # 对OOB样本进行预测
    oob_predictions <- rep(NA, n_samples)
    if (length(oob_indices) > 0) {
      X_oob <- X[oob_indices, feature_indices, drop = FALSE]
      oob_predictions[oob_indices] <- predict_tree(tree, X_oob)
    }

    list(tree = tree, feature_indices = feature_indices, oob_predictions = oob_predictions)
  }, future.seed = TRUE) # 设置future.seed为TRUE
  # 提取树和OOB预测
  forest <- lapply(forest_and_oob, function(x) list(tree = x$tree, feature_indices = x$feature_indices))
  oob_predictions <- do.call(cbind, lapply(forest_and_oob, `[[`, "oob_predictions"))
  # 计算OOB误差
  oob_error <- mean(sapply(1:n_samples, function(i) {
    row <- oob_predictions[i, ]
    if (all(is.na(row))) {
      return(NA) # 如果该样本从未作为OOB样本，返回NA
    }

    if (type == "classification") {
      pred <- names(which.max(table(row[!is.na(row)])))
      return(pred != y[i, 1])
    } else {
      pred <- mean(row[!is.na(row)])
      return((pred - y[i, 1])^2)
    }
  }), na.rm = TRUE)

  # 关闭并行计算
  future::plan(future::sequential)

  return_result <- list(
    forest = forest,
    oob_error = oob_error,
    mtry = mtry,
    n_trees = n_trees,
    max_depth = max_depth,
    min_samples_split = min_samples_split,
    min_samples_leaf = min_samples_leaf,
    subsample = subsample,
    replace = replace,
    type = type,
    seed = seed
  )
  class(return_result) <- c("random_forest")
  return(return_result)
}

#' @title Predict using a trained random forest
#' @param forest A random forest object.
#' @param new_data A data frame containing the new data to be predicted.
#' @param type The type of classification prediction to make. Default is "value".
#' @param n_cores The number of cores to use for parallel processing.
#' Default is the number of available cores minus one.
#' @return A vector of predictions.
#' @export
predict_random_forest <- function(forest,
                                  new_data,
                                  type = "value",
                                  n_cores = future::availableCores() - 1) {
  forest_list <- forest$forest
  # Set up parallel processing
  future::plan(future::multisession, workers = n_cores)
  # Define a function to predict for a single row
  predict_row <- function(row) {
    sample_predictions <- vector("numeric", length(forest_list))
    for (j in seq_along(forest_list)) {
      tree_info <- forest_list[[j]]
      tree <- tree_info$tree
      feature_indices <- tree_info$feature_indices
      new_data_subset <- row[feature_indices]
      sample_predictions[j] <- predict_tree(tree, new_data_subset)
    }

    if (forest$type == "classification") {
      if (type == "value") {
        return(names(which.max(table(sample_predictions))))
      } else {
        return(prop.table(table(sample_predictions)))
      }
    } else {
      return(round(mean(sample_predictions), 5))
    }
  }
  # Use future_lapply to parallelize predictions
  predictions <- future.apply::future_lapply(1:nrow(new_data), function(i) predict_row(new_data[i, ]))
  if (type == "value") {
    predictions <- unlist(predictions, use.names = FALSE)
  } else {
    predictions <- do.call(rbind, predictions)
  }
  return(predictions)
}

#' @title Calculate the accuracy of a random forest
#' @param forest A random forest object.
#' @param X_test A data frame containing the test data.
#' @param y_test A vector containing the true labels for the test data.
#' @return The accuracy of the random forest.
#' @export
calculate_accuracy <- function(forest, X_test, y_test) {
  if (!is.data.frame(y_test)) {
    y_test <- as.data.frame(y_test)
  }
  predictions <- predict_random_forest(forest, X_test)
  if (forest$type == "classification") {
    accuracy <- sum(predictions == y_test[, 1]) / nrow(y_test)
  } else {
    accuracy <- cor(y_test[, 1], predictions)^2
  }
  return(accuracy)
}

# test iris
# data = iris|>select(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,Species)
# forest <- random_forest(data[1:4],data[,5], n_trees = 100, max_depth = 5, min_samples_split = 2, min_samples_leaf = 1,replace = TRUE,seed = 42)

# test wine
# data <- read.csv("wine.txt",header = T)
# data = read.csv("housing.txt",header = T)
# data[7,2]=NA
# data[3,1]=NA
# forest <- random_forest(data[1:400,1:13],data[1:400,14],n_trees = 100, max_depth = 5, min_samples_split = 50, min_samples_leaf = 10,replace = T,type = "regression")
# forest <- random_forest(data=data,formula = MEDV~.,n_trees = 100, max_depth = 5, min_samples_split = 50, min_samples_leaf = 10,replace = T,type = "regression")

#' title "Build Decision Tree"
#' description "Build a decision tree for classification or regression"
#' @param X a data frame of features
#' @param y a vector of target values
#' @param max_depth the maximum depth of the tree
#' @param min_samples_split the minimum number of samples required to split an internal node
#' @param min_samples_leaf the minimum number of samples required to be at a leaf node
#' @param type the type of the tree, either "classification" or "regression"
#' @return a decision tree
#' @examples
#' # Build a classification tree
#' X <- data.frame(x1 = c(1, 2, 3, 4, 5), x2 = c(1, 2, 3, 4, 5))
#' y <- c(1, 0, 1, 0, 1)
#' tree <- build_decision_tree(X, y, type = "classification")
#' # Build a regression tree
#' X <- data.frame(x1 = c(1, 2, 3, 4, 5), x2 = c(1, 2, 3, 4, 5))
#' y <- c(1, 2, 3, 4, 5)
#' tree <- build_decision_tree(X, y, type = "regression")
#' @export
build_decision_tree <- function(X, y, max_depth = Inf, min_samples_split = 2, min_samples_leaf = 1, type = "classification") {
  data <- cbind(X, y)
  features <- colnames(X)
  target <- colnames(y)[1]

  build_tree_recursive <- function(data, depth = 0) {
    # 检查停止条件
    if (nrow(data) < min_samples_split || depth == max_depth || length(unique(data[[target]])) == 1 || nrow(data) <= min_samples_leaf * 2) {
      leaf <- calc_leaf(data, target, type)
      return(leaf)
    }

    if (type == "classification") {
      best_split <- find_best_split_classification(data, features, target, min_samples_leaf)
    } else if (type == "regression") {
      best_split <- find_best_split_regression(data, features, target, min_samples_leaf)
    } else {
      best_split <- find_best_split_extratrees(data, features, target)
    }
    if (is.null(best_split)) {
      leaf <- calc_leaf(data, target, type)
      return(leaf)
    }

    # 分割数据
    if (is.numeric(best_split$value)) {
      left_data <- data[data[[best_split$feature]] <= best_split$value, ]
      right_data <- data[data[[best_split$feature]] > best_split$value, ]
    } else {
      left_data <- data[data[[best_split$feature]] == best_split$value, ]
      right_data <- data[data[[best_split$feature]] != best_split$value, ]
    }
    # 递归构建左右子树
    left_branch <- build_tree_recursive(left_data, depth + 1)
    right_branch <- build_tree_recursive(right_data, depth + 1)

    # 返回节点信息
    return(list(
      type = "node",
      feature = best_split$feature,
      value = best_split$value,
      gini = best_split$gini,
      samples = nrow(data),
      left = left_branch,
      right = right_branch
    ))
  }

  return(build_tree_recursive(data))
}

#' title "Predict using decision tree"
#' @param tree  decision tree
#' @param new_data  new data
#' @export
predict_single <- function(tree, new_data) {
  if (tree$type == "leaf") {
    return(tree$class)
  }

  if (is.numeric(new_data[[tree$feature]])) {
    if (new_data[[tree$feature]] <= tree$value) {
      return(predict_single(tree$left, new_data))
    } else {
      return(predict_single(tree$right, new_data))
    }
  } else {
    if (new_data[[tree$feature]] == tree$value) {
      return(predict_single(tree$left, new_data))
    } else {
      return(predict_single(tree$right, new_data))
    }
  }
}

#' title "Predict using decision tree"
#' @param tree  decision tree
#' @param new_data  new data
#' @return the predict result
#' @export
predict_tree <- function(tree, new_data) {
  if (is.data.frame(new_data) && nrow(new_data) > 1) {
    return(apply(new_data, 1, function(row) predict_single(tree, as.data.frame(t(row)))))
  } else {
    return(predict_single(tree, new_data))
  }
}

#' @title Find the best split for classification
#' @description Find the best split for classification
#' @param data A data frame
#' @param features A vector of feature names
#' @param target The target variable name
#' @param min_samples_leaf The minimum number of samples required to be at a leaf node
#' @return A list containing the best split feature, value, and gini impurity
#' @export
find_best_split_classification <- function(data, features, target, min_samples_leaf) {
  best_gini <- Inf
  best_split <- NULL

  for (feature in features) {
    if (is.numeric(data[[feature]])) {
      split_points <- if (length(unique(data[[feature]])) <= 100) {
        sort(unique(data[[feature]]))
      } else {
        quantile(data[[feature]], probs = seq(0, 1, length.out = 100))
      }

      for (split in split_points) {
        left <- data[[target]][data[[feature]] <= split]
        right <- data[[target]][data[[feature]] > split]

        # 检查分割后的子节点是否满足最小样本数要求
        if (length(left) < min_samples_leaf || length(right) < min_samples_leaf) {
          next
        }

        gini <- (length(left) * calculate_gini(left) + length(right) * calculate_gini(right)) / nrow(data)

        if (gini < best_gini) {
          best_gini <- gini
          best_split <- list(feature = feature, value = split, gini = gini)
        }
      }
    } else {
      # 对分类变量，考虑所有可能的二分法
      levels <- unique(data[[feature]])
      for (level in levels) {
        left <- data[[target]][data[[feature]] == level]
        right <- data[[target]][data[[feature]] != level]

        # 检查分割后的子节点是否满足最小样本数要求
        if (length(left) < min_samples_leaf || length(right) < min_samples_leaf) {
          next
        }

        gini <- (length(left) * calculate_gini(left) + length(right) * calculate_gini(right)) / nrow(data)

        if (gini < best_gini) {
          best_gini <- gini
          best_split <- list(feature = feature, value = level, gini = gini)
        }
      }
    }
  }

  return(best_split)
}
#' @title Find the best split for regression
#' @description Find the best split for regression
#' @param data A data frame
#' @param features A vector of feature names
#' @param target The target variable name
#' @param min_samples_leaf The minimum number of samples required to be at a leaf node
#' @return A list containing the best split feature, value, and mean squared error
#' @export
find_best_split_regression <- function(data, features, target, min_samples_leaf) {
  best_mse <- Inf
  best_split <- NULL

  for (feature in features) {
    if (is.numeric(data[[feature]])) {
      split_points <- if (length(unique(data[[feature]])) <= 100) {
        sort(unique(data[[feature]]))
      } else {
        quantile(data[[feature]], probs = seq(0, 1, length.out = 100))
      }

      for (split in split_points) {
        left <- data[[target]][data[[feature]] <= split]
        right <- data[[target]][data[[feature]] > split]
        # 检查分割后的子节点是否满足最小样本数要求
        if (length(left) < min_samples_leaf || length(right) < min_samples_leaf) {
          next
        }

        # 计算左右子节点的均方误差和
        left_mse <- if (length(left) > 0) mean((left - mean(left))^2) else 0
        right_mse <- if (length(right) > 0) mean((right - mean(right))^2)
        mse_sum <- left_mse * length(left) + right_mse * length(right)
        total_length <- length(left) + length(right)
        mse <- mse_sum / total_length

        if (mse < best_mse) {
          best_mse <- mse
          best_split <- list(feature = feature, value = split, gini = mse)
        }
      }
    } else {
      levels <- unique(data[[feature]])
      for (level in levels) {
        left <- data[[target]][data[[feature]] == level]
        right <- data[[target]][data[[feature]] != level]

        # 检查分割后的子节点是否满足最小样本数要求
        if (length(left) < min_samples_leaf || length(right) < min_samples_leaf) {
          next
        }
        # 计算左右子节点的均方误差和
        left_mse <- if (length(left) > 0) mean((left - mean(left))^2) else 0
        right_mse <- if (length(right) > 0) mean((right - mean(right))^2)
        mse_sum <- left_mse * length(left) + right_mse * length(right)
        total_length <- length(left) + length(right)
        mse <- mse_sum / total_length

        if (mse < best_mse) {
          best_mse <- mse
          best_split <- list(feature = feature, value = level, gini = mse)
        }
      }
    }
  }
  return(best_split)
}
#' @title Find the best split for extratrees
#' @description Find the best split for extratrees
#' @param data A data frame
#' @param features A vector of feature names
#' @param target The target variable name
#' @return A list containing the best split feature, value, and gini impurity
#' @export
find_best_split_extratrees <- function(data, features, target) {
  feature <- sample(features, 1)
  value <- sample(data[, feature], 1)
  gini <- NA
  best_split <- list(feature = feature, value = value, gini = gini)
  return(best_split)
}
#' @title Calculate the Gini impurity of a vector
#' @description Calculate the Gini impurity of a vector
#' @param y A vector
#' @return The Gini impurity of the vector
#' @export
calculate_gini <- function(y) {
  if (length(y) == 0) {
    return(0)
  }
  p <- table(y) / length(y)
  return(1 - sum(p^2))
}

#' @title get the information of a tree
#' @description get the information of a tree
#' @param forest A list of trees
#' @param tree_number The number of the tree to get information from
#' @return A data frame containing the information of the tree
#' @export
tree_info <- function(forest, tree_number = 1) {
  get_tree_info <- function(node, nodeID = 0) {
    if (node$type == "leaf") {
      return(data.frame(
        nodeID = nodeID,
        leftChild = NA,
        rightChild = NA,
        splitvarName = NA,
        splitval = NA,
        terminal = TRUE,
        prediction = node$class
      ))
    } else {
      counter <- 1
      new_row <- data.frame(
        nodeID = nodeID,
        leftChild = nodeID * 2 + counter,
        rightChild = nodeID * 2 + counter + 1,
        splitvarName = node$feature,
        splitval = node$value,
        terminal = FALSE,
        prediction = NA
      )
      left_df <- get_tree_info(node$left, nodeID * 2 + counter)
      right_df <- get_tree_info(node$right, nodeID * 2 + counter + 1)
      combined_df <- rbind(new_row, left_df, right_df)
      combined_df <- combined_df[order(combined_df$nodeID), ]
      # 处理ID无法按顺序排列的问题
      n <- sum(!is.na(combined_df$leftChild) & !is.na(combined_df$rightChild))
      new_values <- seq(from = 1, by = 1, length.out = n * 2)
      combined_df$leftChild[!is.na(combined_df$leftChild)] <- new_values[seq(1, n * 2, 2)]
      combined_df$rightChild[!is.na(combined_df$rightChild)] <- new_values[seq(2, n * 2, 2)]
      # combined_df$nodeID <- seq(0, nrow(combined_df) - 1)
      rownames(combined_df) <- NULL
      return(combined_df)
    }
  }
  node <- forest$forest[[tree_number]]$tree
  df <- get_tree_info(node)
  return(df)
}

#' @title Calculate the best split leaf value for a given data frame and target variable
#' @description Calculate the best split leaf value for a given data frame and target variable
#' @param data A data frame
#' @param target The target variable
#' @param type The type of the target variable, either "classification" or "regression"
#' @return A list containing the best split leaf value and the corresponding Gini impurity
calc_leaf <- function(data, target, type) {
  if (type == "classification") {
    class_counts <- table(data[[target]])
    return(list(
      type = "leaf",
      class = names(which.max(class_counts)),
      prob = as.numeric(class_counts) / sum(class_counts),
      samples = nrow(data)
    ))
  } else {
    return(list(
      type = "leaf",
      class = round(mean(data[[target]]), 5),
      prob = NA,
      samples = nrow(data)
    ))
  }
}

#' @title collapse
#' @description collapse
#' @param x a vactor
#' @return x with spaces replaced by underscores
collapse <- function(x) {
  x <- sub(" ", "_", x)

  return(x)
}

#' @title build_tree
#' @description build_tree
#' @param df a dataframe
#' @return A tree structure
build_tree <- function(df) {
  df$row <- as.numeric(rownames(df))
  colnames(df) <- sapply(colnames(df), collapse)
  # 初始化一个空的列表来存储节点
  nodes <- vector("list", nrow(df))

  # 遍历数据框的每一行
  for (i in 1:nrow(df)) {
    # 创建一个新的节点
    if (df$status[i] == -1) {
      nodes[[i]] <- data.tree::Node$new(name = as.character(df$prediction[i]))
    } else {
      nodes[[i]] <- data.tree::Node$new(name = paste(df$split_var[i], "<=", df$split_point[i]))
    }
  }

  # 再次遍历数据框的每一行，将子节点添加到父节点
  for (i in 1:nrow(df)) {
    if (df$left_daughter[i] != 0) {
      nodes[[i]]$AddChildNode(nodes[[df$left_daughter[i]]])
    }
    if (df$right_daughter[i] != 0) {
      nodes[[i]]$AddChildNode(nodes[[df$right_daughter[i]]])
    }
  }

  # 返回根节点
  return(nodes[[1]])
}

#' @title plot_tree
#' @description plot_tree
#' @param tree the tree info dataframe
#' @param tree_from the method of making tree
#' @return A tree structure
#' @export
plot_tree <- function(tree, tree_from = "ranger") {
  if (tree_from == "ranger") {
    tree_data <- data.frame(
      "left_daughter" = tree$leftChild + 1,
      "right_daughter" = tree$rightChild + 1,
      "split_var" = tree$splitvarName,
      "split_point" = tree$splitval,
      "status" = ifelse(tree$terminal, -1, 1),
      "prediction" = tree$prediction
    )

    tree_data$left_daughter[is.na(tree_data$left_daughter)] <- 0
    tree_data$right_daughter[is.na(tree_data$right_daughter)] <- 0
    tree_plot <- build_tree(tree_data)
  } else if (tree_from == "randomForest") {
    tree_plot <- build_tree(tree)
  } else {
    stop("tree_from must be ranger or randomForest!")
  }
  return(tree_plot)
}

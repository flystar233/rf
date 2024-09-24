# 辅助函数：寻找最佳分割点
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
        mse = mse_sum / total_length
        
        if (mse < best_mse) {
          best_mse <- mse
          best_split <- list(feature = feature, value = split, gini = mse)
        }
      }
    } else {
      stop("分类变量不支持回归树")
    }
  }
  return(best_split)
}
find_best_split_extratrees <- function(data, features, target) {
  feature <- sample(features, 1)
  value <- sample(data[,feature],1)
  gini <- NA
  best_split <- list(feature = feature, value = value, gini = gini)
  return(best_split)
}
# 辅助函数：计算基尼不纯度
calculate_gini <- function(y) {
  if (length(y) == 0) return(0)
  p <- table(y) / length(y)
  return(1 - sum(p^2))
}
calculate_r_squared <- function(y_true, y_pred) {
  not_na <- !is.na(y_pred)
  y_true <- y_true[not_na,]
  y_pred <- y_pred[not_na]
  rss <- sum((y_pred - y_true) ^ 2)
  tss <- sum((y_true - mean(y_true)) ^ 2)
  r_squared <- 1 - rss / tss
  return(r_squared)
}

tree_info <- function(node, nodeID = 0) {
  # 如果是叶子节点
  if (node$type == "leaf") {
    return(data.frame(nodeID = nodeID,
                      leftChild = NA,
                      rightChild = NA,
                      splitvarName = NA,
                      splitval = NA,
                      terminal = TRUE,
                      prediction = node$class))
  } else { # 如果是内部节点
    counter <- 1
    new_row <- data.frame(nodeID = nodeID,
                          leftChild = nodeID * 2 + counter,
                          rightChild = nodeID * 2 + counter + 1,
                          splitvarName = node$feature,
                          splitval = node$value,
                          terminal = FALSE,
                          prediction = NA)
    left_df <- tree_info(node$left, nodeID * 2 + counter)
    right_df <- tree_info(node$right, nodeID * 2 + counter + 1)
    combined_df <- rbind(new_row, left_df, right_df)
    combined_df <- combined_df[order(combined_df$nodeID), ]
    #处理ID无法按顺序排列的问题
    n <- sum(!is.na(combined_df$leftChild) & !is.na(combined_df$rightChild))
    new_values <- seq(from = 1, by = 1, length.out = n * 2)
    combined_df$leftChild[!is.na(combined_df$leftChild)] <- new_values[seq(1, n * 2, 2)]
    combined_df$rightChild[!is.na(combined_df$rightChild)] <- new_values[seq(2, n * 2, 2)]
    combined_df$nodeID <- seq(0, nrow(combined_df) - 1)
    rownames(combined_df) <- NULL

    return(combined_df)
  }
}
calc_leaf <- function(data,target,type) {
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
    class = mean(data[[target]]),
    prob = NA,
    samples = nrow(data)
    ))
    
  }
}
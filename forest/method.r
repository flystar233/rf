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
  best_variance_sum <- Inf
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
        
        # 计算左右子节点的方差和
        variance_sum <- safe_var(left)+ safe_var(right)
        
        if (variance_sum < best_variance_sum) {
          best_variance_sum <- variance_sum
          best_split <- list(feature = feature, value = split, gini = variance_sum)
        }
      }
    } else {
        stop("分类变量不支持回归树")
    }
  }
  return(best_split)
}
# 辅助函数：计算基尼不纯度
calculate_gini <- function(y) {
  if (length(y) == 0) return(0)
  p <- table(y) / length(y)
  return(1 - sum(p^2))
}
# 打印决策树结构
tree_info <- function(object, tree_number = 1) {
  if (tree_number > object$n_trees){
    stop(paste("Error: Requesting tree",tree_number, ",but the tree number in this forest is", object$n_trees))
  }
  tree <- object$forest[[tree_number]]$tree
  print_tree <- function(tree, indent = "") {
  if (tree$type == "leaf") {
    cat(sprintf("%s|-- Leaf: class = %s, probability = %.2f, samples = %d\n", 
                indent, tree$class, tree$prob, tree$samples))
  } else {
    if(object$type == "classification") {
    cat(sprintf("%s|-- Node: feature = %s, split value = %.2f, gini = %.4f, samples = %d\n", 
                indent, tree$feature, tree$value, tree$gini, tree$samples))
        }else if(object$type == "regression") {
    cat(sprintf("%s|-- Node: feature = %s, split value = %.2f, variance = %.4f, samples = %d\n", 
                indent, tree$feature, tree$value, tree$gini, tree$samples))
        }
                
    print_tree(tree$left, paste0(indent, "|   "))
    print_tree(tree$right, paste0(indent, "|   "))
    }
  }
  print_tree(tree)
}

safe_var <- function(x) {
  if (length(unique(x)) <= 1) return(0)
  var(x)
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
  } else if (type == "regression") {
    return(list(
    type = "leaf",
    class = mean(data[[target]]),
    prob = NA,
    samples = nrow(data)
    ))
    
  } else {
    stop("Unknown tree type")
  }
}
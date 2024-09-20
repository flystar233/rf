build_decision_tree <- function(X, y, max_depth = Inf, min_samples_split = 2, min_samples_leaf = 1,type = "classification") {
  data <- cbind(X, y)
  features <- colnames(X)
  target <- colnames(y)[1]
  
  build_tree_recursive <- function(data, depth = 0) {
    # 检查停止条件
    if (nrow(data) < min_samples_split || depth == max_depth || length(unique(data[[target]])) == 1 || nrow(data) <= min_samples_leaf * 2) {
      leaf<- calc_leaf(data,target,type)
      return(leaf)
    }
    
    if (type == "classification") {
      best_split <- find_best_split_classification(data, features, target, min_samples_leaf)
    } else {
      best_split <- find_best_split_regression(data, features, target, min_samples_leaf)
    }
    if (is.null(best_split)) {
      leaf<- calc_leaf(data,target,type)
      return(leaf)
    }
    
    # 分割数据
    if(is.numeric(best_split$value)) {
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

# 预测函数
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

# 预测函数（可处理单个样本或整个数据集）
predict_tree <- function(tree, new_data) {
  if (is.data.frame(new_data) && nrow(new_data) > 1) {
    return(apply(new_data, 1, function(row) predict_single(tree, as.data.frame(t(row)))))
  } else {
    return(predict_single(tree, new_data))
  }
}
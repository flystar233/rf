library(dplyr)
library(future)
library(future.apply)
source('./tree.r')
source('./method.r')
# 随机森林函数
random_forest <- function(X, 
                          y,
                          n_trees = 100,
                          max_depth = NULL,
                          min_samples_split = 2,
                          min_samples_leaf = 1,
                          mtry = NULL,
                          subsample = ifelse(replace, 1, 0.632),
                          replace = TRUE,
                          seed = NULL,
                          type = "classification",
                          n_cores = parallel::detectCores() - 1) {
  
  n_features <- ncol(X)
  n_samples <- nrow(X)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if(is.null(max_depth)){
    max_depth <- Inf
  }
  if (is.null(mtry)) {
    mtry <- floor(sqrt(n_features))
  } else if (mtry < 1) {
    mtry <- 1
  } else if (mtry > n_features) {
    mtry <- n_features
  }
  if(!is.data.frame(y)){
    y <- as.data.frame(y)
    colnames(y) <- "target"
  }
  samples_per_tree <- floor(n_samples * subsample)
  
  # 设置并行计算
  plan(multisession, workers = n_cores)
  
  # 使用future_lapply并行构建树，并设置future.seed
  forest_and_oob <- future_lapply(1:n_trees, function(i) {
    sample_indices <- sample(1:n_samples, samples_per_tree, replace = replace)
    oob_indices <- setdiff(1:n_samples, unique(sample_indices))
    
    X_sample <- X[sample_indices, ]
    y_sample <- y[sample_indices,, drop=FALSE]
    
    # 随机选择特征子集
    feature_indices <- sample(1:n_features, mtry)
    X_sample_subset <- X_sample[, feature_indices, drop = FALSE]
    tree <- build_decision_tree(X_sample_subset, y_sample, max_depth, min_samples_split, min_samples_leaf,type)
    
    # 对OOB样本进行预测
    oob_predictions <- rep(NA, n_samples)
    if (length(oob_indices) > 0) {
      X_oob <- X[oob_indices, feature_indices, drop = FALSE]
      oob_predictions[oob_indices] <- predict_tree(tree, X_oob)
    }
    
    list(tree = tree, feature_indices = feature_indices, oob_predictions = oob_predictions)
  }, future.seed = TRUE)  # 设置future.seed为TRUE
  # 提取树和OOB预测
  forest <- lapply(forest_and_oob, function(x) list(tree = x$tree, feature_indices = x$feature_indices))
  oob_predictions <- do.call(cbind, lapply(forest_and_oob, `[[`, "oob_predictions"))
  # 计算OOB误差
  oob_error <- mean(sapply(1:n_samples, function(i) {
    row <- oob_predictions[i, ]
    if (all(is.na(row))) {
      return(NA)  # 如果该样本从未作为OOB样本，返回NA
    }

    if (type == "classification") {
      pred <- names(which.max(table(row[!is.na(row)])))
      return(pred != y[i, 1])
    } else {
      pred <- mean(row[!is.na(row)])
      return((pred- y[i, 1])^2)
    }
  }), na.rm = TRUE)
  
  # 关闭并行计算
  plan(sequential)
  
  return(list(forest = forest,
              oob_error = oob_error,
              mtry = mtry,
              n_trees = n_trees,
              max_depth = max_depth,
              min_samples_split = min_samples_split,
              min_samples_leaf = min_samples_leaf,
              subsample = subsample,
              replace = replace,
              type = type,
              seed = seed))
}

# 随机森林预测函数
predict_random_forest <- function(forest, new_data) {
  forest_list <- forest$forest
  predictions <- if (forest$type == "classification") 
                  {vector("character", nrow(new_data))
                  } else 
                  {vector("numeric", nrow(new_data))}
  for (i in 1:nrow(new_data)) {
    sample_predictions <- vector("numeric", length(forest_list))
    for (j in 1:length(forest_list)) {
      tree_info <- forest_list[[j]]
      tree <- tree_info$tree
      feature_indices <- tree_info$feature_indices
      new_data_subset <- new_data[i, feature_indices, drop = FALSE]
      sample_predictions[j] <- predict_tree(tree, new_data_subset)
    }
    if (forest$type == "classification") {
      predictions[i] <- names(which.max(table(sample_predictions)))
    } else {
      predictions[i] <- round(mean(sample_predictions),5)
    }
  }
  return(predictions)
}

# 计算随机森林的预测准确率
calculate_accuracy <- function(forest, X_test, y_test) {
  if(!is.data.frame(y_test)){
    y_test <- as.data.frame(y_test)
    colnames(y_test) <- "target"
  }
  predictions <- predict_random_forest(forest, X_test)
  accuracy <- sum(predictions == y_test) / nrow(y_test)
  return(accuracy)
}

#test iris
#data = iris|>select(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,Species)|>filter(Species!="setosa")
#forest <- random_forest(data[1:4],data[,5], n_trees = 100, max_depth = 5, min_samples_split = 2, min_samples_leaf = 1,replace = TRUE,seed = 42)

# test wine
data = read.csv("housing.txt",header = T)
forest <- random_forest(data[1:13],data[,14],n_trees = 100, max_depth = 5, min_samples_split = 10, min_samples_leaf = 5,replace = TRUE,seed = 42,type = "regression")
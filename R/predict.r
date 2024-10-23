#' @title Predict using a trained random forest
#' @param forest A random forest object.
#' @param new_data A data frame containing the new data to be predicted.
#' @param type The type of classification prediction to make. Default is "value".
#' @param n_cores The number of cores to use for parallel processing.
#' Default is the number of available cores minus one.
#' @return A vector of predictions.
#' @export
predict.random_forest <- function(forest,
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
      } else if (type == "prob"){
        return(prop.table(table(factor(sample_predictions, levels = forest$class_level))))
      } else {
        stop("The type parameter must be one of c('value', 'prob').")
      }
    } else {
      return(round(mean(sample_predictions), 5))
    }
  }
  # Use future_lapply to parallelize predictions
  predictions <- future.apply::future_lapply(1:nrow(new_data), function(i) predict_row(new_data[i, ]))
  if (forest$type == "classification" & type == "prob") {
    predictions <- do.call(rbind, predictions)
  } else {
    predictions <- unlist(predictions, use.names = FALSE)
  }
  return(predictions)
}
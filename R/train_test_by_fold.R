# create a function that trains a random forest model on a given set of rows and
# predicts on a disjunct set of rows
train_test_by_fold <- function(dfs, idx_train, idx_val){

  set.seed(1)   # for reproducibility

  target_vector <-dfs$leafN  # a vector of the target values

  mod <- ranger::ranger(
    x =  dfs[idx_train, 4:9],       # data frame with columns corresponding to predictors
    y =  target_vector[idx_train],  # a vector of the target values (not a data frame!)
    mtry = 3,                       # use the same hyperparameters as for random cv
    min.node.size = 12,
    splitrule = "variance",
    num.trees = 100,
    replace = FALSE,
    sample.fraction = 0.5,
    seed = 1
  )
  print(mod)

  pred <- predict(mod,       # the fitted model object
                  data = dfs[idx_val, 4:9] # a data frame with columns corresponding to predictors
  )

  results <- dfs[idx_val,]
  results$pred <- pred$predictions
  rmse <- yardstick::rmse(results, "leafN", "pred") # the root mean square error on the validation set
  rsq <- yardstick::rsq(results, "leafN", "pred")   # the R-squared determined on the validation set

  return(tibble(rsq = rsq, rmse = rmse))
}

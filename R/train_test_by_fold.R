# create a function that trains a random forest model on a given set of rows and
# predicts on a disjunct set of rows
train_test_by_fold <- function(idx_train, idx_val){
  print(df)
  print(idx_train)
  print(idx_val)
  mod <- ranger::ranger(
    x =  dfs[idx_train, 4:9],  # data frame with columns corresponding to predictors
    y =  target_vector[idx_val],
    tuneGrid = expand.grid( .mtry = 3,
                            .min.node.size = 12,
                            .splitrule = "variance"),
    num.trees = 100,
    metric = "RMSE",
    replace = FALSE,
    sample.fraction = 0.5 # a vector of the target values (not a data frame!)
    )

  print(mod$metric)

  pred <- predict(mod,       # the fitted model object
                  data = dfs[idx_val, 4:9] # a data frame with columns corresponding to predictors
  )
  print(pred)
  rsq <- mod$resample[,2]  # the R-squared determined on the validation set
  rmse <- mod$resample[,1] # the root mean square error on the validation set
  return(tibble(rsq = rsq, rmse = rmse))
}

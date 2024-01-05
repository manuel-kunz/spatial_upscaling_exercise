### Libraries
library(ranger)
library(caret)
library(recipes)
library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)
library(MODISTools)
library(appeears)
library(vroom)
library(rsample)
library(parsnip)
library(workflows)
library(tune)
library(dials)
library(xgboost)
library(stringr)

# load prepared data
dfs <- readRDS(here::here("./data/dfs.rds"))

# Specify target: leafN
target <- "leafN"

# Specify predictors: elv, mat, map, ndep, mai, Species
predictors <- names(dfs)[4:ncol(dfs)]

cat("The target is:", target,
    "\nThe predictors_all are:", paste0(predictors[1:6]))

# Set seed for reproducibility
set.seed(0)
# Split data
split <- rsample::initial_split(dfs, prop = 0.7)
dfs_train <- rsample::training(split)
dfs_test <- rsample::testing(split)

saveRDS(dfs_train, (paste0(here::here(),"./data/dfs_train.rds")))
saveRDS(dfs_test, (paste0(here::here(),"./data/dfs_test.rds")))

# Check if data split worked
n_tot <- nrow(dfs_train) + nrow(dfs_test)
perc_cal <- (nrow(dfs_train) / n_tot) |> round(2) * 100
perc_val <- (nrow(dfs_test)  / n_tot) |> round(2) * 100

cat("For model training, we have a calibration / validation split of: ",
    perc_cal, "/", perc_val, "%")


# Model formulation (from Chapter 11 of AGDS I)
pp <- recipes::recipe(leafN ~elv + mat + map + ndep + mai + Species,
                      data = dfs_train) |>
  recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) |>
  recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes())

mod <- train(
  pp,
  data = dfs_train %>%
    drop_na(),
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5, savePredictions = "final"),
  tuneGrid = expand.grid( .mtry = 3,
                          .min.node.size = 12,
                          .splitrule = "variance"),
  metric = "RMSE",
  replace = FALSE,
  sample.fraction = 0.5,
  num.trees = 100,
  seed = 0
)

print(mod)

# get the results per cross-validation fold
results_foldwise <-mod$resample

# round the results
results_foldwise[, c("RMSE", "Rsquared", "MAE")] <- round(results_foldwise[, c("RMSE", "Rsquared")], 2)

# remove MAE
results_foldwise$MAE <- NULL

# reorder the columns
results_foldwise <- results_foldwise[, c("Resample", "RMSE", "Rsquared")]
saveRDS(results_foldwise, (paste0(here::here(),"./data/results_foldwise.rds")))



# import the eval_model function from chapter 9 of AGSI I
source(here::here("./R/eval_model.R"))

plots_cv_results <- eval_model(mod = mod, df_train = dfs_train, df_test = dfs_test)
saveRDS(plots_cv_results, (paste0(here::here(),"./data/plots_cv_results.rds")))

# make model evaluation into a function to reuse code
eval_model <- function(mod, df_train, df_test){

  # add predictions to the data frames
  df_train <- df_train |>
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)

  df_test <- df_test |>
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)

  # get metrics tables
  metrics_train <- df_train |>
    yardstick::metrics(leafN, fitted)

  metrics_test <- df_test |>
    yardstick::metrics(leafN, fitted)

  # extract values from metrics tables
  rmse_train <- metrics_train |>
    filter(.metric == "rmse") |>
    pull(.estimate)
  rsq_train <- metrics_train |>
    filter(.metric == "rsq") |>
    pull(.estimate)

  rmse_test <- metrics_test |>
    filter(.metric == "rmse") |>
    pull(.estimate)
  rsq_test <- metrics_test |>
    filter(.metric == "rsq") |>
    pull(.estimate)

  # visualise as a scatterplot
  # adding information of metrics as sub-titles
  plot_1 <- ggplot(data = df_train, aes(leafN, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_train, digits = 2)) ~~
                              RMSE == .(format(rmse_train, digits = 3))),
         title = "Training set") +
    theme_classic()

  plot_2 <- ggplot(data = df_test, aes(leafN, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_test, digits = 2)) ~~
                              RMSE == .(format(rmse_test, digits = 3))),
         title = "Test set") +
    theme_classic()

  out <- cowplot::plot_grid(plot_1, plot_2)

  return(out)
}

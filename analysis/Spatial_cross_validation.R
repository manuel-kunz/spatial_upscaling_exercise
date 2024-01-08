### Libraries
library(ggplot2)
library(leaflet)
library(purrr)
library(tidyr)
library(dplyr)
library(Metrics)
library(yardstick)
library(ranger)

### 3.1 Spatial distribution of data
# load prepared data
dfs <- readRDS(here::here("./data/dfs.rds"))


map_datapoints <- leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") |>
  addProviderTiles(providers$Esri.WorldTopoMap, group = "World Topo") |>
  addCircleMarkers(
    data = dfs,
    lng = ~lon,
    lat = ~lat,
    color = "red",
    radius = 0.2,
    group = "Data points"
  ) |>
  addLayersControl(
    baseGroups = c("World Imagery","World Topo"),
    position = "topleft",
    options = layersControlOptions(collapsed = FALSE),
    overlayGroups = c("Data points")
  )

saveRDS(map_datapoints, (paste0(here::here(),"./data/map_datapoints.rds")))

### 3.2 Kmeans clusters
# cluster the data
clusters <- kmeans(
  dfs[,2:3],
  centers = 5
)

dfs$cluster <- clusters$cluster

saveRDS(dfs, here::here("./data/dfs.rds"))

palcol <- colorFactor(
  c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3'),
  domain = 1:5,
  na.color = "transparent"
  )

map_clustered <- leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") |>
  addProviderTiles(providers$Esri.WorldTopoMap, group = "World Topo") |>
  addCircleMarkers(
    data = dfs,
    lng = ~lon,
    lat = ~lat,
    color = "red",
    radius = 0.1,
    opacity = 1,
    fillOpacity = 1,
    group = "Data points"
  ) |>
  addCircleMarkers(
    data = dfs,
    lng = ~lon,
    lat = ~lat,
    color = ~palcol(cluster),
    radius = 0.1,
    opacity = 1,
    fillOpacity = 1,
    group = "Data points spatially clustered"
  ) |>
  addLayersControl(
    baseGroups = c("World Imagery","World Topo"),
    position = "topleft",
    options = layersControlOptions(collapsed = FALSE),
    overlayGroups = c("Data points", "Data points spatially clustered")
  ) |>
  addLegend(
    colors = palcol(1:5),
    values = c(1, 2, 3, 4, 5),
    title = "cluster",
    labels = c(1, 2, 3, 4, 5)
  )

saveRDS(map_clustered, (paste0(here::here(),"./data/map_datapoints_clustered.rds")))

### 3.3 Density plots
# create density plots to show the leaf N values of the 5 clusters
density_plots <- ggplot(dfs, aes (x = leafN, fill = factor(cluster))) +
  geom_density(alpha = 0.7) +
  facet_wrap(~cluster, scales = "fixed") +
  scale_fill_manual(values = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3')) +
  theme_gray() +
  labs(title = "Leaf N density plot for each cluster", x = "Leaf N values", y = "Density")+
  guides(fill = guide_legend(title = "Cluster"))

saveRDS(density_plots, (paste0(here::here(),"./data/density_plots.rds")))


### 3.4 Random forest with clusters as folds -> Spatial cross-validation

# create folds based on clusters assuming 'dfs' contains the data and a column called 'cluster' containing the
# result of the k-means clustering -> code taken from exercise
group_folds_train <- purrr::map(
  seq(length(unique(dfs$cluster))),
  ~ {
    dfs |>
      select(cluster) |>
      mutate(idx = 1:n()) |>
      filter(cluster != .) |>
      pull(idx)
  }
)

saveRDS(group_folds_train, (paste0(here::here(),"./data/group_folds_train.rds")))

group_folds_test <- purrr::map(
  seq(length(unique(dfs$cluster))),
  ~ {
    dfs |>
      select(cluster) |>
      mutate(idx = 1:n()) |>
      filter(cluster == .) |>
      pull(idx)
  }
)
saveRDS(group_folds_test, (paste0(here::here(),"./data/group_folds_test.rds")))


# apply function on each custom fold and collect validation results in a nice
# data frame
source(here::here("./R/train_test_by_fold.R"))

out <- purrr::map2_dfr(group_folds_train,
                       group_folds_test,
                       ~train_test_by_fold(dfs, .x, .y)
) |>
  mutate(test_fold = 1:5)
out
# remove estimator
out$rsq$.estimator <- NULL
out$rmse$.estimator <- NULL

out$rsq$.estimate <- round(out$rsq$.estimate, digits = 2)
out$rmse$.estimate <- round(out$rmse$.estimate, digits = 2)

out <- out[, c("test_fold", "rmse", "rsq")]

out_df <- data.frame(
  "Test_fold" = out$test_fold,
  "RMSE" = out$rmse$.estimate,
  "R-Squared" = out$rsq$.estimate
)


saveRDS(out_df, (paste0(here::here(),"./data/results_spatial_cv.rds")))


### 3.5 Random forest with clusters as folds -> Environmental cross-validation

# cluster the data
clusters_env <- kmeans(
  dfs[,5:6],
  centers = 5
)

dfs$cluster_env <- clusters_env$cluster

saveRDS(dfs, here::here("./data/dfs.rds"))

palcol <- colorFactor(
  c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3'),
  domain = 1:5,
  na.color = "transparent"
)

map_clustered_env <- leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") |>
  addProviderTiles(providers$Esri.WorldTopoMap, group = "World Topo") |>
  addCircleMarkers(
    data = dfs,
    lng = ~lon,
    lat = ~lat,
    color = "red",
    radius = 0.1,
    opacity = 1,
    fillOpacity = 1,
    group = "Data points"
  ) |>
  addCircleMarkers(
    data = dfs,
    lng = ~lon,
    lat = ~lat,
    color = ~palcol(cluster_env),
    radius = 0.1,
    opacity = 1,
    fillOpacity = 1,
    group = "Data points environmentally clustered"
  ) |>
  addLayersControl(
    baseGroups = c("World Imagery","World Topo"),
    position = "topleft",
    options = layersControlOptions(collapsed = FALSE),
    overlayGroups = c("Data points", "Data points environmentally clustered")
  ) |>
  addLegend(
    colors = palcol(1:5),
    values = c(1, 2, 3, 4, 5),
    title = "cluster",
    labels = c(1, 2, 3, 4, 5)
  )

saveRDS(map_clustered_env, (paste0(here::here(),"./data/map_datapoints_clustered_env.rds")))

### 3.3 Density plots
# create density plots to show the leaf N values of the 5 clusters
density_plots <- ggplot(dfs, aes (x = leafN, fill = factor(cluster_env))) +
  geom_density(alpha = 0.7) +
  facet_wrap(~cluster_env, scales = "fixed") +
  scale_fill_manual(values = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3')) +
  theme_gray() +
  labs(title = "Leaf N density plot for each cluster", x = "Leaf N values", y = "Density")+
  guides(fill = guide_legend(title = "Cluster"))

saveRDS(density_plots, (paste0(here::here(),"./data/density_plots_env.rds")))


# create folds based on clusters assuming 'dfs' contains the data and a column called 'cluster' containing the
# result of the k-means clustering -> code taken from exercise
group_folds_train <- purrr::map(
  seq(length(unique(dfs$cluster_env))),
  ~ {
    dfs |>
      select(cluster_env) |>
      mutate(idx = 1:n()) |>
      filter(cluster_env != .) |>
      pull(idx)
  }
)

group_folds_test <- purrr::map(
  seq(length(unique(dfs$cluster_env))),
  ~ {
    dfs |>
      select(cluster_env) |>
      mutate(idx = 1:n()) |>
      filter(cluster_env == .) |>
      pull(idx)
  }
)

# apply function on each custom fold and collect validation results in a nice
# data frame
source(here::here("./R/train_test_by_fold.R"))

out <- purrr::map2_dfr(group_folds_train,
                       group_folds_test,
                       ~train_test_by_fold(dfs, .x, .y)
) |>
  mutate(test_fold = 1:5)
out

# remove estimator
out$rsq$.estimator <- NULL
out$rmse$.estimator <- NULL

out$rsq$.estimate <- round(out$rsq$.estimate, digits = 2)
out$rmse$.estimate <- round(out$rmse$.estimate, digits = 2)

out <- out[, c("test_fold", "rmse", "rsq")]

out_df <- data.frame(
  "Test_fold" = out$test_fold,
  "RMSE" = out$rmse$.estimate,
  "R-Squared" = out$rsq$.estimate
)

saveRDS(out_df, (paste0(here::here(),"./data/results_environmental_cv.rds")))

library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# load prepared data
dfs <- readRDS(here::here("./data/dfs.rds"))

# map with data points

# get coast outline
coast <- rnaturalearth::ne_coastline(scale = 110, returnclass = "sf")

map_datapoints <-ggplot() +

  # plot coastline
  geom_sf(data = coast,
          colour = 'black',
          size = 0.2) +

  # set extent in longitude and latitude
  coord_sf(
    ylim = c(-60, 80),
    expand = FALSE) +  # to draw map strictly bounded by the specified extent

  # plot points on map
  geom_point(data = dfs, aes(x = lon, y = lat), color = "red", size = 0.2) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom")



library(leaflet)

# set te colour scale manually
palcol <- colorFactor(
  c("#78d203", "#f9ffa4", "#5F9EA0", "#EEE0E5"),
  domain = 1:4,
  na.color = "transparent"
)

# build the leaflet map

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


# cluster the data
clusters <- kmeans(
  dfs[,2:3],
  centers = 5
)

dfs$cluster <- clusters$cluster

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




# get coast outline
coast <- rnaturalearth::ne_coastline(scale = 110, returnclass = "sf")

map_datapoints_clustered <- ggplot() +

  # plot coastline
  geom_sf(data = coast,
          colour = 'black',
          size = 0.2) +

  # set extent in longitude and latitude
  coord_sf(
    ylim = c(-60, 80),
    expand = FALSE) +  # to draw map strictly bounded by the specified extent

  # plot points on map
  geom_point(data = dfs, aes(x = lon, y = lat), color = dfs$cluster, size = 0.2) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom")


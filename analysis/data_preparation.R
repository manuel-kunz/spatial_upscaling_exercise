library(tidyverse)
library(readr)


# read data from github
df <- readr::read_csv("https://raw.githubusercontent.com/stineb/leafnp_data/main/data/leafnp_tian_et_al.csv")

# select a subset
common_species <- df |>
  group_by(Species) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  slice(1:50) |>
  pull(Species)

dfs <- df |>
  dplyr::select(leafN, lon, lat, elv, mat, map, ndep, mai, Species) |>
  filter(Species %in% common_species)
  #group_by(lon, lat) |>
  #summarise(across(where(is.numeric), mean))

# quick overview of data
skimr::skim(dfs)

# show missing data
visdat::vis_miss(dfs)

saveRDS(dfs, here::here("./data/dfs.rds"))

install.packages("cancensus")
library(cancensus)
library(sf)
library(dplyr)



# Put your CensusMapper API key here
# (get a free one from https://censusmapper.ca/api)
options(cancensus.api_key = "CensusMapper_15a1d936b80c6b2a46ce96659b8944e3")


vancouver_da <- cancensus::get_census(
  dataset   = "CA21",             # 2021 Census
  regions   = list(CMA = "59933"),# Vancouver CMA code :contentReference[oaicite:2]{index=2}
  level     = "DA",               # Dissemination Area
  vectors   = c("v_CA21_1"),      # Total population, 2021
  geo_format = "sf",              # return sf object
  labels    = "short"             # short labels for variables
)


names(vancouver_da)
head(vancouver_da)


vancouver_blocks <- vancouver_da |>
  mutate(
    DAUID    = GeoUID,      # ID column
    POP_2021 = v_CA21_1     # population column
  ) |>
  select(DAUID, POP_2021, geometry)

dir.create("data/custom", recursive = TRUE, showWarnings = FALSE)

st_write(
  vancouver_blocks,
  "C:\\Users\\hengyinl.stu\\Desktop\\city_walkability\\analysis\\data\\custom\\vancouver_blocks.gpkg",
  delete_dsn = TRUE
)


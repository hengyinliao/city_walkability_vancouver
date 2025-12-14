## #######################################################################################
##
## Download points of interest (POIs) for Vancouver, BC using OpenStreetMap
##
## Adapted from Seattle-specific script by Nathaniel Henry
## Purpose: Prepare city-specific destinations (parks, transit stops, libraries)
##          using OSM + the bounding box from script 1, and also export
##          SkyTrain and FTN bus lines.
##
## Before running:
##  - Run 1_prepare_blocks.R (to create extended_bbox.yaml)
##  - Install: argparse, data.table, glue, sf, yaml, osmdata, dplyr, units
##
## Output:
##  - destinations_city_specific.csv (parks, transit stops, libraries)
##  - skytrain_lines.gpkg
##  - ftn_bus_lines.gpkg
##
## #######################################################################################

## SET DEFAULT CONFIG PATH
DEFAULT_CONFIG_FILEPATH <- "C:/Users/hengyinl.stu/Desktop/city_walkability/analysis/config.yaml"

## Setup -------------------------------------------------------------------------------->

load_packages <- c(
  "argparse", "data.table", "glue", "sf", "yaml",
  "osmdata", "dplyr", "units"
)
load_packages |> lapply(library, character.only = TRUE) |> invisible()

# Load config.yaml
if (interactive()) {
  config_filepath <- DEFAULT_CONFIG_FILEPATH
  message("Using default config location: ", config_filepath)
} else {
  parser <- argparse::ArgumentParser()
  parser$add_argument(
    "--config_filepath",
    type = "character",
    help = "Full path to the config.yaml file containing run settings"
  )
  config_filepath <- parser$parse_args(commandArgs(trailingOnly = TRUE))$config_filepath
}
config   <- yaml::read_yaml(config_filepath)
settings <- config$project_settings

# Create all output folders
for (dir in config$directories) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

# Load the bounding box for this analysis (created in script 1)
bb_list <- yaml::read_yaml(
  file.path(config$directories$prepared_data, "extended_bbox.yaml")
)
bb_vec <- c(bb_list$xmin, bb_list$ymin, bb_list$xmax, bb_list$ymax)

working_crs <- sf::st_crs(settings$working_crs)

## Helper: query OSM and return points --------------------------------------------------

get_osm_points <- function(bb_vec, tags_list, type_label,
                           filter_polygons_area = FALSE,
                           area_threshold_sq_m = NULL,
                           working_crs = NULL) {
  
  message("Querying OSM for ", type_label, " ...")
  
  q <- opq(bb_vec)
  
  # tags_list is a named list, e.g. list(leisure = c("park", "garden"))
  for (key in names(tags_list)) {
    values <- tags_list[[key]]
    q <- add_osm_feature(q, key = key, value = values)
  }
  
  res <- osmdata_sf(q)
  
  pts   <- res$osm_points
  polys <- res$osm_polygons
  
  out_pts <- list()
  
  # 1. Point features directly
  if (!is.null(pts) && nrow(pts) > 0) {
    coords <- sf::st_coordinates(pts)
    out_pts[[length(out_pts) + 1]] <- data.table::data.table(
      name    = pts$name,
      lon     = coords[, 1],
      lat     = coords[, 2],
      address = pts$`addr:street`,
      type    = type_label
    )
  }
  
  # 2. Convert polygons to centroids if requested (for parks)
  if (!is.null(polys) && nrow(polys) > 0) {
    
    if (!is.null(working_crs) && filter_polygons_area && !is.null(area_threshold_sq_m)) {
      polys_proj <- sf::st_transform(polys, crs = working_crs)
      areas      <- sf::st_area(polys_proj)
      keep_idx   <- which(areas > units::set_units(area_threshold_sq_m, m^2))
      if (length(keep_idx) > 0) {
        polys <- polys[keep_idx, ]
      } else {
        polys <- polys[0, ]
      }
    }
    
    if (nrow(polys) > 0) {
      cent <- sf::st_centroid(polys)
      coords <- sf::st_coordinates(cent)
      out_pts[[length(out_pts) + 1]] <- data.table::data.table(
        name    = cent$name,
        lon     = coords[, 1],
        lat     = coords[, 2],
        address = polys$`addr:street`,
        type    = type_label
      )
    }
  }
  
  if (length(out_pts) == 0) {
    warning("No OSM features found for ", type_label)
    return(NULL)
  }
  
  data.table::rbindlist(out_pts, use.names = TRUE, fill = TRUE)
}

## 2. Parks ----------------------------------------------------------------------------- 

# Park size threshold: from config if available, otherwise 20,000 sqft
park_min_sqft <- if (!is.null(settings$park_min_size_sqft)) {
  settings$park_min_size_sqft
} else {
  20000
}
park_min_sqm <- as.numeric(park_min_sqft) * 0.092903  # 1 sqft = 0.092903 m²

parks_tags <- list(
  leisure = c("park", "garden", "playground"),
  landuse = c("recreation_ground")
)

parks_table <- get_osm_points(
  bb_vec,
  tags_list = parks_tags,
  type_label = "Parks",
  filter_polygons_area = TRUE,
  area_threshold_sq_m = park_min_sqm,
  working_crs = working_crs
)

## 3. Transit stops (bus + SkyTrain-ish, classify FTN vs others) ------------------------ 

message("Querying OSM for transit stops ...")

# We’ll query bus stops + public_transport platforms/stations + railway stations
q_transit <- opq(bb_vec) |>
  add_osm_feature(key = "highway",          value = "bus_stop") |>
  add_osm_feature(key = "public_transport", value = c("platform", "stop_position", "station"), match_case = FALSE)

res_transit <- osmdata_sf(q_transit)
transit_pts <- res_transit$osm_points

transit_table <- NULL
if (!is.null(transit_pts) && nrow(transit_pts) > 0) {
  coords <- sf::st_coordinates(transit_pts)
  dt <- as.data.table(transit_pts)
  dt[, lon := coords[, 1]]
  dt[, lat := coords[, 2]]
  
  # Build a text field that aggregates route-ish info to search for FTN & SkyTrain
  # Many OSM stops may have route_ref, ref, name, network, etc.
  safe_col <- function(x) if (x %in% names(dt)) dt[[x]] else NA_character_
  dt[, route_info := paste(
    safe_col("route_ref"),
    safe_col("ref"),
    name,
    safe_col("network"),
    safe_col("operator")
  )]
  
  # Default type: Other transit stops
  dt[, type := "Other transit stops"]
  
  # SkyTrain-ish: look for keywords in name/route_info
  skytrain_pattern <- "SkyTrain|Expo Line|Canada Line|Millennium Line|Station"
  dt[grepl(skytrain_pattern, route_info, ignore.case = TRUE),
     type := "Light rail / SkyTrain stops"]
  
  # FTN: 99 B-Line & RapidBus (R1, R2, R4, R5 etc.)
  # Heuristic: look for these route numbers in route_info
  ftn_pattern <- "(^|[^0-9])(99|R1|R2|R3|R4|R5)([^0-9]|$)"
  dt[grepl(ftn_pattern, route_info, ignore.case = TRUE),
     type := "FTN bus stops"]
  
  # Final table for transit stops
  transit_table <- dt[, .(name = name, lon, lat, type)]
}

## 4. Libraries ------------------------------------------------------------------------- 

libraries_tags <- list(
  amenity = "library"
)

libraries_raw <- get_osm_points(
  bb_vec,
  tags_list = libraries_tags,
  type_label = "Libraries",
  filter_polygons_area = FALSE,
  working_crs = working_crs
)

libraries_table <- NULL
if (!is.null(libraries_raw)) {
  lib_dt <- as.data.table(libraries_raw)
  libraries_table <- lib_dt[, .(name, lon, lat, type = "Libraries")]
}

## 5. SkyTrain lines -------------------------------------------------------------------- 

message("Querying OSM for SkyTrain lines ...")

q_sky <- opq(bb_vec) |>
  add_osm_feature(key = "railway", value = c("light_rail", "subway"))

res_sky <- osmdata_sf(q_sky)
sky_lines <- res_sky$osm_lines

if (!is.null(sky_lines) && nrow(sky_lines) > 0) {
  # Keep mainly SkyTrain-ish lines using name/operator as a heuristic
  sky_keep <- grepl("SkyTrain|Expo Line|Canada Line|Millennium Line", sky_lines$name, ignore.case = TRUE) |
    grepl("TransLink", sky_lines$operator, ignore.case = TRUE)
  
  sky_lines_keep <- sky_lines[sky_keep, ]
  
  if (nrow(sky_lines_keep) > 0) {
    sky_lines_keep <- sf::st_transform(sky_lines_keep, crs = working_crs)
    sf::st_write(
      sky_lines_keep,
      dsn = file.path(config$directories$prepared_data, "skytrain_lines.gpkg"),
      delete_dsn = TRUE
    )
    message("Saved SkyTrain lines to skytrain_lines.gpkg")
  } else {
    message("No SkyTrain-like lines after filtering; not writing skytrain_lines.gpkg")
  }
} else {
  message("No railway=light_rail/subway lines found in bbox for SkyTrain.")
}

## 6. FTN bus lines (99, R-series) ------------------------------------------------------ 

message("Querying OSM for FTN bus lines (99, R1, R2, R4, R5, etc.) ...")

# route=bus + ref in FTN set
q_ftn <- opq(bb_vec) |>
  add_osm_feature(key = "route", value = "bus") |>
  add_osm_feature(key = "ref",   value = c("99", "R1", "R2", "R3", "R4", "R5"))

res_ftn <- osmdata_sf(q_ftn)
ftn_lines <- res_ftn$osm_lines

if (!is.null(ftn_lines) && nrow(ftn_lines) > 0) {
  ftn_lines <- sf::st_transform(ftn_lines, crs = working_crs)
  sf::st_write(
    ftn_lines,
    dsn = file.path(config$directories$prepared_data, "ftn_bus_lines.gpkg"),
    delete_dsn = TRUE
  )
  message("Saved FTN bus lines to ftn_bus_lines.gpkg")
} else {
  message("No FTN bus lines found (route=bus & ref in {99,R1,R2,R3,R4,R5}).")
}

## 7. Combine POI tables and save ------------------------------------------------------- 

tables_to_bind <- list(parks_table, transit_table, libraries_table)
tables_to_bind <- Filter(Negate(is.null), tables_to_bind)

if (length(tables_to_bind) == 0) {
  stop("No POIs (parks, transit stops, libraries) were found in the OSM queries.")
}

destinations_table_city_specific <- data.table::rbindlist(
  tables_to_bind,
  use.names = TRUE,
  fill = TRUE
)

data.table::fwrite(
  destinations_table_city_specific,
  file = file.path(config$directories$prepared_data, "destinations_city_specific.csv")
)

message(
  "Finished preparing city-specific destinations. Saved to: ",
  file.path(config$directories$prepared_data, "destinations_city_specific.csv")
)

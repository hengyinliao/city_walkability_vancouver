## #######################################################################################
## Prepare destinations for walkability (Vancouver / Metro Vancouver)
##
## Destinations generated:
##   - Parks: from your QGIS-exported leisure=park layer
##   - Libraries: from OSM (amenity=library)
##   - Transit (from TransLink GTFS Static):
##       1) Bus stops (local bus; excludes RapidBus)
##       2) RapidBus stops (R1–R6)
##       3) SkyTrain stops
##
## Output:
##   prepared_data/destinations_city_specific.csv
## #######################################################################################

## -------------------- USER EDIT (paths) --------------------

DEFAULT_CONFIG_FILEPATH <- "C:/Users/hengyinl.stu/Desktop/city_walkability/analysis/config.yaml"

PARKS_LAYER_PATH <- "C:/Users/hengyinl.stu/Documents/ArcGIS/Projects/CityWalkability Project/leisure park.gpkg"
PARKS_LAYER_NAME <- NULL   # if gpkg has multiple layers, set e.g. "parks"

GTFS_ZIP_PATH <- "C:/Users/hengyinl.stu/Desktop/city_walkability_vancouver/analysis/google_transit.zip"

USE_OSM_LIBRARIES <- TRUE

## -----------------------------------------------------------

suppressPackageStartupMessages({
  library(argparse)
  library(data.table)
  library(sf)
  library(yaml)
  library(osmdata)
  library(dplyr)
  library(utils)
})

## Load config.yaml
if (interactive()) {
  config_filepath <- DEFAULT_CONFIG_FILEPATH
  message("Using default config location: ", config_filepath)
} else {
  parser <- argparse::ArgumentParser()
  parser$add_argument("--config_filepath", type = "character",
                      help = "Full path to config.yaml")
  config_filepath <- parser$parse_args(commandArgs(trailingOnly = TRUE))$config_filepath
}

config   <- yaml::read_yaml(config_filepath)
settings <- config$project_settings

for (dir in config$directories) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

bb_list <- yaml::read_yaml(file.path(config$directories$prepared_data, "extended_bbox.yaml"))
bb_vec  <- c(bb_list$xmin, bb_list$ymin, bb_list$xmax, bb_list$ymax)
message("BBox (WGS84):")
print(bb_vec)

working_crs <- sf::st_crs(settings$working_crs)
if (is.na(working_crs)) stop("working_crs in config is invalid.")

## -------------------- Helper functions --------------------

read_layer <- function(path, layer = NULL) {
  if (is.null(path)) return(NULL)
  if (!file.exists(path)) stop("File not found: ", path)
  
  if (is.null(layer) || length(layer) == 0 || is.na(layer) || !is.character(layer) || nchar(layer) == 0) {
    return(sf::st_read(path, quiet = TRUE))
  } else {
    return(sf::st_read(path, layer = as.character(layer), quiet = TRUE))
  }
}

to_points_wgs84 <- function(x, centroid_crs = 26910) {
  stopifnot(inherits(x, "sf"))
  g <- sf::st_geometry_type(x, by_geometry = TRUE)
  
  is_poly <- g %in% c("POLYGON","MULTIPOLYGON","MULTISURFACE","CURVEPOLYGON")
  if (any(is_poly)) {
    x_proj <- sf::st_transform(x, centroid_crs)
    x_proj <- sf::st_make_valid(x_proj)
    x_cent <- sf::st_centroid(x_proj)
    x      <- sf::st_transform(x_cent, 4326)
  } else {
    x <- sf::st_transform(x, 4326)
  }
  x
}

sf_to_dest_dt <- function(x_pts, type_label, name_field = NULL) {
  if (is.null(x_pts) || nrow(x_pts) == 0) return(NULL)
  xy <- sf::st_coordinates(x_pts)
  
  if (!is.null(name_field) && name_field %in% names(x_pts)) {
    nm <- as.character(x_pts[[name_field]])
  } else if ("name" %in% names(x_pts)) {
    nm <- as.character(x_pts[["name"]])
  } else if ("NAME" %in% names(x_pts)) {
    nm <- as.character(x_pts[["NAME"]])
  } else if ("park_name" %in% names(x_pts)) {
    nm <- as.character(x_pts[["park_name"]])
  } else {
    nm <- rep(NA_character_, nrow(x_pts))
  }
  
  data.table(
    name = nm,
    lon  = xy[,1],
    lat  = xy[,2],
    type = type_label
  )
}

dedup_dest <- function(dt) {
  if (is.null(dt) || nrow(dt) == 0) return(dt)
  dt[, lon_round := round(lon, 6)]
  dt[, lat_round := round(lat, 6)]
  dt <- unique(dt, by = c("type","name","lon_round","lat_round"))
  dt[, c("lon_round","lat_round") := NULL]
  dt
}

osm_points_simple <- function(bb_vec, tags_list, type_label, timeout = 180) {
  q <- opq(bb_vec, timeout = timeout)
  for (k in names(tags_list)) q <- add_osm_feature(q, key = k, value = tags_list[[k]])
  res <- osmdata_sf(q)
  pts <- res$osm_points
  if (is.null(pts) || nrow(pts) == 0) return(NULL)
  pts <- sf::st_transform(pts, 4326)
  sf_to_dest_dt(pts, type_label)
}

read_gtfs_tables <- function(gtfs_zip_path) {
  if (is.null(gtfs_zip_path) || !file.exists(gtfs_zip_path)) {
    stop(paste0("GTFS zip not found: ", gtfs_zip_path))
  }
  
  gtfs_dir <- file.path(tempdir(), paste0("gtfs_", as.integer(Sys.time())))
  dir.create(gtfs_dir, recursive = TRUE, showWarnings = FALSE)
  unzip(gtfs_zip_path, exdir = gtfs_dir)
  
  need_files <- c("routes.txt", "trips.txt", "stop_times.txt", "stops.txt")
  missing <- need_files[!file.exists(file.path(gtfs_dir, need_files))]
  if (length(missing) > 0) {
    stop("GTFS zip missing required files: ", paste(missing, collapse = ", "))
  }
  
  list(
    routes     = fread(file.path(gtfs_dir, "routes.txt")),
    trips      = fread(file.path(gtfs_dir, "trips.txt")),
    stop_times = fread(file.path(gtfs_dir, "stop_times.txt")),
    stops      = fread(file.path(gtfs_dir, "stops.txt"))
  )
}

get_stops_for_routes <- function(routes_df, trips_df, stop_times_df, stops_df, route_ids, type_label) {
  if (length(route_ids) == 0) return(NULL)
  
  tr <- trips_df[route_id %in% route_ids]
  if (nrow(tr) == 0) return(NULL)
  
  stop_ids <- unique(stop_times_df[trip_id %in% tr$trip_id, stop_id])
  if (length(stop_ids) == 0) return(NULL)
  
  s <- stops_df[stop_id %in% stop_ids]
  if (nrow(s) == 0) return(NULL)
  
  dt <- data.table(
    name   = as.character(s$stop_name),
    lon    = as.numeric(s$stop_lon),
    lat    = as.numeric(s$stop_lat),
    type   = type_label,
    stop_id = as.character(s$stop_id)
  )
  dt <- dt[!is.na(lon) & !is.na(lat)]
  dt <- unique(dt, by = c("type","stop_id"))
  dt[, stop_id := NULL]
  dt
}

## -------------------- 1) Parks --------------------

message("Loading Parks from local layer ...")
parks_sf  <- read_layer(PARKS_LAYER_PATH, PARKS_LAYER_NAME)
parks_pts <- to_points_wgs84(parks_sf, centroid_crs = 26910)
parks_dt  <- sf_to_dest_dt(parks_pts, type_label = "Parks", name_field = NULL)
parks_dt  <- dedup_dest(parks_dt)
message("Parks loaded: ", nrow(parks_dt))

## -------------------- 2) Libraries --------------------

libraries_dt <- NULL
if (isTRUE(USE_OSM_LIBRARIES)) {
  message("Querying OSM for libraries ...")
  libraries_dt <- osm_points_simple(bb_vec, list(amenity = "library"), "Libraries", timeout = 180)
  libraries_dt <- dedup_dest(libraries_dt)
  message("Libraries loaded: ", ifelse(is.null(libraries_dt), 0, nrow(libraries_dt)))
}

## -------------------- 3) Transit from GTFS --------------------

message("Reading GTFS ...")
gtfs <- read_gtfs_tables(GTFS_ZIP_PATH)
routes     <- gtfs$routes
trips      <- gtfs$trips
stop_times <- gtfs$stop_times
stops      <- gtfs$stops

# RapidBus
RAPID_NAMES <- c("R1","R2","R3","R4","R5","R6")
rapid_routes <- routes[route_short_name %in% RAPID_NAMES]
message("RapidBus routes found: ", nrow(rapid_routes))

rapid_dt <- get_stops_for_routes(routes, trips, stop_times, stops,
                                 route_ids = rapid_routes$route_id,
                                 type_label = "RapidBus stops")
rapid_dt <- dedup_dest(rapid_dt)
message("RapidBus stops: ", ifelse(is.null(rapid_dt), 0, nrow(rapid_dt)))

# SkyTrain
sky_routes <- routes[
  grepl("SkyTrain|Expo|Millennium|Canada|Evergreen",
        paste(route_short_name, route_long_name, route_desc),
        ignore.case = TRUE)
]
message("SkyTrain-like routes found: ", nrow(sky_routes))

sky_dt <- get_stops_for_routes(routes, trips, stop_times, stops,
                               route_ids = sky_routes$route_id,
                               type_label = "SkyTrain stops")
sky_dt <- dedup_dest(sky_dt)
message("SkyTrain stops: ", ifelse(is.null(sky_dt), 0, nrow(sky_dt)))

# Bus stops (route_type == 3)
bus_dt <- NULL
if ("route_type" %in% names(routes)) {
  bus_routes <- routes[route_type == 3]
  message("Bus routes (route_type==3) found: ", nrow(bus_routes))
  
  bus_all_dt <- get_stops_for_routes(routes, trips, stop_times, stops,
                                     route_ids = bus_routes$route_id,
                                     type_label = "Bus stops")
  
  # Exclude RapidBus stops (mutually exclusive categories)
  if (!is.null(bus_all_dt) && !is.null(rapid_dt)) {
    bus_all_dt[, key := paste0(round(lon,6), "_", round(lat,6))]
    rapid_dt[,   key := paste0(round(lon,6), "_", round(lat,6))]
    bus_all_dt <- bus_all_dt[!key %in% rapid_dt$key]
    bus_all_dt[, key := NULL]
    rapid_dt[,   key := NULL]
  }
  
  bus_dt <- dedup_dest(bus_all_dt)
  message("Bus stops (excluding RapidBus): ", ifelse(is.null(bus_dt), 0, nrow(bus_dt)))
} else {
  warning("routes.txt has no route_type; cannot robustly build Bus stops from GTFS.")
}

## -------------------- 4) Combine & write --------------------

dest_list <- list(parks_dt, libraries_dt, bus_dt, rapid_dt, sky_dt)
dest_list <- Filter(Negate(is.null), dest_list)

if (length(dest_list) == 0) stop("No destinations were generated.")

destinations <- rbindlist(dest_list, use.names = TRUE, fill = TRUE)
destinations <- destinations[!is.na(lon) & !is.na(lat)]

message("Destination counts:")
print(destinations[, .N, by = type][order(-N)])

out_csv <- file.path(config$directories$prepared_data, "destinations_city_specific.csv")
fwrite(destinations, out_csv)
message("Saved: ", out_csv)

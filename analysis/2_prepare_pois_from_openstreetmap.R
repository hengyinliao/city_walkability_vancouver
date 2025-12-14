## #######################################################################################
##
## Download points of interest (POIs) from OpenStreetMap
##
## Created by Nathaniel Henry, nat@henryspatialanalysis.com
## Purpose: Download destinations from OpenStreetMap based on place keywords
##
## Before running (see README.md for details):
##  - Download R version 4.1 or newer
##  - Install R packages: argparse, data.table, httr, glue, yaml
##  - Run 1_prepare_blocks.R to define a bounding box for your city ("extended_bbox.yaml",
##    saved in the prepared data folder).
##  - Optional: Update the `osm_destination_queries` arguments config.yaml to define 
##    destination types and their associated OpenStreetMap tags. For more information,
##    see the README.
##
## What happens in this script:
##  1. Create a function to download download POIs from OpenStreetMap
##  2. Download POIs based on the config, format as a single table, and save
##
## #######################################################################################

## SET DEFAULT CONFIG PATH
#  Set the location of the config.yaml file that determines all settings for this script.
#  This path will only be used if you run this script interactively; if you run this
#  script from the command line, this default will be ignored.
#
#  If config arguments are set correctly, you should not need to edit this script below
#  line 30.
DEFAULT_CONFIG_FILEPATH <- "C:/Users/hengyinl.stu/Desktop/city_walkability/analysis/config.yaml"


## Setup -------------------------------------------------------------------------------->

# Load all required packages
load_packages <- c('argparse', 'data.table', 'httr', 'glue', 'yaml')
load_packages |> lapply(library, character.only = T) |> invisible()

library(osmdata)
library(data.table)

# Load config.yaml
if(interactive()){
  config_filepath <- DEFAULT_CONFIG_FILEPATH
  message("Using default config location: ", config_filepath)
} else {
  parser <- argparse::ArgumentParser()
  parser$add_argument(
    "--config_filepath",
    type = 'character',
    help = "Full path to the config.yaml file containing run settings"
  )
  config_filepath <- parser$parse_args(commandArgs(trailingOnly = TRUE))$config_filepath
}
config <- yaml::read_yaml(config_filepath)
settings <- config$project_settings

# Create all folders
for(dir in config$directories) dir.create(dir, recursive = TRUE, showWarnings = FALSE)


## 1. Create a function to download download POIs from OpenStreetMap -------------------->

API_ENDPOINT <- config$download_paths$overpass_api_endpoint

# Load bounding box saved in 1_prepare_blocks.R and convert to string
bb <- yaml::read_yaml(
  file.path(config$directories$prepared_data, 'extended_bbox.yaml')
)
BBOX_TEXT <- glue::glue("[bbox:{bb$ymin},{bb$xmin},{bb$ymax},{bb$xmax}];")

# Helper function to query Overpass for nodes, ways, and relations matching a tag or set
#  of tags and return JSON results
query_overpass <- function(tags){
  # Build the query
  format_tags <- function(tag) glue::glue("node[{tag}]; way[{tag}]; relation[{tag}];")
  tag_text <- lapply(tags, format_tags) |> unlist() |> paste(collapse = ' ')
  address_tags <- c('housenumber', 'street', 'city', 'postcode')
  address_text <- paste0('"addr:', address_tags, '"') |> paste(collapse = ', ')
  full_query_text <- glue::glue(
    '[out:csv(::lat, ::lon, name, {address_text}; true; ",")]',
    '{BBOX_TEXT}({tag_text}); out center;'
  )
  # Submit the query to the Overpass API
  query_results <- httr::POST(API_ENDPOINT, body=full_query_text)
  # If the query fails, send an informative error
  httr::stop_for_status(query_results)
  # Otherwise, return the query JSON
  return(httr::content(query_results, as = 'text', encoding = 'UTF-8'))
}


##  2. Download POIs based on the config, format as a single table, and save ------------>

destination_types <- names(config$osm_destination_queries)

### Change 1 - fix outdated query_overpass()
### helper (osm tag normalizer)
normalize_osm_tags <- function(x, d_type = NA_character_) {
  # Already a named list? Just return it
  if (is.list(x) && !is.null(names(x))) {
    return(x)
  }
  
  # Character vector like "shop=supermarket" or c("shop=supermarket", "shop=superstore")
  if (is.character(x)) {
    parts <- strsplit(x, "=", fixed = TRUE)
    keys  <- vapply(parts, `[`, character(1), 1)
    vals  <- vapply(parts, `[`, character(1), 2)
    
    # Build a named list where each key can have multiple values
    out <- split(vals, keys)
    return(out)
  }
  
  stop(
    "OSM tags for ", d_type, " could not be interpreted.\n",
    "Got type: ", paste(class(x), collapse = ", "),
    ". Expected either a named list or character vector like 'key=value'."
  )
}

destinations_table <- lapply(destination_types, function(d_type) {
  
  message("Querying Overpass for ", d_type, "...")
  
  # Raw tags from config (could be list or character)
  raw_osm_tags <- config$osm_destination_queries[[d_type]]
  
  if (is.null(raw_osm_tags)) {
    stop("No OSM tags found in config.yaml for destination type: ", d_type)
  }
  
  # Normalize to a named list: list(key = c("value1", "value2", ...))
  osm_tags <- normalize_osm_tags(raw_osm_tags, d_type = d_type)
  
  # Load bbox
  bb_list <- yaml::read_yaml(file.path(config$directories$prepared_data, "extended_bbox.yaml"))
  bb <- c(bb_list$xmin, bb_list$ymin, bb_list$xmax, bb_list$ymax)
  
  # Build query
  q <- opq(bb)
  
  # Add each key/value pair
  for (key in names(osm_tags)) {
    values <- osm_tags[[key]]
    for (value in values) {
      q <- add_osm_feature(q, key = key, value = value)
    }
  }
  
  # Run Overpass
  res <- osmdata_sf(q)
  pts <- res$osm_points
  
  if (is.null(pts) || nrow(pts) == 0) {
    warning("No results for ", d_type)
    return(NULL)
  }
  
  formatted <- data.table(
    name = pts$name,
    lon = st_coordinates(pts)[, 1],
    lat = st_coordinates(pts)[, 2],
    address = pts$`addr:street`,
    type = d_type
  )
  
  return(formatted)
}) |> data.table::rbindlist(fill = TRUE)




# Summarize results
message("Summary of results:")
knitr::kable(destinations_table[, .(Count = .N), by = .(`Destination type` = type)])

# Save table
data.table::fwrite(
  destinations_table,
  file = file.path(config$directories$prepared_data, 'destinations_osm.csv')
)

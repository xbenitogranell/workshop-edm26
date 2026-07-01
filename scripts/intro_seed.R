## SeeD workshop @ 16th European Diatom Meeting, Murcia, Spain
## July 6th, 2026
## Xavier Benito
## contact: xavier.benito@irta.cat
## https://xbenitogranell.github.io/ 

#------------------------------------------------------------------------------------------------------------------
# This document contains a simple workflow to query and extract data from the Iberoamerican Diatom Database (SeeD)
#------------------------------------------------------------------------------------------------------------------

# Clean workspace first
rm(list = ls())

#install.packages("pacman") #install the library
pacman::p_load(readxl, DBI, duckdb, dplyr, tibble, sf, mapview, geodata, terra)

# source the necessary functions to work with the db
source('scripts/seed_tools.R')

# name the output db and load it into your R environment
# the resulting "db" object does not contain the data. It is indeed a route to the database stored in the internal memory
SEED_XLSX <- "data/seed-database.xlsx"
db <- load_seed_database(SEED_XLSX)
db

# 
seed_meta <- get_seed_metadata(db)
head(seed_meta)

# create an spatial object
seed_meta_for_spatial <- seed_meta %>%
  filter(!is.na(latitude))

seed_map  <- sf::st_as_sf(seed_meta_for_spatial, 
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84")

# quick view of spatial distribution of sites
mapview(seed_map)




my_data <- get_data_by_metadata(db,country=c('Spain','Ecuador'), habitat=c("cryoconite_hole","salicornia"), exact=FALSE, include_envir=TRUE)

my_data_merged <- merge_records(my_data)
export_seed_to_xlsx(my_data_merged,"my_data_merged_exported.xlsx")

my_data_merged_harm <- rename_diatom_columns(
  my_data_merged,
  taxonomy_col = "accepted_name")
export_seed_to_xlsx(my_data_merged_harm,"my_data_merged_harm.xlsx")

# diatom-based selection
navicula <- get_data_by_taxa(
  db,
  accepted_name = "Navicula spp.",
  include_zeros = FALSE,
  include_envir = TRUE,
  exact = FALSE
)

## 

## another example

records_with_taxonomic_uncertainties <- merge_records(get_data_by_taxa(db, accepted_name=c(" aff. ", " cf. ", ' spp. '), exact=FALSE, include_envir=FALSE))

# ---- Prepare site coordinates for mapping ----
# Distinct sites only, so repeated records do not overplot unnecessarily


site_points <- records_with_taxonomic_uncertainties$metadata %>% left_join(records_with_taxonomic_uncertainties$diatoms, by=c('site_id', 'record_id')) %>% group_by(site_id, latitude, longitude) %>%
  summarise(n_taxa=n()) %>% arrange(desc(n_taxa))


# Optio
library(ggplot2)

world_map <- ggplot2::map_data("world")


p <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "grey92",
    colour = "grey70",
    linewidth = 0.2
  ) +
  geom_point(
    data = site_points,
    aes(x = longitude, y = latitude, size=n_taxa),
    #size = n_sites,
    colour = "#007C91",
    alpha = 0.9
  ) +
  coord_quickmap(
    xlim = c(-120, -50),
    ylim = c(-56, 10)
  ) +
  labs(
    title = "Argentinian SEeD sites east of 70°W",
    subtitle = paste("Unique sites:", nrow(site_points)),
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 12)

print(p)


## see Gavin's analysis of GAM of diatom taxa against environmentla gradient

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
pacman::p_load(readxl, DBI, duckdb, dplyr, tibble, sf, mapview, geodata, terra, egg)

# source the necessary functions to work with the db
source('scripts/seed_tools.R')

# name the output db and load it into your R environment
# the resulting "db" object does not contain the data. It is indeed a route to the database stored in the internal memory
SEED_XLSX <- "input/seed-database.xlsx"
db <- load_seed_database(SEED_XLSX)
db

# always use that "db" first to extract the data in each function.
# For instance:
south_america <- get_data_by_coordinates(
  db, #db element
  longitude_min = -85, #specific arguments of the function
  longitude_max = -30, #specific arguments of the function
  latitude_min = -25, #specific arguments of the function
  latitude_max = 15 #specific arguments of the function
)

# Let´s inspect the resulting output
names(south_america)

# All SeeD functions return the same type of output: an R list
south_america$metadata #information about the sites and records
south_america$diatoms[2] #this return the second record in the list containing the actual diatom data (abundance or P/A)
south_america$taxonomy # diatom taxon names with out "proposed" harmonization

#-------------------------------------------------------------------------------
# Metadata-based selection
#-------------------------------------------------------------------------------

# This function extract all the metadata from the sites included in the db
seed_meta <- get_seed_metadata(db)
head(seed_meta)

# We can create an spatial object taking the geographical coordinates
seed_meta_for_spatial <- seed_meta %>%
  filter(!is.na(latitude)) #first we filter out for sites without coordinates

# We create an
seed_map  <- sf::st_as_sf(seed_meta_for_spatial, 
                        coords = c("longitude", "latitude"),
                        crs = "+proj=longlat +datum=WGS84")

# quick view of spatial distribution of sites
mapview(seed_map)

# The main data extraction function: get_data_by_metadata()
my_data <- get_data_by_metadata(db,country=c('Spain','Ecuador'), 
                                habitat=c("cryoconite_hole","salicornia"), 
                                exact=FALSE, #character matching allows for some flexibility (fuzzy matching)
                                include_envir=TRUE) #if environmental variables are present, merge data to the diatom table

# inspect the output
length(my_data$diatoms) #this tell us that there are 63 diatom records: 63 tables, one for each record_id.

# Another interesting function is merge_records() which aggregates all the diatom tables into one 
my_data_merged <- merge_records(my_data)
length(my_data_merged$diatoms) #compare the length of unmerged and merged diatom records

print(my_data_merged)

#-------------------------------------------------------------------------------
# Export the queried database to Excel
#-------------------------------------------------------------------------------

# If you are happy with your data query, you can export the output list into Excel
export_seed_to_xlsx(my_data_merged,
                    path = "outputs/my_data_merged_exported.xlsx")

#-------------------------------------------------------------------------------
# Rename diatom taxa using the harmonization table
#-------------------------------------------------------------------------------
# The SEED database stores taxa using the original names provided in the source datasets. 
# These names are preserved in the taxonomy table to ensure traceability and to avoid altering the original published data.
# However, users may wish to work with an standardized taxonomy. 
# The function rename_diatom_columns() replaces the column names in the diatoms object using one of the desired taxonomy columns from the taxonomy table.

my_data_merged_harm <- rename_diatom_columns(
  my_data_merged,
  taxonomy_col = "accepted_name")

# if multiple original taxa map the same new, harmonized name, their columns (i.e., abundances) are summed

# then we can compare if indeed different diatom taxa have been grouped by comparing the diatom names from the two lists 
length(names(my_data_merged$diatoms))
length(names(my_data_merged_harm$diatoms))

setdiff(names(my_data_merged$diatoms), names(my_data_merged_harm$diatoms)) 

# sometimes we are interested in performing genus-level analyses. The function rename_diatom_columns() can help:
my_data_merged_genus_harm <- rename_diatom_columns(
  my_data_merged,
  taxonomy_col = "accepted_genus") #here is where we specify to map original names to accepted genus

#-------------------------------------------------------------------------------
# Diatom-based selection
#-------------------------------------------------------------------------------

# One may want to select diatom records based on the presence of certain taxa
# Example: get all the records where "Navicula salinicola" is found
nav_sal <- get_data_by_taxa(
  db,
  accepted_name = "Navicula salinicola",
  include_zeros = FALSE, #samples where the selected taxa have zeros are removed. If TRUE, samples without the taxa are kept. 
  include_envir = TRUE,
  exact = FALSE #allows for fuzzy matching
)

# merge records where Navicula salinicola is found
nav_sal_merged <- merge_records(nav_sal)

# calculate the frequency of samples per site where the taxa is found
nav_sal_sites <- nav_sal_merged$metadata %>% 
  left_join(nav_sal_merged$diatoms, by=c('site_id', 'record_id')) %>% 
  group_by(site_id, latitude, longitude) %>%
  summarise(freq=n()) %>% 
  arrange(desc(freq))

# transform the table into a spatial object
nav_sal_sites_plt  <- sf::st_as_sf(nav_sal_sites, 
                          coords = c("longitude", "latitude"),
                          crs = "+proj=longlat +datum=WGS84")

# quickly plot the distribution
mapview(nav_sal_sites_plt)

## A very similar useful function is get_distribution(): instead of returning the usual SeeD list (metadata, diatoms, taxonomy), it returns a much simpler tibble with only two columns: latitude and longitude
# Each row corresponds to one unique pair of coordinates where the selected taxon was found.
# Run the function for looking where the same species (Navicula salinicola) is found in the db
nav_sal_distr <- get_distribution(
  db,
  original_name = "Navicula salinicola",
  exact = FALSE,
  include_paleo = TRUE
)

# now create an static map using the ggplot2() package
world_map <- ggplot2::map_data("world")
ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "grey92",
    colour = "grey70",
    linewidth = 0.2
  ) +
  geom_point(
    data = nav_sal_distr,
    aes(x = long, y = lat),
    colour = "#007C91",
    alpha = 0.9
  ) +
  theme_article()

## another example
# Here we extract all diatom records where there are taxonomic uncertainties 
records_with_taxonomic_uncertainties <- merge_records(get_data_by_taxa(db, accepted_name=c(" aff. ", " cf. ", ' spp. '), exact=FALSE, include_envir=FALSE))

# Distinct sites only, so repeated records do not overlay when mapping the taxa
record_sites <- records_with_taxonomic_uncertainties$metadata %>% 
  left_join(records_with_taxonomic_uncertainties$diatoms, by=c('site_id', 'record_id')) %>% 
  group_by(site_id, latitude, longitude) %>%
  summarise(n_taxa=n()) %>% 
  arrange(desc(n_taxa)) %>%
  filter(!is.na(latitude))

# transform the table into a spatial object
uncert_sites_plt  <- sf::st_as_sf(record_sites, 
                                   coords = c("longitude", "latitude"),
                                   crs = "+proj=longlat +datum=WGS84")

# quickly plot the distribution
mapview(uncert_sites_plt)


#-------------------------------------------------------------------------------
# Fit a regression to model the diatom response to a environmental gradient
#-------------------------------------------------------------------------------

# Extract diatom records where the accepted genera Eunotia is found
eun <- get_data_by_taxa(
  db,
  accepted_genus = "Eunotia",
  include_zeros = TRUE, #samples where the selected taxa have zeros are removed. If TRUE, samples without the taxa are kept. 
  include_envir = TRUE,
  exact = FALSE 
)

# Merge records. We create a different output list
eun_merged <- merge_records(eun)

# Harmonize counts
eun_genus_harm <- rename_diatom_columns(
  eun_merged,
  taxonomy_col = "accepted_genus") #here is where we specify to map original names to accepted genus

# Inspect the data_type where the taxon is found:
unique(eun_merged$metadata$data_type)

# We want to model counts so we filter data_type by counts and then join with the diatom samples table
eunotia_pH <- eun_genus_harm$metadata %>%
  dplyr::filter(data_type=="count") %>%
  left_join(eun_genus_harm$diatoms, by=c('site_id', 'record_id')) %>%
  dplyr::select(site_id,site_name,country,substrate,habitat,'pH','Eunotia') %>%
  dplyr::filter(!is.na(pH))

unique(eunotia_pH$country)

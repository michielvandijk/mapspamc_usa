#'========================================================================================
#' Project:  mapspamc
#' Subject:  Prepare adm map and grid
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#' 
#' Updated: Carlos baez 09/2023
#'========================================================================================

# SOURCE PARAMETERS ----------------------------------------------------------------------
#setwd("D:/GLOBIOM/")
source(here::here("01_model_setup/01_model_setup.r"))


# LOAD DATA ------------------------------------------------------------------------------
# replace the name of the shapefile with that of your own country.
iso3c_shp <- "USA_2000.shp"

# load shapefile
adm_map_raw <- read_sf(file.path(param$db_path, glue("adm/{param$iso3c}/{iso3c_shp}")))

# plot
plot(adm_map_raw$geometry)


# PROCESS --------------------------------------------------------------------------------
# Project to standard global projection
adm_map <- adm_map_raw %>%
  st_transform(param$crs)

# Check names
head(adm_map)
names(adm_map)

# In order to use the country polygon as input, the column names of the attribute table need to have
# the right names referring to the different adms, which correspond to the names in the crop statistics.
# The names of the administrative units should be set to admX_name, where X is the adm level.
# The codes of the administrative units should be set to admX_code, where X is the adm code.

# If the attribute table already contains all adm names and codes but with incorrect header names,
# set the original names, i.e. the ones that will be replaced, below.
# Add adm0_code and adm0_name of these are not not part of attribute table
# e.g. %>% mutate(adm0_name  = "COUNTRY.NAME)

#[Modified Code]
adm0_name_orig <- "NAME_CNTRY"
adm0_code_orig <- "CODE_CNTRY"
adm1_name_orig <- "NAME_STATE"
adm1_code_orig <- "CODE_STATE"
adm2_name_orig <- "NAME_COUNT"
adm2_code_orig <- "CODE_COUNT"

# Replace the names
names(adm_map)[names(adm_map) == adm0_name_orig] <- "adm0_name"
names(adm_map)[names(adm_map) == adm0_code_orig] <- "adm0_code"
names(adm_map)[names(adm_map) == adm1_name_orig] <- "adm1_name"
names(adm_map)[names(adm_map) == adm1_code_orig] <- "adm1_code"
names(adm_map)[names(adm_map) == adm2_name_orig] <- "adm2_name"
names(adm_map)[names(adm_map) == adm2_code_orig] <- "adm2_code"

#Update Codes for US [New Code]
state <- bind_cols(state.name, state.abb)
names(state) <- c("name", "abv")

adm_map <-   
  adm_map %>% 
  mutate(adm0_code = "USA") %>%
  left_join(state, by  = join_by(adm1_name == name)) %>%
  mutate(adm1_code = paste0("US", abv),
         adm2_code = paste0("US", abv,adm2_code)) %>%
  select(-abv)
  

# Only select relevant columns
adm_map <- adm_map %>%
  dplyr::select(adm0_name, adm0_code, adm1_name, adm1_code, adm2_name, adm2_code)

# Union separate polygons that belong to the same adm
adm_map <- adm_map %>%
  group_by(adm0_name, adm0_code, adm1_name, adm1_code, adm2_name, adm2_code) %>%
  summarize(geometry = st_union(geometry),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(adm0_name = param$country,
         adm0_code = param$iso3c) %>%
  mutate(adm0_name = toupper(adm0_name),
         adm0_code = toupper(adm0_code),
         adm1_name = toupper(adm1_name),
         adm1_code = toupper(adm1_code),
         adm2_name = toupper(adm2_name),
         adm2_code = toupper(adm2_code)
  )

# Check names
head(adm_map)
names(adm_map)

par(mfrow=c(1,2))
plot(adm_map$geometry, main = "ADM all polygons")

# Set names of ADMs that need to be removed from the polygon.
# These are ADMs where no crops should be allocated. Here we remove
# Area under National Administration, which is the part of Lake Malawi that belongs to Malawi
# and Likoma, several small islands in the lake that are not covered by the statistics.
# Set the adm_name by ADM level which need to be removed. Otherwise remove the script.
 adm1_to_remove <- c("RHODE ISLAND", "ALASKA")
 adm2_to_remove <- c("")

# Remove ADMs
adm_map <- adm_map %>%
  filter(!adm1_name %in% adm1_to_remove) %>%
  filter(!adm2_name %in% adm2_to_remove)

plot(adm_map$geometry, main = "ADM polygons removed")
par(mfrow=c(1,1))

# Create adm_list
create_adm_list(adm_map, param)


# SAVE -----------------------------------------------------------------------------------
temp_path <- file.path(param$model_path, glue("processed_data/maps/adm/{param$res}"))
dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)

saveRDS(adm_map, file.path(temp_path, glue("adm_map_{param$year}_{param$iso3c}.rds")))
write_sf(adm_map, file.path(temp_path, glue("adm_map_{param$year}_{param$iso3c}.shp")))


# CREATE PDF -----------------------------------------------------------------------------
# Create pdf with the location of administrative units
create_adm_map_pdf(param)


# CREATE GRID ----------------------------------------------------------------------------
create_grid(param)


# RASTERIZE ADM_MAP ----------------------------------------------------------------------
rasterize_adm_map(param)


# CLEAN UP -------------------------------------------------------------------------------
rm(adm_map, adm_map_raw, iso3c_shp, temp_path)
rm(list = ls()[grep("code_orig", ls())])
rm(list = ls()[grep("name_orig", ls())])
rm(list = ls()[grep("to_remove", ls())])

#'========================================================================================
#' Project:  mapspamc
#' Subject:  Script to analyze results
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================

# SOURCE PARAMETERS ----------------------------------------------------------------------
source(here::here("01_model_setup/01_model_setup.r"))

load_data("adm_list", param)

ac <- "MWI"
# COMPARE ADM ----------------------------------------------------------------------------
prepare_results_adm_level <- function(ac, param) {

  cat("\n=> Extract results for", ac)
  load_data(c("grid", "adm_map_r"), param, mess = F)

  model_folder <- mapspamc:::create_model_folder(param)
  results_file <- file.path(param$model_path,
                            glue::glue("processed_data/intermediate_output/{model_folder}/{ac}/spamc_{param$model}_{param$res}_{param$year}_{ac}_{param$iso3c}"))

  grid_df <- as.data.frame(grid, xy = TRUE)
  df <- gdxrrw::rgdx.param(results_file, "palloc", names = c("gridID", "crop_system", "pa"),  compress = T) %>%
    dplyr::mutate(gridID = as.numeric(as.character(gridID)),
                  crop_system = as.character(crop_system)) %>%
    tidyr::separate(crop_system, into = c("crop", "system"), sep = "_", remove = T) %>%
    dplyr::left_join(adm_map_r, by = "gridID") %>%
    dplyr::left_join(grid_df, by = "gridID")

  return(df)
}

df_x <- df %>%
  group_by(adm2_name) %>%
  summarize(pa = sum(pa, na.rm = T))

### LOAD DATA
## GAMS OUTPUT
# Set SPAM GDX file
model_sel <- "min_slack_ent"
model_setup <- "standard"
spam_file <- file.path(spam_path, paste0("output/results_", model_sel, "_", grid_sel, "_", year_sel, "_", iso3c_sel, ".gdx"))
message(basename(spam_file))

# ir slack
ir_slack_raw <- rgdx.param(spam_file, "ir_slack_fix", names = c("gridID", "ir_slack"),  compress = T) %>%
  mutate(gridID = as.numeric(as.character(gridID)))

# alloc
alloc <- rgdx.param(spam_file, "palloc", names = c("gridID", "crop_system", "alloc"),  compress = T) %>%
  mutate(gridID = as.numeric(as.character(gridID))) %>%
  separate(crop_system, into = c("crop", "system"), sep = "_", remove = F)

# det alloc
det_alloc <- rgdx.param(spam_file, "det_alloc_check", names = c("gridID", "crop_system", "alloc"),  compress = T) %>%
  mutate(gridID = as.numeric(as.character(gridID)))


## GAMS INPUT
# adm
adm <- readRDS(file.path(proc_path, paste0("maps/adm/adm_", year_sel, "_", iso3c_sel, ".rds")))

# adm_r
adm_r <- readRDS(file.path(proc_path, paste0("maps/adm/adm_r_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))

# grid
grid <- raster(file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# lu_adm
lu_adm_raw <- readRDS(file.path(proc_path, paste0("harmonized/lu_adm_harm_", year_sel, "_", iso3c_sel, ".rds")))

# Ir
lu_ir_raw <- readRDS(file.path(proc_path, paste0("harmonized/lu_ir_harm_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))

# Lc
lc <- readRDS(file.path(proc_path, paste0("harmonized/lc_harm_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))

# Priors
priors <- readRDS(file.path(proc_path, paste0("harmonized/priors_harm_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))

# lu_det
lu_det <- readRDS(file.path(proc_path, paste0("harmonized/lu_det_grid_harm_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds"))) %>%
  rename(det = area)

# lu_det_all
lu_det_all <- readRDS(file.path(proc_path, paste0("harmonized/lu_det_all_grid_harm_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds"))) %>%
  rename(det_all = value)

# sy
lu_sy_raw <- readRDS(file.path(proc_path, paste0("harmonized/lu_sy_harm_", year_sel, "_", iso3c_sel, ".rds"))) %>%
  rename(stat = value)

# field size threshold
field_size <- readRDS(file.path(iso3c_path, paste0("parameters/field_size_threshold_", year_sel, "_", iso3c_sel, ".rds"))) %>%
  separate(crop_system, into = c("crop", "system"), remove = F)


### FUNCTIONS
# Plus function that ensures NA + NA is NA not 0 as in sum. If na.rm = F (default), NA + 0 = NA, otherwise 0, similar to sum
plus <- function(x, na.rm = F){
  if(all(is.na(x))){
    c(x[0],NA)
  } else {
    if(na.rm == T){
      sum(x, na.rm = TRUE)
    } else {
      sum(x, na.rm)
    }
  }
}

# Function to show histogram of grid allocation per crop and system
crop_hist_f <- function(crp, df = db){
  p <- df %>%
    filter(alloc >0) %>%
    filter(crop == crp) %>%
    ggplot() +
    geom_histogram(aes(x = alloc, fill = crp)) +
    geom_vline(data = filter(field_size, crop == crp), aes(xintercept = min_size)) +
    facet_wrap(~crop_system, scales = "free") +
    guides(fill = F)
  p
}

# Function to compare crop maps per system in a panel
view_4p_f <- function(crp, vr, sy = c("S", "L", "H", "I"), df = db){
  ext <- extent(grid)
  df <- df %>%
    filter(crop == crp, system %in% sy) %>%
    left_join(grid_df)
  sys <- unique(df$system)
  st <- lapply(sys, function(x) rasterFromXYZ(df[df$system == x, c("x", "y", vr)], crs = crs(grid)))
  st <- lapply(st, function(x) raster::extend(x, ext)) # Harmonize exent for stacking
  st <- lapply(seq(length(st)), function(i){
    mapview(st[[i]], layer.name = paste(crp, sys[i], sep = "_"))
  })
  sync(st)
}

# Function to compare crop maps in stack
view_st_f <- function(crp, vr, sy = c("S", "L", "H", "I"), df = db){
  ext <- extent(grid)
  df <- df %>%
    filter(crop == crp, system %in% sy) %>%
    left_join(grid_df)
  sys <- unique(df$system)
  st <- lapply(sys, function(x) rasterFromXYZ(df[df$system == x, c("x", "y", vr)], crs = crs(grid)))
  st <- lapply(st, function(x) raster::extend(x, ext)) # Harmonize exent for stacking
  if(length(sys) >1){
    st <- stack(st)
  }else{
    st <- st[[1]]
  }
  names(st) <- paste(crp, sys, vr, sep = "_")
  st[st==0] <- NA
  mapview(st, use.layer.names = T)
}

source(file.path(root, "code/general/log_gams.r"))

### LOG GAMS RUN INFO
log_gams_f(model_sel = model_sel, iso3c_sel = iso3c_sel, grid_sel = grid_sel,
           year_sel = year_sel, adm_sel = adm_sel, model_setup = model_setup)


### PREPARE
# Add grid cell coordinates
grid_df <- as.data.frame(rasterToPoints(grid))

# Combine all relevant variables
db <- full_join(priors, alloc) %>%
  left_join(lu_ir_raw) %>%
  left_join(lu_det %>%
              dplyr::select(gridID, crop, category, det, name, system)) %>%
  left_join(adm_r)
summary(db)

db <- mutate(db, alloc = round(alloc, 2))


### CHECKS
# Check if is.na(alloc) and !is.na(det)
check_det <- filter(db, is.na(alloc) & !is.na(det))
nrow(check_det)==0

# Check if alloc < det
check_det2 <- filter(db, abs(alloc -det) > 0.0001)
nrow(check_det2)==0

# Compare total adm with total allocation
adm0_check <- lu_adm_raw %>%
  filter(adm_level == 0) %>%
  dplyr::select(crop, fips, adm, stat = value) %>%
  left_join(
    db %>%
      group_by(crop) %>%
      summarize(alloc = sum(alloc, na.rm = T))) %>%
  filter()
all.equal(adm0_check$stat, adm0_check$alloc)

adm1_check <- lu_adm_raw %>%
  filter(adm_level == 1) %>%
  dplyr::select(crop, fips1 = fips, adm1 = adm, stat = value) %>%
  left_join(
    db %>%
      group_by(crop, adm1, fips1) %>%
      summarize(alloc = sum(alloc, na.rm = T))) %>%
  drop_na()
all.equal(adm1_check$stat, adm1_check$alloc)

adm2_check <- lu_adm_raw %>%
  filter(adm_level == 2) %>%
  dplyr::select(crop, fips2 = fips, adm2 = adm, stat = value) %>%
  left_join(
    db %>%
      group_by(crop, adm2, fips2) %>%
      summarize(alloc = sum(alloc, na.rm = T))) %>%
  drop_na()
all.equal(adm2_check$stat, adm2_check$alloc)


priors %>%
  filter(crop == "acof") %>%
  group_by(system) %>%
  summarize(prior = sum(prior, na.rm = T))

alloc %>%
  filter(crop == "acof") %>%
  group_by(system) %>%
  summarize(alloc = sum(alloc, na.rm = T))

lu_sy %>%
  filter(crop == "acof")


### VISUALIZE
# histogram
crop_hist_f("rice")

db2 <- filter(db, alloc > 0.001)

# Crop allocation
view_4p_f("vege", "alloc", df = db2)

# Irrigated stat
lu_sy <- lu_sy_raw %>%
  filter(adm_level == 0) %>%
  spread(system, stat) %>%
  filter(I >0) %>%
  arrange(desc(I))

view_st_f("rice", "alloc", "S")
view_st_f("rice", "prior_area", "S") +
  mapview(adm, alpha.regions = 0)

# Across systems
by_system <- db %>%
  group_by(gridID, system) %>%
  summarize(alloc = plus(alloc, na.rm = T)) %>%
  ungroup() %>%
  mutate(crop = "total")

view_4p_f("total", "alloc", df = by_system)


### COMPARE ADM2 ALLOCATION
adm2_alloc <- db %>%
  group_by(adm1, fips1, adm2, fips2, crop, system, crop_system) %>%
  summarize(alloc = sum(alloc, na.rm = T),
            det = sum(det, na.rm = T),
            ir_area = sum(ir_area, na.rm = T)
  ) %>%
  filter(alloc >0)

### SHOW WHICH crop-systems are allocated to fl
fl_alloc <- db %>%
  filter(!is.na(det_all)) %>%
  group_by(crop, system) %>%
  summarize(alloc = sum(alloc, na.rm = T),
            det = sum(det, na.rm = T),
            prior_area = sum(prior_area, na.rm = T))


### CHECK WHERE IR_SLACK_FIX WAS APPLIED (IF ANY)
# Prepare slack data
ir_slack <- db %>%
  filter(!is.na(ir_slack), system == "I", !is.na(alloc))
sum(ir_slack$alloc)

ir_slack_r <- rasterFromXYZ(left_join(grid_df, ir_slack) %>% dplyr::select(x, y, ir_slack))
crs(ir_slack_r) <- crs(adm)
mapview(ir_slack_r, maxpixels = 1655090, col.regions = "green") +
  mapview(adm, alpha.regions = 0)

# Prepare ir map
lu_ir <- left_join(lu_ir_raw, grid_df) %>%
  dplyr::select(x, y, value = ir_area)
lu_ir_r <- rasterFromXYZ(lu_ir)
crs(lu_ir_r) <- crs(adm)

mapview(lu_ir_r) +
  mapview(ir_slack_r, maxpixels = 1655090, col.regions = "green") +
  mapview(adm, alpha.regions = 0)


### CHECK DET
# Compare lu_det and det_alloc
det_alloc <- db %>%
  filter(!is.na(det)) %>%
  mutate(check_det_alloc = det - alloc)

# Check if there are other crops allocated to det gridID
other_in_det <- db %>%
  filter(gridID %in% det_alloc$gridID, !is.na(alloc))
sum(other_in_det$alloc, na.rm = T)
sum(other_in_det$det, na.rm = T)


### COMPARE Proirs with alloc
# Compare prior area total with allocated area total
# CHECK WHY WHEA H and I are 8 different?
check_alloc_tot <- db %>%
  group_by(crop_system) %>%
  summarize(tot_prior_area = sum(prior_area, na.rm = T),
            tot_alloc  = sum(alloc, na.rm = T)) %>%
  mutate(dif = tot_prior_area-tot_alloc)


### SAVE
temp_path <- file.path(proc_path, "spam_2.0")
dir.create(temp_path, showWarnings = F, recursive = T)

# It is useful to know where zero crops have been allocated, which are marked as NA. Replace by zero
spam_final <- db %>%
  dplyr::select(gridID, crop, system, crop_system, alloc)  %>%
  mutate(alloc = ifelse(is.na(alloc), 0, alloc),
         year = year_sel,
         res = grid_sel,
         iso3c = iso3c_sel)
summary(spam_final)

saveRDS(spam_final, file.path(temp_path, paste0("spam_final_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))



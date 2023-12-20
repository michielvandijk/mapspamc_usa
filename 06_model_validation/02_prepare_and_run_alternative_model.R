#'========================================================================================
#' Project:  mapspamc
#' Subject:  Script to run validation model
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================

# Conneticut (USCT) is very small and therefore only covers a single cropland value. 
# This creates an error as terra cannot create a tif file from a single grid cell.
# Probably we can run the validation without this state but that would require tweaking the code.

# SOURCE PARAMETERS ----------------------------------------------------------------------
source(here::here("06_model_validation/01_alternative_model_setup.r"))


# PREPARE PHYSICAL AREA ------------------------------------------------------------------
prepare_physical_area(alt_param)


# CREATE SYNERGY CROPLAND INPUT ----------------------------------------------------------
prepare_cropland(alt_param)


# PROCESS --------------------------------------------------------------------------------
prepare_irrigated_area(alt_param)


# HARMONIZE INPUT DATA -------------------------------------------------------------------
harmonize_inputs(alt_param)


# PREPARE SCORE --------------------------------------------------------------------------
prepare_priors_and_scores(alt_param)


# COMBINE MODEL INPUTS -------------------------------------------------------------------
combine_inputs(alt_param)


# RUN MODEL -----------------------------------------------------------------------------
if(alt_param$model == "min_entropy"){
  #run_mapspamc(alt_param, solver = "IPOPT")
  run_mapspamc(alt_param, solver = "CONOPT4")
} else {
  run_mapspamc(alt_param, solver = "CPLEX")
}
toc()


# COMBINE ADM1 RESULTS ------------------------------------------------------------------
combine_results(alt_param)




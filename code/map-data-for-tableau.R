########################################
#### EXPORTING MAP DATA FOR TABLEAU ####
########################################

## ---- load_packages ----
if(!require("pacman")) {install.packages("pacman")}
pacman::p_load(here, readr)

## ---- load_data ----
# load scotch analysis data
source(here("analysis", "scotch-analysis-part3.R"))

## ---- write_data ----
write_csv(distillery_map, here("data", "distillery-tableau-map.csv"))

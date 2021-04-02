#########################################
#### EXPLORING WHISKY: SCOTCH PART 5 ####
#########################################

## ---- load_packages ----

## ---- load_data ----
# add the data
# source(here("code", "get-scotch-flavors.R"))

## ----join_data ----
#scotch_flavor_notes <- scotch %>% 
#  fuzzy_left_join(scotch_flavors,
#                  match_fun = ignore_case_string,
#                  by = c(whisky = "distillery_name"))
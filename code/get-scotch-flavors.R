#####################################
#### GETTING SCOTCH FLAVOR NOTES ####
#####################################

if(!require("pacman")) {install.packages("pacman")}
pacman::p_load(here, tidyverse)

#####################################
#### READING + CLEANING THE DATA ####
#####################################

scotch_flavors <- read_csv("https://outreach.mathstat.strath.ac.uk/outreach/nessie/datasets/whiskies.txt") %>% 
  janitor::clean_names() %>% 
  rename(distillery_name = distillery) %>% 
  
  # delete indie distillery with confusing join implications given Speyside region
  filter(distillery_name != "Speyside") %>%
  
  # correct several of spelling inconsistencies
  mutate(distillery_name = case_when(
    distillery_name == "ArranIsleOf" ~ "Arran",
    distillery_name == "Belvenie" ~ "Balvenie",
    distillery_name == "BenNevis" ~ "Ben Nevis",
    distillery_name == "BlairAthol" ~ "Blair Athol",
    distillery_name == "Craigallechie" ~ "Craigellachie",
    distillery_name == "Craigganmore" ~ "Cragganmore",
    distillery_name == "GlenDeveronMacduff" ~ "Macduff", # this regex should be Macduff|Deveron if that's possible
    distillery_name == "GlenElgin" ~ "Glen Elgin",
    distillery_name == "GlenGarioch" ~ "Glen Garioch",
    distillery_name == "GlenGrant" ~ "Glen Grant",
    distillery_name == "GlenKeith" ~ "Glen Keith",
    distillery_name == "GlenMoray" ~ "Glen Moray",
    distillery_name == "GlenOrd" ~ "Glen Ord",
    distillery_name == "GlenScotia" ~ "Glen Scotia",
    distillery_name == "GlenSpey" ~ "Glen Spey",
    distillery_name == "Isle of Jura" ~ "Jura",
    distillery_name == "Knochando" ~ "Knockando",
    distillery_name == "Laphroig" ~ "Laphroaig",
    distillery_name == "OldFettercairn" ~ "Fettercairn",
    distillery_name == "OldPulteney" ~ "Pulteney",
    distillery_name == "RoyalBrackla" ~ "Royal Brackla",
    distillery_name == "RoyalLochnagar" ~ "Royal Lochnagar",
    TRUE ~ distillery_name)) %>% 
  
  # delete unnecessary columns
  select(-c(row_id, ends_with("tude")))

#########################
#### SAVING THE DATA ####
#########################

saveRDS(scotch_flavors, file = here("data", "scotch-flavors.rdata"))

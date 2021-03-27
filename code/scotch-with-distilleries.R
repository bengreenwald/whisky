#####################################################
#### JOINING SCOTCH RATINGS & DISTILLERY DETAILS ####
#####################################################

if(!require("pacman")) {install.packages("pacman")}
pacman::p_load(here, tidyverse, fuzzyjoin)

# # load scotch ratings data
scotch_ratings <- read_rds(here("data", "scotch-ratings.rdata"))

# load scotch distillery data
distilleries <- read_rds(here("data", "scotch-distilleries.rdata"))

# create match function to ignore case irregularities when joining
ignore_case_string <- function(x, y) {
  
  str_detect(x, regex(y, ignore_case = TRUE))
  
}

##########################
#### JOINING THE DATA ####
##########################

scotch_with_distillery <- scotch_ratings %>% 
  fuzzy_left_join(distilleries,
                  match_fun = ignore_case_string,
                  by = c(whisky = "regex"))
  
###########################
#### CLEANING THE DATA ####
###########################

# includes ~100 whiskies from collaborations or independent bottlers with multiple distilleries 
scotch_with_collabs <- scotch_with_distillery %>% 
  distinct() %>% 
  
  # delete a few edge cases incorrectly matched by fuzzyjoin
  filter(!(str_detect(whisky, "Speyburn Arranta") & distillery == "Arran") & 
           !(str_detect(whisky, "William Grant") & distillery == "Grant's") & 
           !(str_detect(whisky, "Ceobanach") & distillery == "Oban")) %>% 
  
  # mark duplicates with tally
  group_by(whisky, description) %>% 
  add_tally() %>% 
  ungroup() %>% 
  
  # denotes collaboration between multiple distilleries (duplicate matches)
  mutate(collab = ifelse(n >= 2, "Yes", "No")) %>% 
  select(-c(regex, n))

# remove the collaborator duplicates for non-brand related analyses
scotch <- scotch_with_collabs %>% 
  filter(!(str_detect(whisky, "Adelphi") & !str_detect(distillery, "Adelphi")) &
           !(str_detect(whisky, "Ardbeg") & !str_detect(distillery, "Ardbeg")) &
           !(str_detect(whisky, "Ballantine") & !str_detect(distillery, "Ballantine")) &
           !(str_detect(whisky, "Caol Ila") & !str_detect(distillery, "Caol Ila")) &
           !(str_detect(whisky, "Chivas") & !str_detect(distillery, "Chivas")) &
           !(str_detect(whisky, "Glenmorangie") & !str_detect(distillery, "Glenmorangie")) &
           !(str_detect(whisky, "Gordon & MacPhail") & !str_detect(distillery, "Gordon & MacPhail")) &
           !(str_detect(whisky, "Johnnie Walker") & !str_detect(distillery, "Johnnie Walker")) &
           !(str_detect(whisky, "Laphroaig") & !str_detect(distillery, "Laphroaig")) &
           !(str_detect(whisky, "Talisker") & !str_detect(distillery, "Talisker")))

#########################
#### SAVING THE DATA ####
#########################

saveRDS(scotch_with_collabs, file = here("data", "scotch-ratings-distilleries-with-collabs.rdata"))
saveRDS(scotch, file = here("data", "scotch-ratings-distilleries.rdata"))
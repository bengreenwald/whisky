#####################################################
#### SCRAPING SCOTCH DISTILLERIES FROM WIKIPEDIA ####
#####################################################

if(!require("pacman")) {install.packages("pacman")}
pacman::p_load(here, tidyverse, rvest)

######################
#### WIKI PAGE #1 ####
######################

# scotch distillery table with names, regions, and URLs
url1 <- "https://en.wikipedia.org/wiki/List_of_whisky_distilleries_in_Scotland"

# entire table of active distilleries 
distillery_table <- read_html(url1) %>% 
  html_nodes("table") %>% 
  .[[2]] %>% 
  html_table() %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  
  # remove indie distillery with confusing join implications given Speyside region
  filter(!str_detect(distillery, "Speyside")) %>%

  # Knockdhu name is outdated
  mutate(distillery = str_replace(distillery, "Knockdhu", "AnCnoc")) %>% 
  
  select(distillery, region)

######################
#### WIKI PAGE #2 ####
######################

# add more distillery names and URLs (some overlap with first page)
url2 <- "https://en.wikipedia.org/wiki/Category:Distilleries_in_Scotland"

# distillery names
distillery_list <- read_html(url2) %>% 
  html_nodes("#mw-pages div div ul li a") %>% 
  html_text() %>% 
  as_tibble() %>% 
  rename(distillery = value) %>%
  
  # clean names for compatibility
  mutate(distillery = str_replace(distillery, "Highland Distillers", "Highland Park"),
         distillery = str_replace_all(distillery, " \\(whisky\\)| [w|W]hisky", ""),
         distillery = str_replace_all(distillery, " [d|D]istillery", ""),
         region = NA) %>% 
  
  filter(!(distillery %in% distillery_table$distillery) & # exclude duplicates already found on 1st page
           !str_detect(distillery, "Old Pulteney")) %>% # already known as "Pulteney" on 1st page
  
  # remove first two lines captured in scrape
  slice(-c(1:2))

######################
#### WIKI PAGE #3 ####
######################

# add blended whisky brands
url3 <- "https://en.wikipedia.org/wiki/Category:Blended_Scotch_whisky"

# distillery names
blended_list <- read_html(url3) %>% 
  html_nodes("#mw-pages div div ul li a") %>% 
  html_text() %>% 
  as_tibble() %>% 
  rename(distillery = value) %>% 
  
  # clean names for compatibility
  mutate(distillery = str_replace_all(distillery, " \\(whisky\\)| distillery| whisky", ""),
         distillery = str_replace(distillery, "Grant's", "Glen Grant"),
         region = NA) %>%
  
  # exclude duplicates already found on 1st or 2nd pages
  filter(!distillery %in% c(distillery_table$distillery,
                            distillery_list$distillery))


##########################################################
#### COMBINING RESULTS FROM ALL THREE WIKIPEDIA PAGES ####
##########################################################

distilleries_brands <- bind_rows(distillery_table,
                                 distillery_list,
                                 blended_list) %>% 
  arrange(distillery)

##########################################################################
#### SCRAPING REGION + LATITUTE/LONGITUDE COORDINATES FROM WHISKY.COM ####
##########################################################################

distillery_scrape <- distilleries_brands %>%
  
         # create scraping URLs
  mutate(for_URL = tolower(str_replace_all(distillery, " ", "-")),
         for_URL = ifelse(for_URL == "ancnoc", "knockdhu-ancnoc", for_URL),
         scrape_URL = glue::glue("https://www.whisky.com/whisky-database/distilleries/details/{for_URL}.html"),
         
         # scrape region, latitude + longitude
         region2 = map2(scrape_URL,
                        ".region a",
                        ~ read_html(.x) %>% 
                          html_nodes(.y) %>% 
                          html_text()),
         long_lat = map2(scrape_URL,
                         "tr:nth-child(4) a",
                         ~ read_html(.x) %>% 
                           html_nodes(.y) %>% 
                           html_text() %>% 
                           str_trim())) 

#########################################################
#### CLEANING + ADDING REGEX FOR BETTER FUZZYJOINING ####
#########################################################

# regex irregularities that don't join properly with scotch whisky names
regex_messy <- c("Ballantine's",
                 "Buchanan's",
                 "Chivas Regal",
                 "Dewar's",
                 "Johnnie Walker",
                 "Macduff",
                 "The Famous Grouse",
                 "William Grant & Sons")

# cleaned, more inclusive regex that will be used for joining
regex_clean <- c("Ballantine",
                 "Buchanan",
                 "Chivas",
                 "Dewar",
                 "Walker",
                 "Macduff|Deveron",
                 "Famous Grouse",
                 "William Grant")

# final clean to consolidate regions, extract lat/long
distilleries <- distillery_scrape %>% 
  unnest(cols = c(region2, long_lat), keep_empty = TRUE) %>% 
  mutate(region2 = case_when(
                    region2 == "Islands" ~ "Island",
                    region2 == "Highlands" ~ "Highland",
                    region2 == "Lowlands" ~ "Lowland",
                    TRUE ~ region2),
         region = coalesce(region, region2),
         lat = parse_number(word(long_lat, 2, sep = fixed(" "))),
         long = parse_number(word(long_lat, 1, sep = fixed(" "))),
         scrape_URL = ifelse(is.na(lat), NA, scrape_URL),
         regex = replace(distillery,
                         distillery %in% regex_messy,
                         regex_clean)) %>% 
  select(distillery, region, lat, long, scrape_URL, regex)

#########################
#### SAVING THE DATA ####
#########################

saveRDS(distilleries, file = here("data", "scotch-distilleries.rdata"))

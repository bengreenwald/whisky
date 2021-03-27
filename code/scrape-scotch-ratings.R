#################################################
#### SCRAPING WHISKY ADVOCATE SCOTCH RATINGS ####
#################################################

if(!require("pacman")) {install.packages("pacman")}
pacman::p_load(here,
               tidyverse,
               rvest, 
               stringi)

# simple scraper function
scrape_element <- function(url = url, node) {
  
  read_html(url) %>% 
    html_nodes(node) %>% 
    html_text
  
}

# website url to capture scotch ratings
url <- "https://www.whiskyadvocate.com/ratings-reviews/?search=&submit=+&brand_id=0&rating=0&price=0&category=1%2C3%2C4%2C6%2C51&styles_id=0&issue_id=0"

################################################
#### WHISKY ADVOCATE: VARIABLES OF INTEREST ####
################################################

# scotch whisky names
whisky <- scrape_element(url, "h1")
whisky <- whisky[-1] # removing "Whisky Advocate" title captured in scrape

# rating points
points <- scrape_element(url, "h2 span") %>% 
  parse_double()

# type of scotch (single malt, blended malt, blended)
type <- scrape_element(url, "span span:nth-child(1)")
type <- type[-c(1:2)] # removing messy line/tab breaks captured in scrape

# price of bottle
price <- scrape_element(url, "p span span:nth-child(3)") %>% 
  parse_number()

# explanation of review + description of whisky's flavor
description <- scrape_element(url, "[itemprop='description']")

# Whisky Advocate author who provided the review (plus season/year of review)
author_date <- scrape_element(url, "#review-id-1212 div:nth-child(4) p")

# combine variables into a data frame
scrape_whisky_advocate <- tibble(whisky,
                                 points,
                                 type,
                                 price,
                                 description,
                                 author_date)

###########################
#### CLEANING THE DATA ####
###########################

scotch_ratings <- scrape_whisky_advocate %>% 
  
  # extract relevant author and review info
  separate(author_date,
           c("author", "review_date"),
           sep = "[(]") %>%
  separate(review_date,
           c("review_season", "review_year"),
           sep = " ") %>% 
  
         # extract & remove ABV from whisky name
  mutate(ABV = parse_number(stri_extract_last_regex(whisky,
                                                    "\\d+(\\.\\d+){0,1}%")),
         whisky = str_replace_all(whisky, ",", ""),
         whisky = str_remove(whisky,
                             stri_extract_last_regex(whisky,
                                                     "\\d+(\\.\\d+){0,1}%")),
         whisky = str_trim(whisky),
         
         # extract age from whisky name
         age = parse_number(str_extract(whisky,
                                        "\\d{1,2} [y|Y]")),
         
         # clean up review description and author name
         description = str_trim(description),
         author = str_replace_all(author,
                                  "Reviewed by: ",
                                  ""),
         
         # create ordinal system for review date for easier sorting of multiple reviews
         review_season_num = case_when(
           review_season == "Winter" ~ "1",
           review_season == "Spring" ~ "2",
           review_season == "Summer" ~ "3",
           TRUE ~ "4"),
         review_year = str_replace_all(review_year, "\\)", "")) %>% 
  
  # duplicate listings for Top 20 re-reviews
  filter(!is.na(whisky) & !str_detect(description, "Top 20")) %>%
  
  select(whisky, points, type, age, ABV, everything()) %>% 
  arrange(whisky, age)

#########################
#### SAVING THE DATA ####
#########################

saveRDS(scotch_ratings, file = here("data", "scotch-ratings.rdata"))

#########################################
#### EXPLORING WHISKY: SCOTCH PART 4 ####
#########################################

# PLAN
# * text analysis of review description content
# * variables of interest: description
# * Q: do flavor notes emerge overall? by brand? by type?
# * Q: what descriptive words are used commonly to describe scotch?
# * sentiment analysis
# * groupings of interest: author
# * Q: how does sentiment vary across review authors?
# * Q: does preference for types or brands vary across review authors?
# * Q: with which authors do my tastes align?
# 
# https://www.tidytextmining.com/preface.html

## ---- load_packages ----
if(!require("pacman")) {install.packages("pacman")}
pacman::p_load(here, # project workflow
               tidyverse, # reshaping + plotting the data
               tidytext, # text analysis
               hunspell, # word stems
               ggtext) # custom ggplot formatting

## ---- load_data ----
scotch <- read_rds(here("data", "scotch-ratings-distilleries.rdata")) # load data without dupes
source(here("code", "color-palettes.R")) # custom color palettes for analyses
source(here("code", "plot-theme-bg.R")) # custom ggplot theme

## ---- eda ----
# overall popular words
scotch %>% 
  unnest_tokens(word, description) %>% 
  anti_join(stop_words) %>% 
  count(word, name = "count", sort = TRUE) %>% 
  head(20)

# create the word df
custom_stop_words <- c("palate",
                       "nose",
                       "note",
                       "notes",
                       "finish")

remove_distillery_names <- str_to_lower(scotch$distillery)

# idea: pull list of locations from internet, remove those as well

scotch_words <- scotch %>% 
  unnest_tokens(word, description) %>% 
  anti_join(stop_words) %>% 
  filter(!word %in% c(custom_stop_words, remove_distillery_names),
         !str_detect(word, "[:digit:]"))
# add in word stems - write a function to determine which to replace? how can i tell?
# worry about words that aren't recognized with a stem - coalesce, but won't let me with list
# choose the write stemming program

# RESOURCES
# https://cran.r-project.org/web/packages/hunspell/vignettes/intro.html
# https://github.com/juliasilge/tidytext/issues/17
# https://smltar.com/stemming.html#how-to-stem-text-in-r
# https://smltar.com/stemming.html#lemmatization
# https://gregrs-uk.github.io/2018-02-03/tidytext-walkthrough-correcting-spellings-reproducible-word-clouds/
# https://tidyr.tidyverse.org/reference/hoist.html

head(hunspell_stem(scotch_words$word), n = 20)

# un-nesting causes problems and deletes whichever words don't have stems at all
scotch_words %>% 
  mutate(word_stem = hunspell_stem(word)) %>%
  unnest(word_stem)

# idea: use locations or distillery names to facet (top locations? top prolific distilleries?)
# idea: sentiment analysis - does positive match with well-reviewed, vs. negative with poorly?

# regional flavors
scotch_words %>% 
  count(region, word, name = "count") %>%
  group_by(region) %>% 
  filter(!is.na(region)) %>% 
  slice_max(order_by = count, n = 10) %>% 
  print(n = Inf) %>% 
  ggplot(aes(x = reorder_within(word, count, region),
             y = count,
             fill = region)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~region, 
             scales = "free") + 
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  coord_flip() + 
  labs(x = "", y = "") + 
  theme_bg() + 
  theme(legend.position = "none")




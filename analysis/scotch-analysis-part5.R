#########################################
#### EXPLORING WHISKY: SCOTCH PART 5 ####
#########################################

# PLAN
# * get flavor profiles, combine with data (fuzzyjoin)
# * confirmatory analysis of part 3 in some ways: do regions have flavor profiles?
# * eventually, do cluster analysis or similarity scores related to flavor notes - which whiskeys are most similar?
# * put stuff together into a shiny app: input certain parameters, and it suggests whisky
# * be sure to prototype before deployment!
# 
# cool shiny app
# https://scotchgit.bitbucket.io/

## Next Steps
# Add next steps.
# beyond this project, could do:
# * similar progression, but for irish whisky
# * similar progression, but for american whisky / bourbon
# * similar progression, but for japanese whisky
# * compare across different styles: if i like this scotch, which japanese is similar?

# Resources
# source data: https://www.kaggle.com/koki25ando/scotch-whisky-dataset

# https://outreach.mathstat.strath.ac.uk/outreach/nessie/nessie_whisky.html
# https://blog.revolutionanalytics.com/2013/12/k-means-clustering-86-single-malt-scotch-whiskies.html

# https://whiskyanalysis.com/index.php/database/
# https://whiskyanalysis.com/index.php/methodology-introduction/methodology-flavour-comparison/

# https://rss.onlinelibrary.wiley.com/doi/epdf/10.1111/j.1740-9713.2009.00337.x

# EXAMPLE ANALYSIS: https://whiskyanalysis.com/index.php/methodology-introduction/methodology-flavour-comparison/
# https://www.r-bloggers.com/2014/01/where-the-whisky-flavor-profile-data-came-from/
# https://www.r-bloggers.com/2013/12/k-means-clustering-86-single-malt-scotch-whiskies/
# https://www.r-bloggers.com/2014/01/mapping-the-taste-profile-of-scottish-whishkeys/
# https://towardsdatascience.com/recommending-scotch-whisky-ea440c2eb289


## ---- load_packages ----

## ---- load_data ----
# add the data
# source(here("code", "get-scotch-flavors.R"))

## ----join_data ----
#scotch_flavor_notes <- scotch %>% 
#  fuzzy_left_join(scotch_flavors,
#                  match_fun = ignore_case_string,
#                  by = c(whisky = "distillery_name"))
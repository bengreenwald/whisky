###########################################
#### SAVING HTML WIDGETS FROM ANALYSES ####
###########################################

## ---- source_code ----
source(here::here(".", "analysis", "scotch-analysis-part2.R"))
source(here::here(".", "analysis", "scotch-analysis-part3.R"))

## ---- part_2 ----
htmlwidgets::saveWidget(age_plotly,
                        file = "output/age_plotly.html",
                        selfcontained = TRUE)

## ---- part_3 ----
htmlwidgets::saveWidget(brand_plotly,
                        file = "output/brand_plotly.html", 
                        selfcontained = TRUE)
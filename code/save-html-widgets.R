###########################################
#### SAVING HTML WIDGETS FROM ANALYSES ####
###########################################

## ---- source_code ----
source(here::here(".", "analysis", "scotch-analysis-part2.R"))
source(here::here(".", "analysis", "scotch-analysis-part3.R"))

## ---- part_2 ----
htmlwidgets::saveWidget(benchmark_table,
                        file = "output/benchmark_table.html",
                        selfcontained = TRUE)
htmlwidgets::saveWidget(age_plotly,
                        file = "output/age_plotly.html",
                        selfcontained = TRUE)
htmlwidgets::saveWidget(pct_expensive_table,
                        file = "output/pct_expensive_table.html", 
                        selfcontained = TRUE)
htmlwidgets::saveWidget(expensive_blend_table,
                        file = "output/expensive_blend_table.html", 
                        selfcontained = TRUE)
htmlwidgets::saveWidget(low_points_table,
                        file = "output/low_points_table.html",
                        selfcontained = TRUE)

## ---- part_3 ----
htmlwidgets::saveWidget(brand_plotly,
                        file = "output/brand_plotly.html", 
                        selfcontained = TRUE)
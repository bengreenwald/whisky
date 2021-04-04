#########################################
#### EXPLORING WHISKY: SCOTCH PART 1 ####
#########################################

## ---- load_packages ----
if(!require("pacman")) {install.packages("pacman")}
pacman::p_load(here, # project workflow
               tidyverse, # reshaping + plotting the data
               gt) # table of variables + descriptions

## ---- load_data ----
scotch <- read_rds(here("data", "scotch-ratings-distilleries.rdata")) # load the data
source(here("code", "color-palettes.R")) # custom color palettes
source(here("code", "plot-theme-bg.R")) # custom ggplot theme

## ---- variable_table ----
# create inline code using markdown
var_names <- glue::glue("`{names(scotch)}`") 

# variable descriptions
var_descriptions <- c("name of whisky release",
                      "rating points assigned by Whisky Advocate reviewer (50-100)",
                      "whether the release is single malt, blended, or blended malt",
                      "age at which the whisky was bottled",
                      "alcohol by volume (expressed as a volume percent)",
                      "cost of a single bottle of whisky at the time of review",
                      "full text of Whisky Advocate review",
                      "name of Whisky Advocate reviewer",
                      "season during which review was published",
                      "year during which review was published",
                      "numeric code for sorting seasons (1 = Winter, 2 = Spring, 3 = Summer, 4 = Fall)",
                      "name of whisky distillery",
                      "regional location of distillery",
                      "latitudinal coordinates of distillery",
                      "longitudinal coordinates of distillery",
                      "whisky.com URL from where distillery details were scraped",
                      "whether a whisky release includes 2+ distilleries in its name")

# create table data                    
var_table <- tibble(Variable = var_names,
                    Description = var_descriptions) %>% 
  gt() %>% 
  theme_gt() %>% 
  # markdown format for variable styling
  fmt_markdown(columns = vars(Variable)) %>% 
  # cell text and color styling
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = vars(Variable))) %>% 
  # adjust column widths and alignment
  cols_width("Variable" ~ px(225),
             "Description" ~ px(600)) %>% 
  cols_align(align = "right",
             columns = vars(Variable)) %>% 
  # add footnote
  tab_footnote(footnote = md("Learn more about [Whisky Advocate's rating scale](https://www.whiskyadvocate.com/ratings-and-reviews/)."),
               locations = cells_body(columns = vars(Description),
                                      rows = 2))

## ---- glimpse_data ----
glimpse_full <- glimpse(scotch)
#########################################
#### EXPLORING SCOTCH WHISKY: PART 1 ####
#########################################

# ---- load_packages ----
pacman::p_load(
  here, # project workflow
  tidyverse, # reshaping + plotting the data
  gt, # generating tables
  webshot2 # saving tables as pngs
)

# ---- load_data ----
scotch <- read_rds(here("data", "scotch-ratings-distilleries.rdata")) # load the data
source(here("code", "color-palettes.R")) # custom color palettes
source(here("code", "plot-theme-bg.R")) # custom ggplot theme

# ---- variable_table ----
# create inline code using markdown
var_names <- names(scotch)

# variable descriptions
var_descriptions <- c(
  "name of whisky release",
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
  "whether a whisky release includes 2+ distilleries in its name"
)

# create table data                    
var_table <- tibble(
  VARIABLE = var_names,
  DESCRIPTION = var_descriptions
  ) %>% 
  gt() %>% 
  theme_gt() %>% 
  # table formatting
  tab_style(
    style = list(
      cell_text(font = "Fira Code", align = "center", weight = "bold")
      ),
    locations = cells_body(columns = VARIABLE)
    ) %>% 
  cols_width(
    VARIABLE ~ px(225),
    DESCRIPTION ~ px(600)
    )

# save table
gtsave(var_table, here("output", "part1", "variable_table.png"))

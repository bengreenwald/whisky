## ---- load_packages ----
if(!require("pacman")) {install.packages("pacman")}
pacman::p_load(here, # project workflow
               tidyverse, # reshaping + plotting the data
               plotly, # interactive plots
               ggridges, # ridgeline plots
               waffle, # pictogram plot
               ggtext, # custom ggplot formatting
               ggpubr, # Q-Q plots
               GGally) # visualizing correlations

## ---- load_data ----
scotch <- read_rds(here("data", "scotch-ratings-distilleries.rdata")) # load data without duplicates
source(here("code", "color-palettes.R")) # custom color palettes for analyses
source(here("code", "load-fonts-glyphs.R")) # access special fonts and glyphs for pictogram viz
source(here("code", "plot-theme-bg.R")) # custom ggplot theme
source(here("code", "plot-functions.R")) # functions for ridgeline and hex plots

## ---- data_glimpse ----
scotch %>% 
  select(whisky, type, age, ABV, price, points) %>% 
  glimpse()

## ---- dt_table ----
scotch %>% 
  filter(whisky %in% c("Glengoyne 21 year old", 
                       "Glenfiddich 12 year old")) %>% 
  select(whisky,
         region, 
         type,
         ABV,
         price,
         points,
         description) %>% 
  
  # nicer column labels
  rename_with(~str_to_title(.),
              -ABV) %>% 
  
  # interactive table, remove unnecessary table features
  DT::datatable(options = list(dom = 't'))

## ---- age_plotly ----
# create data for plot with age highlight
age_data <- scotch %>%
  rename(Age = age) %>% 
  filter(!is.na(Age)) %>% 
  group_by(Age) %>% 
  tally(name = "Releases") %>% 
  mutate(highlight = ifelse(Releases > 75,
                            "highlight", 
                            "no_highlight"))

# create base plot in ggplot
age_plot <- age_data %>% 
  ggplot(aes(x = Age, 
             y = Releases, 
             fill = highlight)) + 
  geom_col(width = 0.8, 
           alpha = 0.9) + 
  scale_fill_manual(values = c(wky_orange, 
                               wky_brown)) + 
  labs(x = "Age (in years)",
       y = "Number of Releases") + 
  theme_bg() + 
  theme(legend.position = "none",
        plot.title = element_markdown())

# interactive plot using plotly 
age_plotly <- ggplotly(age_plot,
                       tooltip = c("x", "y")) %>% 
  layout(title = list(text = "<b> Distilleries overwhelming offer 12 year old scotch whiskies. </b> <br> <sup> <i> 10/15/18/21 years are also common, with specialty releases often starting at 25 years in increments of five. </i> </sup>",
                      x = 0.5))

# create HTML object to center plot on page
htmltools::div(age_plotly,
               align = "center")

## ---- hex_plot ----
ridgeline_plot(ABV, 
               type) + 
  
  # ggtext for special styling
  labs(title = "<span style = 'color:#EC7014;'>Blended scotch whisky</span> is usually diluted to the legal mininum: 40% ABV.",
       subtitle = "Malted scotch sees a bump around 46%, the ideal ABV to avoid chill filtration.",
       x = "Distribution of ABV") + 
  theme(plot.title = element_markdown())
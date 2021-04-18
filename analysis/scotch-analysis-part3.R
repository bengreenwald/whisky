#########################################
#### EXPLORING WHISKY: SCOTCH PART 3 ####
#########################################

## ---- load_packages ----
if(!require("pacman")) {install.packages("pacman")}
pacman::p_load(here, # project workflow
               tidyverse, # reshaping + plotting the data
               ggtext, # custom ggplot formatting
               plotly, # interactive plots
               treemapify) # treemap plot

## ---- load_data ----
scotch <- read_rds(here("data", "scotch-ratings-distilleries.rdata")) # load data without dupes
scotch_with_collabs <- read_rds(here("data", "scotch-ratings-distilleries-with-collabs.rdata")) # load data with collaborations
source(here("code", "color-palettes.R")) # custom color palettes for analyses
source(here("code", "plot-theme-bg.R")) # custom ggplot theme

## ---- data_glimpse ----
glimpse_data <- scotch %>% 
  select(whisky, distillery, region, lat, long) %>% 
  glimpse()

## ---- regions ----
# explore regional breakdown within these data
regions <- scotch_with_collabs %>% 
  filter(!is.na(region)) %>% 
  group_by(region) %>% 
  summarise(releases = n()) %>% 
  mutate(prop =  releases/sum(releases))

# combined share of data originating from speyside + highland regions
spey_high <- regions %>% 
  filter(region %in% c("Highland", "Speyside")) %>% 
  summarise(perc = sum(prop)) %>% 
  pull(perc) %>% 
  scales::percent()

# number of releases with known regions
n_release_region <- scales::comma(sum(regions$releases))

# treemap plot
region_treemap <- scotch_with_collabs %>% 
  filter(!is.na(region)) %>% 
  group_by(region, distillery) %>% 
  summarise(releases = n()) %>% 
  ggplot(aes(area = releases,
             fill = region,
             label = distillery,
             subgroup = region)) + 
  geom_treemap() + 
  geom_treemap_text(colour = "white",
                    place = "center",
                    grow = TRUE,
                    reflow = TRUE) + 
  geom_treemap_subgroup_border() + 
  geom_treemap_subgroup_text(colour = "white",
                             place = "center",
                             grow = TRUE,
                             alpha = 0.3) +
  scale_fill_manual(values = c(sco_khaki,
                               sco_green, 
                               sco_magenta,
                               sco_blue, 
                               sco_brown, 
                               sco_lime)) +
  labs(title = glue::glue("{spey_high} of whiskies originate from the <span style = 'color:#A3B92F;'>Speyside</span> or <span style = 'color:#48652C;'>Highland</span> regions."),
       subtitle = glue::glue("Includes {n_release_region} reviewed whisky releases out of {scales::comma(nrow(scotch_with_collabs))} with known regions")) +
  theme_bg() + 
  theme(legend.position = "none",
        plot.title = element_markdown())

# violin plot
region_violin <- scotch_with_collabs %>% 
  filter(!is.na(region)) %>% 
  ggplot(aes(x = points, 
             y = reorder(region, 
                         points, 
                         FUN = max), 
             fill = region)) + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75),
              alpha = 0.8) + 
  scale_fill_manual(values = c(sco_khaki,
                               sco_green, 
                               sco_magenta,
                               sco_blue, 
                               sco_brown, 
                               sco_lime)) + 
  labs(title = "Which region produces the most highly-rated scotch?",
       subtitle = "Vertical lines inside violins represent 25th, 50th, and 75th quartiles.",
       x = "Distribution of Rating Points",
       y = "") + 
  theme_bg() + 
  theme(legend.position = "none")

## ---- distilleries ----
brands <- scotch_with_collabs %>% 
  filter(!is.na(distillery)) %>% 
  group_by(distillery, region) %>% 
  summarise(Releases = n(),
            `Median Price` = median(price),
            `Average Points` = round(mean(points), 2)) %>% 
  rename(Distillery = distillery,
         Region = region) %>% 
  mutate(Region = ifelse(is.na(Region), "No Region", Region))

# create ggplot base visualization 
brand_plot <- brands %>% 
  filter(Distillery != "Ladyburn") %>% 
  ggplot(aes(x = `Median Price`,
             y = `Average Points`,
             size = Releases,
             color = Region,
             text = glue::glue("Distillery: {Distillery}"))) + 
  geom_point(alpha = 0.8) + 
  scale_color_manual(values = c(sco_khaki,
                                sco_green, 
                                sco_magenta,
                                sco_blue, 
                                sco_brown, 
                                wky_orange,
                                sco_lime)) + 
  labs(x = "Median Price ($)",
       y = "Average Points (50-100)") +
  theme_bg() + 
  theme(legend.title = element_blank())

# plotly interactive plot with title and subtitle left-justified
brand_plotly <- ggplotly(brand_plot,
                         width = 800,
                         height = 450) %>% 
  layout(title = list(text = "<b> Which distilleries are the most profilic, expensive, and highly regarded? </b> <br> <sup> <i> Size of bubbles indicates the number of whisky releases by scotch brand. </i> </sup>", x = 0))

# researching which distilleries are not included - what happened?
no_regions <- scotch_with_collabs %>% 
  filter(!is.na(distillery) & is.na(region)) %>% 
  group_by(distillery) %>% 
  tally(name = "releases")

## ---- mapping ----
distillery_map <- scotch_with_collabs %>% 
  filter(!is.na(lat)) %>% 
  group_by(distillery, region, lat, long) %>% 
  summarise(releases = n(),
            median_price = median(price),
            avg_points = mean(points))
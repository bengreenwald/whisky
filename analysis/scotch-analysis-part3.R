#########################################
#### EXPLORING SCOTCH WHISKY: PART 3 ####
#########################################

# ---- load_packages ----
pacman::p_load(
  here, # project workflow
  tidyverse, # reshaping + plotting the data
  ggtext, # custom ggplot formatting
  treemapify # treemap plot
)

# ---- load_data ----
scotch <- read_rds(here("data", "scotch-ratings-distilleries.rdata")) # load data without dupes
scotch_with_collabs <- read_rds(here("data", "scotch-ratings-distilleries-with-collabs.rdata")) # load data with collaborations
source(here("code", "color-palettes.R")) # custom color palettes for analyses
source(here("code", "plot-theme-bg.R")) # custom ggplot theme

# ---- data_glimpse ----
scotch %>% 
  select(whisky, distillery, region, lat, long) %>% 
  glimpse()

# ---- exploring_regions ----
# explore regional breakdown within these data
regions <- scotch_with_collabs %>% 
  filter(!is.na(region)) %>% 
  group_by(region) %>% 
  summarise(releases = n()) %>% 
  mutate(prop =  releases/sum(releases))

# combined share of data originating from speyside + highland regions
spey_high <- regions %>% 
  filter(region %in% c("Highland", "Speyside")) %>% 
  summarise(perc = sum(prop) * 100) %>% 
  pull(perc) %>% 
  sprintf("%1.0f%%", .)

# number of releases with known regions
n_release_region <- scales::comma(sum(regions$releases))

# treemap plot
region_treemap <- scotch_with_collabs %>% 
  filter(!is.na(region)) %>% 
  group_by(region, distillery) %>% 
  summarise(releases = n(), .groups = "drop") %>% 
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
                               wky_orange, 
                               sco_lime)) +
  labs(title = glue::glue("{spey_high} of whiskies originate from the <span style = 'color:#A3B92F;'>Speyside</span> or <span style = 'color:#48652C;'>Highland</span> regions."),
       subtitle = glue::glue("Treemap data includes {n_release_region} reviewed whisky releases with known regions out of {scales::comma(nrow(scotch_with_collabs))} total releases")) +
  theme_bg() + 
  theme(legend.position = "none",
        plot.title = element_markdown())

# save plot
ggsave(here("output", "part3", "region_treemap.png"),
       width = 8, height = 4.5, dpi = 300)

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
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  scale_fill_manual(values = c(sco_khaki,
                               sco_green, 
                               sco_magenta,
                               sco_blue, 
                               wky_orange, 
                               sco_lime)) + 
  labs(title = "Which region produces the most highly-rated scotch?",
       subtitle = "Vertical lines inside violins represent 25th, 50th, and 75th quartiles.",
       x = "Distribution of Rating Points",
       y = "") + 
  theme_bg() + 
  theme(legend.position = "none")

# save plot
ggsave(here("output", "part3", "region_violin.png"),
       width = 8, height = 4.5, dpi = 300)

# ---- distilleries ----
# which distilleries have no region?
no_regions <- scotch_with_collabs %>% 
  filter(!is.na(distillery) & is.na(region)) %>% 
  group_by(distillery) %>% 
  tally(name = "releases")

# data for map & scatterplot
distillery_map <- scotch_with_collabs %>% 
  filter(!is.na(lat)) %>% 
  group_by(distillery, region, lat, long) %>% 
  summarise(releases = n(),
            median_price = median(price),
            avg_points = mean(points))

# write data for tableau
write_csv(distillery_map, here("data", "distillery-tableau-map.csv"))

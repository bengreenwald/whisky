#########################################
#### EXPLORING SCOTCH WHISKY: PART 2 ####
#########################################

# ---- load_packages ----
pacman::p_load(
  here, # project workflow
  tidyverse, # reshaping + plotting the data
  gt, # generating tables
  webshot2, # saving tables as pngs
  ggridges, # ridgeline plots
  waffle, # pictogram plot
  ggtext, # custom ggplot formatting
  ggpubr, # Q-Q plots
  GGally # visualizing correlations
)

# ---- load_data ----
scotch <- read_rds(here("data", "scotch-ratings-distilleries.rdata")) # load data without dupes
source(here("code", "color-palettes.R")) # custom color palettes for analyses
#source(here("code", "load-fonts-glyphs.R")) # access special fonts and glyphs for pictogram viz
source(here("code", "plot-theme-bg.R")) # custom ggplot theme
source(here("code", "plot-functions.R")) # functions for ridgeline and hex plots

# ---- data_glimpse ----
scotch %>% 
  select(whisky, type, age, ABV, price, points) %>% 
  glimpse()

# ---- benchmark_whiskies ----
benchmark_table <- scotch %>% 
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
  # create table
  gt() %>% 
  theme_gt() %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Whisky)
  ) %>% 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = c(ABV, Price, Points))
  ) %>% 
  cols_width(
    Whisky ~ px(100),
    Description ~ px(400), 
    everything() ~ px(90)
  )

gtsave(benchmark_table, here("output", "part2", "benchmark_whisky_table.png"))

# ---- type ----
# data for waffle plot
types <- scotch %>% 
  count(type, name = "count") %>% 
  mutate(percent = count / sum(count), 
         waffle = round(percent * 100)) %>% 
  arrange(desc(count))
# create waffle plot
type_waffle <- types %>% 
  ggplot(aes(fill = type, 
             values = waffle)) + 
  geom_waffle(n_rows = 20, 
              size = 1, 
              color = "white", 
              flip = TRUE) + 
  # style the waffle plot + legend
  scale_fill_manual(name = NULL,
                    values = c(wky_yellow,
                               wky_orange,
                               wky_brown),
                    labels = c("Blended Malt\nScotch Whisky",
                               "Blended Scotch\nWhisky",
                               "Single Malt\nScotch"),
                    guide = guide_legend(reverse = TRUE)) +
  # ggtext package for custom html/markdown formatting in title and subtitle
  labs(title = "<span style = 'color:#8C2D04;'>Over 80%</span> of scotch whiskies reviewed are classified as <span style = 'color:#8C2D04;'>Single Malt Scotch</span>.",
       subtitle = "**1** square = **1%** of whisky releases reviewed by Whisky Advocate") +
  # more formatting/design edits
  theme_bg() +
  theme_enhance_waffle() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(
          face = "bold",
          margin = margin(r = 25, unit = "pt")),
        plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.margin = grid::unit(c(15, 10, 7.5, 10), "pt"))

# save plot
ggsave(here("output", "part2", "type_waffle.png"),
       width = 8, height = 4.5, dpi = 300)

# ---- age ----
# data for age plot
age_data <- scotch %>%
  rename(Age = age) %>% 
  filter(!is.na(Age)) %>% 
  group_by(Age) %>% 
  tally(name = "Releases") %>% 
  mutate(highlight = ifelse(Releases > 75,
                            "highlight", 
                            "no_highlight"))

# base age plot
age_plot <- age_data %>% 
  ggplot(aes(x = Age, 
             y = Releases, 
             fill = highlight)) + 
  geom_col(width = 0.8, 
           alpha = 0.9) + 
  scale_fill_manual(values = c(wky_orange, 
                               "gray35")) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  labs(x = "Age (in years)",
       y = "Number of Releases") + 
  theme_bg() + 
  theme(legend.position = "none")

# adding titles + formatting
age_plot_title <- age_plot + 
  labs(title = "Distilleries overwhelming offer <span style = 'color:#EC7014;'>12 year old</span> scotch whiskies.",
       subtitle = "Whiskies aged <b style = 'color:#EC7014;'>10, 15, 18, and 21 years</b> are also very common, with specialty<br>releases often starting at <b style = 'color:#EC7014;'>25 years</b> and increasing in increments of five.") + 
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())

# saving plot
ggsave(here("output", "part2", "age_plot.png"),
       width = 8, height = 4.5, dpi = 300)

# ---- ABV ----
ABV_ridge <- ridgeline_plot(ABV, type) + 
  # ggtext for special styling
  labs(title = "<span style = 'color:#EC7014;'>Blended scotch whisky</span> is usually diluted to the legal mininum: 40% ABV.",
       subtitle = "Malted scotch sees a bump around 46%, the ideal ABV to avoid chill filtration.",
       x = "Distribution of ABV") + 
  theme(plot.title = element_markdown())

# saving illegible plot
ggsave(here("output", "part2", "ABV_plot.png"),
       width = 8, height = 4.5, dpi = 300)

# ---- price_plots ----
# illegible plot
price_ridge <- ridgeline_plot(price, type) + 
  labs(title = "Oh no, an illegible plot!")

# saving illegible plot
ggsave(here("output", "part2", "price_illegible.png"),
       width = 8, height = 4.5, dpi = 300)

# log transformation for readability
price_log_ridge <- ridgeline_plot(price, type, log_trans = TRUE) + 
  # ggtext for special styling
  labs(title = "<span style = 'color:#8C2D04;'>Single malt scotch</span> is generally more expensive than blended varieties.",
       subtitle = "However, select <b style = 'color:#EC7014;'>blended scotch</b> brands maintain premium prices well beyond $1,000.") + 
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())

# saving the log trans plot
ggsave(here("output", "part2", "price_log_trans.png"),
       width = 8, height = 4.5, dpi = 300)

# ---- price_tables ----
# %expensive by type
pct_expensive_table <- scotch %>% 
  mutate(expensive = ifelse(price >= 1000, 
                            "Over $1,000", 
                            "Under $1,000")) %>% 
  count(type, expensive) %>% 
  pivot_wider(names_from = expensive,
              values_from = n) %>% 
  mutate(`% Expensive` = `Over $1,000` / (`Over $1,000` + `Under $1,000`)) %>% 
  rename(Type = type) %>% 
  arrange(desc(`Over $1,000`)) %>% 
  ungroup() %>% 
  gt() %>% 
  theme_gt() %>% 
  fmt_percent(columns = `% Expensive`) %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = `% Expensive`)
    ) %>% 
  cols_align(
    align = c("center"),
    columns = !Type
    ) %>% 
  cols_width(
    Type ~ px(200),
    everything() ~ px(150)
    )

# save table
gtsave(pct_expensive_table, here("output", "part2", "percent_expensive_table.png"))

# expensive blended whiskies
expensive_blend_table <- scotch %>% 
  arrange(desc(price)) %>% 
  filter(price >= 1000 & type == "Blended Scotch Whisky") %>% 
  select(whisky, price, points) %>% 
  rename_with(~str_to_title(.)) %>%
  gt() %>% 
  theme_gt() %>% 
  fmt_currency(columns = Price) %>% 
  cols_align(
    align = c("center"),
    columns = !Whisky
    ) %>% 
  cols_width(
    Whisky ~ px(350),
    everything() ~ px(150)
    )

# save table
gtsave(expensive_blend_table, here("output", "part2", "expensive_blend_table.png"))

# ---- price_comparison ----
# Glenfiddich 12
glenfiddich_12_price <- scotch %>% 
  filter(whisky == "Glenfiddich 12 year old") %>% 
  pull(price)
# Glengoyne 21
glengoyne_21_price <- scotch %>% 
  filter(whisky == "Glengoyne 21 year old") %>% 
  pull(price)
# Diamond Jubilee
diamond_jubilee_price <- scotch %>% 
  filter(whisky == "Diamond Jubilee by John Walker & Sons") %>% 
  pull(price)

# ---- points ----
# distribution of points
points_ridge <- ridgeline_plot(points, type) +
  labs(title = "Different types of scotch are *generally* rated similarly on Whisky Advocate.",
       subtitle = "However, the only whiskies **Not Recommended** by Whisky Advocate are <b style = 'color:#8C2D04;'>single malts</b>.") + 
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())

# save plot
ggsave(here("output", "part2", "points_plot.png"),
       width = 8, height = 4.5, dpi = 300)

low_points_table <- scotch %>% 
  filter(points < 75) %>% 
  select(whisky, points, description) %>% 
  arrange(points) %>% 
  rename_with(~str_to_title(.)) %>%  # nicer column labels
  # create table
  gt() %>% 
  theme_gt() %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Points)
  ) %>% 
  cols_width(
    Whisky ~ px(200),
    Points ~ px(100),
    Description ~ px(650)
    ) %>% 
  cols_align(
    align = c("center"),
    columns = Points
    )

# save table
gtsave(low_points_table, here("output", "part2", "low_points_table.png"))

# ---- qq_plots ----
# draw individual q-q plots
qq_age <- draw_qq_plot(age) # age
qq_ABV <- draw_qq_plot(ABV) # ABV
qq_price <- draw_qq_plot(price) # Price
qq_points <- draw_qq_plot(points) # Points

# combine plots into a 2x2 panel
qq_grid <- ggarrange(
  qq_age + rremove("xlab"),
  qq_ABV + rremove("xlab") + rremove("ylab"),
  qq_price,
  qq_points + rremove("ylab"),
  labels = c("Age", "ABV", "Price", "Points"),
  ncol = 2, nrow = 2,
  label.x = 0.4, label.y = 0.9,
  widths = c(2,2),
  font.label = list(face = "bold",family =  "Source Sans Pro"))

# save plot
ggsave(here("output", "part2", "qq_grid_plot.png"),
       width = 8, height = 4.5, dpi = 300)

# ---- correlations ----
# create correlation data
for_corr <- scotch %>% 
  select(age, ABV, price, points)

# create correlation plot
corr_plot <- ggcorr(
  for_corr,
  # correlation coefficient for non-linear data
  method = c("pairwise", "spearman"),
  # formatting and styling within ggcorr
  angle = 25,
  size = 5,
  color = "gray90",
  label = TRUE,
  label_round = 2,
  label_size = 3.5,
  high = wky_orange,
  low = sco_magenta) +
  # adjust ggplot formatting features
  labs(title = "Correlation Coefficients",
       subtitle = "Using Spearman's Rank-Order") + 
  guides(fill = guide_colourbar(barwidth = 1, 
                                barheight = 15.75,
                                ticks = FALSE)) + 
  theme_bg_dark() + 
  theme(plot.title = element_text(size = 17,
                                  hjust = 0.17,
                                  vjust = -24),
        plot.subtitle = element_text(size = 14,
                                     hjust = 0.17,
                                     vjust = -30),
        plot.margin = margin(-50, 15, 15, 15, "pt"),
        legend.margin = margin(t = 58))

# save plot
ggsave(here("output", "part2", "corr_plot.png"),
       width = 7, height = 6, dpi = 300)

# ---- scatterplots ----
# age vs. price
age_price_scatter <- scatter_plot(x = age, y = price, log_trans = "price") +
  labs(title = "Scotch age has a strong, positive, non-linear relationship with price.")

# save plot
ggsave(here("output", "part2", "age_price_scatter.png"),
       width = 8, height = 4.5, dpi = 300)

# age vs. points
age_points_scatter <- scatter_plot(x = age, y = points) + 
  labs(title = "Scotch age has a weak, positive relationship with rating points.") + 
  # add arrow + annotation
  geom_curve(aes(x = 48, 
                 y = 67, 
                 xend = 55, 
                 yend = 72.5),
             colour = "white",
             lineend = "round",
             arrow = arrow(length = unit(0.03, "npc"))) + 
  annotate("text",
           x = 42,
           y = 67.5,
           colour = "white",
           label = "Macallan 55\nLalique",
           fontface = "bold")

# save plot
ggsave(here("output", "part2", "age_points_scatter.png"),
       width = 8, height = 4.5, dpi = 300)

# price vs. points
price_points_scatter <- scatter_plot(x = price, y = points, log_trans = "price") + 
  labs(title = "Scotch price has a weak, positive, non-linear relationship with rating points.") + 
  # add arrow + annotation
  geom_curve(aes(x = 5000, 
                 y = 67, 
                 xend = 11700, 
                 yend = 72.5),
             colour = "white",
             lineend = "round",
             arrow = arrow(length = unit(0.03, "npc"))) + 
  annotate("text",
           x = 2000,
           y = 67.5,
           colour = "white",
           label = "Macallan 55\nLalique",
           fontface = "bold")

# save plot
ggsave(here("output", "part2", "price_points_scatter.png"),
       width = 8, height = 4.5, dpi = 300)


# ABV vs. price
ABV_price_scatter <- scatter_plot(x = ABV, y = price, log_trans = "price") + 
  labs(title = "Scotch ABV has a weak, positive, non-linear relationship with price.",
       x = "ABV")

# save plot
ggsave(here("output", "part2", "ABV_price_scatter.png"),
       width = 8, height = 4.5, dpi = 300)

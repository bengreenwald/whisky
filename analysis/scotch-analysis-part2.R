#########################################
#### EXPLORING WHISKY: SCOTCH PART 2 ####
#########################################

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
scotch <- read_rds(here("data", "scotch-ratings-distilleries.rdata")) # load data without dupes
source(here("code", "color-palettes.R")) # custom color palettes for analyses
source(here("code", "load-fonts-glyphs.R")) # access special fonts and glyphs for pictogram viz
source(here("code", "plot-theme-bg.R")) # custom ggplot theme
source(here("code", "plot-functions.R")) # functions for ridgeline and hex plots

## ---- data_glimpse ----
glimpse_data <- scotch %>% 
  select(whisky, type, age, ABV, price, points) %>% 
  glimpse()

## ---- benchmark_whiskies ----
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
  # interactive table, remove unnecessary table features
  DT::datatable(options = list(dom = 't'))

htmlwidgets::saveWidget(benchmark_table,
                        file = "output/benchmark_table.html",
                        selfcontained = TRUE)

## ---- type ----
# data for pictogram plot
types <- scotch %>% 
  group_by(type) %>% 
  summarise(count = n()) %>% 
  mutate(waffle = count/10,
         percent = count/sum(count)) %>% 
  arrange(desc(count))

# create pictogram plot
type_pictogram <- types %>%
  ggplot(aes(label = type,
             values = waffle)) +
  geom_pictogram(n_rows = 10,
                 aes(colour = type),
                 size = 5) +
  # style the pictogram plot + legend
  scale_color_manual(name = NULL,
                     values = c(wky_yellow,
                                wky_orange,
                                wky_brown),
                     labels = c("Blended Malt\nScotch Whisky",
                                "Blended Scotch\nWhisky",
                                "Single Malt\nScotch"),
                     guide = guide_legend(reverse = TRUE)) +
  scale_label_pictogram(name = NULL,
                        values = c("glass-whiskey",
                                   "glass-whiskey",
                                   "glass-whiskey"),
                        labels = c("Blended Malt\nScotch Whisky",
                                   "Blended Scotch\nWhisky",
                                   "Single Malt\nScotch"),
                        guide = guide_legend(reverse = TRUE)) + 
  # ggtext package for custom html/markdown formatting in title and subtitle
  labs(title = "<span style = 'color:#8C2D04;'>Over 80%</span> of scotch whiskies reviewed are classified as <span style = 'color:#8C2D04;'>Single Malt Scotch</span>.",
       subtitle = "**1** tumbler glass = **10** releases reviewed by Whisky Advocate") + 
  # more formatting edits
  coord_equal() +
  theme_bg() + 
  theme_enhance_waffle() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(face = "bold",
                                   margin = margin(b = 10,
                                                   r = 25,
                                                   unit = "pt")),
        plot.title = element_markdown(hjust = 0.5),
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.margin = grid::unit(c(15, 10, 7.5, 10), "pt"))

 ## ---- age ----
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
age_plotly <- ggplotly(age_plot, tooltip = c("x", "y")) %>% 
  layout(title = list(text = "<b> Distilleries overwhelming offer 12 year old scotch whiskies. </b> <br> <sup> <i> 10/15/18/21 years are also common, with specialty releases often starting at 25 years in increments of five. </i> </sup>",
                      x = 0.5))

htmlwidgets::saveWidget(age_plotly,
                        file = "output/age_plotly.html",
                        selfcontained = TRUE)

## ---- ABV ----
ABV_ridge <- ridgeline_plot(ABV, type) + 
  # ggtext for special styling
  labs(title = "<span style = 'color:#EC7014;'>Blended scotch whisky</span> is usually diluted to the legal mininum: 40% ABV.",
       subtitle = "Malted scotch sees a bump around 46%, the ideal ABV to avoid chill filtration.",
       x = "Distribution of ABV") + 
  theme(plot.title = element_markdown())

## ---- price_plots ----
price_ridge <- ridgeline_plot(price, type) + 
  labs(title = "Oh no, an illegible plot!")

price_log_ridge <- ridgeline_plot(price, type, log_trans = TRUE) + 
  # ggtext for special styling
  labs(title = "<span style = 'color:#8C2D04;'>Single malt scotch</span> is generally more expensive than blended varieties.",
       subtitle = "However, select <b style = 'color:#EC7014;'>blended scotch</b> brands maintain premium prices well beyond $1,000.") + 
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())

## ---- price_tables ----
pct_expensive_table <- scotch %>% 
  mutate(expensive = ifelse(price >= 1000, "Over_1000", "Under_1000")) %>% 
  group_by(type, expensive) %>% 
  tally() %>% 
  pivot_wider(names_from = expensive,
              values_from = n) %>% 
  # calculate expensive whisky by type
  mutate(Pct_Expensive = scales::percent(Over_1000/sum(Over_1000 + Under_1000),
                                         accuracy = 0.1)) %>% 
  rename(Type = type) %>% 
  arrange(desc(Over_1000)) %>% 
  # interactive table, remove unnecessary features
  DT::datatable(options = list(dom = 't'))

htmlwidgets::saveWidget(pct_expensive_table,
                        file = "output/pct_expensive_table.html", 
                        selfcontained = TRUE)

expensive_blend_table <- scotch %>% 
  arrange(price) %>% 
  filter(price >= 1000 & type == "Blended Scotch Whisky") %>% 
  mutate(price = scales::dollar(price)) %>% 
  select(whisky, price, points) %>% 
  # nicer column labels
  rename_with(~str_to_title(.)) %>%
  # interactive table, remove unnecessary features, show all rows
  DT::datatable(options = list(dom = 't', pageLength = 13))

htmlwidgets::saveWidget(expensive_blend_table,
                        file = "output/expensive_blend_table.html", 
                        selfcontained = TRUE)

## ---- price_comparison ----
glenfiddich_12_price <- scotch %>% 
  filter(whisky == "Glenfiddich 12 year old") %>% 
  pull(price)

glengoyne_21_price <- scotch %>% 
  filter(whisky == "Glengoyne 21 year old") %>% 
  pull(price)

diamond_jubilee_price <- scotch %>% 
  filter(whisky == "Diamond Jubilee by John Walker & Sons") %>% 
  pull(price)

## ---- points ----
points_ridge <- ridgeline_plot(points, type) +
  # ggtext for special styling
  labs(title = "Different types of scotch are *generally* rated similarly on Whisky Advocate.",
       subtitle = "However, the only whiskies **Not Recommended** by Whisky Advocate are <b style = 'color:#8C2D04;'>single malts</b>.") + 
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown())

low_points_table <- scotch %>% 
  filter(points < 75) %>% 
  select(whisky, points, description) %>% 
  arrange(points) %>% 
  rename_with(~str_to_title(.)) %>%  # nicer column labels
  DT::datatable(options = list(dom = 'Bfrtip',  # unnecessary table features
                               autoWidth = TRUE, # wider whisky column
                               columnDefs = list(list(width = '150px',
                                                      targets = 1)),
                               pageLength = 5))

htmlwidgets::saveWidget(low_points_table,
                        file = "output/low_points_table.html",
                        selfcontained = TRUE)

## ---- qq_plots ----
# draw individual q-q plots
qq_age <- draw_qq_plot(age)
qq_ABV <- draw_qq_plot(ABV)
qq_price <- draw_qq_plot(price)
qq_points <- draw_qq_plot(points)
# combine plots into a 2x2 panel
qq_grid <- ggarrange(qq_age + rremove("xlab"),
                     qq_ABV + rremove("xlab") + rremove("ylab"), 
                     qq_price, 
                     qq_points + rremove("ylab"), 
                     labels = c("Age", "ABV", "Price", "Points"),
                     ncol = 2, 
                     nrow = 2,
                     label.x = 0.4,
                     label.y = 0.9,
                     widths = c(2,2),
                     font.label = list(face = "bold",
                                       family =  "Roboto"))

## ---- correlations ----
# create correlation data
for_corr <- scotch %>% 
  select(age, ABV, price, points)
# create correlation plot
corr_plot <- ggcorr(for_corr,
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

## ---- hex_plots ----
# age vs. price
age_price_hex <- hex_plot(x = age, y = price, log_trans = "price") +
  labs(title = "Scotch age has a strong, positive, non-linear relationship with price.")

# age vs. points
age_points_hex <- hex_plot(x = age, y = points) + 
  labs(title = "Scotch age has a weak, positive relationship with rating points.") + 
  # add arrow + annotation
  geom_curve(aes(x = 48, 
                 y = 67, 
                 xend = 56, 
                 yend = 72.5),
             colour = "white",
             lineend = "round",
             linejoin = "mitre",
             arrow = arrow(length = unit(0.03, "npc"))) + 
  annotate("text",
           x = 42,
           y = 67.5,
           colour = "white",
           label = "Macallan 55\nLalique",
           face = "bold")

# price vs. points
price_points_hex <- hex_plot(x = price, y = points, log_trans = "price") + 
  labs(title = "Scotch price has a weak, positive, non-linear relationship with rating points.") + 
  # add arrow + annotation
  geom_curve(aes(x = 5000, 
                 y = 67, 
                 xend = 11700, 
                 yend = 72.5),
             colour = "white",
             lineend = "round",
             linejoin = "mitre",
             arrow = arrow(length = unit(0.03, "npc"))) + 
  annotate("text",
           x = 2000,
           y = 67.5,
           colour = "white",
           label = "Macallan 55\nLalique",
           face = "bold")

# ABV vs. price
ABV_price_hex <- hex_plot(x = ABV, y = price, log_trans = "price") + 
  labs(title = "Scotch ABV has a weak, non-linear relationship with price.",
       x = "ABV")
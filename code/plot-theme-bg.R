##############################
#### CUSTOM GGPLOT THEMES ####
##############################

## ---- theme_bg ----
theme_bg <- function () { 
  theme_minimal(base_family = "Roboto") %+replace% 
    theme(
      # titles, caption, subtitle, legend
      plot.title = element_text(face = "bold",
                                size = 13,
                                margin = margin(b = 5),
                                hjust = 0.5),
      plot.subtitle = element_text(face = "italic",
                                   size = 10.5,
                                   margin = margin(b = 12.5),
                                   hjust = 0.5),
      plot.caption = element_text(size=8,
                                  hjust = 1,
                                  margin = margin(t = 10),
                                  colour = "gray70"),
      legend.title = element_text(face = "bold"),
      # margin
      plot.margin = margin(15, 15, 15, 15, "pt"),
      # panel & background
      plot.background = element_rect(fill = "gray95",
                                     colour = "gray95"),
      panel.background = element_rect(fill = "gray95",
                                      colour = "gray95"),
      panel.grid.major = element_line(colour = "gray85"),
      panel.grid.minor = element_line(colour = "gray91"),
      # axis text, ticks, labels
      axis.text = element_text(size = 9),
      axis.title.x = element_text(face = "bold",
                                  margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold",
                                  margin = margin(r = 10),
                                  angle = 90)
    )
}

## ---- theme_bg_dark ----
theme_bg_dark <- function () { 
  theme_minimal(base_family = "Roboto") %+replace% 
    theme(
      # titles, caption, subtitle, legend
      plot.title = element_text(face = "bold",
                                size = 13,
                                margin = margin(b = 5),
                                hjust = 0.5,
                                colour = "gray90"),
      plot.subtitle = element_text(face = "italic",
                                   size = 10.5,
                                   margin = margin(b = 12.5),
                                   hjust = 0.5,
                                   colour = "gray90"),
      plot.caption = element_text(size=8,
                                  hjust = 1,
                                  margin = margin(t = 10),
                                  colour = "gray50"),
      legend.title = element_text(face = "bold",
                                  colour = "gray90"),
      legend.text = element_text(colour = "gray90"),
      # margin
      plot.margin = margin(15, 15, 15, 15, "pt"),
      # panel & background
      plot.background = element_rect(fill = "gray10",
                                     colour = "gray10"),
      panel.background = element_rect(fill = "gray10",
                                      colour = "gray10"),
      panel.grid.major = element_line(colour = "gray25"),
      panel.grid.minor = element_line(colour = "gray15"),
      # axis text, ticks, labels
      axis.text = element_text(colour = "gray80",
                               size = 9),
      axis.title.x = element_text(face = "bold",
                                  margin = margin(t = 10),
                                  colour = "gray90"),
      axis.title.y = element_text(face = "bold",
                                  margin = margin(r = 10),
                                  angle = 90,
                                  colour = "gray90")
    )
}

## ---- theme_bg_dark ----
theme_gt <- function(data, ...){
  data %>%
    # style column labels
    tab_style(style = list(cell_text(weight = "bold",
                                     color = wky_yellow),
                           cell_fill(color = wky_brown)),
              locations = cells_column_labels(columns = everything())) %>% 
    # add cell & table borders
    tab_style(style = cell_borders(sides = "right", 
                                   color = "#d9d9d9", 
                                   weight = px(1)),
              locations = cells_body(columns = everything())) %>% 
    tab_style(style = cell_borders(sides = "right", 
                                   color = "#d9d9d9", 
                                   weight = px(1)),
              locations = cells_column_labels(columns = everything())) %>% 
    opt_table_outline() %>% 
    tab_options(table.background.color = "#222222",
                data_row.padding = px(5),
                ...) %>% 
    opt_row_striping(row_striping = FALSE) %>% 
    # custom css for table background, column header alignment
    opt_css(css = "
    #one .gt_table {
      background-color: #e9e9e9;
    }
    #one .gt_col_heading {
      text-align: center !important;
    }
    ", add = TRUE)
}

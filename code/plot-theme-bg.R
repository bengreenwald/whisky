##############################
#### CUSTOM GGPLOT THEMES ####
##############################

## ---- theme_bg ----
theme_bg <- function () { 
  theme_minimal(base_family = "Source Sans Pro") %+replace% 
    theme(
      
      # titles, caption, subtitle, legend
      plot.title = element_text(
        face = "bold",
        size = 13,
        margin = margin(b = 5),
        hjust = 0.5),
      plot.title.position = "plot",
      plot.subtitle = element_text(
        face = "italic",
        size = 10.5,
        margin = margin(b = 12.5),
        hjust = 0.5),
      plot.caption = element_text(
        size = 8,
        hjust = 1,
        margin = margin(t = 10),
        colour = "gray70"),
      legend.title = element_text(face = "bold"),
      
      # margin
      plot.margin = margin(15, 15, 15, 15, "pt"),
      
      # panel & background
      plot.background = element_rect(
        fill = "white",
        colour = "white"),
      panel.background = element_rect(
        fill = "white",
        colour = "white"),
      panel.grid.major = element_line(colour = "gray90"),
      panel.grid.minor = element_line(colour = "gray96"),
      
      # axis text, ticks, labels
      axis.text = element_text(size = 9),
      axis.title.x = element_text(
        face = "bold",
        margin = margin(t = 10)),
      axis.title.y = element_text(
        face = "bold",
        margin = margin(r = 10),
        angle = 90)
    )
}

## ---- theme_bg_dark ----
theme_bg_dark <- function () { 
  theme_minimal(base_family = "Source Sans Pro") %+replace% 
    theme(
      
      # titles, caption, subtitle, legend
      plot.title = element_text(
        face = "bold",
        size = 13,
        margin = margin(b = 5),
        hjust = 0.5,
        colour = "gray90"),
      plot.title.position = "plot",
      plot.subtitle = element_text(
        face = "italic",
        size = 10.5,
        margin = margin(b = 12.5),
        hjust = 0.5,
        colour = "gray90"),
      plot.caption = element_text(
        size=8,
        hjust = 1,
        margin = margin(t = 10),
        colour = "gray50"),
      legend.title = element_text(
        face = "bold",
        colour = "gray90"),
      legend.text = element_text(colour = "gray90"),
      
      # margin
      plot.margin = margin(15, 15, 15, 15, "pt"),
      
      # panel & background
      plot.background = element_rect(
        fill = "gray10",
        colour = "gray10"),
      panel.background = element_rect(
        fill = "gray10",
        colour = "gray10"),
      panel.grid.major = element_line(colour = "gray25"),
      panel.grid.minor = element_line(colour = "gray15"),
      
      # axis text, ticks, labels
      axis.text = element_text(
        colour = "gray80",
        size = 9),
      axis.title.x = element_text(
        face = "bold",
        margin = margin(t = 10),
        colour = "gray90"),
      axis.title.y = element_text(
        face = "bold",
        margin = margin(r = 10),
        angle = 90,
        colour = "gray90")
    )
}

## ---- theme_gt ----
theme_gt <- function(data, ...){
  data %>%
    # style column labels
    tab_style(
      style = list(
        cell_text(weight = "bold", align = "center", color = "#FFFFFF"),
        cell_fill(color = wky_brown)
        ),
      locations = cells_column_labels(
        columns = everything())
      ) %>% 
    # adding font
    opt_table_font(
      font = "Source Sans Pro"
      ) %>% 
    # other formatting
    opt_table_lines() %>% 
    opt_row_striping() %>% 
    tab_options(
      column_labels.font.size = 20,
    )
 
}

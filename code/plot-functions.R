###############################
#### CUSTOM PLOT FUNCTIONS ####
###############################

## ---- ridgeline_plot ----
ridgeline_plot <- function(var,
                           group = type,
                           log_trans = FALSE) {
  
  # create dynamic axis title
  axis_title <- stringr::str_to_title(rlang::ensym(var))
  var <- rlang::enquo(var)
  group <- rlang::enquo(group)
  
  # create the base plot
  rlp <- scotch %>% 
    ggplot(aes(x = !!var, 
               y = !!group, 
               fill = !!group,
               height = ..density..)) + 
    geom_density_ridges(stat = "density",
                        alpha = 0.8,
                        trim = TRUE) + 
    scale_fill_manual(values = c(wky_yellow, 
                                 wky_orange, 
                                 wky_brown)) + 
    scale_y_discrete(expand = c(0, 0),
                     labels = c("Blended Malt\nScotch Whisky\n\n",
                                "Blended Scotch\nWhisky\n\n",
                                "Single Malt\nScotch\n\n")) + 
    coord_cartesian(clip = "off") + 
    theme_bg() +
    theme(legend.position = "none",
          plot.title.position = "plot")
  
  # plot if no log transformation included
  if(isFALSE(log_trans)) {
    rlp <- rlp + 
      scale_x_continuous(expand = c(0, 0)) + 
      labs(x = glue::glue("Distribution of {axis_title}"),
           y = "")
    
  # plot if variable of interest is log transformed   
  } else {
    rlp <- rlp + 
      scale_x_continuous(expand = c(0, 0),
                         trans = "log10",
                         breaks = scales::trans_breaks("log10", function(x) 10^x),
                         labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
      labs(x = glue::glue("Distribution of Log({axis_title})"),
           y = "")
  }
  rlp
}

## ---- hex_plot ----
hex_plot <- function(x,
                     y,
                     log_trans = NULL) {
  
  # create dynamic axis titles
  x_axis_title <- stringr::str_to_title(rlang::ensym(x))
  y_axis_title <- stringr::str_to_title(rlang::ensym(y))
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  
  # create base plot
  hxp <- scotch %>% 
    ggplot(aes(x = !!x, y = !!y)) + 
    geom_hex() + 
    scale_fill_gradientn(name = "Releases",
                         colours = rev(brewer.pal(8, "YlOrBr")),
                         n.breaks = 6) + 
    labs(x = x_axis_title,
         y = y_axis_title) + 
    theme_bg_dark() + 
    theme(plot.title = element_text(margin = margin(b = 20)),
          plot.title.position = "plot") + 
    guides(fill = guide_colourbar(barwidth = 1, 
                                  barheight = 12,
                                  ticks = FALSE))
  
  # plot if no log transformation included
  if(is.null(log_trans)) {
    hxp +
      labs(x = x_axis_title,
           y = y_axis_title)
  
  } else {
    # plot if x-axis variable is log transformed     
    if(log_trans == rlang::ensym(x)) {
      hxp + 
        scale_x_continuous(trans = "log10",
                           breaks = scales::trans_breaks("log10", function(x) 10^x),
                           labels = scales::trans_format("log10", scales::math_format(10^.x))) +
        labs(x = glue::glue("Log({x_axis_title})"),
             y = y_axis_title)
      
    # plot if x-axis variable is log transformed       
    } else {
      hxp + 
        scale_y_continuous(trans = "log10",
                           breaks = scales::trans_breaks("log10", function(x) 10^x),
                           labels = scales::trans_format("log10", scales::math_format(10^.x))) +
        labs(x = x_axis_title,
             y = glue::glue("Log({y_axis_title})"))
    }
  }
}

## ---- qq_plot ----
draw_qq_plot <- function(var) {
  var <- rlang::enquo(var)
  
  # filter the data
  data <- scotch %>% 
    filter(!is.na(!!var)) %>% 
    select(!!var) %>% 
    as_vector
  
  # create q-q plot
  ggqqplot(data, 
           add = "qqline",
           color = wky_brown,
           ggtheme = theme_bg())
}

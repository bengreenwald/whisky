###############################################################
#### LOADING FONTS & GLYPHS USING EXTRAFONTS + FONTAWESOME ####
###############################################################

# Required for geom_pictogram through the waffle package. 
# reference: https://www.listendata.com/2019/06/create-infographics-with-r.html

if(!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, extrafont)

font_test <- fonttable() %>% 
  as_tibble() %>% 
  filter(grepl("Awesome",
               FamilyName)) %>% 
  select(FamilyName,
         FontName,
         fontfile)

if (nrow(font_test) == 3) {
  
  message("Fonts already imported and loaded successfully.")
  
} else {
  
  # if needed, importing and loading the fonts
    
  font_import(path = "~/Library/Fonts",
                pattern = "fa-",
                prompt =  FALSE)
    
  loadfonts(device = "pdf")
  
  library(showtext)
  
  font_add(family = "FontAwesome5Free-Solid",
           regular = "~/Library/Fonts/fa-solid-900.ttf")
  font_add(family = "FontAwesome5Free-Regular",
           regular = "~/Library/Fonts/fa-regular-400.ttf")
  font_add(family = "FontAwesome5Brands-Regular",
           regular = "~/Library/Fonts/fa-brands-400.ttf")
  
  showtext_auto()
  
}
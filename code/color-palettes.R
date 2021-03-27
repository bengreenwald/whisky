########################
#### COLOR PALETTES ####
########################

if(!require("RColorBrewer")) {install.packages("RColorBrewer")}
pacman::p_load(RColorBrewer)

################################
#### WHISKY - COLOR PALETTE ####
################################

wky_brown <- brewer.pal(n = 8, 
                        name = 'YlOrBr')[8]

wky_orange <- brewer.pal(n = 8, 
                         name = 'YlOrBr')[6]

wky_yellow <- brewer.pal(n = 8, 
                         name = 'YlOrBr')[4]

wky_colors <- c("palette" = "YlOrBr",
                "wky_brown" = wky_brown,
                "wky_orange" = wky_orange,
                "wky_yellow" = wky_yellow)

##################################
#### SCOTLAND - COLOR PALETTE ####
##################################

# Palette: https://coolors.co/48652c-a3b92f-8eb8e2-8c6e96-391a24-b6ac9d

# Designed from photo of Isle of Skye taken on honeymoon. 
# Reference: https://app.box.com/s/z8rllokndlccxte1m6c6s012usvpf986

sco_green <- "#48652C"

sco_lime <- "#A3B92F"

sco_blue <- "#8EB8E2"

sco_magenta <- "#8C6E96"

sco_brown <- "#391A24"

sco_khaki <- "#B6AC9D"

sco_colors <- c("sco_green" = sco_green,
                "sco_lime" = sco_lime,
                "sco_blue" = sco_blue,
                "sco_magenta" = sco_magenta,
                "sco_brown" = sco_brown,
                "sco_khaki" = sco_khaki)

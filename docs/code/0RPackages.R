##------------------------------------------------------------------------------
#  Loading R Packages
##------------------------------------------------------------------------------
Packages <- 
    c(
        "flexdashboard"
      , "tidytable"
      , "tidyverse"
      , "ggthemes"
      , "plotly"
      , "DT"
    )

Packages[!(Packages %in% installed.packages()[ ,"Package"])]

if (!require("pacman")) install.packages("pacman")
pacman::p_load(Packages, character.only = TRUE)
# pacman::p_up(update = TRUE, ask = FALSE)

##------------------------------------------------------------------------------
#                             The End
##------------------------------------------------------------------------------

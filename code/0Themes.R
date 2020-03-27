##------------------------------------------------------------------------------
#  Custom Themes
##------------------------------------------------------------------------------
myTheme1 <- function() {
  theme_igray() + 
    theme(
      plot.title      = element_text(hjust = 0.5)
      , plot.subtitle   = element_text(hjust = 0.5)
      , axis.text.x     = element_text(angle = 90, hjust = 0.95, vjust = 0.5, face = "bold")
      , axis.text.y     = element_text(face = "bold")
      , strip.text.x    = element_text(face = "bold")
      , axis.title.x    = element_text(face = "bold")
      , axis.title.y    = element_text(face = "bold")
    )
}

##------------------------------------------------------------------------------
#                             The End
##------------------------------------------------------------------------------

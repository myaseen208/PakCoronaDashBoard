##----Set-WD----
setwd("/media/myaseen208/Documents/MYaseen208/Consultancy_at_UAF/Mr._M._Yaseen/2017-09-14RPackages/PakCoronaDashBoard/PakCoronaDashboard")

##----Packages----
library(flexdashboard)
library(tidyverse)
library(plotly)
library(ggthemes)
library(DT)


##----Parameters----
confirmed_color <- "purple"
active_color    <- "#1f77b4"
recovered_color <- "forestgreen"
death_color     <- "red"


##----PakCoronaData----
PakCoronaData <- 
  readxl::read_xlsx("Pakistan.xlsx") %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(NewCases = Confirmed - lag(Confirmed, default = 0)) 


##----PakCoronaData12----
tb1 <-
  PakCoronaData %>% 
  pivot_longer(cols = Confirmed:NewCases, names_to = "Var", values_to = "Cases")
  
txtb1 <- highlight_key(tb1)

widgetstb1 <- 
  bscols(
    widths = c(12, 12, 12)
    #, filter_select("Var", "Var", txtb1, ~Var)
    , filter_checkbox("Var", "Var", txtb1, ~Var, inline = TRUE)
  )

bscols(
  widths = c(4, 8)
  , widgetstb1
  , plot_ly(txtb1, x = ~Date, y = ~Cases, showlegend = FALSE) %>%
    add_lines(color = ~Var, colors = "black")
)

bscols(
  widths = c(4, 8)
  , widgetstb1
  , ggplotly(Plot1) %>%
    add_lines(color = ~Var, colors = "black")
)

ggplotly(Plot1)

Plot1 <- 
 ggplot(data = txtb1, mapping = aes(x = Date, y = Cases, color = Var)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0),  breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Cases"
  ) +
  theme_igray() + 
  scale_colour_tableau() +
  theme(
    plot.title      = element_text(hjust = 0.5)
    , plot.subtitle   = element_text(hjust = 0.5)
    , axis.text.x     = element_text(angle = 90, hjust = 0.95, vjust = 0.5, face = "bold")
    , axis.text.y     = element_text(face = "bold")
    , strip.text.x    = element_text(face = "bold")
    , axis.title.x    = element_text(face = "bold")
    , axis.title.y    = element_text(face = "bold")
  )


##----PakConfirmed----
# paste(format(tail(PakCoronaData$Confirmed, 1), big.mark = ","), " (",  format(Sys.time(),  "%b %d, %Y at %X"), ")", sep = "")
valueBox(
    value   = format(tail(PakCoronaData$Confirmed, 1), big.mark = ",")
  , caption = "Total Confirmed Cases"
  , icon    = "fas fa-user-md"
  , color   = confirmed_color
)


##----PakActive----
valueBox(
   value = paste(format(tail(PakCoronaData$Active, 1), big.mark = ","), " (", round(100 * tail(PakCoronaData$Active, 1) / tail(PakCoronaData$Confirmed, 1), 1), "%)", sep = "")
  , caption = "Active Cases"
  , icon    = "fas fa-ambulance"
  , color   = active_color
)


##----PakRecovered----
valueBox(
  value = paste(format(tail(PakCoronaData$Recovered, 1), big.mark = ","), " (",
                round(100 * tail(PakCoronaData$Recovered, 1) / tail(PakCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Recovered Cases"
  , icon = "fas fa-heartbeat"
  , color = recovered_color
)


##----PakDeaths----
valueBox(
  value = paste(format(tail(PakCoronaData$Deaths, 1), big.mark = ","), " (",
                round(100 * tail(PakCoronaData$Deaths, 1) / tail(PakCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Death Cases"
  , icon    = "fas fa-heart-broken"
  , color   = death_color
)


##----PakConfirmedPlot1----
PakConfirmedPlot1 <- 
  ggplot(data = PakCoronaData, mapping = aes(x = Date, y = Confirmed)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PakCoronaData$Confirmed)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Confirmed Cases"
  ) +
  theme_igray() + 
  scale_colour_tableau() +
  theme(
    plot.title      = element_text(hjust = 0.5)
    , plot.subtitle   = element_text(hjust = 0.5)
    , axis.text.x     = element_text(angle = 90, hjust = 0.95, vjust = 0.5, face = "bold")
    , axis.text.y     = element_text(face = "bold")
    , strip.text.x    = element_text(face = "bold")
    , axis.title.x    = element_text(face = "bold")
    , axis.title.y    = element_text(face = "bold")
  )

# PakConfirmedPlot1
ggplotly(PakConfirmedPlot1)


##----PakActivePlot1----
PakActivePlot1 <- 
  ggplot(data = PakCoronaData, mapping = aes(x = Date, y = Active)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PakCoronaData$Active)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Active Cases"
  ) +
  theme_igray() + 
  scale_colour_tableau() +
  theme(
    plot.title      = element_text(hjust = 0.5)
    , plot.subtitle   = element_text(hjust = 0.5)
    , axis.text.x     = element_text(angle = 90, hjust = 0.95, vjust = 0.5, face = "bold")
    , axis.text.y     = element_text(face = "bold")
    , strip.text.x    = element_text(face = "bold")
    , axis.title.x    = element_text(face = "bold")
    , axis.title.y    = element_text(face = "bold")
  )

# PakActivePlot1
ggplotly(PakActivePlot1)


##----PakNewPlot1----
PakNewPlot1 <- 
  ggplot(data = PakCoronaData, mapping = aes(x = Date, y = NewCases)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PakCoronaData$NewCases)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "New Cases"
  ) +
  theme_igray() + 
  scale_colour_tableau() +
  theme(
    plot.title      = element_text(hjust = 0.5)
    , plot.subtitle   = element_text(hjust = 0.5)
    , axis.text.x     = element_text(angle = 90, hjust = 0.95, vjust = 0.5, face = "bold")
    , axis.text.y     = element_text(face = "bold")
    , strip.text.x    = element_text(face = "bold")
    , axis.title.x    = element_text(face = "bold")
    , axis.title.y    = element_text(face = "bold")
    , plot.caption    = element_text(color = "green", face = "italic")
  )

ggplotly(PakNewPlot1)


##----PakRecoveredPlot1----
PakRecoveredPlot1 <- 
  ggplot(data = PakCoronaData, mapping = aes(x = Date, y = Recovered)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PakCoronaData$Recovered)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Recovered Cases"
  ) +
  theme_igray() + 
  scale_colour_tableau() +
  theme(
    plot.title      = element_text(hjust = 0.5)
    , plot.subtitle   = element_text(hjust = 0.5)
    , axis.text.x     = element_text(angle = 90, hjust = 0.95, vjust = 0.5, face = "bold")
    , axis.text.y     = element_text(face = "bold")
    , strip.text.x    = element_text(face = "bold")
    , axis.title.x    = element_text(face = "bold")
    , axis.title.y    = element_text(face = "bold")
    , plot.caption    = element_text(color = "green", face = "italic")
  )

ggplotly(PakRecoveredPlot1)

##----PakDeathsPlot1----
PakDeathsPlot1 <- 
  ggplot(data = PakCoronaData, mapping = aes(x = Date, y = Deaths)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PakCoronaData$Deaths)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Noumber of Deaths"
  ) +
  theme_igray() + 
  scale_colour_tableau() +
  theme(
    plot.title      = element_text(hjust = 0.5)
    , plot.subtitle   = element_text(hjust = 0.5)
    , axis.text.x     = element_text(angle = 90, hjust = 0.95, vjust = 0.5, face = "bold")
    , axis.text.y     = element_text(face = "bold")
    , strip.text.x    = element_text(face = "bold")
    , axis.title.x    = element_text(face = "bold")
    , axis.title.y    = element_text(face = "bold")
    , plot.caption    = element_text(color = "green", face = "italic")
  )

ggplotly(PakDeathsPlot1)

##----PakCoronaData1----
PakCoronaData %>% 
  arrange(desc(Date)) %>% 
  datatable(rownames = FALSE)

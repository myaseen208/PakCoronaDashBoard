##----Set-WD----
setwd("/media/myaseen208/Documents/MYaseen208/Consultancy_at_UAF/Mr._M._Yaseen/2017-09-14RPackages/PakCoronaDashBoard/PakCoronaDashboard")

##----GBPackages----
library(flexdashboard)
library(tidyverse)
library(plotly)
library(ggthemes)
library(DT)


##----GBParameters----
confirmed_color <- "purple"
active_color    <- "#1f77b4"
recovered_color <- "forestgreen"
death_color     <- "red"


##----GBCoronaData----
GBCoronaData <-
  readr::read_csv("data/07GB.csv") %>%
  mutate(Date = as.Date(Date)) %>% 
  mutate(NewCases = Confirmed - lag(Confirmed, default = 0)) 


##----GBConfirmed----
# paste(format(tail(GBCoronaData$Confirmed, 1), big.mark = ","), " (",  format(Sys.time(),  "%b %d, %Y at %X"), ")", sep = "")
valueBox(
    value   = format(tail(GBCoronaData$Confirmed, 1), big.mark = ",")
  , caption = "Total Confirmed Cases"
  , icon    = "fas fa-user-md"
  , color   = confirmed_color
)


##----GBActive----
valueBox(
   value = paste(format(tail(GBCoronaData$Active, 1), big.mark = ","), " (", round(100 * tail(GBCoronaData$Active, 1) / tail(GBCoronaData$Confirmed, 1), 1), "%)", sep = "")
  , caption = "Active Cases"
  , icon    = "fas fa-ambulance"
  , color   = active_color
)


##----GBRecovered----
valueBox(
  value = paste(format(tail(GBCoronaData$Recovered, 1), big.mark = ","), " (",
                round(100 * tail(GBCoronaData$Recovered, 1) / tail(GBCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Recovered Cases"
  , icon = "fas fa-heartbeat"
  , color = recovered_color
)


##----GBDeaths----
valueBox(
  value = paste(format(tail(GBCoronaData$Deaths, 1), big.mark = ","), " (",
                round(100 * tail(GBCoronaData$Deaths, 1) / tail(GBCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Death Cases"
  , icon    = "fas fa-heart-broken"
  , color   = death_color
)


##----GBConfirmedPlot1----
GBConfirmedPlot1 <- 
  ggplot(data = GBCoronaData, mapping = aes(x = Date, y = Confirmed)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(GBCoronaData$Confirmed)*1.1), breaks = scales::pretty_breaks(8)) +
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

# GBConfirmedPlot1
ggplotly(GBConfirmedPlot1)


##----GBActivePlot1----
GBActivePlot1 <- 
  ggplot(data = GBCoronaData, mapping = aes(x = Date, y = Active)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(GBCoronaData$Active)*1.1), breaks = scales::pretty_breaks(8)) +
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

# GBActivePlot1
ggplotly(GBActivePlot1)


##----GBNewPlot1----
GBNewPlot1 <- 
  ggplot(data = GBCoronaData, mapping = aes(x = Date, y = NewCases)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(GBCoronaData$NewCases)*1.1), breaks = scales::pretty_breaks(8)) +
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

ggplotly(GBNewPlot1)


##----GBRecoveredPlot1----
GBRecoveredPlot1 <- 
  ggplot(data = GBCoronaData, mapping = aes(x = Date, y = Recovered)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(GBCoronaData$Recovered)*1.1), breaks = scales::pretty_breaks(8)) +
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

ggplotly(GBRecoveredPlot1)

##----GBDeathsPlot1----
GBDeathsPlot1 <- 
  ggplot(data = GBCoronaData, mapping = aes(x = Date, y = Deaths)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(GBCoronaData$Deaths)*1.1), breaks = scales::pretty_breaks(8)) +
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

ggplotly(GBDeathsPlot1)

##----GBCoronaData1----
GBCoronaData %>% 
  arrange(desc(Date)) %>% 
  datatable(rownames = FALSE)

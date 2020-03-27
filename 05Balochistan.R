##----Set-WD----
setwd("/media/myaseen208/Documents/MYaseen208/Consultancy_at_UAF/Mr._M._Yaseen/2017-09-14RPackages/PakCoronaDashBoard/PakCoronaDashboard")

##----BalochistanPackages----
library(flexdashboard)
library(tidyverse)
library(plotly)
library(ggthemes)
library(DT)


##----BalochistanParameters----
confirmed_color <- "purple"
active_color    <- "#1f77b4"
recovered_color <- "forestgreen"
death_color     <- "red"


##----BalochistanCoronaData----
BalochistanCoronaData <- 
  readr::read_csv("data/05Balochistan.csv") %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(NewCases = Confirmed - lag(Confirmed, default = 0)) 


##----BalochistanConfirmed----
# paste(format(tail(BalochistanCoronaData$Confirmed, 1), big.mark = ","), " (",  format(Sys.time(),  "%b %d, %Y at %X"), ")", sep = "")
valueBox(
    value   = format(tail(BalochistanCoronaData$Confirmed, 1), big.mark = ",")
  , caption = "Total Confirmed Cases"
  , icon    = "fas fa-user-md"
  , color   = confirmed_color
)


##----BalochistanActive----
valueBox(
   value = paste(format(tail(BalochistanCoronaData$Active, 1), big.mark = ","), " (", round(100 * tail(BalochistanCoronaData$Active, 1) / tail(BalochistanCoronaData$Confirmed, 1), 1), "%)", sep = "")
  , caption = "Active Cases"
  , icon    = "fas fa-ambulance"
  , color   = active_color
)


##----BalochistanRecovered----
valueBox(
  value = paste(format(tail(BalochistanCoronaData$Recovered, 1), big.mark = ","), " (",
                round(100 * tail(BalochistanCoronaData$Recovered, 1) / tail(BalochistanCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Recovered Cases"
  , icon = "fas fa-heartbeat"
  , color = recovered_color
)


##----BalochistanDeaths----
valueBox(
  value = paste(format(tail(BalochistanCoronaData$Deaths, 1), big.mark = ","), " (",
                round(100 * tail(BalochistanCoronaData$Deaths, 1) / tail(BalochistanCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Death Cases"
  , icon    = "fas fa-heart-broken"
  , color   = death_color
)


##----BalochistanConfirmedPlot1----
BalochistanConfirmedPlot1 <- 
  ggplot(data = BalochistanCoronaData, mapping = aes(x = Date, y = Confirmed)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(BalochistanCoronaData$Confirmed)*1.1), breaks = scales::pretty_breaks(8)) +
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

# BalochistanConfirmedPlot1
ggplotly(BalochistanConfirmedPlot1)


##----BalochistanActivePlot1----
BalochistanActivePlot1 <- 
  ggplot(data = BalochistanCoronaData, mapping = aes(x = Date, y = Active)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(BalochistanCoronaData$Active)*1.1), breaks = scales::pretty_breaks(8)) +
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

# BalochistanActivePlot1
ggplotly(BalochistanActivePlot1)


##----BalochistanNewPlot1----
BalochistanNewPlot1 <- 
  ggplot(data = BalochistanCoronaData, mapping = aes(x = Date, y = NewCases)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(BalochistanCoronaData$NewCases)*1.1), breaks = scales::pretty_breaks(8)) +
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

ggplotly(BalochistanNewPlot1)


##----BalochistanRecoveredPlot1----
BalochistanRecoveredPlot1 <- 
  ggplot(data = BalochistanCoronaData, mapping = aes(x = Date, y = Recovered)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(BalochistanCoronaData$Recovered)*1.1), breaks = scales::pretty_breaks(8)) +
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

ggplotly(BalochistanRecoveredPlot1)

##----BalochistanDeathsPlot1----
BalochistanDeathsPlot1 <- 
  ggplot(data = BalochistanCoronaData, mapping = aes(x = Date, y = Deaths)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(BalochistanCoronaData$Deaths)*1.1), breaks = scales::pretty_breaks(8)) +
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

ggplotly(BalochistanDeathsPlot1)

##----BalochistanCoronaData1----
BalochistanCoronaData %>% 
  arrange(desc(Date)) %>% 
  datatable(rownames = FALSE)

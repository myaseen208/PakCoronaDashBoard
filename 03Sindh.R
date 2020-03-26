##----Set-WD----
setwd("/media/myaseen208/Documents/MYaseen208/Consultancy_at_UAF/Mr._M._Yaseen/2017-09-14RPackages/PakCoronaDashBoard/PakCoronaDashboard")

##----SindhPackages----
library(flexdashboard)
library(tidyverse)
library(plotly)
library(ggthemes)
library(DT)


##----SindhParameters----
confirmed_color <- "purple"
active_color    <- "#1f77b4"
recovered_color <- "forestgreen"
death_color     <- "red"


##----SindhCoronaData----
SindhCoronaData <- 
  readxl::read_xlsx("data/03Sindh.xlsx") %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(NewCases = Confirmed - lag(Confirmed, default = 0)) 


##----SindhConfirmed----
# paste(format(tail(SindhCoronaData$Confirmed, 1), big.mark = ","), " (",  format(Sys.time(),  "%b %d, %Y at %X"), ")", sep = "")
valueBox(
    value   = format(tail(SindhCoronaData$Confirmed, 1), big.mark = ",")
  , caption = "Total Confirmed Cases"
  , icon    = "fas fa-user-md"
  , color   = confirmed_color
)


##----SindhActive----
valueBox(
   value = paste(format(tail(SindhCoronaData$Active, 1), big.mark = ","), " (", round(100 * tail(SindhCoronaData$Active, 1) / tail(SindhCoronaData$Confirmed, 1), 1), "%)", sep = "")
  , caption = "Active Cases"
  , icon    = "fas fa-ambulance"
  , color   = active_color
)


##----SindhRecovered----
valueBox(
  value = paste(format(tail(SindhCoronaData$Recovered, 1), big.mark = ","), " (",
                round(100 * tail(SindhCoronaData$Recovered, 1) / tail(SindhCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Recovered Cases"
  , icon = "fas fa-heartbeat"
  , color = recovered_color
)


##----SindhDeaths----
valueBox(
  value = paste(format(tail(SindhCoronaData$Deaths, 1), big.mark = ","), " (",
                round(100 * tail(SindhCoronaData$Deaths, 1) / tail(SindhCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Death Cases"
  , icon    = "fas fa-heart-broken"
  , color   = death_color
)


##----SindhConfirmedPlot1----
SindhConfirmedPlot1 <- 
  ggplot(data = SindhCoronaData, mapping = aes(x = Date, y = Confirmed)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(SindhCoronaData$Confirmed)*1.1), breaks = scales::pretty_breaks(8)) +
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

# SindhConfirmedPlot1
ggplotly(SindhConfirmedPlot1)


##----SindhActivePlot1----
SindhActivePlot1 <- 
  ggplot(data = SindhCoronaData, mapping = aes(x = Date, y = Active)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(SindhCoronaData$Active)*1.1), breaks = scales::pretty_breaks(8)) +
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

# SindhActivePlot1
ggplotly(SindhActivePlot1)


##----SindhNewPlot1----
SindhNewPlot1 <- 
  ggplot(data = SindhCoronaData, mapping = aes(x = Date, y = NewCases)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(SindhCoronaData$NewCases)*1.1), breaks = scales::pretty_breaks(8)) +
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

ggplotly(SindhNewPlot1)


##----SindhRecoveredPlot1----
SindhRecoveredPlot1 <- 
  ggplot(data = SindhCoronaData, mapping = aes(x = Date, y = Recovered)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(SindhCoronaData$Recovered)*1.1), breaks = scales::pretty_breaks(8)) +
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

ggplotly(SindhRecoveredPlot1)

##----SindhDeathsPlot1----
SindhDeathsPlot1 <- 
  ggplot(data = SindhCoronaData, mapping = aes(x = Date, y = Deaths)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(SindhCoronaData$Deaths)*1.1), breaks = scales::pretty_breaks(8)) +
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

ggplotly(SindhDeathsPlot1)

##----SindhCoronaData1----
SindhCoronaData %>% 
  arrange(desc(Date)) %>% 
  datatable(rownames = FALSE)

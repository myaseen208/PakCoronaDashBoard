##----Set-WD----
setwd("/media/myaseen208/Documents/MYaseen208/Consultancy_at_UAF/Mr._M._Yaseen/2017-09-14RPackages/PakCoronaDashBoard/PakCoronaDashboard")

##----KPKPackages----
library(flexdashboard)
library(tidyverse)
library(plotly)
library(ggthemes)
library(DT)


##----KPKParameters----
confirmed_color <- "purple"
active_color    <- "#1f77b4"
recovered_color <- "forestgreen"
death_color     <- "red"


##----KPKCoronaData----
KPKCoronaData <- 
  readxl::read_xlsx("data/04KPK.xlsx") %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(NewCases = Confirmed - lag(Confirmed, default = 0)) 


##----KPKConfirmed----
# paste(format(tail(KPKCoronaData$Confirmed, 1), big.mark = ","), " (",  format(Sys.time(),  "%b %d, %Y at %X"), ")", sep = "")
valueBox(
    value   = format(tail(KPKCoronaData$Confirmed, 1), big.mark = ",")
  , caption = "Total Confirmed Cases"
  , icon    = "fas fa-user-md"
  , color   = confirmed_color
)


##----KPKActive----
valueBox(
   value = paste(format(tail(KPKCoronaData$Active, 1), big.mark = ","), " (", round(100 * tail(KPKCoronaData$Active, 1) / tail(KPKCoronaData$Confirmed, 1), 1), "%)", sep = "")
  , caption = "Active Cases"
  , icon    = "fas fa-ambulance"
  , color   = active_color
)


##----KPKRecovered----
valueBox(
  value = paste(format(tail(KPKCoronaData$Recovered, 1), big.mark = ","), " (",
                round(100 * tail(KPKCoronaData$Recovered, 1) / tail(KPKCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Recovered Cases"
  , icon = "fas fa-heartbeat"
  , color = recovered_color
)


##----KPKDeaths----
valueBox(
  value = paste(format(tail(KPKCoronaData$Deaths, 1), big.mark = ","), " (",
                round(100 * tail(KPKCoronaData$Deaths, 1) / tail(KPKCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Death Cases"
  , icon    = "fas fa-heart-broken"
  , color   = death_color
)


##----KPKConfirmedPlot1----
KPKConfirmedPlot1 <- 
  ggplot(data = KPKCoronaData, mapping = aes(x = Date, y = Confirmed)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(KPKCoronaData$Confirmed)*1.1), breaks = scales::pretty_breaks(8)) +
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

# KPKConfirmedPlot1
ggplotly(KPKConfirmedPlot1)


##----KPKActivePlot1----
KPKActivePlot1 <- 
  ggplot(data = KPKCoronaData, mapping = aes(x = Date, y = Active)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(KPKCoronaData$Active)*1.1), breaks = scales::pretty_breaks(8)) +
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

# KPKActivePlot1
ggplotly(KPKActivePlot1)


##----KPKNewPlot1----
KPKNewPlot1 <- 
  ggplot(data = KPKCoronaData, mapping = aes(x = Date, y = NewCases)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(KPKCoronaData$NewCases)*1.1), breaks = scales::pretty_breaks(8)) +
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

ggplotly(KPKNewPlot1)


##----KPKRecoveredPlot1----
KPKRecoveredPlot1 <- 
  ggplot(data = KPKCoronaData, mapping = aes(x = Date, y = Recovered)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(KPKCoronaData$Recovered)*1.1), breaks = scales::pretty_breaks(8)) +
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

ggplotly(KPKRecoveredPlot1)

##----KPKDeathsPlot1----
KPKDeathsPlot1 <- 
  ggplot(data = KPKCoronaData, mapping = aes(x = Date, y = Deaths)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(KPKCoronaData$Deaths)*1.1), breaks = scales::pretty_breaks(8)) +
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

ggplotly(KPKDeathsPlot1)

##----KPKCoronaData1----
KPKCoronaData %>% 
  arrange(desc(Date)) %>% 
  datatable(rownames = FALSE)

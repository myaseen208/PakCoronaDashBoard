##----Set-WD----
setwd("/media/myaseen208/Documents/MYaseen208/Consultancy_at_UAF/Mr._M._Yaseen/2017-09-14RPackages/PakCoronaDashboard/PakCoronaDashboard")

##----PakistanPackages----
source("code/0RPackages.R")
source("code/0Themes.R")


##----PakistanParameters----
confirmed_color <- "purple"
active_color    <- "#1f77b4"
recovered_color <- "forestgreen"
death_color     <- "red"


##----PakistanCoronaData----
load("data/PakCoronaData.RData")

PakistanCoronaData <- 
  PakCoronaData %>%
  dt_filter(Region == "Pakistan") %>% 
  dt_mutate(NewCases = Confirmed - lag(Confirmed, default = 0)) 


##----PakistanConfirmed----
# paste(format(tail(PakistanCoronaData$Confirmed, 1), big.mark = ","), " (",  format(Sys.time(),  "%b %d, %Y at %X"), ")", sep = "")
valueBox(
    value   = format(tail(PakistanCoronaData$Confirmed, 1), big.mark = ",")
  , caption = "Total Confirmed Cases"
  , icon    = "fas fa-user-md"
  , color   = confirmed_color
)


##----PakistanActive----
valueBox(
   value = paste(format(tail(PakistanCoronaData$Active, 1), big.mark = ","), " (", round(100 * tail(PakistanCoronaData$Active, 1) / tail(PakistanCoronaData$Confirmed, 1), 1), "%)", sep = "")
  , caption = "Active Cases"
  , icon    = "fas fa-ambulance"
  , color   = active_color
)


##----PakistanRecovered----
valueBox(
  value = paste(format(tail(PakistanCoronaData$Recovered, 1), big.mark = ","), " (",
                round(100 * tail(PakistanCoronaData$Recovered, 1) / tail(PakistanCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Recovered Cases"
  , icon = "fas fa-heartbeat"
  , color = recovered_color
)


##----PakistanDeaths----
valueBox(
  value = paste(format(tail(PakistanCoronaData$Deaths, 1), big.mark = ","), " (",
                round(100 * tail(PakistanCoronaData$Deaths, 1) / tail(PakistanCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Death Cases"
  , icon    = "fas fa-heart-broken"
  , color   = death_color
)


##----PakistanConfirmedPlot1----
PakistanConfirmedPlot1 <- 
  ggplot(data = PakistanCoronaData, mapping = aes(x = Date, y = Confirmed)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PakistanCoronaData$Confirmed)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Confirmed Cases"
  ) +
  scale_colour_tableau() +
  myTheme1()


# PakistanConfirmedPlot1
ggplotly(PakistanConfirmedPlot1)


##----PakistanActivePlot1----
PakistanActivePlot1 <- 
  ggplot(data = PakistanCoronaData, mapping = aes(x = Date, y = Active)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PakistanCoronaData$Active)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Active Cases"
  ) +
  scale_colour_tableau() +
  myTheme1()

# PakistanActivePlot1
ggplotly(PakistanActivePlot1)


##----PakistanNewPlot1----
PakistanNewPlot1 <- 
  ggplot(data = PakistanCoronaData, mapping = aes(x = Date, y = NewCases)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PakistanCoronaData$NewCases)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "New Cases"
  ) +
  scale_colour_tableau() +
  myTheme1()

ggplotly(PakistanNewPlot1)


##----PakistanRecoveredPlot1----
PakistanRecoveredPlot1 <- 
  ggplot(data = PakistanCoronaData, mapping = aes(x = Date, y = Recovered)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PakistanCoronaData$Recovered)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Recovered Cases"
  ) +
  scale_colour_tableau() +
  myTheme1()

ggplotly(PakistanRecoveredPlot1)

##----PakistanDeathsPlot1----
PakistanDeathsPlot1 <- 
  ggplot(data = PakistanCoronaData, mapping = aes(x = Date, y = Deaths)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PakistanCoronaData$Deaths)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Noumber of Deaths"
  ) +
  scale_colour_tableau() +
  myTheme1()

ggplotly(PakistanDeathsPlot1)

##----PakistanCoronaData1----
PakistanCoronaData %>% 
  arrange(desc(Date)) %>% 
  datatable(rownames = FALSE)

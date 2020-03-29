##----Set-WD----
setwd("/media/myaseen208/Documents/MYaseen208/Consultancy_at_UAF/Mr._M._Yaseen/2017-09-14RPackages/PakCoronaDashBoard/PakCoronaDashboard")

##----BalochistanPackages----
source("R/0RPackages.R")
source("R/0Themes.R")

##----BalochistanParameters----
confirmed_color <- "purple"
active_color    <- "#1f77b4"
recovered_color <- "forestgreen"
death_color     <- "red"


##----BalochistanCoronaData----
BalochistanCoronaData <- 
  PakCoronaData %>%
  dt_filter(Region == "Balochistan") %>% 
  dt_mutate(NewCases = Confirmed - lag(Confirmed, default = 0)) 


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
  scale_colour_tableau() +
  myTheme1()


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
  scale_colour_tableau() +
  myTheme1()


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
  scale_colour_tableau() +
  myTheme1()


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
  scale_colour_tableau() +
  myTheme1()


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
  scale_colour_tableau() +
  myTheme1()


ggplotly(BalochistanDeathsPlot1)

##----BalochistanCoronaData1----
BalochistanCoronaData %>% 
  arrange(desc(Date)) %>% 
  datatable(rownames = FALSE)

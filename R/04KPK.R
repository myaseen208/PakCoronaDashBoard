##----Set-WD----
setwd("/media/myaseen208/Documents/MYaseen208/Consultancy_at_UAF/Mr._M._Yaseen/2017-09-14RPackages/PakCoronaDashBoard/PakCoronaDashboard")

##----KPKPackages----
source("R/0RPackages.R")
source("R/0Themes.R")

##----KPKParameters----
confirmed_color <- "purple"
active_color    <- "#1f77b4"
recovered_color <- "forestgreen"
death_color     <- "red"


##----KPKCoronaData----
load("data/PakCoronaData.RData")

KPKCoronaData <- 
  PakCoronaData %>%
  dt_filter(Region == "KPK") %>% 
  dt_mutate(NewCases = Confirmed - lag(Confirmed, default = 0)) 


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
  scale_colour_tableau() +
  myTheme1()


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
  scale_colour_tableau() +
  myTheme1()


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
  scale_colour_tableau() +
  myTheme1()


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
  scale_colour_tableau() +
  myTheme1()


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
  scale_colour_tableau() +
  myTheme1()


ggplotly(KPKDeathsPlot1)

##----KPKCoronaData1----
KPKCoronaData %>% 
  arrange(desc(Date)) %>% 
  datatable(rownames = FALSE)

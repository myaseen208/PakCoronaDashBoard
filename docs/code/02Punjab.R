##----Set-WD----
setwd("/media/myaseen208/Documents/MYaseen208/Consultancy_at_UAF/Mr._M._Yaseen/2017-09-14RPackages/PakCoronaDashBoard/PakCoronaDashboard")

##----PunjabPackages----
source("code/0RPackages.R")
source("code/0Themes.R")

##----PunjabParameters----
confirmed_color <- "purple"
active_color    <- "#1f77b4"
recovered_color <- "forestgreen"
death_color     <- "red"


##----PunjabCoronaData----
PunjabCoronaData <- 
  readr::read_csv("data/02Punjab.csv") %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(NewCases = Confirmed - lag(Confirmed, default = 0)) 


##----PunjabConfirmed----
# paste(format(tail(PunjabCoronaData$Confirmed, 1), big.mark = ","), " (",  format(Sys.time(),  "%b %d, %Y at %X"), ")", sep = "")
valueBox(
    value   = format(tail(PunjabCoronaData$Confirmed, 1), big.mark = ",")
  , caption = "Total Confirmed Cases"
  , icon    = "fas fa-user-md"
  , color   = confirmed_color
)


##----PunjabActive----
valueBox(
   value = paste(format(tail(PunjabCoronaData$Active, 1), big.mark = ","), " (", round(100 * tail(PunjabCoronaData$Active, 1) / tail(PunjabCoronaData$Confirmed, 1), 1), "%)", sep = "")
  , caption = "Active Cases"
  , icon    = "fas fa-ambulance"
  , color   = active_color
)


##----PunjabRecovered----
valueBox(
  value = paste(format(tail(PunjabCoronaData$Recovered, 1), big.mark = ","), " (",
                round(100 * tail(PunjabCoronaData$Recovered, 1) / tail(PunjabCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Recovered Cases"
  , icon = "fas fa-heartbeat"
  , color = recovered_color
)


##----PunjabDeaths----
valueBox(
  value = paste(format(tail(PunjabCoronaData$Deaths, 1), big.mark = ","), " (",
                round(100 * tail(PunjabCoronaData$Deaths, 1) / tail(PunjabCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Death Cases"
  , icon    = "fas fa-heart-broken"
  , color   = death_color
)


##----PunjabConfirmedPlot1----
PunjabConfirmedPlot1 <- 
  ggplot(data = PunjabCoronaData, mapping = aes(x = Date, y = Confirmed)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PunjabCoronaData$Confirmed)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Confirmed Cases"
  ) +
  scale_colour_tableau() +
  myTheme1()

# PunjabConfirmedPlot1
ggplotly(PunjabConfirmedPlot1)


##----PunjabActivePlot1----
PunjabActivePlot1 <- 
  ggplot(data = PunjabCoronaData, mapping = aes(x = Date, y = Active)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PunjabCoronaData$Active)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Active Cases"
  ) +
  scale_colour_tableau() +
  myTheme1()

# PunjabActivePlot1
ggplotly(PunjabActivePlot1)


##----PunjabNewPlot1----
PunjabNewPlot1 <- 
  ggplot(data = PunjabCoronaData, mapping = aes(x = Date, y = NewCases)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PunjabCoronaData$NewCases)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "New Cases"
  ) +
  scale_colour_tableau() +
  myTheme1()

ggplotly(PunjabNewPlot1)


##----PunjabRecoveredPlot1----
PunjabRecoveredPlot1 <- 
  ggplot(data = PunjabCoronaData, mapping = aes(x = Date, y = Recovered)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PunjabCoronaData$Recovered)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Recovered Cases"
  ) +
  scale_colour_tableau() +
  myTheme1()

ggplotly(PunjabRecoveredPlot1)

##----PunjabDeathsPlot1----
PunjabDeathsPlot1 <- 
  ggplot(data = PunjabCoronaData, mapping = aes(x = Date, y = Deaths)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(PunjabCoronaData$Deaths)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Noumber of Deaths"
  ) +
  scale_colour_tableau() +
  myTheme1()

ggplotly(PunjabDeathsPlot1)

##----PunjabCoronaData1----
PunjabCoronaData %>% 
  arrange(desc(Date)) %>% 
  datatable(rownames = FALSE)

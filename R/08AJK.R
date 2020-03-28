##----Set-WD----
setwd("/media/myaseen208/Documents/MYaseen208/Consultancy_at_UAF/Mr._M._Yaseen/2017-09-14RPackages/PakCoronaDashBoard/PakCoronaDashboard")

##----AJKPackages----
source("R/0RPackages.R")
source("R/0Themes.R")


##----AJKParameters----
confirmed_color <- "purple"
active_color    <- "#1f77b4"
recovered_color <- "forestgreen"
death_color     <- "red"


##----AJKCoronaData----
load("data/PakCoronaData.RData")

AJKCoronaData <- 
  PakCoronaData %>%
  dt_filter(Region == "AJK") %>% 
  dt_mutate(NewCases = Confirmed - lag(Confirmed, default = 0)) 


##----AJKConfirmed----
# paste(format(tail(AJKCoronaData$Confirmed, 1), big.mark = ","), " (",  format(Sys.time(),  "%b %d, %Y at %X"), ")", sep = "")
valueBox(
    value   = format(tail(AJKCoronaData$Confirmed, 1), big.mark = ",")
  , caption = "Total Confirmed Cases"
  , icon    = "fas fa-user-md"
  , color   = confirmed_color
)


##----AJKActive----
valueBox(
   value = paste(format(tail(AJKCoronaData$Active, 1), big.mark = ","), " (", round(100 * tail(AJKCoronaData$Active, 1) / tail(AJKCoronaData$Confirmed, 1), 1), "%)", sep = "")
  , caption = "Active Cases"
  , icon    = "fas fa-ambulance"
  , color   = active_color
)


##----AJKRecovered----
valueBox(
  value = paste(format(tail(AJKCoronaData$Recovered, 1), big.mark = ","), " (",
                round(100 * tail(AJKCoronaData$Recovered, 1) / tail(AJKCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Recovered Cases"
  , icon = "fas fa-heartbeat"
  , color = recovered_color
)


##----AJKDeaths----
valueBox(
  value = paste(format(tail(AJKCoronaData$Deaths, 1), big.mark = ","), " (",
                round(100 * tail(AJKCoronaData$Deaths, 1) / tail(AJKCoronaData$Confirmed, 1), 1), 
                "%)", sep = "")
  , caption = "Death Cases"
  , icon    = "fas fa-heart-broken"
  , color   = death_color
)


##----AJKConfirmedPlot1----
AJKConfirmedPlot1 <- 
  ggplot(data = AJKCoronaData, mapping = aes(x = Date, y = Confirmed)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(AJKCoronaData$Confirmed)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Confirmed Cases"
  ) +
  scale_colour_tableau() +
  myTheme1()

# AJKConfirmedPlot1
ggplotly(AJKConfirmedPlot1)


##----AJKActivePlot1----
AJKActivePlot1 <- 
  ggplot(data = AJKCoronaData, mapping = aes(x = Date, y = Active)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(AJKCoronaData$Active)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Active Cases"
  ) +
  scale_colour_tableau() +
  myTheme1()

# AJKActivePlot1
ggplotly(AJKActivePlot1)


##----AJKNewPlot1----
AJKNewPlot1 <- 
  ggplot(data = AJKCoronaData, mapping = aes(x = Date, y = NewCases)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(AJKCoronaData$NewCases)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "New Cases"
  ) +
  scale_colour_tableau() +
  myTheme1()

ggplotly(AJKNewPlot1)


##----AJKRecoveredPlot1----
AJKRecoveredPlot1 <- 
  ggplot(data = AJKCoronaData, mapping = aes(x = Date, y = Recovered)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(AJKCoronaData$Recovered)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Recovered Cases"
  ) +
  scale_colour_tableau() +
  myTheme1()

ggplotly(AJKRecoveredPlot1)

##----AJKDeathsPlot1----
AJKDeathsPlot1 <- 
  ggplot(data = AJKCoronaData, mapping = aes(x = Date, y = Deaths)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%b %d", date_breaks = "day") +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, max(AJKCoronaData$Deaths)*1.1), breaks = scales::pretty_breaks(8)) +
  labs( 
    x = "Date"
    , y = "Noumber of Deaths"
  ) +
  scale_colour_tableau() +
  myTheme1()

ggplotly(AJKDeathsPlot1)

##----AJKCoronaData1----
AJKCoronaData %>% 
  arrange(desc(Date)) %>% 
  datatable(rownames = FALSE)

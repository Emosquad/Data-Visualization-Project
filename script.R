library(tidyverse)
library(dplyr)
library(comprehenr)
library(readxl)
library(purrr)
library(stringr)
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(DT)

date_range = 1:124 + as.Date("2022-02-26")
date_str = c()
for (i in 1:124) {
  date_str <- c(date_str, paste0(
    as.character(format(date_range[i], format = "%Y")),
    as.character(format(date_range[i], format = "%m")),
    as.character(format(date_range[i], format = "%d"))))
}

# City data set
path = "./district/"
file_name = paste0(path,date_str[1],"_city.xlsx")
df_city <- read_xlsx(file_name)
for (s in 2:124) {
  file_name = paste0(path,date_str[s],"_city.xlsx")
  df_seperate = read_xlsx(file_name)
  df_city <- dplyr::bind_rows(df_city, df_seperate)
}
df_city[is.na(df_city)] = 0
colnames(df_city) <-
  c('Date',
    'Total',
    'Positive',
    'Asymptomatic',
    'Asymptomatic_to_Positive',
    'Deaths')

df_city$Date = as.Date(df_city$Date)
p = ggplot(data = df_city,mapping = aes(x = Deaths, y = Date))+
    geom_boxplot()+
    theme_classic()
p = p + geom_boxplot(data = df_city, mapping = aes(x = Positive, y = Date))
p = p + geom_boxplot(data = df_city, mapping = aes(x = Asymptomatic, y = Date))
ggplotly(p)

# District data set
file_name = paste0(path,date_str[20],"_dis.xlsx")
df_district <- read_xlsx(file_name)
for (s in 21:124) {
  file_name = paste0(path,date_str[s],"_dis.xlsx")
  df_seperate = read_xlsx(file_name)
  df_district <- dplyr::bind_rows(df_district, df_seperate)
}
df_district[is.na(df_district)] = 0
colnames(df_district) <-
  c('Districts', 'Positive', 'Asymptomatic', 'Date')
c=df_district%>%
  select(Positive,Districts,Asymptomatic)%>%
  group_by(Districts)%>%
  summarise(posavg = mean(Positive),poscount=sum(Positive),poshigh=max(Positive),asyavg = mean(Asymptomatic), asycount=sum(Asymptomatic),asyhigh=max(Asymptomatic))
c$posavg=round(c$posavg,1)
c$asyavg=round(c$asyavg,1)

c_top= c%>% arrange(desc(poscount))%>% dplyr::select( Districts ) %>%
  as.matrix() %>%
  as.character

c_plot = df_district %>% filter(Districts %in% c_top) %>% mutate(Districts = factor(Districts, levels = c_top)) %>%
  arrange(Districts)

c_top_a = c %>% arrange(desc(asycount)) %>%
  dplyr::select(Districts) %>%
  as.matrix() %>%
  as.character

c_plot_a = df_district %>% filter(Districts %in% c_top_a) %>%
  mutate(Districts = factor(Districts, levels = c_top_a)) %>%
  arrange(Districts)

# Individual data set
path = "./community/"
file_name = paste0(path,date_str[8],"_geo.xlsx")
df_ind <- read_xlsx(file_name)
df_ind$?Ա? <- NULL
df_ind$???? <- NULL
df_ind$???? <- NULL
df_ind = df_ind %>%
  mutate(???? = as.numeric(????)) %>%
  mutate(γ?? = as.numeric(γ??))
for (s in 8:94) {
  file_name = paste0(path,date_str[s],"_geo.xlsx")
  df_seperate = read_xlsx(file_name)
  df_seperate$?Ա? <- NULL
  df_seperate$???? <- NULL
  df_seperate$???? <- NULL
  df_seperate = df_seperate %>%
    mutate(???? = as.numeric(????)) %>%
    mutate(γ?? = as.numeric(γ??))
  df_ind <- dplyr::bind_rows(df_ind, df_seperate)
}


loc_data = df_ind %>%
  filter(as.Date(Date, '%Y-%m-%d') == as.Date("2022/03/06", "%Y/%m/%d")) %>%
  na.omit() %>%
  group_by(lng = round(Longtitude, 3), lat = round(Latitude, 3)) %>%
  summarise(N = n()) %>%
  mutate(latL = lat - 0.0005) %>%
  mutate(latH = lat + 0.0005) %>%
  mutate(lngL = lng - 0.0005) %>%
  mutate(lngH = lng + 0.0005)

m = loc_data %>% leaflet() %>% addTiles() %>%
  setView(121.5, 31.2, zoom = 10) %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addLayersControl(baseGroups = c("Toner", "OSM"),
                   options = layersControlOptions(collapsed = FALSE))%>%
  addRectangles(
    lng1 =  ~ lngL,
    lat1 =  ~ latL,
    lng2 =  ~ lngH,
    lat2 =  ~ latH,
    fillOpacity = ~ N / 150,
    fillColor = "red",
    label = ~ N
  )

loc_data = df_ind %>%
  filter(as.Date(Date, '%Y-%m-%d') == as.Date("2022/03/09", "%Y/%m/%d")) %>%
  na.omit() %>%
  group_by(lng = round(Longtitude, 3), lat = round(Latitude, 3)) %>%
  summarise(N = n()) %>%
  mutate(latL = lat - 0.0005) %>%
  mutate(latH = lat + 0.0005) %>%
  mutate(lngL = lng - 0.0005) %>%
  mutate(lngH = lng + 0.0005)

m = m %>% addRectangles(
  lng1 =  loc_data$lngL,
  lat1 =  loc_data$latL,
  lng2 =  loc_data$lngH,
  lat2 =  loc_data$latH,
  fillOpacity = loc_data$N / 150,
  fillColor = "red",
  label = loc_data$N
)

m


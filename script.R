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

# District data set
file_name = paste0(path,date_str[20],"_dis.xlsx")
df_district <- read_xlsx(file_name)
for (s in 21:124) {
  file_name = paste0(path,date_str[s],"_dis.xlsx")
  df_seperate = read_xlsx(file_name)
  df_district <- dplyr::bind_rows(df_district, df_seperate)
}
df_district[is.na(df_district)] = 0

# Individual data set
path = "./community/"
file_name = paste0(path,date_str[8],"_geo.xlsx")
df_ind <- read_xlsx(file_name)
df_ind$性别 <- NULL
df_ind$年龄 <- NULL
df_ind$类型 <- NULL
df_ind = df_ind %>%
  mutate(经度 = as.numeric(经度)) %>%
  mutate(纬度 = as.numeric(纬度))
for (s in 8:94) {
  file_name = paste0(path,date_str[s],"_geo.xlsx")
  df_seperate = read_xlsx(file_name)
  df_seperate$性别 <- NULL
  df_seperate$年龄 <- NULL
  df_seperate$类型 <- NULL
  df_seperate = df_seperate %>%
    mutate(经度 = as.numeric(经度)) %>%
    mutate(纬度 = as.numeric(纬度))
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


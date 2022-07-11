#plot1
df_city%>%
  ggplot(mapping = aes(x=Date, y = Total))+
  geom_line()


#plot2
df_district%>%
  ggplot(mapping = aes(x=Date,y=Positive, color = Districts))+
  geom_line()

#plot3
df_district%>%
  ggplot(mapping = aes(x=Date,y=Asymptomatic, color = Districts))+
  geom_line()

#plot4
loc_data = df_ind %>%
  group_by(lng = round(Longtitude, 3), lat = round(Latitude, 3)) %>%
  summarise(N = n()) %>%
  mutate(latL = lat - 0.0005) %>%
  mutate(latH = lat + 0.0005) %>%
  mutate(lngL = lng - 0.0005) %>%
  mutate(lngH = lng + 0.0005)

m = loc_data %>% leaflet() %>% addTiles() %>%
  setView(121, 31, zoom = 12) %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addLayersControl(baseGroups = c("Toner", "OSM"),
                   options = layersControlOptions(collapsed = FALSE))%>%
  addRectangles(
    lng1 =  ~ lngL,
    lat1 =  ~ latL,
    lng2 =  ~ lngH,
    lat2 =  ~ latH,
    #fillOpacity = ~ N / 150,
    #opacity = 0,
    fillColor = "red",
    #label = ~ N
  )

#Table1
datatable(df_city, rownames = FALSE)

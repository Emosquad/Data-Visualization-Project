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

ui <- dashboardPage(
  dashboardHeader(title = "Shanghai COVID-19 Lockdown in 2022",titleWidth = 280),
  dashboardSidebar(width=280,sidebarMenu(
    width = 280,
    menuItem("Overview",tabName="intro",icon = icon("info-circle")),
    menuItem("COVID-19 Trend", tabName = "page1", icon = icon("chart-area")),
    menuItem("Statistics", icon = icon("server"),
             menuSubItem("Trend by Districts", tabName = "page2"),
             menuSubItem("Asympotomatic Trend by Districts", tabName = "page3")),
    menuItem("Maps", icon = icon("globe"),
             menuSubItem("Dot Map", tabName = "page4"),
             menuSubItem("Interactive Map", tabName = "page8")),
    menuItem("Data", icon = icon("database"),
             menuSubItem("Data of City", tabName = "page5"),
             menuSubItem("Data of Districts", tabName = "page6"),
             menuSubItem("Data of Individuals", tabName = "page7"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName='intro',
              fluidRow(
                column(width = 12,
                       box(
                         title = "New Omicron Variant Found in China, Shanghai Carries Out New Rounds of Covid-19 Testing", width = NULL, status="primary",solidHeader=TRUE,collapsible = TRUE,align="center",
                         HTML(
                           '<iframe width="65%" height="300"
                  src="https://www.youtube.com/embed/nmIJXUV8WCw"
                  frameborder="0" allowfullscreen></iframe>'
                         )),
                       box(
                         title= "Introduction", width = NULL, solidHeader=TRUE,status = "primary",collapsible = TRUE,
                         div(img(src="SH1.jpg",height=250,width=450), style="text-align: center;"),br(),
                         tags$div("Since the first discovery of the Covid-19 in 2019, there have been many mutations. 
                         The Omicron variant has the characteristics of a ",tags$strong("relatively hidden transmission path, 
                         a long incubation period, and a high asymptomatic infection rate"),". Since March 2022, 
                         the Omicron variant has been on the rise. As a mega city and the largest urban economy of China, 
                         Shanghai has severe and complicated forms of epidemic prevention and control and 
                         experienced a ",tags$strong("two-month rigid lockdown"), "from the beginning of April to the end of May. 
                         The full-scale lockdown of Shanghai severely harmed the economy and strained the nerves 
                         and affected livelihoods of its 25 million residents and eroded the public's trust in authorities. 
                         Moreover, it will have far-reaching economic and social costs. 
                         As the arrival of BA.5 coronavirus variants, Shanghai fears the second lockdown as 
                         Chinese government battles BA.5 outbreaks. Our Project aims to collect the specialized datasets of 
                         2022 Shanghai COVID pandemic and provides data support for understanding the epidemic situation, 
                         controlling, and predicting the epidemic situation.",style = "font-size:15px")
                         
                         
                       ),
                       box(
                         title= "Data Source",
                         width = NULL, solidHeader=TRUE, status = "primary",collapsible = TRUE,
                         tags$div("Our datasets are retrieved from",tags$strong("Model Whale"), "website and contain epidemic data based on city, districts and individuals.",style="font-size:15px"),br(),
                         
                         a("Navigate to Model Whale",href="https://www.heywhale.com/auth/login?redirect=%2Fmw%2Fworkspace%2Findex", targets="_blank")
                       ),
                       box(
                         title= "Main Content",
                         width = NULL, solidHeader=TRUE, status = "primary",collapsible = TRUE,
                         tags$span(tags$li(tags$strong("Covid-19 Trend:"),"A plot of confirmed cases and asymptomatic cases trend based on the two-month data."),br(),
                                   tags$li(tags$strong("Statistics:"),"A plot of confirmed cases and asymptomatic cases based on different districts."), br(),
                                   tags$li(tags$strong("Map of Test Positive:"), "An interactive heat map of Covid cases distribution based on different locations in the map of Shanghai, 
                                  and visualization of severity of the covid situation based on colors."),br(),
                                   tags$li(tags$strong("Data:"), "Raw datasets."),style="font-size:16px")
                       ),
                       box(title= "Team",width=NULL, solidHeader=TRUE, status = "primary",collapsible = TRUE,
                           tags$div(tags$strong("Our team members are all from MSBA program at Carey Business School, Johns Hopkins University:"), style = "font-size:15px"),br(),
                           tags$li(tags$strong("Member 1: Jialu Ni - jni15@jh.edu")),br(),
                           tags$li(tags$strong("Member 2: Yifan Wu - wuyifan2017@gmail.com")),br(),
                           tags$li(tags$strong("Member 3: Yijia Wu - wuyifan2017@gmail.com")),br(),
                           tags$li(tags$strong("Member 4: Yiming Ding - yding56@jh.edu")),br(),
                           tags$li(tags$strong("Member 5: Zihui Chen - zchen109@jh.edu")),br(),
                           tags$div("Feel free to contact us via email if you have any concerns related to this web.", style = "font-size:15px")
                       )
                )
                
                
              )),
      tabItem(
        tabName = "page1",
        checkboxGroupInput("checkGroup", label = "Select the graph",
                           choices = list(
                             "Positive" = 1,
                             "Asymptomatic" = 2
                           ),
                           selected = 1),
        plotlyOutput("plot1", height = 500)
      ),
      tabItem(tabName = "page2",
              sliderInput("year", "Year:", min = 2014, max = 2020, value = 1, 
                          step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
              plotOutput("plot2")
      ),
      tabItem(tabName = "page3",
              tabItem(tabName = "page2",
                      selectInput("select", label = "District:", choices = list("JiaDing District" ="嘉定", "FengXian District"="奉贤",
                                                                                "BaoShan District"="宝山", "ChongMing District"="崇明",
                                                                                "XuHui District"="徐汇","PuTuo District"="普陀",
                                                                                "YangPu District"="杨浦","SongJiang District"="松江",
                                                                                "PuDong District"="浦东","HongKou District"="虹口",
                                                                                "JingShan District"="金山","ChangNing District"="长宁",
                                                                                "MinHang District"="闵行","QingPu District"="青浦",
                                                                                "JingAn District"="静安","HuangPu District"="黄浦"
                      ), selected = 1),
                      hr(),
                      fluidRow(column(9,plotlyOutput("plot3")
                      ))
                      
              ),
      ),
      tabItem(tabName = "page4",
                sliderInput(
                  "date1",
                  "Date:",
                  min = as.Date("2022/03/06", "%Y/%m/%d"),
                  max = as.Date("2022/05/31", "%Y/%m/%d"),
                  value = as.Date("2022/03/06", "%Y/%m/%d"),
                  step = 1,
                  animate = animationOptions(interval = 2000, loop = FALSE)
                ),
                checkboxInput("Agg", label = "Show Cumulative map", value = FALSE),
                leafletOutput("myMap", width = "100%")
              ), 
      tabItem(tabName = "page5",
              dataTableOutput("myTable1")
      ),
      tabItem(tabName = "page6",
              dataTableOutput("myTable2")
      ),
      tabItem(tabName = "page7",
              dataTableOutput("myTable3")
      ),
      tabItem(tabName = "page8",
              sliderInput(
                "date2",
                "Date:",
                min = as.Date("2022/03/06", "%Y/%m/%d"),
                max = as.Date("2022/05/31", "%Y/%m/%d"),
                value = as.Date("2022/03/06", "%Y/%m/%d"),
                step = 1,
                animate = animationOptions(interval = 2000, loop = FALSE)
              ),
              checkboxInput("Agg", label = "Show Cumulative map", value = FALSE),
              leafletOutput("myMap2", width = "100%")
      )
      
    )
    
  )
)


server <- function(input, output, session){
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
  colnames(df_city) <- c('Date', 'Total', 'Positive', 'Asymptomatic', 
                         'Asymptomatic_to_Positive', 'Deaths')
  
  # District data set
  file_name = paste0(path,date_str[20],"_dis.xlsx")
  df_district <- read_xlsx(file_name)
  for (s in 21:124) {
    file_name = paste0(path,date_str[s],"_dis.xlsx")
    df_seperate = read_xlsx(file_name)
    df_district <- dplyr::bind_rows(df_district, df_seperate)
  }
  df_district[is.na(df_district)] = 0
  colnames(df_district) <- c('Districts', 'Positive', 'Asymptomatic', 'Date')
  
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
  for (s in 9:94) {
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
  colnames(df_ind) <- c('Date', 'Address', 'Districts', 'Longtitude', 'Latitude')
  
  output$plot1 = renderPlotly({
    Sys.setlocale("LC_TIME","English")
    df_city$Date = as.Date(df_city$Date, format = "d%m%Y")
    
    
    if(!is.null(input$checkGroup)){
    if (length(input$checkGroup) == 1 & as.numeric(input$checkGroup)==1) {
      p = ggplot(data = df_city, aes(x = Date, y = Positive)) + geom_area(fill =
                                                                            "#F08080") +
        scale_x_date(limits = as.Date(c('2022-03-01', '2022-04-13'))) +
        labs(title = "COVID-19 Trend in Shanghai City 2022",
             y = "Positive") +
        theme_classic()
    }
    
    else if(length(input$checkGroup) == 1 & as.numeric(input$checkGroup)==2){
      p = ggplot(data = df_city, aes(x = Date, y = Asymptomatic)) +
        geom_area(fill = "#FFAFCC") +
        labs(title = "COVID-19 Trend in Shanghai City 2022",
             y = "Asymptomatic") +
        scale_x_date(limits = as.Date(c('2022-03-01', '2022-04-13'))) +
        theme_classic()
    }
    
    else if(length(input$checkGroup) == 2){
      p = ggplot(data = df_city, aes(x = Date, y = Asymptomatic)) +
        geom_area(fill = "#FFAFCC") +
        labs(title = "COVID-19 Trend in Shanghai City 2022",
             y = "Asymptomatic") +
        theme_classic()
      p = p + geom_area(data = df_city, aes(x = Date, y = Positive), fill =
                          "#F08080") +
        scale_x_date(limits = as.Date(c('2022-03-01', '2022-04-13'))) +
        labs(y = "Total")
    }
    }
    else {
      
    }
    
    
  }) 
  
  output$plot2 = renderPlotly({
    
    
  })
  
  output$plot3 = renderPlotly({
    pos=df_district %>%
      filter(df_district$Districts == input$select) %>%
      ggplot(mapping = aes(x=Date, y=Positive))+
      geom_line(color="tomato2")+
      coord_cartesian(ylim = c(0,400))+
      annotate("text", x=as.POSIXct(0.00, origin = "2022-06-18"),y=200,label=input$select,color="grey",size=12)+
      labs(
        title = "Positive Trend By Districts 2022")+
      theme(axis.ticks.y = element_blank(), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank())
    ggplotly(pos)
    
  })
  
  output$myMap = renderLeaflet({
    if(input$Agg == TRUE) {
      loc_data = df_ind %>%
        filter(as.Date(Date, '%Y-%m-%d') <= input$date1) %>%
        na.omit() %>%
        group_by(lng = round(Longtitude, 3), lat = round(Latitude, 3)) %>%
        summarise(N = n()) %>%
        mutate(latL = lat - 0.0005) %>%
        mutate(latH = lat + 0.0005) %>%
        mutate(lngL = lng - 0.0005) %>%
        mutate(lngH = lng + 0.0005)
      
      m = m = loc_data %>% leaflet() %>% addTiles() %>%
        setView(121.5, 31.2, zoom = 10) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("Toner", "OSM"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addRectangles(
          lng1 =  loc_data$lngL,
          lat1 =  loc_data$latL,
          lng2 =  loc_data$lngH,
          lat2 =  loc_data$latH,
          fillOpacity = loc_data$N / 150,
          color = "blue",
          label = loc_data$N
        )
    }else {
      loc_data = df_ind %>%
        filter(as.Date(Date, '%Y-%m-%d') == input$date1) %>%
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
    }
    
  })
  
  output$myTable1 = renderDataTable({
    return(datatable(df_city, rownames= FALSE))
  })
  
  output$myTable2 = renderDataTable({
    return(datatable(df_district, rownames= FALSE))
  })
  
  output$myTable3 = renderDataTable({
    return(datatable(df_ind, rownames= FALSE))
  })
  
  output$myMap2 = renderLeaflet(
    {
      if(input$Agg == TRUE) {
        loc_data = df_ind %>%
          filter(as.Date(Date, '%Y-%m-%d') <= input$date2) %>%
          na.omit()
        
        m = m = loc_data %>% leaflet() %>% addTiles() %>%
          setView(121.5, 31.2, zoom = 10) %>%
          addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
          addLayersControl(baseGroups = c("Toner", "OSM"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addMarkers(
            lng =  ~ Longtitude,
            lat =  ~ Latitude,
            popup = ~paste0(
              "Date: ",  ~Date, br(),
              "Address: ", ~Address, br(),
              "Longtitude: ", ~longtitude, br(),
              "Latitude: ", ~latitude
            ),
            clusterOptions = markerClusterOptions()
          )
      }else {
        loc_data = df_ind %>%
          filter(as.Date(Date, '%Y-%m-%d') == input$date2) %>%
          na.omit()
        
        m = loc_data %>% leaflet() %>% addTiles() %>%
          setView(121.5, 31.2, zoom = 10) %>%
          addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
          addLayersControl(baseGroups = c("Toner", "OSM"),
                           options = layersControlOptions(collapsed = FALSE))%>%
          addMarkers(
            lng =  loc_data$Longtitude,
            lat =  loc_data$Latitude,
            popup = ~paste0(
              "Date: ",  loc_data$Date, br(),
              "Address: ", loc_data$Address, br(),
              "Longtitude: ", loc_data$Longtitude, br(),
              "Latitude: ", loc_data$Latitude
            ),
            clusterOptions = markerClusterOptions()
          )
      }
    }
  )
  
  output$myMap3 = renderLeaflet(
    {
      if(input$Agg == TRUE) {
        loc_data = df_ind %>%
          filter(as.Date(Date, '%Y-%m-%d') <= input$date2) %>%
          na.omit()
        
        m = m = loc_data %>% leaflet() %>% addTiles() %>%
          setView(121.5, 31.2, zoom = 10) %>%
          addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
          addLayersControl(baseGroups = c("Toner", "OSM"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addMarkers(
            lng =  ~ Longtitude,
            lat =  ~ Latitude,
            popup = ~paste0(
              "Date: ",  ~Date, br(),
              "Address: ", ~Address, br(),
              "Longtitude: ", ~longtitude, br(),
              "Latitude: ", ~latitude
            ),
            clusterOptions = markerClusterOptions()
          )
      }else {
        loc_data = df_ind %>%
          filter(as.Date(Date, '%Y-%m-%d') == input$date2) %>%
          na.omit()
        
        m = loc_data %>% leaflet() %>% addTiles() %>%
          setView(121.5, 31.2, zoom = 10) %>%
          addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
          addLayersControl(baseGroups = c("Toner", "OSM"),
                           options = layersControlOptions(collapsed = FALSE))%>%
          addMarkers(
            lng =  loc_data$Longtitude,
            lat =  loc_data$Latitude,
            popup = ~paste0(
              "Date: ",  loc_data$Date, br(),
              "Address: ", loc_data$Address, br(),
              "Longtitude: ", loc_data$Longtitude, br(),
              "Latitude: ", loc_data$Latitude
            ),
            clusterOptions = markerClusterOptions()
          )
      }
    }
  )
  
}

shinyApp(ui = ui, server = server)
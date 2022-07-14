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
library(shinyjs)
library(highcharter)
library(shinyWidgets)

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$strong("Shanghai COVID-19 Lockdown in 2022"),
    titleWidth = 280
  ),
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      width = 280,
      menuItem(
        "Overview",
        tabName = "intro",
        icon = icon("info-circle")
      ),
      menuItem(
        "COVID-19 Trend",
        tabName = "page1",
        icon = icon("chart-area")
      ),
      menuItem(
        "Statistics",
        icon = icon("server"),
        tabName = "page2"
      ),
      menuItem(
        "Maps",
        icon = icon("globe"),
        menuSubItem("Dot Map", tabName = "page4"),
        menuSubItem("Interactive Map", tabName = "page8")
      ),
      menuItem(
        "Data",
        icon = icon("database"),
        menuSubItem("Data of City", tabName = "page5"),
        menuSubItem("Data of Districts", tabName = "page6"),
        menuSubItem("Data of Individuals", tabName = "page7")
      ),
      menuItem(
        "Source Code for App",
        icon = icon("send", lib = 'glyphicon'),
        href = "https://github.com/Emosquad/Data-Visualization-Project"
      )
    )
  ),
  dashboardBody(
    tags$head(tags$style(
      HTML(
        '
                                /* logo */
                                .skin-black .main-header .logo {
                                background-color: #8C92AC;
                                }

                                /* logo when hovered */
                                .skin-black .main-header .logo:hover {
                                background-color: #8C92AC;
                                }

                                /* navbar (rest of the header) */
                                .skin-black .main-header .navbar {
                                background-color:#8C92AC;
                                }

                                /* main sidebar */
                                .skin-black .main-sidebar {
                                background-color: #494E65;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #A7ABBF;
                                }

                                /* other links in the sidebarmenu */
                                .skin-black .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #494E65;
                                color: #DCDEE6;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #8C92AC;
                                }
                                /* toggle button when hovered  */
                                .skin-black .main-header .navbar .sidebar-toggle:hover{
                                background-color: #8C92AC;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #A7ABBF;
                                }'
      )
    ),
    
    tags$style(
      HTML(
        ".box.box-solid.box-primary>.box-header {color:#fff;
              background:#494E65
                    }
                    .box.box-solid.box-primary{
                    border-bottom-color:#494E65;
                    border-left-color:#494E65;
                    border-right-color:#494E65;
                    border-top-color:#494E65;
                    background:#A7ABBF}
                              "
      )
    )),
    tabItems(
      tabItem(tabName = 'intro',
              fluidRow(
                column(
                  width = 12,
                  
                  
                  box(
                    title = tags$strong(
                      "New Omicron Variant Found in China, Shanghai Carries Out New Rounds of Covid-19 Testing"
                    ),
                    status = "primary",
                    width = NULL,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    align = "center",
                    HTML(
                      '<iframe width="65%" height="300"
                  src="https://www.youtube.com/embed/nmIJXUV8WCw"
                  frameborder="0" allowfullscreen></iframe>'
                    )
                  ),
                  box(
                    title = tags$strong("Introduction"),
                    status = "primary",
                    width = NULL,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(6, br(),
                           div(
                             img(
                               src = "SH1.jpg",
                               height = 300,
                               width = 450
                             ), style = "margin-left: auto;"
                           )),
                    column(
                      6,
                      tags$span(
                        "Since the first discovery of the Covid-19 in 2019, there have been many mutations.
                         The Omicron variant has the characteristics of a ",
                        tags$strong(
                          "relatively hidden transmission path,
                         a long incubation period, and a high asymptomatic infection rate"
                        ),
                        ". Since March 2022,
                         the Omicron variant has been on the rise. As a mega city and the largest urban economy of China,
                         Shanghai has severe and complicated forms of epidemic prevention and control and
                         experienced a ",
                        tags$strong("two-month rigid lockdown"),
                        "from the beginning of April to the end of May.
                         The full-scale lockdown of Shanghai severely harmed the economy and strained the nerves
                         and affected livelihoods of its 25 million residents and eroded the public’s trust in authorities.
                         Moreover, it will have far-reaching economic and social costs.
                         As the arrival of BA.5 coronavirus variants, Shanghai fears the second lockdown as
                         Chinese government battles BA.5 outbreaks. Our Project aims to collect the specialized datasets of
                         2022 Shanghai COVID pandemic and provides data support for",
                        tags$strong(
                          "understanding,
                         controlling, and predicting the epidemic situation."
                        ),
                        style = "font-size:15px"
                      )
                    )
                    
                    
                  ),
                  box(
                    title = tags$strong("Data Source"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    tags$div(
                      "Our datasets are retrieved from",
                      tags$strong("Model Whale"),
                      "website and contain epidemic data based on city, districts and individuals.",
                      style = "font-size:15px"
                    ),
                    
                    a("Navigate to Model Whale", href =
                        "https://www.heywhale.com/auth/login?redirect=%2Fmw%2Fworkspace%2Findex", targets =
                        "_blank")
                  ),
                  box(
                    title = tags$strong("Main Content"),
                    width = NULL,
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    tags$span(
                      tags$li(
                        tags$strong("Covid-19 Trend:"),
                        "A plot of confirmed cases and asymptomatic cases trend based on the two-month data."
                      ),
                      tags$li(
                        tags$strong("Statistics:"),
                        "A plot of confirmed cases and asymptomatic cases based on different districts."
                      ),
                      tags$li(
                        tags$strong("Map of Test Positive:"),
                        "An interactive heat map of Covid cases distribution based on different locations in the map of Shanghai,
                                  and visualization of severity of the covid situation based on colors."
                      ),
                      tags$li(tags$strong("Data:"), "Raw datasets."),
                      style = "font-size:15px"
                    )
                  ),
                  box(
                    title = tags$strong("Team"),
                    status = "primary",
                    width = NULL,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    
                    valueBox(
                      "Jialu Ni",
                      "jni15@jh.edu",
                      width = 4,
                      icon = icon("contact-card", lib = "glyphicon"),
                      color = "olive"
                    ),
                    valueBox(
                      "Yifan Wu",
                      "wuyifan2017@gmail.com",
                      width = 4,
                      icon = icon("contact-card", lib = "glyphicon"),
                      color = "maroon"
                    ),
                    valueBox(
                      "Yijia Wu",
                      "ywu214@jh.edu",
                      width = 4,
                      icon = icon("contact-card", lib = "glyphicon"),
                      color = "purple"
                    ),
                    valueBox(
                      "Yiming Ding",
                      "yding56@jh.edu",
                      width = 4,
                      icon = icon("contact-card", lib = "glyphicon"),
                      color = "orange"
                    ),
                    valueBox(
                      "Zihui Chen",
                      "zchen109@jh.edu",
                      width = 4,
                      icon = icon("contact-card", lib = "glyphicon"),
                      color = "light-blue"
                    ),
                    br(),
                    tags$div(
                      tags$strong(
                        "Our team members are all from MSBA program at Carey Business School, Johns Hopkins University."
                      ),
                      style = "font-size:15px"
                    ),
                    
                    
                    
                    tags$div(
                      "Feel free to contact us via email if you have any concerns related to this App.",
                      style = "font-size:15px"
                    )
                  )
                )
                
                
              )),
      tabItem(tabName = "page1",
              fluidRow(column(
                12,
                box(
                  title = tags$strong("COVID-19 Trend in Shanghai City 2022"),
                  width = NULL,
                  solidHeader = TRUE,
                  status = "primary",
                  collapsible = TRUE,
                  
                  prettyCheckboxGroup(
                    inputId = "checkGroup",
                    label = h3(tags$strong("Select the Graph", style =
                                             "font-size:16px")),
                    choices = list("Positive" = 1,
                                   "Asymptomatic" = 2),
                    icon = icon("times"),
                    animation = "tada",
                    selected = c(1, 2)
                  ),
                  plotlyOutput("plot1", height = 500)
                  
                )
              ))
              ,
              fluidRow(
                box(
                  title = tags$strong("Rates and Counts"),
                  width = NULL,
                  solidHeader = TRUE,
                  status = "primary",
                  collapsible = TRUE,
                  column(6,
                         plotlyOutput("plot1.2", height = 300)),
                  column(6,
                         plotlyOutput("plot1.3", height = 300))
                )
              )),
      tabItem(
        tabName = "page2",
        h2("Trend by Districts Plot"),
        fluidRow(box(highchartOutput("chart1")),
                 box(highchartOutput("chart2"))),
        fluidRow(
          box(
            title = "Conclusion",
            solidHeader = TRUE,
            status = "warning",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            h5("#######")
          )
        ),
        fluidRow(column(
          4,
          selectInput(
            "select",
            label = "District:",
            choices = list(
              "嘉定JiaDing District" ="嘉定", "奉贤FengXian District"="奉贤",
              "宝山BaoShan District"="宝山", "崇明ChongMing District"="崇明",
              "徐汇XuHui District"="徐汇","普陀PuTuo District"="普陀",
              "杨浦YangPu District"="杨浦","松江SongJiang District"="松江",
              "浦东PuDong District"="浦东","虹口HongKou District"="虹口",
              "金山JingShan District"="金山","长宁ChangNing District"="长宁",
              "闵行MinHang District"="闵行","青浦QingPu District"="青浦",
              "静安JingAn District"="静安","黄浦HuangPu District"="黄浦"
            ),
            selected = 1,
            multiple = FALSE
          )
        )),
        h2(paste0("Positive")),
        fluidRow(
          valueBoxOutput("vbptotal") ,
          valueBoxOutput("vbpavg") ,
          valueBoxOutput("vbphigh")
        ),
        h2(paste0("Asymptomatic")),
        fluidRow(
          valueBoxOutput("vbatotal") ,
          valueBoxOutput("vbaavg") ,
          valueBoxOutput("vbahigh")
        ),
        fluidRow(box(highchartOutput("chart3")),
                 box(highchartOutput("chart4"))),
        fluidRow(
          box(
            title = "Result",
            solidHeader = TRUE,
            status = "danger",
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            h5("#######")
          )
        )
        
      ),
      tabItem(
        tabName = "page4",
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
              dataTableOutput("myTable1")),
      tabItem(tabName = "page6",
              dataTableOutput("myTable2")),
      tabItem(tabName = "page7",
              dataTableOutput("myTable3")),
      tabItem(
        tabName = "page8",
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


server <- function(input, output, session) {
  date_range = 1:124 + as.Date("2022-02-26")
  date_str = c()
  for (i in 1:124) {
    date_str <- c(date_str,
                  paste0(
                    as.character(format(date_range[i], format = "%Y")),
                    as.character(format(date_range[i], format = "%m")),
                    as.character(format(date_range[i], format = "%d"))
                  ))
  }
  
  # City data set
  path = "./district/"
  file_name = paste0(path, date_str[1], "_city.xlsx")
  df_city <- read_xlsx(file_name)
  for (s in 2:124) {
    file_name = paste0(path, date_str[s], "_city.xlsx")
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
  
  # District data set
  file_name = paste0(path, date_str[20], "_dis.xlsx")
  df_district <- read_xlsx(file_name)
  for (s in 21:124) {
    file_name = paste0(path, date_str[s], "_dis.xlsx")
    df_seperate = read_xlsx(file_name)
    df_district <- dplyr::bind_rows(df_district, df_seperate)
  }
  df_district[is.na(df_district)] = 0
  colnames(df_district) <-
    c('Districts', 'Positive', 'Asymptomatic', 'Date')
  df_district$Date<-as.Date(df_district$Date)
  c=df_district%>%
    select(Positive,Districts,Asymptomatic)%>%
    group_by(Districts)%>%
    summarise(posavg = mean(Positive),poscount=sum(Positive),poshigh=max(Positive),asyavg = mean(Asymptomatic), asycount=sum(Asymptomatic),asyhigh=max(Asymptomatic))
  c$posavg=round(c$posavg,1)
  c$asyavg=round(c$asyavg,1)
  
  c_top = c %>% arrange(desc(poscount)) %>%
    dplyr::select(Districts) %>%
    as.matrix() %>%
    as.character
  
  c_plot = df_district %>% filter(Districts %in% c_top) %>%
    mutate(Districts = factor(Districts, levels = c_top)) %>%
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
  file_name = paste0(path, date_str[8], "_geo.xlsx")
  df_ind <- read_xlsx(file_name)
  df_ind$性别  <- NULL
  df_ind$年龄  <- NULL
  df_ind$类型  <- NULL
  df_ind = df_ind %>%
    mutate(经度  = as.numeric(经度)) %>%
    mutate(纬度  = as.numeric(纬度))
  for (s in 9:94) {
    file_name = paste0(path, date_str[s], "_geo.xlsx")
    df_seperate = read_xlsx(file_name)
    df_seperate$性别  <- NULL
    df_seperate$年龄  <- NULL
    df_seperate$类型  <- NULL
    df_seperate = df_seperate %>%
      mutate(经度  = as.numeric(经度)) %>%
      mutate(纬度  = as.numeric(纬度))
    df_ind <- dplyr::bind_rows(df_ind, df_seperate)
  }
  colnames(df_ind) <-
    c('Date', 'Address', 'Districts', 'Longtitude', 'Latitude')
  
  output$plot1 = renderPlotly({
    Sys.setlocale("LC_TIME", "English")
    df_city$Date = as.Date(df_city$Date, format = "d%m%Y")
    
    
    if (!is.null(input$checkGroup)) {
      if (length(input$checkGroup) == 1) {
        if (as.numeric(input$checkGroup) == 1) {
          p = ggplot(data = df_city, aes(x = Date, y = Positive)) + geom_area(fill =
                                                                                "#F08080") +
            scale_x_date(limits = as.Date(c('2022-03-01', '2022-06-30'))) +
            labs(title = "COVID-19 Trend in Shanghai City 2022",
                 y = "Positive") +
            theme_classic()
        }
        
        else if (as.numeric(input$checkGroup) == 2) {
          p = ggplot(data = df_city, aes(x = Date, y = Asymptomatic)) +
            geom_area(fill = "#FFAFCC") +
            labs(title = "COVID-19 Trend in Shanghai City 2022",
                 y = "Asymptomatic") +
            scale_x_date(limits = as.Date(c('2022-03-01', '2022-06-30'))) +
            theme_classic()
        }
      }
      else  {
        p = ggplot(data = df_city, aes(x = Date, y = Asymptomatic)) +
          geom_area(fill = "#FFAFCC") +
          labs(title = "COVID-19 Trend in Shanghai City 2022",
               y = "Asymptomatic") +
          theme_classic()
        p = p + geom_area(data = df_city,
                          aes(x = Date, y = Positive),
                          fill =
                            "#F08080") +
          scale_x_date(limits = as.Date(c('2022-03-01', '2022-06-30'))) +
          labs(y = "Total")
      }
    }
    else {
      
    }
    
    
  })
  
  output$plot1.2 = renderPlotly({
    data_city = df_city %>%
      mutate(
        Asymptomatic_to_Positive_Rate = as.numeric(Asymptomatic_to_Positive) / as.numeric(Asymptomatic)
      ) %>%
      mutate(Death_Rate = as.numeric(Deaths) / as.numeric(Total)) %>%
      filter(!is.na(Asymptomatic_to_Positive_Rate)) %>%
      filter(!is.na(Death_Rate))
    
    p = ggplot(data = data_city,
               mapping = aes(x = Date, y = Asymptomatic_to_Positive_Rate)) +
      geom_line(color = "#FFAFCC") +
      theme_classic()
    p = p + geom_line(data = data_city,
                      aes(x = Date, y = Death_Rate),
                      color = "#F08080") +
      labs(title = "Asymptomatic to Positive/Death Rates",
           y = "Rates")
    ggplotly(p)
  })
  
  output$plot1.3 = renderPlotly({
    p = ggplot(data = df_city,
               mapping = aes(x = Date, y = Asymptomatic_to_Positive)) +
      geom_line(color = "#FFAFCC") +
      theme_classic()
    p = p + geom_line(data = df_city, aes(x = Date, y = Deaths), color = "#F08080") +
      labs(title = "Asymptomatic to Positive/Death",
           y = "Counts")
    ggplotly(p)
  })
  
  output$chart1 = renderHighchart({
    c_plot$Date<-as.Date(c_plot$Date)
      highchart()%>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank"))%>%
      hc_add_series(data = c_plot,
        mapping = hcaes(x = "Date", y = "Positive",group = "Districts"),
                    type = 'line',
                    visible = c(T, T, rep(F,length(c_top)-2)))%>%
      hc_title(text = "Positive Trend of Districts",
               margin = 10, align = "left",
               style = list(color = "#51B749", useHTML = TRUE))%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">{point.key}</span>'
      )%>%
      hc_legend(layout = "vertical", verticalAlign = "middle",
                align = "right", valueDecimals = 0)%>%
      hc_size(height = 430) %>%
      hc_xAxis(title = list(text = "Date"), type="datetime")
    
    
  })
  output$chart2 = renderHighchart({
    c_plot_a$Date<-as.Date(c_plot_a$Date)
    highchart()%>%
      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank"))%>%
      hc_add_series(data = c_plot_a,
                    mapping = hcaes(x = "Date", y = "Asymptomatic",group = "Districts"),
                    type = 'line',
                    visible = c(T, T, rep(F,length(c_top_a)-2)))%>%
      hc_title(text = "Positive Trend of Districts",
               margin = 10, align = "left",
               style = list(color = "#51B749", useHTML = TRUE))%>%
      hc_tooltip(table = TRUE,
                 sort = TRUE,
                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                       " {series.name}: {point.y}"),
                 headerFormat = '<span style="font-size: 13px">{point.key}</span>'
      )%>%
      hc_legend(layout = "vertical", verticalAlign = "middle",
                align = "right", valueDecimals = 0)%>%
      hc_size(height = 430) %>%
      hc_xAxis(title = list(text = "Date"), type="datetime")
    
    
  })
  ptotal<-reactive({
    c%>%
      filter(c$Districts==input$select)%>%
      select(poscount)
  })
  
  output$vbptotal <- renderValueBox({
    valueBox(
      value = ptotal(),
      subtitle = "Positive Total Amount ",
      icon = icon("chart-bar"),
      width = 4,
      color = "red",
      href = NULL)
    
  })
  
  pavg<-reactive({
    c%>%
      filter(c$Districts==input$select)%>%
      select(posavg)    
  }) 
  output$vbpavg <- renderValueBox({
    valueBox(
      value = pavg(),
      subtitle = "Positive Average Amount ",
      icon = icon("group"),
      width = 4,
      color = "blue",
      href = NULL)
    
  })
  phigh<-reactive({
    c%>%
      filter(c$Districts==input$select)%>%
      select(poshigh)    
  })
  output$vbphigh <- renderValueBox({
    valueBox(
      value = phigh(),
      subtitle = "Positive Highest Point ",
      icon = icon("exclamation-circle"),
      width = 4,
      color = "purple",
      href = NULL)
    
  })  
  atotal<-reactive({
    c%>%
      filter(c$Districts==input$select)%>%
      select(asycount)
  })
  
  output$vbatotal <- renderValueBox({
    valueBox(
      value = atotal(),
      subtitle = "Asymptomatic Total Amount ",
      icon = icon("chart-bar"),
      width = 4,
      color = "red",
      href = NULL)
    
  })
  
  aavg<-reactive({
    c%>%
      filter(c$Districts==input$select)%>%
      select(asyavg)    
  }) 
  output$vbaavg <- renderValueBox({
    valueBox(
      value = aavg(),
      subtitle = "Asymptomatic Average Amount ",
      icon = icon("group"),
      width = 4,
      color = "blue",
      href = NULL)
    
  })
  ahigh<-reactive({
    c%>%
      filter(c$Districts==input$select)%>%
      select(asyhigh)    
  })
  output$vbahigh <- renderValueBox({
    valueBox(
      value = ahigh(),
      subtitle = "Asymptomatic Highest Point ",
      icon = icon("exclamation-circle"),
      width = 4,
      color = "purple",
      href = NULL)
    
  }) 
  
  output$chart3 = renderHighchart({
    hchart(c,"column",hcaes(x=Districts,y=poscount),name="Total Positive",showInLegend = TRUE)%>%
      hc_add_series(c,type='column',hcaes(x=Districts,y=asycount), name = "Total Asypmtomatic",showInLegend = TRUE) %>% 
      hc_title(text = 'Total number of infections by district') %>%
      hc_yAxis(min = 0, title = list(text = "Number of Infections"))%>%
      hc_exporting(enabled = T)
  })
  
  output$chart4 = renderHighchart({
    hchart(c,"column",hcaes(x=Districts,y=posavg),name="Mean of Positive",showInLegend = TRUE)%>%
      hc_add_series(c,type='column',hcaes(x=Districts,y=asyavg), name = "Mean of Asypmtomatic",showInLegend = TRUE) %>% 
      hc_title(text = 'Average number of infections by district') %>%
      hc_yAxis(min = 0, title = list(text = "Number of Infections"))%>%
      hc_exporting(enabled = T)
  })  
  
  output$plot3 = renderPlotly({
    
  })
  
  output$myMap = renderLeaflet({
    if (input$Agg == TRUE) {
      loc_data = df_ind %>%
        filter(as.Date(Date, '%Y-%m-%d') <= input$date1) %>%
        na.omit() %>%
        group_by(lng = round(Longtitude, 3),
                 lat = round(Latitude, 3)) %>%
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
    } else {
      loc_data = df_ind %>%
        filter(as.Date(Date, '%Y-%m-%d') == input$date1) %>%
        na.omit() %>%
        group_by(lng = round(Longtitude, 3),
                 lat = round(Latitude, 3)) %>%
        summarise(N = n()) %>%
        mutate(latL = lat - 0.0005) %>%
        mutate(latH = lat + 0.0005) %>%
        mutate(lngL = lng - 0.0005) %>%
        mutate(lngH = lng + 0.0005)
      
      m = loc_data %>% leaflet() %>% addTiles() %>%
        setView(121.5, 31.2, zoom = 10) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("Toner", "OSM"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
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
    return(datatable(df_city, rownames = FALSE))
  })
  
  output$myTable2 = renderDataTable({
    return(datatable(df_district, rownames = FALSE))
  })
  
  output$myTable3 = renderDataTable({
    return(datatable(df_ind, rownames = FALSE))
  })
  
  output$myMap2 = renderLeaflet({
    if (input$Agg == TRUE) {
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
          popup = ~ paste0(
            "Date: ",
            ~ Date,
            br(),
            "Address: ",
            ~ Address,
            br(),
            "Longtitude: ",
            ~ longtitude,
            br(),
            "Latitude: ",
            ~ latitude
          ),
          clusterOptions = markerClusterOptions()
        )
    } else {
      loc_data = df_ind %>%
        filter(as.Date(Date, '%Y-%m-%d') == input$date2) %>%
        na.omit()
      
      m = loc_data %>% leaflet() %>% addTiles() %>%
        setView(121.5, 31.2, zoom = 10) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("Toner", "OSM"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMarkers(
          lng =  loc_data$Longtitude,
          lat =  loc_data$Latitude,
          popup = ~ paste0(
            "Date: ",
            loc_data$Date,
            br(),
            "Address: ",
            loc_data$Address,
            br(),
            "Longtitude: ",
            loc_data$Longtitude,
            br(),
            "Latitude: ",
            loc_data$Latitude
          ),
          clusterOptions = markerClusterOptions()
        )
    }
  })
  
  output$myMap3 = renderLeaflet({
    if (input$Agg == TRUE) {
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
          popup = ~ paste0(
            "Date: ",
            ~ Date,
            br(),
            "Address: ",
            ~ Address,
            br(),
            "Longtitude: ",
            ~ longtitude,
            br(),
            "Latitude: ",
            ~ latitude
          ),
          clusterOptions = markerClusterOptions()
        )
    } else {
      loc_data = df_ind %>%
        filter(as.Date(Date, '%Y-%m-%d') == input$date2) %>%
        na.omit()
      
      m = loc_data %>% leaflet() %>% addTiles() %>%
        setView(121.5, 31.2, zoom = 10) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("Toner", "OSM"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMarkers(
          lng =  loc_data$Longtitude,
          lat =  loc_data$Latitude,
          popup = ~ paste0(
            "Date: ",
            loc_data$Date,
            br(),
            "Address: ",
            loc_data$Address,
            br(),
            "Longtitude: ",
            loc_data$Longtitude,
            br(),
            "Latitude: ",
            loc_data$Latitude
          ),
          clusterOptions = markerClusterOptions()
        )
    }
  })
  
}

shinyApp(ui = ui, server = server)

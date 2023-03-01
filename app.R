

library(shinythemes)
library(shiny)
library(readr)
library(stringr)
library(plyr)
library(tidyverse)
library(readxl)
library(openxlsx)
library(writexl)
library(stringi)
library(here)
library(scales)
library(cowplot)
library(lubridate)
library(here)
library(leaflet)
library(mapview)
library(highcharter)
library(dygraphs)
library(xts)
library(broom)
library(data.table)


# load Data
load("Rad_2015_2022_15Min_.Rda")
load("data_zaehl.Rda")

data_minute = data_gesamt
data_minute$Year = substr(data_minute$datum, 1,4)


#### Shiny App ####

# Definiere die UI
ui <- fluidPage(theme = shinytheme("united"),
                
                # Titel der App
                titlePanel("Fahrrad Aktivität München"),
                
                # Sidebar mit Datumseingabe 
                sidebarLayout(
                  sidebarPanel(
                    selectInput("Year", "Wähle ein Jahr:",
                                choices = c("2015", "2016", "2017", 
                                            "2018", "2019", "2020", "2021", "2022"), selected = "2022"),
                    dateInput("date", "Datum:"),
                    
                    # Download Button
                    downloadButton("downloadData", "Download Graphik")
                  ),
                  
                  # Zeige Plot, Tabelle und Karte 
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Fahrradaktivität", highchartOutput("distPlot"), br(), br(), highchartOutput("summary")),
                      tabPanel("Fahrtrichtung", htmlOutput("distPlot_2"), br(), br(), highchartOutput("distPlot_3")),
                      tabPanel("Jahreswerte", highchartOutput("Jahr1"), br(), br(), highchartOutput("Jahr2"), br(), br(), highchartOutput("Heat")),
                      tabPanel("2015-2022", dygraphOutput("Jahr3"), br(), br(), highchartOutput("Heat_Jahr")),
                      tabPanel("Standort", mapviewOutput("map", width = "100%", height = 400)),
                      tabPanel("Info", p("Hallo das ist nur etwas Info"))
                    )
                  )
                )
)

# Define Server
server <- function(input, output, session) {
  
  observe({
    # We'll use the input$controller variable multiple times, so save it as x
    # for convenience.
    x <- input$Year
    
    updateDateInput(session, "date",
                    value = paste(x, "-01-04",sep=""))
  })
  
  ### Initialisieren ###
  
  
  # Fahrradaktivität   
  p <- reactive({
    data_minute %>%
      filter(Year == input$Year) %>%
      filter(datum == input$date) %>%
      hchart("line",
             hcaes(x = uhrzeit_start, y = gesamt, group = Zählstelle)) %>%
      hc_title(text = "15 Minuten Aktivität") 
  })
  
  
  # Differenz der Fahrtrichtung
  d <- reactive({
    data_minute %>%
      filter(Year == input$Year) %>%
      filter(datum == input$date) %>%
      mutate(delta = richtung_1 - richtung_2) %>%
      hchart("line",
             hcaes(x = uhrzeit_start, y = delta, group = Zählstelle)) %>%
      hc_title(text = "Differenz: Richtung 1 - Richtung 2")
    
  })
  
  # Fahrtrichtung pro Zählstelle
  p_2 <- reactive({
    map(unique(data_minute$Zählstelle), function(x) {
      data_minute %>% 
        filter(Year == input$Year) %>%
        filter(Zählstelle == x) %>%
        filter(datum == input$date) %>%
        gather("key", "Aktivität", richtung_1, richtung_2) %>%
        hchart(type='line', hcaes(x= uhrzeit_start, y= Aktivität , group= key)) %>%
        hc_title(text = x)
    })
  })
  
  
  # Jahreswerte
  # Kummuliert
  output$Jahr1 <- renderHighchart({
    data_minute %>%
      filter(Year == input$Year) %>%
      mutate(Date = as.Date(t)) %>%
      group_by(Date) %>%
      summarise(Sum = sum(gesamt)) %>%
      hchart("line",
             hcaes(x = Date, y = Sum)) %>%
      hc_title(text = paste("Jahresgesamtaktivität", input$Year, sep = " "))  %>%
      hc_subtitle(text = "über alle Zählstationen kummuliert") 
  })
  
  # Alle Stationen
  output$Jahr2 <- renderHighchart({
    data_minute %>%
      filter(Year == input$Year) %>%
      mutate(Date = as.Date(t)) %>%
      group_by(Zählstelle, Date) %>%
      summarise(Sum = sum(gesamt)) %>%
      hchart("line",
             hcaes(x = Date, y = Sum, group = Zählstelle)) %>%
      hc_title(text = paste("Jahresgesamtaktivität", input$Year, sep = " ")) %>%
      hc_subtitle(text = "Farbe nach Zählstationen") 
  })
  
  
  # Heatmap
  output$Heat <- renderHighchart({ 
    data_week <- data_minute %>% 
      mutate(Date = as.Date(t)) %>%
      group_by(Date, Zählstelle) %>%
      summarise(Sum = sum(gesamt)) %>%
      mutate(weekday = wday(Date))  %>%
      mutate(Year = substr(Date, 1, 4)) %>%
      filter(Year == input$Year) %>%
      group_by(Zählstelle, weekday) %>%
      summarise(Mean = mean(Sum), .groups="keep") %>%
      mutate(weekday_2 = case_when(
        weekday == 1 ~ "So",
        weekday == 2 ~ "Mo",
        weekday == 3 ~ "Di",
        weekday == 4 ~ "Mi",
        weekday == 5 ~ "Do",
        weekday == 6 ~ "Fr",
        weekday == 7 ~ "Sa",))  %>%
      group_by(Zählstelle) %>%
      mutate(Perc = round(Mean/sum(Mean) * 100, digits = 2)) %>%
      select(-Mean)
    
    # Transformation in Wide Format
    data_week_2 <- spread(data_week,                                  
                          key = Zählstelle,
                          value = Perc)
    
    mat = data_week_2
    mat = as.data.frame(mat)
    rownames(mat) <- mat[,2]
    mat = mat %>% select(-weekday_2) 
    mat = t(mat)
    mat = na.omit(mat)
    
    mat <- as.matrix(mat) 
    mat = mat [-1,]
    
    hchart(mat, type = "heatmap") %>% 
      hc_title(text = paste("Wochenaktivität", input$Year, sep = " ")) %>%
      hc_subtitle(text = "Prozentuale Aktivität nach Zählstationen") 
    
  })
  
  
  
  # Jahreswerte 2008-2022
  output$Jahr3 <- renderDygraph({
    data_minute = data_minute %>%
      mutate(Date = as.Date(t)) %>%
      group_by(Date) %>%
      summarise(Sum = sum(gesamt))
    
    don <- xts(x = data_minute$Sum, order.by = data_minute$Date)
    dygraph(don, main = "Fahrradaktivität 2008-2022") %>% 
      dyRangeSelector(strokeColor = "")
  })
  
  
  # Heatmap für alle Jahre
  output$Heat_Jahr <- renderHighchart({ 
    data_week = data_minute %>%
      mutate(Date = as.Date(t)) %>%
      group_by(Date, Zählstelle) %>%
      summarise(Sum = sum(gesamt)) %>%
      mutate(weekday = wday(Date))  %>%
      group_by(Zählstelle, weekday) %>%
      summarise(Mean = mean(Sum), .groups="keep") %>%
      mutate(weekday_2 = case_when(
        weekday == 1 ~ "So",
        weekday == 2 ~ "Mo",
        weekday == 3 ~ "Di",
        weekday == 4 ~ "Mi",
        weekday == 5 ~ "Do",
        weekday == 6 ~ "Fr",
        weekday == 7 ~ "Sa",)) %>%
      group_by(Zählstelle) %>%
      mutate(Perc = round(Mean/sum(Mean) * 100, digits = 2)) %>%
      select(-Mean)
    
    # Transformation in Wide Format
    data_week_2 <- spread(data_week,                                  
                          key = Zählstelle,
                          value = Perc)
    mat = data_week_2
    mat = as.data.frame(mat)
    rownames(mat) <- mat[,2]
    mat = mat %>% select(-weekday_2) 
    mat = t(mat)
    mat = na.omit(mat)
    
    mat <- as.matrix(mat) 
    mat = mat [-1,]
    
    hchart(mat, type = "heatmap") %>% 
      hc_title(text = "Gesamt Wochenaktivität") %>%
      hc_subtitle(text = "Prozentuale Aktivität nach Zählstationen") 
    
  })
  
  # Summary Statistics
  data_minute_2 <- reactive({
    data_minute  %>%
      filter(Year == input$Year) %>% 
      filter(datum == input$date) 
  })
  
  s <- reactive({
    hcboxplot(
      outliers = FALSE,
      x = data_minute_2()$gesamt,
      var = data_minute_2()$Zählstelle,
      name = "Zählstelle",
      color = "#31a354") %>%
      hc_title(text = "Gesamtaktivität: Tageswerte") %>%
      hc_yAxis(title = list(text = "Gesammtaktivität (alle 15 min)")) 
  })
  
  # Bild zum runterladen   
  image <- reactive({
    ggplot(data_minute %>%
             filter(Year == input$Year) %>%
             filter(datum == input$date),
           aes(x = t, y = gesamt)) + 
      geom_line(aes(color = Zählstelle)) +
      labs(x = "", y ="Aktivität") +
      theme_test()
  })
  
  ### Ausgabe ###
  
  # Fahrradaktivität Plot
  output$distPlot <- renderHighchart({
    p()
  })
  
  # Fahrdrichtung Plot
  output$distPlot_2 <- renderUI({
    hw_grid(p_2(), rowheight = 300)
  })
  
  # Differenz Fahrdrichtung Plot   
  output$distPlot_3 <- renderHighchart({
    d()
  })
  
  # Summary Tabelle 
  output$summary <- renderHighchart({
    s()
  })
  
  
  # Karte
  map <- mapview(data_zaehl, zcol = "zaehlstelle") 
  output$map <- renderLeaflet({
    map@map
  })
  
  # Download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Fahrradaktivität_", input$date, ".png", sep = "")
    },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 400, units = "in")
      ggsave(file, plot = image(), device = device)
    }
  )
}

# Lass die App laufen! 
shinyApp(ui = ui, server = server)



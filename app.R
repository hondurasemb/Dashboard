###### Load Libraries ######

library(plyr)
library(plyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(xts)
library(dygraphs)
library(lubridate)
library(highcharter)
library(plotly)
library(googlesheets)
library(scales)
library(ggedit)



####################
### Cargar Datos ###
####################

aprehensiones <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vS59SjalGKZ1-lHEnr3SPpXzNpVCbrxbG5EbMiuS3kcGYAxPzUpEnOeQClH6WRARIC8daUd8Fr-s7nO/pub?output=csv")
retornados <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRrbiua29FjhzOefTMWsVHkGSHbAAK-MHt_8NxfB5vGPC5fz4gI41EQcvNXtJbmWDbB02w7ykpe6OOK/pub?output=csv")
homicidios <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vToq3XPKJuAUjXb5rGgZLz6XIQakp2XgWmMMQp0NuoOaTZN3FW10cTW-jg_eqFObGNbxAvYAyLkrSW3/pub?output=csv")
separados <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTFScyL_9TYLtw18daOcXN1NW09rdJpUZa46r1hTYguKbOogj_S60cLv2kzMKraCU-YvwSav97hLcqw/pub?output=csv")
flags <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSZwd0Ulsg-1BY9Uk5OSHoo3jHXoMXseB8c5SOFmEYRKS6Lto4LCne5cMQH3b6F1RUqCZJWC2lRzk64/pub?output=csv", stringsAsFactors = FALSE)



################
### Limpieza ###
################

aprehensiones$mes <- mdy(aprehensiones$mes)
retornados$mes <- mdy(retornados$mes)
retornados$total <- as.numeric(retornados$total)
retornados$totalusa <- as.numeric(retornados$totalusa)
homicidios$mes <- mdy(homicidios$mes)

segment_data = data.frame(
  x = mdy(c("1/1/2012", "1/1/2014", "1/1/2016", "1/1/2018")),
  xend = mdy(c("1/1/2012", "1/1/2014", "1/1/2016", "1/1/2018")), 
  y = c(0, 0, 0, 0),
  yend = c(634, 561, 382, 326))

vectorBulletList <- function(vector) {
  if(length(vector > 1)) {
    paste0("<ul><li>", 
           paste0(
             paste0(vector, collpase = ""), collapse = "</li><li>"),
           "</li></ul>")   
  }
}


######################
### User Interface ###
######################

ui <-
  
  fluidPage(
    
    theme = shinytheme("journal"),
    
    #### Dashboard Single ####
    
    fluidRow(
      
      column(width = 12, height = "20px", align = "center",
             h3(("Dashboard Embajada de Honduras")),
             br())),
 
    
    fluidRow(
      
      column(width = 1, height = "400px"),
      
      column(width = 5, height = "400px",
             plotOutput("aprehensiones.plot", height = '400px')),
      
      column(width = 5, height = 400,
             plotOutput("retornados.plot", height = "400px")),
      
      column(width = 1, height = "400px")
      
    ),
    
    
    fluidRow(
      
      column(width = 1, height = "400px"),
      
      column(width = 5, height = 400,
             
             plotOutput("homicidios.plot", height = "400px")),
      
      column(width = 5, height = 400,
             h4("Puntos de Interes", align = "center"),
             
             uiOutput("flags.text2")
             ),
      
      column(width = 1, height = 400)
      
    )
    
    
  )


###############
### Server ###
##############

server <- function(input, output, session) {
  
  #### Immigration Graphs GGPLOT ####
  
  output$aprehensiones.plot <- renderPlot({
    
    ggplot(aprehensiones, aes(mes, total)) + 
      geom_segment(aes(x = max(aprehensiones$mes), y = 0, 
                       xend = max(aprehensiones$mes), yend = aprehensiones[nrow(aprehensiones), 2]),
                   colour = 'gray60', 
                   linetype = 3, 
                   size = 2) +
      
      geom_smooth(method = "loess", 
                  span = 0.15, 
                  aes(x = mes, y = total),
                  se = FALSE, 
                  color = "steelblue",
                  linetype = 1,
                  size = 1.3) +
      
      geom_smooth(method = "loess", 
                  span = 0.15, 
                  aes(x = mes, y = menores),
                  se = FALSE, 
                  color = "orangered2",
                  linetype = 1,
                  size = 1.3) +
      
      scale_x_date(name = "", 
                   expand = c(0.10, 0, 0.10, 0),
                   breaks = "6 months",
                   minor_breaks = "6 months",
                   date_labels = "%b %Y") + 
      
      scale_y_continuous(name = "",
                         breaks = c(0, 2500, 5000, 7500, 10000, 12500),
                         minor_breaks = c(0,2500,5000, 7500, 10000, 12500)) + 
      
      geom_point(aes(x = max(aprehensiones$mes), y = aprehensiones[nrow(aprehensiones), 2]),
                 size = 5, color = "steelblue") + 
      
      geom_point(aes(x = max(aprehensiones$mes), y = aprehensiones[nrow(aprehensiones), 4]),
                 size = 5, color = "orangered2") + 
      
      geom_label(aes(x = max(aprehensiones$mes), y = aprehensiones[nrow(aprehensiones), 2],
                     label = paste("Total:",  comma(aprehensiones[nrow(aprehensiones), 2]))), 
                 nudge_y = -900,
                 size = 5.5,
                 label.size = 0,
                 colour = "steelblue",
                 show.legend = FALSE) +
      
      geom_label(aes(x = max(aprehensiones$mes), y = aprehensiones[nrow(aprehensiones), 4],
                     label = paste("Menores:", "\n", comma(aprehensiones[nrow(aprehensiones), 4]))),
                 nudge_y = -900,
                 size = 5.5,
                 label.size = 0,
                 colour = "orangered2",
                 show.legend = FALSE) +
      
      labs(title = paste("Aprehensiones en la Frontera:", 
                         aprehensiones[nrow(aprehensiones),8], 
                         year(max(aprehensiones$mes))), 
           x = NULL, y = NULL,  subtitle = "Fuente: Reporte Quincenal de CBP") + 
      
      ggthemes::theme_hc() + 
      
      theme(plot.subtitle = element_text(size = 13, 
                                         face = "italic"), 
            axis.line = element_line(colour = "azure4", 
                                     size = 0.6, linetype = "solid"), 
            axis.ticks = element_line(size = 1), 
            axis.text = element_text(family = "serif", 
                                     size = 15), 
            plot.title = element_text(size = 20))
    
  })
  
  output$retornados.plot <- renderPlot({
    
    ggplot(retornados, aes(mes, total)) + 
      
      geom_segment(aes(x = max(retornados$mes), y = 0, 
                       xend = max(retornados$mes), yend = retornados[nrow(retornados), 8]),
                   colour = 'gray60', 
                   linetype = 3, 
                   size = 2) +
      
      geom_smooth(method = "loess", 
                  span = 0.15, 
                  aes(x = mes, y = total),
                  se = FALSE, 
                  color = "steelblue",
                  linetype = 1,
                  size = 1.3) +
      
      geom_smooth(method = "loess", 
                  span = 0.15, 
                  aes(x = mes, y = totalusa),
                  se = FALSE, 
                  color = "orangered2",
                  linetype = 1,
                  size = 1.3) +
      
      scale_x_date(name = "", 
                   expand = c(0.10, 0, 0.10, 0),
                   breaks = "3 months",
                   minor_breaks = "3 months",
                   date_labels = "%b %Y") + 
      
      scale_y_continuous(name = "",
                         breaks = c(0, 1500, 3000, 4500, 6000, 7500, 9000),
                         minor_breaks = c(0, 1500, 3000, 4500, 6000, 7500, 9000)) +
      
      geom_point(aes(x = max(retornados$mes), y = retornados[nrow(retornados), 8]),
                 size = 5, color = "steelblue") + 
      
      geom_point(aes(x = max(retornados$mes), y = retornados[nrow(retornados), 7]),
                 size = 5, color = "orangered2") + 
      
      geom_label(aes(x = max(retornados$mes), y = retornados[nrow(retornados), 8],
                     label = paste("Total:",  comma(retornados[nrow(retornados), 8]))), 
                 nudge_y = -500,
                 size = 5.5,
                 label.size = 0,
                 colour = "steelblue",
                 show.legend = FALSE) +
      
      geom_label(aes(x = max(retornados$mes), y = retornados[nrow(retornados), 7],
                     label = paste("Desde EEUU:", "\n",  comma(retornados[nrow(retornados), 7]))),
                 nudge_y = -500,
                 size = 5.5,
                 label.size = 0,
                 colour = "orangered2",
                 show.legend = FALSE) +
      
      labs(title = paste("Retornados:", 
                         retornados[nrow(retornados), 10], 
                         year(max(retornados$mes))), 
           x = NULL, y = NULL,  subtitle = "Fuente: Cifras Observatorio Consular") +
      
      theme_hc() + 
      
      theme(plot.subtitle = element_text(size = 13, 
                                         face = "italic"), 
            axis.line = element_line(colour = "azure4", 
                                     size = 0.6, linetype = "solid"), 
            axis.ticks = element_line(size = 1), 
            axis.text = element_text(family = "serif", 
                                     size = 15), 
            plot.title = element_text(size = 20))
    
    
  })
  
  output$homicidios.plot <- renderPlot({
    
    ggplot(homicidios, aes(mes, total)) + 
      
      geom_segment(aes(x = max(homicidios$mes), y = 0, 
                       xend = max(homicidios$mes), yend = homicidios[nrow(homicidios), 2]),
                   colour = 'gray60', 
                   linetype = 3, 
                   size = 1.5) +
      
      geom_segment(data = segment_data, 
                   aes(x = x, y = y, xend = xend, yend = yend),
                   colour = "gray60",
                   linetype = 3,
                   size = 1.5) +
      
      geom_smooth(method = "loess", 
                  span = 0.05, 
                  aes(x = mes, y = total),
                  se = FALSE, 
                  color = "steelblue",
                  linetype = 1,
                  size = 1.3) +
      
      geom_point(data = segment_data,
                 aes(x = x, y = yend),
                 size = 5, color = "steelblue") + 
      
      geom_point(aes(x = max(homicidios$mes), y = homicidios[nrow(homicidios), 2]),
                 size = 5, color = "steelblue") + 
      
      scale_x_date(name = "", 
                   expand = c(0.10, 0.2, 0.10, 0.2),
                   breaks = "8 months",
                   minor_breaks = "8 months",
                   date_labels = "%b %Y") + 
      
      scale_y_continuous(name = "",
                         breaks = c(0, 100, 200, 300, 400, 500, 600,700),
                         minor_breaks = c(0, 100, 200, 300, 400, 500, 600,700),
                         limits = c(0, 700)) + 
      
      geom_label(aes(x = max(homicidios$mes), y = homicidios[nrow(homicidios), 2],
                     label = paste("Total:",  comma(homicidios[nrow(homicidios), 2]))), 
                 nudge_y = -50,
                 size = 5.5,
                 label.size = 0,
                 colour = "steelblue",
                 show.legend = FALSE) +
      
      geom_label(data = segment_data,
                 aes(x = x, y = yend),
                 label = as.character(segment_data$yend),
                 nudge_y = 50,
                 size = 5.5,
                 label.size = 0,
                 colour = "steelblue",
                 show.legend = FALSE) + 
      
      labs(title = paste("Numero de Homicidios por Mes:", 
                         homicidios[nrow(homicidios), 4], 
                         year(max(homicidios$mes))), 
           x = NULL, y = NULL,  subtitle = "Fuente: Secretaría de Seguridad") +
      
      theme_hc() + 
      
      theme(plot.subtitle = element_text(size = 13, face = "italic"), 
            axis.line = element_line(colour = "azure4", size = 0.6, linetype = "solid"), 
            axis.ticks = element_line(size = 1), 
            axis.text = element_text(family = "serif", size = 15), 
            plot.title = element_text(size = 20))
    
    
  })
  
  output$flags.text <- renderText({
    
    (vectorBulletList(flags$congreso))
    
    })
  
  output$flags.text2 <- renderUI({
    
    HTML(vectorBulletList(flags$congreso))
    
  })
  
}


shinyApp(ui = ui, server = server)

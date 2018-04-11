library(DT)
library(shiny)
library(arules)
library(arulesSequences)
library(arulesViz)
library(shinydashboard)
library(googleVis)
#library(rsconnect)
#library(dbConnect)
#library(DBI)
#library(RODBC)
library(highlight)
#library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)
library(plyr)
library(plotly)
library(ggplot2)
library(htmlwidgets)
library(shinythemes)
library(visNetwork)
#=============================================
tr <- read.transactions('~/Projects/market_basket1.csv', format = 'basket', sep = ',')
rules <- apriori(tr, parameter = list(supp = 0.01, conf = 0.5, maxlen = 20))
rulesbyconf <- sort(rules, by = 'confidence', decreasing = TRUE)
rulesbycount <- sort(rules, by = 'count', decreasing = TRUE)

ui<- fluidPage(
  theme = shinytheme("cerulean"),
  fluidRow(
    column(12,
           headerPanel("Association Rules"),
           mainPanel(
             tabsetPanel(
               tabPanel("Rules", DT::dataTableOutput("rules")),
               tabPanel("scatter", plotlyOutput("scatter",height = "700px")),
               tabPanel("Graph"  , visNetworkOutput("graph", width = "1000px", height = "1000px")),
               tabPanel("Paracoord"  , plotOutput("paracoord",width = "100%", height = "500px")),
               tabPanel("matrix" , plotlyOutput("matrix",width = "100%", height = "500px"))
               ,tabPanel("Two-key-plot", plotlyOutput("two_key_plot",width = "100%", height = "800px"))
               ,tabPanel("Grouped matrix"  , plotOutput("grouped_matrix",width = "1000px", height = "900px"))
             )
           )
    )))

server <- function(input, output) {
  output$rules = DT::renderDataTable({
    inspectDT(rulesbyconf[1:1000])
  })
  output$scatter = renderPlotly({
    plot(rules[1:10000], method = "scatter", engine = "htmlwidget")
  })
  output$graph = renderVisNetwork({
    plot(rules[1:1000], method = "graph", engine = "htmlwidget")
  })
  output$paracoord = renderPlot({
    plot(rulesbyconf[1:20], method="paracoord",control=list(reorder=TRUE))
  })
  
  output$matrix  = renderPlotly({
    plot(rules[1:1000], "matrix", engine = "htmlwidget")
  })
  output$two_key_plot  = renderPlotly({
    plotly_arules(rules, method = "two-key plot",measure = c("confidence","lift")
                  , jitter = 20
                  , shading = "confidence"
                  , max = 15000)
  })
  output$grouped_matrix = renderPlot({
    plot(rules[1:5000], method = "grouped matrix")
  })
}
shinyApp(ui=ui,server = server)

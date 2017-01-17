library(shiny)
library(plotly)

ui <- fluidPage(
  headerPanel('Voters and undicided voters before Czech parliamentary elections 2013'),
  sidebarPanel(
    selectInput('ycol', 'Y Variable', c("PI_1a", "OV_1", "IDE_1", "EV_10", "PS_1", "OV_132", "IDE_6"),
                selected="PI_1a"),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    
    plotOutput("plot"),
    verbatimTextOutput("event"))
)

server <- function(input, output) {
  
  
  output$plot <- renderPlot({
    plot(mtcars$mpg,mtcars$cyl)
    # source("C:/Dokumenty/CVVM/CzechParliamentaryElections/functions/cvvm201310_analysis_undicided_func.R")
    # colors <- c('rgba(255,128,0,1)', 'rgba(127,0,255,1)', 'rgba(0,0,204,1)', 'rgba(255,255,0,1)',
    #             'rgba(204,204,204,1)', 'rgba(204,204,204,1)', 'rgba(153,153,255,1)', 'rgba(255,0,0,1)')
    # PV4R = makeBubbleMatrixTable201310()
    # 
    # plot_ly(PV4R, x = ~y, y = ~x, type = 'scatter', mode = 'markers',
    #              marker = list(size = ~rel*2.5, opacity = 1, color = colors),
    #              hoverinfo = 'text',
    #              text = ~paste('<b>',party, '</b><br>Model:', rel, '%, N =', abs,'<br>Real:   ',procenta,' %, N =', celkem)) %>%
    #   layout(title = 'Rozložení stran - říjen 2013',
    #          xaxis = list(showgrid = FALSE, title = "Levo-pravé sebezaření (PO.2)"),
    #          yaxis = list(showgrid = FALSE, title = "Y"))
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
}

shinyApp(ui, server)
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

pageWithSidebar(
  headerPanel('Voters and undicided voters before Czech parliamentary elections 2013'),
  sidebarPanel(
    selectInput('ycol', 'Y Variable', c("PI_1a", "OV_1", "IDE_1", "EV_10", "PS_1", "OV_132", "IDE_6"),
                selected="PI_1a"),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)



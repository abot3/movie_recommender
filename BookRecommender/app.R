library(shiny)

source('ui.R')
source('server.R')

# See above for the definitions of ui and server
# ui <- ...
#
# server <-

# options(shiny.autoreload = TRUE)
shinyApp(ui = ui, server = server)

## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

# https://campuswire.com/c/G3D46BBBA/feed/1267
# https://campuswire.com/c/G3D46BBBA/feed/1269

# shinyUI(
ui = dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),

          dashboardSidebar(
            sidebarMenu(
              menuItem("Home Page", tabName = "home", icon = icon("home")),
              menuItem("System 1", tabName = "system_1", icon = icon("dashboard")),
              menuItem("System 2", tabName = "system_2", icon = icon("th"))
            )
          ),

          dashboardBody(includeCSS("css/books.css"),
            tabItems(
              tabItem(tabName = "home",
                  h2("Home tab content"),
                  # TODO(aaronbotelho) - remove this action button when done dbg.
                  actionButton("browser", "Trigger browser()"),
                  div(
                    p("Select on of the recommendation systems in the navbar to get started.") ,
                    br(),
                    p(strong("System 1 "), "recommends top picks based on your favourite genres."),
                    br(),
                    p(strong("System 2 "), "recommends top picks based on the ratings you provide.")
                  )
              ),

              # First tab content
              tabItem(tabName = "system_1",
                  h2("Genres"),
                  # Select genre box
                  fluidRow(
                    box(width = 12, title = "Step 1: Pick your favourite genres.",
                        status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput('genres')
                        )
                    )
                  ),

                  # Output genre results
                  fluidRow(
                      useShinyjs(),
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        title = "Discover movies you might like",
                        br(),
                        withBusyIndicatorUI(
                          actionButton("genre_btn", "Click here to get your recommendations", class = "btn-warning")
                        ),
                        br(),
                        tableOutput("genre_results")
                      )
                   )
              ),

              # Second tab content
              tabItem(tabName = "system_2",
                  h2("Recommender"),
                  fluidRow(
                      box(width = 12, title = "Step 1: Rate as many movies as possible",
                          status = "info", solidHeader = TRUE, collapsible = TRUE,
                          div(class = "rateitems",
                              uiOutput('ratings')
                          )
                      )
                    ),
                  fluidRow(
                      useShinyjs(),
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        title = "Step 2: Discover movies you might like",
                        br(),
                        withBusyIndicatorUI(
                          actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                        ),
                        br(),
                        tableOutput("results")
                      )
                   )
              )
            )
          )
    )
# )
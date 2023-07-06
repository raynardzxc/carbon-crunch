#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("usePackages.R")
pkgnames <- c("tidyverse","shiny","shiny.fluent","shiny.router","DBI")
loadPkgs(pkgnames)

# import modules
source("modules/tutorial/tutorialModule.R")
source("modules/game/gameModule.R")

home_page <- div(
  titlePanel("Carbon Crunch"),
  p("This is the home page"),
  tags$li(a(href = route_link("tutorial"), "Tutorial")),
  tags$li(a(href = route_link("game"), "Game"))
)

# Define UI for application
ui <- fluidPage(
  router_ui(
    route("/", home_page),
    route("tutorial", tutorial_page),
    route("game", game_page)
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  )
)

# Define server logic
server <- function(input, output, session) {
  router_server()
  
  component <- reactive({
    if (is.null(get_query_param()$add)) {
      return(0)
    }
    as.numeric(get_query_param()$add)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

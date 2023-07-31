#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ?%Dt!Gs2z*Y#

source("usePackages.R")
source("setAWSPassword.R")
pkgnames <- c("tidyverse","shiny","shiny.fluent","shiny.router","DBI", "shinyWidgets","ggplot2","dplyr")
loadPkgs(pkgnames)

# import modules
source("modules/tutorial/tutorialModule.R")
source("modules/game/gameModule.R")
source("modules/leaderboard/leaderboardModule.R")
source("modules/leaderboard/publishModule.R")
source("modules/credits/creditModule.R")
source("modules/analysis/analysisModule.R")
source("helper.R")

# UI for Game
home_page <- div(
  fluidRow(
    column(12,  ## Column might be redundant here 
           align="center",
           h1("Welcome to Carbon Crunch", class = "title-text"),
           PrimaryButton.shinyInput(
             "tutorial",
             class="home-button",
             text = "Tutorial"
           ),
           PrimaryButton.shinyInput(
             "play",
             class="home-button",
             text = "Play Game"
           ),
           PrimaryButton.shinyInput(
             "lb",
             class="home-button",
             text = "Leaderboard"
           ),
           PrimaryButton.shinyInput(
             "credits",
             class="home-button",
             text = "Credits"
           ),
           PrimaryButton.shinyInput(
             "quit",
             class="home-button",
             text = "Quit Game"
           )
           )
  )
)

# Define UI for application
ui <- fluidPage(
  router_ui(
    route("/", home_page),
    route("tutorial", tutorial_page("tutorial")),
    route("game", game_page("game")),
    route("leaderboard",leaderboard_page("leaderboard")),
    route("credits", credit_page("credits")),
    route("analysis", analysis_page("analysis")),
    route("publish", publish_page("publish"))
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  )
)

# Define server logic
server <- function(input, output, session) {
  router_server()
  
  # catch routing from URL
  component <- reactive({
    if (is.null(get_query_param()$add)) {
      return(0)
    }
    as.numeric(get_query_param()$add)
  })

  observeEvent(input$tutorial, change_page("tutorial"))
  observeEvent(input$play, change_page("game"))
  observeEvent(input$lb, change_page("leaderboard"))
  observeEvent(input$credits, change_page("credits"))
  observeEvent(input$quit, stopApp())
  
  # Define reactive value for game data
  gameData <- reactiveVal()
  
  tutorial_server("tutorial")
  game_server("game", gameData)
  leaderboard_server("leaderboard")
  credit_server("credits")
  analysis_server("analysis", gameData)
  publish_server("publish", gameData)
}

# Run the application 
shinyApp(ui = ui, server = server)

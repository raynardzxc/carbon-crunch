#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("usePackages.R")
source("setAWSPassword.R")
pkgnames <- c("tidyverse","shiny","shiny.fluent","shiny.router","DBI", "shinyWidgets")
loadPkgs(pkgnames)

# import modules
source("modules/tutorial/tutorialModule.R")
source("modules/game/gameModule.R")
source("modules/leaderboard/leaderboardModule.R")
source("modules/leaderboard/publishModule.R")
source("modules/credits/creditModule.R")
source("modules/analysis/analysisModule.R")

# Initialise all functions related to communicating with database --------------------
getAWSConnection <- function(){
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "student099",
    host = "database-1.ceo4ehzjeeg0.ap-southeast-1.rds.amazonaws.com",
    username = "student099",
    password = getOption("AWSPassword"))
  conn
}

# Push player's score into the Leaderboard table
publishScore <- function(playerid,score){
  conn <- getAWSConnection()
  querytemplate <- "INSERT INTO LeaderScore (playerid,asoftime,score) VALUES (?id1,NOW(),?id2)"
  query <- sqlInterpolate(conn, querytemplate,id1=playerid,id2=score)
  #print(query) #for debug
  success <- FALSE
  tryCatch(
    {  # This is not a SELECT query so we use dbExecute
      result <- dbExecute(conn,query)
      print("Score published")
      success <- TRUE
    }, error=function(cond){print("publishScore: ERROR")
      print(cond)
    }, 
    warning=function(cond){print("publishScore: WARNING")
      print(cond)},
    finally = {}
  )
  dbDisconnect(conn)
}

# Retrieve leaderboard from database
getLeaderBoard <- function(){
  conn <- getAWSConnection()
  
  # Assemble the query
  query <- "SELECT lp.playername,ls.score,ls.asoftime  FROM LeaderScore as ls INNER JOIN LeaderPlayer as lp"
  query <- paste0(query," ON (ls.playerid=lp.playerid) WHERE ls.gamevariantid =")
  query <- paste0(query,gamevariantid)
  
  # Sort in descending order
  query <- paste0(query, " ORDER BY ls.score DESC,ls.asoftime ASC")
  print(query) # for debugging
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}

# Retrieve initial game conditions
getInitialCond <- function(){
  conn <- getAWSConnection()
  query <- "SELECT * FROM InitialCond"
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}

# Retrieve battery information given current level[int] (can be used to get next level info and cost as well)
getBatteryInfo <- function(currentlevel){
  conn <- getAWSConnection()
  # Crafting query
  query <- "SELECT * FROM InitialCond WHERE batlevel ="
  query <- paste0(query, currentlevel)
  # Retrieve results
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}

# Retrieve battery information given current level[int] and line type[binary] (can be used to get next level info and cost as well)
getLineInfo <- function(currentlevel, linetype){
  conn <- getAWSConnection()
  # Crafting query
  query <- "SELECT * FROM InitialCond WHERE currentlevel ="
  query <- paste0(query, currentlevel)
  query <- paste0(query, "AND linetype=")
  query <- paste0(query, linetype)
  # Retrieve results
  result <- dbGetQuery(conn,query)
  dbDisconnect(conn)
  result
}

# ------------------------------------------------------------------------------------

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
  
  tutorial_server("tutorial")
  game_server("game")
  leaderboard_server("leaderboard")
  credit_server("credits")
  analysis_server("analysis")
  publish_server("publish")
}

# Run the application 
shinyApp(ui = ui, server = server)

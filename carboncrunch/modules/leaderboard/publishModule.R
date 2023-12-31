# Done by: Wei Xian

source("helper.R")

publish_page <- function(id) {
  ns <- NS(id)
  div(
    style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100vh;", # Center the content vertically
    titlePanel("Publish Highscore"),
    uiOutput(ns("leaderBoard")),
    uiOutput(ns("publishControls")),
    uiOutput(ns("logregControls")),
    actionButton(ns("back"), label = "Back", class = "final-button")
  )
}

publish_server <- function(id, gameData) {
  
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      
      vals <- reactiveValues(password = NULL, playerid = NULL, playername = NULL, score = NULL, published = FALSE) ##placeholder score
      
      
      observeEvent(input$test, {
        showModal(publishModal(ns,failed=FALSE))
      })
      
      #Fire some code if the user clicks the Register button
      observeEvent(input$register, {
        showModal(registerModal(ns, failed=FALSE))
      })
      
      # Fire some code if the user clicks the registerok button
      observeEvent(input$registerok, {
        # Check that password1 exists and it matches password2 and username is unique
        if (str_length(input$password1) > 0 && (input$password1 == input$password2) && (existPlayername(input$playername1) == FALSE)) {
          #store the password and close the dialog
          vals$password <- input$password1
          #print(vals$password) # for debugging
          
          vals$playername = input$playername1
          
          
          # store in db
          createNewPlayerQuery(vals$playername, vals$password)
          print("New player stored in DB!")
          
          if (!is.null(vals$playername)){
            vals$playerid <- getPlayerID(vals$playername,vals$password)
          }
          print(vals$playerid) # for debugging
          removeModal()
        } else {
          showModal(registerModal(ns, failed = TRUE))
        }
      })
      
      #Fire some code if the user clicks the Login button
      observeEvent(input$login, {
        showModal(loginModal(ns,failed=FALSE))
      })
      
      # Fire some code if the user clicks the loginok button
      observeEvent(input$loginok, {
        print("login ok")
        # Get the playerID and check if it is valid
        playerid <- getPlayerID(input$playername2,input$password3)
        print(playerid)
        if (playerid>0) {
          #store the playerid and playername and close the dialog
          vals$playerid <- playerid
          #print(vals$playerid) # for debugging
          vals$playername <- input$playername2
          print("Player successfully logged in!")
          removeModal()
        } else {
          showModal(loginModal(ns, failed = TRUE))
        }
      })
      
      observeEvent(input$publishok, {
        publishScore(vals$playerid,vals$score)
        "Score successfully published!"
        vals$published <- TRUE
      })
      
      output$leaderBoard <- renderUI({
        tagList(
          renderTable({numclicks <- input$publishok #to force a refresh whenever one of these buttons is clicked
          leaderboard <- getLeaderBoard()
          leaderboard}
          )
        )
      })
      
      output$publishControls <- renderUI({
        data <- gameData()
        limit <- 350
        final_cash <- data$final_cash
        final_emissions <- data$final_emissions
        final_score <- data$final_score
        vals$score <- final_score
        if (is.null(vals$playername)) {## player have not logged in yet
          tagList(
            paste0("Your score is: ", final_score, ". Please login if you want to publish your score.")
          )
        } else {
          if (vals$published == FALSE) {
          tagList(
            paste0("Your score is: ", final_score, "."),
            actionButton(inputId = ns("publishok"), "Publish")
          )
          }
          }
      })
      
      output$logregControls <- renderUI({
        if (is.null(vals$playername)) ## player have not logged in yet
          tagList(
            actionButton(inputId = ns("register"), "Register", class = "final-button"),
            actionButton(inputId = ns("login"), "Login", class = "final-button")
          )
        else 
          paste0("Logged in as: ",vals$playername)
      })
      
      observeEvent(input$back, {
        change_page("analysis")
      })
    }
  )
}

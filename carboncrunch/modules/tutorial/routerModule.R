routerModuleUI <- function(id){
  ns <- NS(id)
  uiOutput(ns("navigation"))
}


routerModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Game state reactive variable
    game_state <- reactiveVal("home")

    output$navigation <- renderUI({
      switch(game_state(),
             "home" = wellPanel(
               fluidRow(
                 column(12, align = "center",
                        tags$style(HTML("
                        .header { text-align: center; }
                        .btn { display: block; margin: 10px auto; width: 200px; }
                        .footer-btns { display: flex; justify-content: space-between; width: 420px; margin: 0 auto; }")),
                        h1("Welcome to Carbon Crunch", class = "header"),
                        actionButton(session$ns("tutorial"), "Tutorial", class = "btn"),
                        actionButton(session$ns("play"), "Play Game", class = "btn"),
                        actionButton(session$ns("leaderboard"), "Leaderboard", class = "btn"),
                        div(class = "footer-btns",
                            actionButton(session$ns("credits"), "Credits"),
                            actionButton(session$ns("quit"), "Quit Game")
                        )
                 )
               )
             ),
             "game" = wellPanel(
               h2("Day 1: Make Your Decisions"),
               checkboxGroupInput(session$ns("energy"), "Select Energy Source for Each Production Line:", choices = c("Solar", "Fossil")),
               actionButton(session$ns("upgrade_battery"), "Upgrade Solar Battery"),
               actionButton(session$ns("upgrade_efficiency"), "Upgrade Line Efficiency"),
               actionButton(session$ns("next_day"), "Next Day")
             ),
             "end" = wellPanel(
               h2("Game Over: Your Score"),
               verbatimTextOutput(session$ns("score")),
               textInput(session$ns("username"), "Username"),
               actionButton(session$ns("publish"), "Publish Score")
             )
      )
    })

    output$score <- renderPrint({
      "Your score: 0"
    })

    observeEvent(input$play, {
      game_state("game")
    })

    observeEvent(input$next_day, {
      game_state("end")
    })
    
    observe({
      print(game_state())
    })
  })
}

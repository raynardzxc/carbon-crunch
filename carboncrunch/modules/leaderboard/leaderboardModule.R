# Done by: Zaina

leaderboard_page <- function(id) {
  ns <- NS(id)
  div(
    style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100vh;", # Center the content vertically
    div(style = "text-align: center; margin-bottom: 20px;", h1("Leaderboard")), # Center the "Leaderboard" text
    div(style = "text-align: center; margin-bottom: 20px;", tableOutput(ns("leaderboard_table"))), # Center the leaderboard table
    actionButton(ns("back"), "Back", class = "final-button")
  )
}

leaderboard_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$back, change_page("/"))
      
      # Assuming getLeaderBoard() is defined here as well (before the leaderboard_server function)
      # Call getLeaderBoard to fetch the leaderboard data
      leaderboard_data <- reactive({
        getLeaderBoard()
      })
      
      # Display the leaderboard data in the table
      output$leaderboard_table <- renderTable({
        leaderboard_data()
      })
    }
  )
}


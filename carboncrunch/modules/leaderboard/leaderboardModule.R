leaderboard_page <- function(id) {
  ns <- NS(id)
  div(
    titlePanel("Leaderboard"),
    p("This is the leaderboard page"),
    PrimaryButton.shinyInput(
      ns("back"),
      class=".btn",
      text = "Back to Home"
    )
  )
}

leaderboard_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$back, change_page("/"))
    }
  )
}

analysis_page <- function(id) {
  ns <- NS(id)
  div(
    titlePanel("Analysis Page"),
    p("This is the analysis page"),
    PrimaryButton.shinyInput(
      ns("back"),
      class=".btn",
      text = "Back to Home"
    )
  )
}

analysis_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$back, change_page("/"))
    }
  )
}
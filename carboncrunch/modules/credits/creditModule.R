credit_page <- function(id) {
  ns <- NS(id)
  div(
    titlePanel("Credits"),
    p("This is the credits page"),
    PrimaryButton.shinyInput(
      ns("back"),
      class=".btn",
      text = "Back to Home"
    )
  )
}

credit_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$back, change_page("/"))
    }
  )
}
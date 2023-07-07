tutorial_page <- function(id) {
  ns <- NS(id)
  div(
    titlePanel("Tutorial Page"),
    p("This is the tutorial page"),
    PrimaryButton.shinyInput(
      ns("back"),
      class=".btn",
      text = "Back to Home"
    )
  )
}

tutorial_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$back, change_page("/"))
    }
  )
}
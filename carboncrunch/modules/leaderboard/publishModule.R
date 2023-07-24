publish_page <- function(id) {
  ns <- NS(id)
  div(
    titlePanel("Publish"),
    p("This is the publish page"),
    PrimaryButton.shinyInput(
      ns("back"),
      class=".btn",
      text = "Back to Home"
    )
  )
}

publish_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$back, change_page("/"))
    }
  )
}

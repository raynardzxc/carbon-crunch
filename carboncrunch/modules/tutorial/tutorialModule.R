tutorial_page <- function(id) {
  ns <- NS(id)
  tags$div(class = "tut_div",
      tags$div(
        style = "text-align: center;",
        actionButton(ns("back"), "Back", class = "final-button")
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
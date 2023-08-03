tutorial_page <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "tut-div",
    fluidRow(h2(class = "tutorial-title", "Tutorial")),
    # fluidRow(
    #   tags$div(
    #     class = "image-div",
    #     column(4,
    #            tags$img(class = "tut-image2", src = "tut_image2.png")),
    #     column(8,
    #            tags$img(class = "tut-image1", src = "tut_image1.png"))
    #   )
    # ),
    fluidRow(div(
      style = "text-align: center;",
      actionButton(ns("back"), "Back", class = "final-button")
    ))
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
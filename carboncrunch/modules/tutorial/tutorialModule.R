tutorial_page <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "tut_div",
    fluidRow(
      tags$div(
        class = "tut1-cont",
        column(5,
               align = "center",
               h2(class = "tut-title", "Tutorial"),
               img(class = "tut1", src = "tut_image1.png"),
               actionButton(ns("back"), "Back", class = "final-button")
        )
      ),
      tags$div(
        class = "tut2-cont",
        column(7,
               align = "center",
               img(class = "tut2", src = "tut_image2.png")
        )
      )
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

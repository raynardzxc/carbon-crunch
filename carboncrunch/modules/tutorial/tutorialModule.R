tutorial_page <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(
      class = "tut-div",
      h2(class = "tutorial-title", "Tutorial"),
      div(
        class = "image-contain",
        img(class = "tut-image", src = "tut_image.png")
      ),
      
      
      div(
        style = "text-align: center;",
        actionButton(ns("back"), "Back", class = "final-button")
      )
    )
  )
}

tutorial_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # output$tut_image <- renderUI({
      #   img(src = "tut_image.png", height = "auto", width = "50%")
      # })
      
      observeEvent(input$back, change_page("/"))
    }
  )
}
analysis_page <- function(id) {
  ns <- NS(id)
  div(
    titlePanel("Analysis Page"),
    p("This is the analysis page"),
    p("Cash: ", textOutput(ns("cashValue"))),
    p(class = "my-class", "Emissions: ", textOutput(ns("emissionsValue"))),
    PrimaryButton.shinyInput(
      ns("back"),
      class=".btn",
      text = "Back to Home"
    )
  )
}

analysis_server <- function(id, gameData) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$back, change_page("/"))
      
      observe({
        data <- gameData()
        if (!is.null(data)) {
          # Access cash and emissions
          cash <- data$cash
          emissions <- data$emissions
          
          # Render cash and emissions values
          output$cashValue <- renderText({ paste("$", format(cash, big.mark = ",")) })  # formatted as currency
          output$emissionsValue <- renderText({ emissions })
        }
      })
    }
  )
}
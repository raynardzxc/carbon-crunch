game_page <- div(
  titlePanel("Game Page"),
  p("This is the game page"),
  tags$li(a(href = route_link("/"), "Back")),
  PrimaryButton.shinyInput(
    "back",
    class=".btn",
    text = "Back to Home"
  )
)

game_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      router_server()
      observeEvent(input$back, change_page("/"))
    }
  )
}
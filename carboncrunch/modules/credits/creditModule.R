credit_page <- function(id) {
  ns <- NS(id)
  div(
    class = "credit-div",
    h2(class= "credit-title", "Credits"),
    h3("Assets"),
    fluidRow(
      column(12, 
             p("Engineers - Murphysdad. (n.d.). Sci-Fi Facility Asset Pack by Murphysdad."),
             a("https://murphysdad.itch.io/sci-fi-facility")
      ),
      column(12, 
             p("Floor - SHQ Design Zone. (n.d.). TileMap SF by OSHQ Design Zone."),
             a("https://oshq.itch.io/tilemap-sf?download")
      ),
      column(12, 
             p("Products - penzilla. (n.d.). Top-down Retro Interior House: Royalty free pixel art asset pack by penzilla."),
             a("https://penzilla.itch.io/top-down-retro-house")
      ),
    ),
    h3("Font"),
    fluidRow(
      column(12, 
             p("Font - Geronimo. (n.d.). Gameplay. Free download: Gameplay Font."),
             a("https://www.urbanfonts.com/fonts/Gameplay.font")
      ),
    ),
    h3(class ="special-thanks", "Special thanks to Prof Jackson, Prof Lingjie and Prof Benita for their guidance and support in this project."),
    div( # Center the back button
      style = "text-align: center;",
      PrimaryButton.shinyInput(
        ns("back"),
        class = ".btn",
        text = "Back to Home"
      )
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
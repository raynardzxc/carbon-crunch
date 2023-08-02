credit_page <- function(id) {
  ns <- NS(id)
  div(
    class = "credit-div",
    h2(class= "credit-title", "Credits"),
    h3("raynard chai insert here"),
    
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
    h3("Cursor"),
    fluidRow(
      column(12, 
             p("Cursor - Chee. (n.d.). Leaf Set."),
             a("https://www.cursors-4u.com/cursor/2011/12/04/leaf-set.html")
      ),
    ),
    
    h3(class ="special-thanks", "Special thanks to Prof Peter Jackson, Prof Duan Lingjie and Prof Francisco Benita for their guidance and support in this project."),
    div( # Center the back button
      style = "text-align: center;",
      actionButton(ns("back"), "Back", class = "final-button")
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
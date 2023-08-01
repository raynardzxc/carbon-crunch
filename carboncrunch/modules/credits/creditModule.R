credit_page <- function(id) {
  ns <- NS(id)
  div(
    div(
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100vh;",
      titlePanel("Credits"),
      p("Engineers:", style = "font-weight: bold;"),
      tags$ul(
        tags$li(a("https://murphysdad.itch.io/sci-fi-facility", "https://murphysdad.itch.io/sci-fi-facility")),
        style = "font-family: 'Arial', sans-serif; font-size: 16px; color: #333; list-style-type: disc; margin-left: 20px;"),
      p("floor:", style = "font-weight: bold;"),
      tags$ul(
        tags$li(a("https://oshq.itch.io/tilemap-sf?download", "https://oshq.itch.io/tilemap-sf?download")),
        style = "font-family: 'Arial', sans-serif; font-size: 16px; color: #333; list-style-type: disc; margin-left: 20px;"),
      p("UI:", style = "font-weight: bold;"),
      tags$ul(
        tags$li(a("https://cupnooble.itch.io/sprout-lands-ui-pack?download", "https://cupnooble.itch.io/sprout-lands-ui-pack?download")),
        style = "font-family: 'Arial', sans-serif; font-size: 16px; color: #333; list-style-type: disc; margin-left: 20px;"),
      p("font:", style = "font-weight: bold;"),
            tags$ul(
              tags$li(a("https://www.urbanfonts.com/fonts/Gameplay.font", "https://www.urbanfonts.com/fonts/Gameplay.font")),
              style = "font-family: 'Arial', sans-serif; font-size: 16px; color: #333; list-style-type: disc; margin-left: 20px;"),
      div( # Center the content in this div
        style = "text-align: center;",
        p("products:", style = "font-weight: bold;"),
        tags$ul(
          tags$li(a("https://penzilla.itch.io/top-down-retro-house/download/eyJpZCI6MTc4NjI3OSwiZXhwaXJlcyI6MTY4NjAzMTYxOH0%3d.vYJFv3q%2faML4kiQ4rRHiJ4wizrA%3d", "https://penzilla.itch.io/top-down-retro-house/download/eyJpZCI6MTc4NjI3OSwiZXhwaXJlcyI6MTY4NjAzMTYxOH0%3d.vYJFv3q%2faML4kiQ4rRHiJ4wizrA%3d")),
          style = "font-family: 'Arial', sans-serif; font-size: 16px; color: #333; list-style-type: disc; margin-left: 20px;"
        )
      ),
      div( 
        style = "text-align: center; margin-top: 20px;",
        h1("Special thanks to Prof. Jackson and Prof. Lingjie for their guidance.", style = "font-size: 24px;")
      )
    ),
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
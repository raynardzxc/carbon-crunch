# Done by: Raynard, Zaina

credit_page <- function(id) {
  ns <- NS(id)
  fluidPage(
    div(
      class = "credit-div",
      h2(class = "credit-title", "Credits"),
      h3("Made with love by"),
      uiOutput(ns("developerTable")),
      h3("Assets used"),
      uiOutput(ns("assetTable")),
      h3("Research references"),
      uiOutput(ns("researchTable")),
      h3(class = "special-thanks",
         "Special thanks to Prof Peter Jackson, Prof Duan Lingjie, and Prof Francisco Benita for their guidance and support in this project."),
      div( # Center the back button
        style = "text-align: center;",
        actionButton(ns("back"), "Back", class = "final-button")
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
      
      output$developerTable <- renderUI({
        HTML(
          paste(
            "<table class='credit-table'>",
            "<thead><tr><th>Name</th><th>ID</th></tr></thead>",
            "<tbody>",
            paste("<tr><td>", c("Chai Yu Cheng, Raynard", "Nandini Prabakaran", "Ng Wei Xian", "Zaina Aafreen"), "</td><td>", c("1003436", "1005390", "1005937", "1006145"), "</td></tr>", collapse = ""),
            "</tbody>",
            "</table>"
          )
        )
      })
      
      output$assetTable <- renderUI({
        HTML(
          paste(
            "<table class='credit-table'>",
            "<thead><tr><th>Asset</th><th>Provider</th><th>Link</th></tr></thead>",
            "<tbody>",
            paste(
              "<tr><td>", c("Engine", "Floor", "Products", "Font", "Font", "Cursor"), "</td><td>",
              c("Sci-Fi Facility Asset Pack by Murphysdad", "TileMap SF by OSHQ Design Zone", 
                "Top-down Retro Interior House: Royalty free pixel art asset pack by penzilla", 
                "Gameplay Font by Geronimo","Munro Font by Ten by Twenty", "Leaf Set by Chee"), "</td><td>",
              c("<a href='https://murphysdad.itch.io/sci-fi-facility'>Link</a>", 
                "<a href='https://oshq.itch.io/tilemap-sf?download'>Link</a>", 
                "<a href='https://penzilla.itch.io/top-down-retro-house'>Link</a>",
                "<a href='https://www.urbanfonts.com/fonts/Gameplay.font'>Link</a>",
                "<a href='https://www.fontspace.com/munro-font-f14903'>Link</a>",
                "<a href='https://www.cursors-4u.com/cursor/2011/12/04/leaf-set.html'>Link</a>"), "</td></tr>",
              collapse = ""),
            "</tbody>",
            "</table>"
          )
        )
      })
      
      output$researchTable <- renderUI({
        HTML(
          paste(
            "<table class='credit-table'>",
            "<thead><tr><th>Research Article</th><th>Link</th></tr></thead>",
            "<tbody>",
            paste(
              "<tr><td>", c("Singapore’s carbon tax increase of up to 16-fold will make low carbon technologies and power imports cost competitive", 
                            "The impact of a carbon tax on manufacturing: Evidence from microdata"), "</td><td>",
              c("<a href='https://www.spglobal.com/commodityinsights/en/ci/research-analysis/singapores-carbon-tax-increase-of-up-to-16fold-will-make-low-c.html'>Link</a>", 
                "<a href='https://www.sciencedirect.com/science/article/pii/S0047272714001078'>Link</a>"), "</td></tr>",
              collapse = ""),
            "</tbody>",
            "</table>"
          )
        )
      })
    }
  )
}
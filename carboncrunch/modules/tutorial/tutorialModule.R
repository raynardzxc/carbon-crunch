tutorial_page <- function(id) {
  ns <- NS(id)
  div(
    div(
      img(src = "tutorial.png", height = "40px", width = "160px", style = "margin: 20px auto; display: block;"),
    div(
      h3("Objective", style = "text-align: left; margin-top: 10px;"),
      p("To balance between investing in sustainable technology (solar power) and maximizing profit at the end of 30 days while adhering to carbon emission levels."),
      h3("How to Play", style = "text-align: left; margin-top: 10px;"),
      div(
        style = "display: flex; align-items: center;margin-bottom: 20px;",
        img(src = "battery_4.png", height = "50px", width = "300px", style = "margin-right: 20px;"),
        p("Battery that indicates the amount of solar power stored and may be upgraded to increase storage capacity")
      ),
      div(
        style = "display: flex; align-items: center;margin-bottom: 20px;",
        img(src = "production_line_tut.jpg", height = "300px", width = "300px", style = "margin-right: 20px;"),
        p("5 production lines of two different types producing items of different value. Lines may be upgraded to increase efficiency")
      ),
      div(
        style = "display: flex; align-items: center; margin-bottom: 20px;",
        img(src = "toggle_fuel_tut.png", height = "45px", width = "45px", style = "margin-right: 20px;"),
        img(src = "toggle_sun_tut.png", height = "50px", width = "50px", style = "margin-right: 20px;"),
        p("Toggle switch at each production line to switch between using solar power or fossil fuels.If insufficient solar power is used to power a production line, the line will shut down.")
      ),
      div(
        style = "display: flex; align-items: center; margin-bottom: 20px;",
        img(src = "display_panel_tut.jpg", height = "400px", width = "300px", style = "margin-right: 20px;"),
        p(
          "Display panel with day indicator, Wallet, Carbon emission Bar, Dashboard, and Next day Button.", tags$br(),
          tags$ul(
            tags$li("Day indicator - displays current day/total number of days"),
            tags$li("Wallet - displays total money player has and may use for upgrade"),
            tags$li("Carbon Emission Bar - displays cumulative carbon emissions. Ensure carbon emissions remain within threshold!"),
            tags$li("Dashboard - displays profit and emissions generated previous day, along with Energy used (solar), Solar Gained, and Solar Overflow from the previous day"),
            tags$li("Next day Button - click to proceed to the next day once necessary upgrades and energy use decisions are made")
          )
        )
      ),
      div(
        style = "text-align: center; margin-top: 50px;",
        h1("What are you waiting for? Start Playing!")
      ),          
      div(
        style = "text-align: center;",
        PrimaryButton.shinyInput(
          ns("back"),
          class = ".btn",
          text = "Back to Home"
        )
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
tutorial_page <- function(id) {
  ns <- NS(id)
  div(class = "tut_div",
      titlePanel("Tutorial"),
      fluidRow(
        column(12, "Objective", 
               "To balance between investing in sustainable technology (solar power) and maximizing profit at the end of 30 days while adhering to carbon emission levels."),
        column(12, tags$img(src = "battery_4.png", height = 300, width = 300), 
               "Battery that indicates the amount of solar power stored and may be upgraded to increase storage capacity."),
        column(12, tags$img(src = "production_line_tut.jpg", height = 300, width = 300), 
               "Battery that indicates the amount of solar power stored and may be upgraded to increase storage capacity."),
        column(12, fluidRow(tags$img(src = "toggle_fuel_tut.png", height = 300, width = 300),
                            tags$img(src = "toggle_sun_tut.png", height = 300, width = 300)),
               "Toggle switch at each production line to switch between using solar power or fossil fuels.If insufficient solar power is used to power a production line, the line will shut down."),
        column(12, tags$img(src = "display_panel_tut.jpg", height = 300, width = 300), 
               "Battery that indicates the amount of solar power stored and may be upgraded to increase storage capacity."),
               ),
      
      # HTML(
      #   paste0(
      #     '<table>',
      #     "<tr>",
      #     "<th>kill</th>",
      #     "<th>me</th>",
      #     "</tr>",
      #     
      #     "<tr>",
      #     "<td>Objective</td>",
      #     "<td>To balance between investing in sustainable technology (solar power) and maximizing profit at the end of 30 days while adhering to carbon emission levels.</td>",
      #     "</tr>",
      #     
      #     "<tr>",
      #     "<td>How to Play</td>",
      #     "<td></td>",
      #     "</tr>",
      #     
      #     "<tr>",
      #     "<td>",'<img src = "battery_4.png" height = "auto" width = "50%">', "</td>",
      #     "<td>Battery that indicates the amount of solar power stored and may be upgraded to increase storage capacity</td>",
      #     "</tr>",
      #     
      #     "<tr>",
      #     "<td>",'<img src = "production_line_tut.jpg" height = "auto" width = "50%">',"</td>",
      #     "<td>5 production lines of two different types producing items of different value. Lines may be upgraded to increase efficiency.</td>",
      #     "</tr>",
      #   
      #     "<tr>",
      #     "<td>",'<img src = "toggle_fuel_tut.png">', '<img src = "toggle_sun_tut.png">',"</td>",
      #     "<td>Toggle switch at each production line to switch between using solar power or fossil fuels.If insufficient solar power is used to power a production line, the line will shut down.</td>",
      #     "</tr>",
      #     
      #     "<tr>",
      #     "<td>",'<img src = "display_panel_tut.jpg" height = "auto" width = "50%">', "</td>",
      #     "<td>Display panel with day indicator, Wallet, Carbon emission Bar, Dashboard, and Next day Button.", "<br>",
      #     "<ul>",
      #       "<li>Day indicator - displays current day/total number of days</li>",
      #       "<li>Wallet - displays total money player has and may use for upgrade</li>",
      #       "<li>Carbon Emission Bar - displays cumulative carbon emissions. Ensure carbon emissions remain within threshold!</li>",
      #       "<li>Dashboard - displays profit and emissions generated previous day, along with Energy used (solar), Solar Gained, and Solar Overflow from the previous day</li>",
      #       "<li>Next day Button - click to proceed to the next day once necessary upgrades and energy use decisions are made</li></ul>",
      #     "</td>",
      #     "</tr>",
      #   
      #     "</table>")
      #   ),
      
      
      div(
        style = "text-align: center;",
        actionButton(ns("back"), "Back", class = "final-button")
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
game_page <- function(id) {
  ns <- NS(id)
  div(class = "game-page",
    fluidRow(
      column(6,
             align="center",
             div(Text(variant = "xLarge", "Carbon Crunch Game!"), class = "big-text")),
      column(4),
      column(2,
             align="center",
             PrimaryButton.shinyInput(
               inputId = ns("back"),
               class=".btn",
               text="X"
             ))
    ),
    fluidRow(
      column(8,
             align="center",
             ### Battery Indicator
             div(class="battery-div",
               uiOutput(ns("battery")),
               textOutput(ns("battery_value"))
               ),
             
             ### Factory Floor
             div(class = "prodline-div",
                 fluidRow(
                   column(12,
                          PrimaryButton.shinyInput(
                            inputId = ns("PL1"),
                            class=".btn",
                            text="Upgrade 1"
                          ),
                          "Production line 1 graphics placeholder",
                          switchInput(inputId = ns("toggle1"), 
                                      offLabel = icon("sun","fa-solid"), ## https://fontawesome.com/icons we can only use free icons from here
                                      onLabel = icon("oil-well"),
                                      offStatus = "success", ## idk why the colours are named like that. idk if there are other colours
                                      onStatus = "danger",
                                      value = TRUE,
                                      inline = TRUE),
                   ),
                   column(12,
                          PrimaryButton.shinyInput(
                            inputId = ns("PL2"),
                            class=".btn",
                            text="Upgrade 2"
                          ),
                          "Production line 2 graphics placeholder",
                          switchInput(inputId = ns("toggle2"), 
                                      offLabel = icon("sun","fa-solid"), 
                                      onLabel = icon("oil-well"),
                                      offStatus = "success", 
                                      onStatus = "danger",
                                      value = TRUE,
                                      inline = TRUE),
                   ),
                   column(12,
                          PrimaryButton.shinyInput(
                            inputId = ns("PL3"),
                            class=".btn",
                            text="Upgrade 3"
                          ),
                          "Production line 3 graphics placeholder",
                          switchInput(inputId = ns("toggle3"), 
                                      offLabel = icon("sun","fa-solid"), 
                                      onLabel = icon("oil-well"),
                                      offStatus = "success", 
                                      onStatus = "danger",
                                      value = TRUE,
                                      inline = TRUE),
                   ),
                   column(12,
                          PrimaryButton.shinyInput(
                            inputId = ns("PL4"),
                            class=".btn",
                            text="Upgrade 4"
                          ),
                          "Production line 4 graphics placeholder",
                          switchInput(inputId = ns("toggle4"), 
                                      offLabel = icon("sun","fa-solid"), 
                                      onLabel = icon("oil-well"),
                                      offStatus = "success", 
                                      onStatus = "danger",
                                      value = FALSE,
                                      inline = TRUE),
                   ),
                   column(12,
                          PrimaryButton.shinyInput(
                            inputId = ns("PL5"),
                            class=".btn",
                            text="Upgrade 5"
                          ),
                          "Production line 5 graphics placeholder",
                          switchInput(inputId = ns("toggle5"), 
                                      offLabel = icon("sun","fa-solid"), 
                                      onLabel = icon("oil-well"),
                                      offStatus = "success", 
                                      onStatus = "danger",
                                      value = FALSE,
                                      inline = TRUE),
                   ),
                 ),
             )),
      
      ### Side panel (day indicator and dashboard)
      column(4,
             align="center",
             ### Panel
             div(class = "stats",
                 textOutput(ns("day")),
                 textOutput(ns("cash")),
                 textOutput(ns("emissions")),
                 uiOutput(ns("selected_component")),
                 uiOutput(ns("next_day_button"))
             ))
    )
  )
}

game_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # initialize state values
      day <- reactiveVal(1)
      cash <- reactiveVal(0)
      emissions <- reactiveVal(0)
      battery_value <- reactiveVal(15)
      battery_cap <- reactiveVal(15)
      sunlight <- reactiveVal(rgamma(1, shape = 2, scale = 3.5)) # This would give you a mean of 7 and a variance of 14.
      selected_component <- reactiveVal("None")
      summary_data <- reactiveVal() 
      
      resetGame <- function() {
        day(1)
        cash(0)
        emissions(0)
        battery_value(15)
        battery_cap(15)
        selected_component("None")
        summary_data <- reactiveVal() 
        print("resetGame")
      }
      
      generateUI <- function(name) {
        div(
          h3(paste0(name, " Selected")),
          p(paste0("Some descriptive text about ", name, "...")),
          actionButton(ns(paste0("button_", name, "_1")), paste0("Button 1 for ", name)),
          actionButton(ns(paste0("button_", name, "_2")), paste0("Button 2 for ", name))
        )
      }
      
      # Helper function to round if numeric
      round_if_numeric <- function(x) {
        if(is.numeric(x) && !is.na(x)) {
          round(x, 1)
        } else {
          x  # keep x as it is if it's not numeric or it's NA
        }
      }
      
      # update values shown
      output$battery_value <- renderText({ paste("Battery:", round_if_numeric(battery_value()),"/",battery_cap()) })
      output$day <- renderText({ paste("Day:", day()) })
      output$cash <- renderText({ paste("Cash:", cash()) })
      output$emissions <- renderText({ paste("Emissions:", emissions()) })
      
      output$battery <- renderUI({
        if (battery_value() >= 0) {
          actionButton(ns("battery"), img(src = "noun-battery-5868848.png", height = 100, width = 200))
        } else {
          actionButton(ns("battery"), img(src = "noun-battery-5868850.png", height = 100, width = 200)) # replace with your image source
        }
      })
      
      output$selected_component <- renderUI({
        req(selected_component())
        if (selected_component() == "PL1") {
          generateUI("Production Line 1")
        } else if (selected_component() == "PL2") {
          generateUI("Production Line 2")
        } else if (selected_component() == "PL3") {
          generateUI("Production Line 3")
        } else if (selected_component() == "PL4") {
          generateUI("Production Line 4")
        } else if (selected_component() == "PL5") {
          generateUI("Production Line 5")
        } else if (selected_component() == "Battery") {
          generateUI("Battery")
        } else if (selected_component() == "NextDay") {
          req(summary_data())  # Make sure summary_data exists
          
          if (is.null(summary_data())) return()
          
          # Generate row HTML strings for first 3 rows
          rows1to3 <- paste0(
            '<tr>',
            '<td>', summary_data()[2:4, "Category"], '</td>',
            '<td>', sapply(summary_data()[2:4, "Old.Value"], round_if_numeric), '</td>',
            '<td> -> </td>',
            '<td>', sapply(summary_data()[2:4, "New.Value"], round_if_numeric), '</td>',
            '<td>', ifelse(summary_data()[2:4, "Change"] >= 0, "+", ""), sapply(summary_data()[2:4, "Change"], round_if_numeric), '</td>',
            '</tr>',
            collapse = ""
          )
          
          # Generate row HTML strings for 'Solar gained' and 'Solar overflow' rows
          rows4to5 <- paste0(
            '<tr>',
            '<td>', summary_data()[5:6, "Category"], '</td>',
            '<td>', sapply(summary_data()[5:6, "New.Value"], round_if_numeric), '</td>',
            '<td></td>',
            '<td></td>',
            '<td></td>',
            '</tr>',
            collapse = ""
          )
          
          HTML(
            paste0(
              '<style>',
              'table {',
              '  margin-left: auto;',
              '  margin-right: auto;',
              '}',
              'table td, table th {',
              '  text-align: center;',
              '  vertical-align: middle;',
              '}',
              '</style>',
              '<table style="width:100%; border: none;">',
              '<tr><th></th><th>', round_if_numeric(summary_data()[1, "Old.Value"]), '</th><th> -> </th><th>', round_if_numeric(summary_data()[1, "New.Value"]), '</th><th></th></tr>',
              rows1to3,
              rows4to5,
              '</table>'
            )
          )
          
          
        } else {
          div(
            h3("No Component Selected"),
            p("Please select a component.")
          )
        }
      })
      
      
      output$next_day_button <- renderUI({
        if (day() < 30) {
          PrimaryButton.shinyInput(
            inputId = ns("next_day"),
            class=".btn",
            text="Next Day"
          )
        } else {
          PrimaryButton.shinyInput(
            inputId = ns("finish_game"),
            class=".btn",
            text="Finish Game"
          )
        }
      })
      
      
      observeEvent(input$battery, {
        selected_component("Battery")
        print("Observe Battery Clicked")
      })
      
      observeEvent(input$PL1, {
        selected_component("PL1")
      })
      
      observeEvent(input$PL2, {
        selected_component("PL2")
      })
      
      observeEvent(input$PL3, {
        selected_component("PL3")
      })
      
      observeEvent(input$PL4, {
        selected_component("PL4")
      })
      
      observeEvent(input$PL5, {
        selected_component("PL5")
      })
      
      observeEvent(input$next_day, {
        
        # Get the current values
        old_day <- day()
        old_cash <- cash()
        old_emissions <- emissions()
        old_battery_value <- battery_value()
        
        # Generate new sunlight value for the next day
        sunlight_value <- rgamma(1, shape = 2, scale = 3.5)
        sunlight_value <- min(sunlight_value, 10) # Ensure that sunlight value is within a reasonable range (e.g., 1 to 10)
        sunlight(sunlight_value)
        
        # Apply updates
        day(day() + 1)
        for(i in 1:5) {
          toggleValue <- input[[paste0("toggle", i)]]
          if(toggleValue == FALSE) {
            cash(cash() + 10) # Cash added
            battery_value(battery_value()-2) # Battery amount used
            updateProgressBar(
              session = session,
              id = ns("battery_bar"),
              value = battery_value()
            )
          } else if(toggleValue == TRUE) {
            cash(cash() + 15) # Cash added
            emissions(emissions() + 10) # Emissions generated
          }
        }
        
        # Add battery from sunlight
        added_from_sunlight <- sunlight()
        if (battery_value() + added_from_sunlight > battery_cap()) {
          overflow <- battery_value() + added_from_sunlight - battery_cap()
          battery_value(battery_cap())
        } else {
          battery_value(battery_value() + added_from_sunlight)
          overflow <- 0
        }
        
        # Get the changes
        change_in_day <- day() - old_day
        change_in_cash <- cash() - old_cash
        change_in_emissions <- emissions() - old_emissions
        change_in_battery <- battery_value() - old_battery_value
        
        # Create data frame
        summary_data(data.frame(
          Category = c("Day", "Cash", "Emissions", "Battery", "Solar gained", "Solar overflow"), 
          "Old Value" = c(old_day, old_cash, old_emissions, old_battery_value, NA, NA),
          "New Value" = c(day(), cash(), emissions(), battery_value(), added_from_sunlight, overflow),
          "Change" = c(change_in_day, change_in_cash, change_in_emissions, change_in_battery, NA, NA)
        ))
  
        # Update summary
        selected_component("NextDay")
        
      })
      
      
      observeEvent(input$finish_game, {
        # Reset the game and go back to the home page
        print("Finish Game")
        resetGame()
        change_page("analysis")
      })
      
      
      observeEvent(input$back, {
        showModal(
          Dialog(
            className = "custom-dialog",
            hidden = FALSE,
            title = "Confirmation",
            subText = "Are you sure you want to go back to home? All progress will be lost.",
            DialogFooter(
              DefaultButton.shinyInput(ns("cancelButton"), text="Cancel"),
              PrimaryButton.shinyInput(ns("confirm_back"), text="Yes")
            ),
            # Prevent automatic dismissal
            dismissOnClickOutside = FALSE,
            dismissOnEscape = FALSE
          )
        )
      })
      
      observeEvent(input$cancelButton, {
        removeModal()
        print("Cancel back to home dialog")
      })
      
      observeEvent(input$confirm_back, {
        resetGame()
        removeModal()
        change_page("/")
        print("Back to home")
      })
      
      
    }
  )
}
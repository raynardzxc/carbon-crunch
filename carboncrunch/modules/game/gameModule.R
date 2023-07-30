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
      
      ## STATE AND LOGIC VALUES
      
      # fixed values
      battery_df <- data.frame(level = 1:3,
                               capacity = c(10, 15, 20),
                               cost = c(0, 10, 20))
      
      pl_df <- data.frame(level = 1:3,
                          cash_generated = c(10, 15, 30),
                          emissions = c(10, 15, 20),
                          solar_consumption = c(2, 5, 10),
                          cost = c(0, 15, 30))
      
      # initialize state values
      battery_level <- reactiveVal(1)
      pl_levels <- reactiveVal(rep(1,5))
      
      day <- reactiveVal(1)
      cash <- reactiveVal(0)
      emissions <- reactiveVal(0)
      battery_value <- reactiveVal(5)
      
      sunlight <- reactiveVal(rgamma(1, shape = 2, scale = 3.5)) # This would give you a mean of 7 and a variance of 14.
      
      selected_component <- reactiveVal("None")
      summary_data <- reactiveVal() 
      
      battery_cap <- reactive({
        battery_df$capacity[battery_level()]
      })
      
      # Initialize cash generated, emissions, and solar consumption for each production line
      cash_generated <- reactive({
        pl_df$cash_generated[pl_levels()]
      })
      emissions_generated <- reactive({
        pl_df$emissions[pl_levels()]
      })
      solar_consumption <- reactive({
        pl_df$solar_consumption[pl_levels()]
      })
      
      # Check if the battery value is sufficient
      battery_is_sufficient <- reactive({
        battery_value() >= total_required_energy()
      })
      
      # lookup the stats according to the level
      battery_stats <- reactive({
        battery_df[battery_df$level == battery_level(),]
      })
      
      pl_stats <- reactive({
        pl_df[pl_df$level == pl_levels()[1],]
      })
      
      upgrade_cost_Battery <- reactive({
        if (battery_level() < 3) {
          battery_df[battery_df$level == battery_level() + 1,]$cost
        } else {
          NA
        }
      })
      
      # For the first production line
      upgrade_cost_ProductionLine1 <- reactive({
        if (pl_levels()[1] < 3) {
          pl_df[pl_df$level == pl_levels()[1] + 1,]$cost
        } else {
          NA
        }
      })
      
      # For the second production line
      upgrade_cost_ProductionLine2 <- reactive({
        if (pl_levels()[2] < 3) {
          pl_df[pl_df$level == pl_levels()[2] + 1,]$cost
        } else {
          NA
        }
      })
      
      # For the third production line
      upgrade_cost_ProductionLine3 <- reactive({
        if (pl_levels()[3] < 3) {
          pl_df[pl_df$level == pl_levels()[3] + 1,]$cost
        } else {
          NA
        }
      })
      
      # For the fourth production line
      upgrade_cost_ProductionLine4 <- reactive({
        if (pl_levels()[4] < 3) {
          pl_df[pl_df$level == pl_levels()[4] + 1,]$cost
        } else {
          NA
        }
      })
      
      # For the fifth production line
      upgrade_cost_ProductionLine5 <- reactive({
        if (pl_levels()[5] < 3) {
          pl_df[pl_df$level == pl_levels()[5] + 1,]$cost
        } else {
          NA
        }
      })
      
      # Compute the total required solar energy
      total_required_energy <- reactive({
        total_energy_needed = 0
        for (i in 1:5) {
          toggleValue <- input[[paste0("toggle", i)]]
          if (toggleValue == FALSE) {  # the production line is using solar energy
            total_energy_needed = total_energy_needed + solar_consumption()[i]
          }
        }
        total_energy_needed
      })
      
      ## FUNCTIONS
      
      resetGame <- function() {
        day(1)
        cash(0)
        emissions(0)
        battery_value(5)
        battery_level <- reactiveVal(1)
        pl_levels <- reactiveVal(rep(1,5))
        selected_component("None")
        summary_data <- reactiveVal() 
        print("resetGame")
      }
      
      generateUI <- function(name) {
        if (name == "Battery") {
          cur_battery_stats <- battery_stats()
          next_battery_stats <- if (battery_level() < 3) {
            battery_df[battery_df$level == battery_level() + 1,]
          } else {
            NULL
          }
          
          # Generate battery stats HTML
          battery_html <- paste0(
            "<tr>",
            "<th>", "", "</th>",
            "<th>", "Current", "</th>",
            "<th>", "Next Level", "</th>",
            "<th>", "Change", "</th>",
            "</tr>",
            "<tr>",
            "<td>", "Capacity: ", "</td>",
            "<td>", cur_battery_stats$capacity, "</td>",
            "<td>", if (!is.null(next_battery_stats)) { next_battery_stats$capacity } else { "N/A" }, "</td>",
            "<td>", if (!is.null(next_battery_stats)) { paste0("+", next_battery_stats$capacity - cur_battery_stats$capacity) } else { "N/A" }, "</td>",
            "</tr>",
            if (!is.null(next_battery_stats)) {
              paste0(
                "<tr>",
                "<td>", "Upgrade Cost: ", "</td>",
                "<td colspan='3'>", next_battery_stats$cost, "</td>",
                "</tr>"
              )
            }
          )
          
          div(
            h3(paste0(name, " Selected")),
            HTML(
              paste0(
                '<table style="width:100%; border: none;">',
                battery_html,
                '</table>'
              )
            ),
            if (!is.null(next_battery_stats)) {
              if (cash() >= next_battery_stats$cost) {
                actionButton(ns(paste0("upgrade_", name)), "Upgrade")
              }
            },
            actionButton(ns("cancel_upgrade"), "Cancel")
          )
          
        } else if (startsWith(name, "Production Line")) {
          print(name)
          pl_index <- as.integer(substr(name, nchar(name), nchar(name)))
          print(pl_index)
          cur_pl_stats <- pl_df[pl_df$level == pl_levels()[pl_index],]
          next_pl_stats <- if (pl_levels()[pl_index] < 3) {
            pl_df[pl_df$level == pl_levels()[pl_index] + 1,]
          } else {
            NULL
          }
          
          # Generate production line stats HTML
          pl_html <- paste0(
            "<tr>",
            "<th>", "", "</th>",
            "<th>", "Current", "</th>",
            "<th>", "Next Level", "</th>",
            "<th>", "Change", "</th>",
            "</tr>",
            "<tr>",
            "<td>", "Cash Generated: ", "</td>",
            "<td>", cur_pl_stats$cash_generated, "</td>",
            "<td>", if (!is.null(next_pl_stats)) { next_pl_stats$cash_generated } else { "N/A" }, "</td>",
            "<td>", if (!is.null(next_pl_stats)) { paste0("+", next_pl_stats$cash_generated - cur_pl_stats$cash_generated) } else { "N/A" }, "</td>",
            "</tr>",
            "<tr>",
            "<td>", "Emissions: ", "</td>",
            "<td>", cur_pl_stats$emissions, "</td>",
            "<td>", if (!is.null(next_pl_stats)) { next_pl_stats$emissions } else { "N/A" }, "</td>",
            "<td>", if (!is.null(next_pl_stats)) { paste0("+", next_pl_stats$emissions - cur_pl_stats$emissions) } else { "N/A" }, "</td>",
            "</tr>",
            "<tr>",
            "<td>", "Solar Consumption: ", "</td>",
            "<td>", cur_pl_stats$solar_consumption, "</td>",
            "<td>", if (!is.null(next_pl_stats)) { next_pl_stats$solar_consumption } else { "N/A" }, "</td>",
            "<td>", if (!is.null(next_pl_stats)) { paste0("+", next_pl_stats$solar_consumption - cur_pl_stats$solar_consumption) } else { "N/A" }, "</td>",
            "</tr>",
            if (!is.null(next_pl_stats)) {
              paste0(
                "<tr>",
                "<td>", "Upgrade Cost: ", "</td>",
                "<td colspan='3'>", next_pl_stats$cost, "</td>",
                "</tr>"
              )
            }
          )
          
          div(
            h3(paste0(name, " Selected")),
            HTML(
              paste0(
                '<table style="width:100%; border: none;">',
                pl_html,
                '</table>'
              )
            ),
            if (!is.null(next_pl_stats)) {
              if (cash() >= next_pl_stats$cost) {
                actionButton(ns(paste0("upgrade_PL", pl_index)), "Upgrade")
              }
            },
            actionButton(ns("cancel_upgrade"), "Cancel")
          )
        }
      }
      
      
      # Helper function to round if numeric
      round_if_numeric <- function(x) {
        if(is.numeric(x) && !is.na(x)) {
          round(x, 1)
        } else {
          x  # keep x as it is if it's not numeric or it's NA
        }
      }
      
      ## OUTPUT RENDERING
      
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
        if (day() < 30 && battery_is_sufficient()) {
          PrimaryButton.shinyInput(
            inputId = ns("next_day"),
            class=".btn",
            text="Next Day",
            disabled=FALSE # Ensure button is enabled again
          )
        } else if (!battery_is_sufficient()) {
          PrimaryButton.shinyInput(
            inputId = ns("next_day"),
            class=".btn",
            text="Next Day",
            disabled = TRUE  # Disable the button
          )
        } else {
          PrimaryButton.shinyInput(
            inputId = ns("finish_game"),
            class=".btn",
            text="Finish Game"
          )
        }
      })
      
      
      ## OBSERVE EVENTS
      
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
      
      # Upgrade Battery
      observeEvent(input$upgrade_Battery, {
        if (cash() >= upgrade_cost_Battery() && battery_level() < 3) {
          cash(cash() - upgrade_cost_Battery())
          battery_level(battery_level() + 1)
          selected_component("Upgraded") # reset selected component
        }
      })
      
      # For the first production line
      observeEvent(input$upgrade_PL1, {
        print("Upgrade button clicked")
        upgrade_cost <- if (pl_levels()[1] < 3) {
          pl_df[pl_df$level == pl_levels()[1] + 1,]$cost
        } else {
          NA
        }
        
        print(paste("Upgrade cost:", upgrade_cost))
        if (!is.na(upgrade_cost) && cash() >= upgrade_cost) {
          print("Upgrading...")
          cash(cash() - upgrade_cost) # deduct cost
          tmp <- pl_levels() # Get a copy of the current levels
          tmp[1] <- tmp[1] + 1 # Increase the level of the first production line
          pl_levels(tmp) # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          selected_component("None") # reset selected component
        }
      })
      
      # For the second production line
      observeEvent(input$upgrade_PL2, {
        upgrade_cost <- if (pl_levels()[2] < 3) {
          pl_df[pl_df$level == pl_levels()[2] + 1,]$cost
        } else {
          NA
        }
        
        if (!is.na(upgrade_cost) && cash() >= upgrade_cost) {
          cash(cash() - upgrade_cost) # deduct cost
          tmp <- pl_levels() # Get a copy of the current levels
          tmp[2] <- tmp[2] + 1 # Increase the level of the first production line
          pl_levels(tmp) # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          selected_component("None") # reset selected component
        }
      })
      
      # For the third production line
      observeEvent(input$upgrade_PL3, {
        upgrade_cost <- if (pl_levels()[3] < 3) {
          pl_df[pl_df$level == pl_levels()[3] + 1,]$cost
        } else {
          NA
        }
        
        if (!is.na(upgrade_cost) && cash() >= upgrade_cost) {
          cash(cash() - upgrade_cost) # deduct cost
          tmp <- pl_levels() # Get a copy of the current levels
          tmp[3] <- tmp[3] + 1 # Increase the level of the first production line
          pl_levels(tmp) # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          selected_component("None") # reset selected component
        }
      })
      
      # For the fourth production line
      observeEvent(input$upgrade_PL4, {
        upgrade_cost <- if (pl_levels()[4] < 3) {
          pl_df[pl_df$level == pl_levels()[4] + 1,]$cost
        } else {
          NA
        }
        
        if (!is.na(upgrade_cost) && cash() >= upgrade_cost) {
          cash(cash() - upgrade_cost) # deduct cost
          tmp <- pl_levels() # Get a copy of the current levels
          tmp[4] <- tmp[4] + 1 # Increase the level of the first production line
          pl_levels(tmp) # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          selected_component("None") # reset selected component
        }
      })
      
      # For the fifth production line
      observeEvent(input$upgrade_PL5, {
        upgrade_cost <- if (pl_levels()[5] < 3) {
          pl_df[pl_df$level == pl_levels()[5] + 1,]$cost
        } else {
          NA
        }
        
        if (!is.na(upgrade_cost) && cash() >= upgrade_cost) {
          cash(cash() - upgrade_cost) # deduct cost
          tmp <- pl_levels() # Get a copy of the current levels
          tmp[5] <- tmp[5] + 1 # Increase the level of the first production line
          pl_levels(tmp) # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          selected_component("None") # reset selected component
        }
      })
      
      # Cancel upgrade
      observeEvent(input$cancel_upgrade, {
        selected_component("None")
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
        # Production Line 1
        if(input$toggle1 == FALSE) {
          cash(cash() + cash_generated()[1]) # Cash added
          battery_value(battery_value()-solar_consumption()[1]) # Battery amount used
        } else if(input$toggle1 == TRUE) {
          cash(cash() + cash_generated()[1]) # Cash added
          emissions(emissions() + emissions_generated()[1]) # Emissions generated
        }
        # Production Line 2
        if(input$toggle2 == FALSE) {
          cash(cash() + cash_generated()[2]) # Cash added
          battery_value(battery_value()-solar_consumption()[2]) # Battery amount used
        } else if(input$toggle2 == TRUE) {
          cash(cash() + cash_generated()[2]) # Cash added
          emissions(emissions() + emissions_generated()[2]) # Emissions generated
        }
        # Production Line 3
        if(input$toggle3 == FALSE) {
          cash(cash() + cash_generated()[3]) # Cash added
          battery_value(battery_value()-solar_consumption()[3]) # Battery amount used
        } else if(input$toggle3 == TRUE) {
          cash(cash() + cash_generated()[3]) # Cash added
          emissions(emissions() + emissions_generated()[3]) # Emissions generated
        }
        # Production Line 4
        if(input$toggle4 == FALSE) {
          cash(cash() + cash_generated()[4]) # Cash added
          battery_value(battery_value()-solar_consumption()[4]) # Battery amount used
        } else if(input$toggle4 == TRUE) {
          cash(cash() + cash_generated()[4]) # Cash added
          emissions(emissions() + emissions_generated()[4]) # Emissions generated
        }
        # Production Line 5
        if(input$toggle5 == FALSE) {
          cash(cash() + cash_generated()[5]) # Cash added
          battery_value(battery_value()-solar_consumption()[5]) # Battery amount used
        } else if(input$toggle5 == TRUE) {
          cash(cash() + cash_generated()[5]) # Cash added
          emissions(emissions() + emissions_generated()[5]) # Emissions generated
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
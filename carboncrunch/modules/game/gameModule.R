source("helper.R")

game_page <- function(id) {
  ns <- NS(id)
  div(class = "game-page",
    fluidRow(
      column(6,
             align="center",
             p(class = "big-text", "Carbon Crunch Game!")),
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
                          textOutput(ns("PL1_text")),
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
                          textOutput(ns("PL2_text")),
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
                          textOutput(ns("PL3_text")),
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
                          textOutput(ns("PL4_text")),
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
                          textOutput(ns("PL5_text")),
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

game_server <- function(id, gameData) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      ## STATE AND LOGIC VALUES
      
      # fixed values
      battery_df <- getBatteryInfo()
      
      pl_df_temp <- getLineInfo()
      
      pl_df_typeA <- pl_df_temp[pl_df_temp$linetype == 0,]
      pl_df_typeB <- pl_df_temp[pl_df_temp$linetype == 1,]
      
      initial_df <- getInitialCond()
      
      # initialize state values
      battery_level <- reactiveVal(1)
      pl_levelsA <- reactiveVal(rep(1,3))
      pl_levelsB <- reactiveVal(rep(1,2))
      
      day <- reactiveVal(initial_df$day)
      cash <- reactiveVal(initial_df$cash)
      emissions <- reactiveVal(initial_df$emissions)
      battery_value <- reactiveVal(initial_df$batteryvalue)
      
      sunlight <- reactiveVal(rgamma(1, shape = 2, scale = 3.5)) # This would give you a mean of 7 and a variance of 14.
      
      selected_component <- reactiveVal("None")
      summary_data <- reactiveVal() 
      
      battery_cap <- reactive({
        battery_df$capacity[battery_df$level == battery_level()]
      })
      
      # Initialize cash generated, emissions, and solar consumption for each production line
      ## For Production Lines 1, 2 and 3
      cash_generatedA <- reactive({
        pl_df_typeA$cash_generated[pl_levelsA()]
      })
      emissions_generatedA <- reactive({
        pl_df_typeA$emissions[pl_levelsA()]
      })
      solar_consumptionA <- reactive({
        pl_df_typeA$solar_consumption[pl_levelsA()]
      })
      
      ## For Production Lines 4 and 5
      cash_generatedB <- reactive({
        pl_df_typeB$cash_generated[pl_levelsB()]
      })
      emissions_generatedB <- reactive({
        pl_df_typeB$emissions[pl_levelsB()]
      })
      solar_consumptionB <- reactive({
        pl_df_typeB$solar_consumption[pl_levelsB()]
      })
      
      # Check if the battery value is sufficient
      battery_is_sufficient <- reactive({
        battery_value() >= total_required_energy()
      })
      
      # lookup the stats according to the level
      battery_stats <- reactive({
        battery_df[battery_df$level == battery_level(),]
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
        if (pl_levelsA()[1] < 3) {
          pl_df_typeA[pl_df_typeA$level == pl_levelsA()[1] + 1,]$cost
        } else {
          NA
        }
      })
      
      # For the second production line
      upgrade_cost_ProductionLine2 <- reactive({
        if (pl_levelsA()[2] < 3) {
          pl_df_typeA[pl_df_typeA$level == pl_levelsA()[2] + 1,]$cost
        } else {
          NA
        }
      })
      
      # For the third production line
      upgrade_cost_ProductionLine3 <- reactive({
        if (pl_levelsA()[3] < 3) {
          pl_df_typeA[pl_df_typeA$level == pl_levelsA()[3] + 1,]$cost
        } else {
          NA
        }
      })
      
      # For the fourth production line
      upgrade_cost_ProductionLine4 <- reactive({
        if (pl_levelsB()[1] < 3) {
          pl_df_typeB[pl_df_typeB$level == pl_levelsB()[1] + 1,]$cost
        } else {
          NA
        }
      })
      
      # For the fifth production line
      upgrade_cost_ProductionLine5 <- reactive({
        if (pl_levelsB()[2] < 3) {
          pl_df_typeB[pl_df_typeB$level == pl_levelsB()[5] + 1,]$cost
        } else {
          NA
        }
      })
      
      # Compute the total required solar energy
      total_required_energy <- reactive({
        total_energy_needed = 0
        
        # process the first array (A)
        for (i in 1:length(pl_levelsA())) {
          toggleValue <- input[[paste0("toggle", i)]]
          if (toggleValue == FALSE) {  # the production line is using solar energy
            total_energy_needed = total_energy_needed + solar_consumptionA()[i]
          }
        }
        
        # process the second array (B)
        for (i in 1:length(pl_levelsB())) {
          toggleValue <- input[[paste0("toggle", i + length(pl_levelsA()))]]  # Offset i by the length of the first array
          if (toggleValue == FALSE) {  # the production line is using solar energy
            total_energy_needed = total_energy_needed + solar_consumptionB()[i]
          }
        }
        
        total_energy_needed
      })
      
      ## FUNCTIONS
      
      resetGame <- function() {
        day <- reactiveVal(initial_df$day)
        cash <- reactiveVal(initial_df$cash)
        emissions <- reactiveVal(initial_df$emissions)
        battery_value <- reactiveVal(initial_df$batteryvalue)
        battery_level <- reactiveVal(1)
        pl_levelsA <- reactiveVal(rep(1,3))
        pl_levelsB <- reactiveVal(rep(1,2))
        selected_component("None")
        summary_data <- reactiveVal() 
        gameData <- reactiveVal() 
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
          pl_index <- as.integer(substr(name, nchar(name), nchar(name)))
          
          # Define the upgrade button ID here
          upgrade_button_id <- paste0("upgrade_PL", pl_index)
          
          if (pl_index <= 3) {  # if the production line is of type A
            cur_pl_stats <- pl_df_typeA[pl_df_typeA$level == pl_levelsA()[pl_index],]
            next_pl_stats <- if (pl_levelsA()[pl_index] < 3) {
              pl_df_typeA[pl_df_typeA$level == pl_levelsA()[pl_index] + 1,]
            } else {
              NULL
            }
          } else {  # if the production line is of type B
            # Adjust pl_index for pl_levelsB and pl_df_typeB
            pl_index <- pl_index - length(pl_levelsA())
            
            cur_pl_stats <- pl_df_typeB[pl_df_typeB$level == pl_levelsB()[pl_index],]
            next_pl_stats <- if (pl_levelsB()[pl_index] < 3) {
              pl_df_typeB[pl_df_typeB$level == pl_levelsB()[pl_index] + 1,]
            } else {
              NULL
            }
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
                actionButton(ns(upgrade_button_id), "Upgrade")
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
      output$cash <- renderText({ paste("Cash ($):", cash()) })
      output$emissions <- renderText({ paste("Emissions (CO2e):", emissions(),"/ 6000") })
      output$PL1_text <- renderText({ paste("Production Line 1:", pl_levelsA()[1],"/ 3") })
      output$PL2_text <- renderText({ paste("Production Line 2:", pl_levelsA()[2],"/ 3") })
      output$PL3_text <- renderText({ paste("Production Line 3:", pl_levelsA()[3],"/ 3") })
      output$PL4_text <- renderText({ paste("Production Line 4:", pl_levelsB()[1],"/ 3") })
      output$PL5_text <- renderText({ paste("Production Line 5:", pl_levelsB()[2],"/ 3") })
      
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
          div(
            p("Insufficient Solar Energy"),
            PrimaryButton.shinyInput(
              inputId = ns("next_day"),
              class=".btn",
              text="Next Day",
              disabled = TRUE  # Disable the button
            )
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
        upgrade_cost <- if (pl_levelsA()[1] < 3) {
          pl_df_typeA[pl_df_typeA$level == pl_levelsA()[1] + 1,]$cost
        } else {
          NA
        }
        
        if (!is.na(upgrade_cost) && cash() >= upgrade_cost) {
          cash(cash() - upgrade_cost) # deduct cost
          tmp <- pl_levelsA() # Get a copy of the current levels
          tmp[1] <- tmp[1] + 1 # Increase the level of the first production line
          pl_levelsA(tmp) # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          selected_component("None") # reset selected component
        }
      })
      
      # For the second production line
      observeEvent(input$upgrade_PL2, {
        upgrade_cost <- if (pl_levelsA()[2] < 3) {
          pl_df_typeA[pl_df_typeA$level == pl_levelsA()[2] + 1,]$cost
        } else {
          NA
        }
        
        if (!is.na(upgrade_cost) && cash() >= upgrade_cost) {
          cash(cash() - upgrade_cost) # deduct cost
          tmp <- pl_levelsA() # Get a copy of the current levels
          tmp[2] <- tmp[2] + 1 # Increase the level of the first production line
          pl_levelsA(tmp) # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          selected_component("None") # reset selected component
        }
      })
      
      # For the third production line
      observeEvent(input$upgrade_PL3, {
        upgrade_cost <- if (pl_levelsA()[3] < 3) {
          pl_df_typeA[pl_df_typeA$level == pl_levelsA()[3] + 1,]$cost
        } else {
          NA
        }
        
        if (!is.na(upgrade_cost) && cash() >= upgrade_cost) {
          cash(cash() - upgrade_cost) # deduct cost
          tmp <- pl_levelsA() # Get a copy of the current levels
          tmp[3] <- tmp[3] + 1 # Increase the level of the first production line
          pl_levelsA(tmp) # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          selected_component("None") # reset selected component
        }
      })
      
      # For the fourth production line
      observeEvent(input$upgrade_PL4, {
        upgrade_cost <- if (pl_levelsB()[1] < 3) {
          pl_df_typeB[pl_df_typeB$level == pl_levelsB()[1] + 1,]$cost
        } else {
          NA
        }
        
        if (!is.na(upgrade_cost) && cash() >= upgrade_cost) {
          cash(cash() - upgrade_cost) # deduct cost
          tmp <- pl_levelsB() # Get a copy of the current levels
          tmp[1] <- tmp[1] + 1 # Increase the level of the first production line
          pl_levelsB(tmp) # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          selected_component("None") # reset selected component
        }
      })
      
      # For the fifth production line
      observeEvent(input$upgrade_PL5, {
        upgrade_cost <- if (pl_levelsB()[2] < 3) {
          pl_df_typeB[pl_df_typeB$level == pl_levelsB()[2] + 1,]$cost
        } else {
          NA
        }
        
        if (!is.na(upgrade_cost) && cash() >= upgrade_cost) {
          cash(cash() - upgrade_cost) # deduct cost
          tmp <- pl_levelsB() # Get a copy of the current levels
          tmp[2] <- tmp[2] + 1 # Increase the level of the first production line
          pl_levelsB(tmp) # Update the production levels
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
          cash(cash() + cash_generatedA()[1]) # Cash added
          battery_value(battery_value()-solar_consumptionA()[1]) # Battery amount used
        } else if(input$toggle1 == TRUE) {
          cash(cash() + cash_generatedA()[1]) # Cash added
          emissions(emissions() + emissions_generatedA()[1]) # Emissions generated
        }
        # Production Line 2
        if(input$toggle2 == FALSE) {
          cash(cash() + cash_generatedA()[2]) # Cash added
          battery_value(battery_value()-solar_consumptionA()[2]) # Battery amount used
        } else if(input$toggle2 == TRUE) {
          cash(cash() + cash_generatedA()[2]) # Cash added
          emissions(emissions() + emissions_generatedA()[2]) # Emissions generated
        }
        # Production Line 3
        if(input$toggle3 == FALSE) {
          cash(cash() + cash_generatedA()[3]) # Cash added
          battery_value(battery_value()-solar_consumptionA()[3]) # Battery amount used
        } else if(input$toggle3 == TRUE) {
          cash(cash() + cash_generatedA()[3]) # Cash added
          emissions(emissions() + emissions_generatedA()[3]) # Emissions generated
        }
        # Production Line 4
        if(input$toggle4 == FALSE) {
          cash(cash() + cash_generatedB()[1]) # Cash added
          battery_value(battery_value()-solar_consumptionB()[1]) # Battery amount used
        } else if(input$toggle4 == TRUE) {
          cash(cash() + cash_generatedB()[1]) # Cash added
          emissions(emissions() + emissions_generatedB()[1]) # Emissions generated
        }
        # Production Line 5
        if(input$toggle5 == FALSE) {
          cash(cash() + cash_generatedB()[2]) # Cash added
          battery_value(battery_value()-solar_consumptionB()[2]) # Battery amount used
        } else if(input$toggle5 == TRUE) {
          cash(cash() + cash_generatedB()[2]) # Cash added
          emissions(emissions() + emissions_generatedB()[2]) # Emissions generated
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
        result <- reactiveVal(list(cash = cash(), emissions = emissions()))
        gameData(result())
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
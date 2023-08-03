game_page <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "game-page",
    fluidRow(
      column(
        10,
        tags$div(
          class = "align-div",
          ### Battery Indicator
          uiOutput(ns("battery")),
          textOutput(ns("battery_value")),
          textOutput(ns("productionNerfWarning"))
        )
      ),
      column(
        2,
        tags$div(
          class = "exit-div",
          PrimaryButton.shinyInput(
            inputId = ns("back"),
            class = "exit-button"
          )
        )
      )
    ),
    fluidRow(
      column(
        8,
        ### Factory Floor
        tags$div(
          class = "factory-floor",
          fluidRow(
            column(
              4,
              tags$div(
                class = "pl-div",
                textOutput(ns("PL1_text1")),
                textOutput(ns("PL1_text2")),
                textOutput(ns("PL1_text3"))
              )
            ),
            column(
              6,
              uiOutput(ns("PL1"))
            ),
            column(
              2,
              switchInput(
                inputId = ns("toggle1"),
                offLabel = icon("sun", "fa-solid"), ## https://fontawesome.com/icons we can only use free icons from here
                onLabel = icon("oil-well"),
                offStatus = "success", ## idk why the colours are named like that. idk if there are other colours
                onStatus = "danger",
                value = TRUE,
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              4,
              tags$div(
                class = "pl-div",
                textOutput(ns("PL2_text1")),
                textOutput(ns("PL2_text2")),
                textOutput(ns("PL2_text3"))
              )
            ),
            column(
              6,
              uiOutput(ns("PL2"))
            ),
            column(
              2,
              switchInput(
                inputId = ns("toggle2"),
                offLabel = icon("sun", "fa-solid"),
                onLabel = icon("oil-well"),
                offStatus = "success",
                onStatus = "danger",
                value = TRUE,
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              4,
              tags$div(
                class = "pl-div",
                textOutput(ns("PL3_text1")),
                textOutput(ns("PL3_text2")),
                textOutput(ns("PL3_text3"))
              )
            ),
            column(
              6,
              uiOutput(ns("PL3"))
            ),
            column(
              2,
              switchInput(
                inputId = ns("toggle3"),
                offLabel = icon("sun", "fa-solid"),
                onLabel = icon("oil-well"),
                offStatus = "success",
                onStatus = "danger",
                value = TRUE,
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              4,
              tags$div(
                class = "pl-div",
                textOutput(ns("PL4_text1")),
                textOutput(ns("PL4_text2")),
                textOutput(ns("PL4_text3"))
              )
            ),
            column(
              6,
              uiOutput(ns("PL4"))
            ),
            column(
              2,
              switchInput(
                inputId = ns("toggle4"),
                offLabel = icon("sun", "fa-solid"),
                onLabel = icon("oil-well"),
                offStatus = "success",
                onStatus = "danger",
                value = TRUE,
                inline = TRUE
              )
            )
          ),
          fluidRow(
            column(
              4,
              tags$div(
                class = "pl-div",
                textOutput(ns("PL5_text1")),
                textOutput(ns("PL5_text2")),
                textOutput(ns("PL5_text3"))
              )
            ),
            column(
              6,
              uiOutput(ns("PL5"))
            ),
            column(
              2,
              switchInput(
                inputId = ns("toggle5"),
                offLabel = icon("sun", "fa-solid"),
                onLabel = icon("oil-well"),
                offStatus = "success",
                onStatus = "danger",
                value = TRUE,
                inline = TRUE
              )
            )
          )
        )
      ),

      ### Side panel (day indicator and dashboard)
      column(
        4,
        ### Panel
        tags$div(
          class = "side-panel",
          fluidRow(
            tags$div(
              class = "stats-div",
              textOutput(ns("day")),
              textOutput(ns("cash")),
              textOutput(ns("emissions"))
            )
          ),
          fluidRow(
            tags$div(
              class = "board-div",
              uiOutput(ns("selected_component"))
            )
          ),
          fluidRow(
            tags$div(
              class = "align-div",
              uiOutput(ns("next_day_button"))
            )
          )
        )
      )
    )
  )
}

game_server <- function(id, gameData) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      ## STATE AND LOGIC VALUES

      # math
      mean <- 23
      sd <- 8

      shape <- (mean / sd)^2
      scale <- mean / shape

      # fixed values
      battery_df <- getBatteryInfo()

      pl_df_temp <- getLineInfo()

      pl_df_typeA <- pl_df_temp[pl_df_temp$linetype == 0, ]
      pl_df_typeB <- pl_df_temp[pl_df_temp$linetype == 1, ]

      initial_df <- getInitialCond()
      carbon_limit <- initial_df$carbonlimit

      # Initial state of the game
      initial_game_state <- data.frame(
        Day = initial_df$day,
        Cash = initial_df$cash,
        Emissions = initial_df$emissions,
        Battery = initial_df$batteryvalue,
        Capacity = battery_df$capacity[battery_df$level == 1],
        SolarGained = 0,
        SolarOverflow = 0,
        CashGeneratedA1 = 0,
        CashGeneratedA2 = 0,
        CashGeneratedA3 = 0,
        CashGeneratedB1 = 0,
        CashGeneratedB2 = 0,
        EmissionsGeneratedA1 = 0,
        EmissionsGeneratedA2 = 0,
        EmissionsGeneratedA3 = 0,
        EmissionsGeneratedB1 = 0,
        EmissionsGeneratedB2 = 0,
        SolarConsumedA1 = 0,
        SolarConsumedA2 = 0,
        SolarConsumedA3 = 0,
        SolarConsumedB1 = 0,
        SolarConsumedB2 = 0
      )

      # initialize state values
      values <- reactiveValues()
      values$battery_level <- 1
      values$pl_levelsA <- rep(1, 3)
      values$pl_levelsB <- rep(1, 2)
      values$day <- initial_df$day
      values$cash <- initial_df$cash
      values$emissions <- initial_df$emissions
      values$battery_value <- initial_df$batteryvalue
      values$sunlight <- rgamma(1, shape = shape, scale = scale)
      values$selected_component <- "None"
      values$game_state_df <- initial_game_state
      values$summary_data <- NULL
      values$batt_upgrade <- 0
      values$line_upgrade <- 0

      battery_cap <- reactive({
        battery_df$capacity[battery_df$level == values$battery_level]
      })

      # Initialize cash generated, emissions, and solar consumption for each production line
      ## For Production Lines 1, 2 and 3
      cash_generatedA <- reactive({
        pl_df_typeA$cash_generated[values$pl_levelsA]
      })
      emissions_generatedA <- reactive({
        pl_df_typeA$emissions[values$pl_levelsA]
      })
      solar_consumptionA <- reactive({
        pl_df_typeA$solar_consumption[values$pl_levelsA]
      })

      ## For Production Lines 4 and 5
      cash_generatedB <- reactive({
        pl_df_typeB$cash_generated[values$pl_levelsB]
      })
      emissions_generatedB <- reactive({
        pl_df_typeB$emissions[values$pl_levelsB]
      })
      solar_consumptionB <- reactive({
        pl_df_typeB$solar_consumption[values$pl_levelsB]
      })

      # Check if the battery value is sufficient
      battery_is_sufficient <- reactive({
        values$battery_value >= total_required_energy()
      })

      # lookup the stats according to the level
      battery_stats <- reactive({
        battery_df[battery_df$level == values$battery_level, ]
      })

      # Compute production nerf factor
      production_nerf_factor <- reactive(({
        ifelse((values$battery_value / battery_cap()) < 0.3, 0.5, 1)
      }))

      upgrade_cost_Battery <- reactive({
        if (values$battery_level < 3) {
          battery_df[battery_df$level == values$battery_level + 1, ]$cost
        } else {
          NA
        }
      })

      # For the first production line
      upgrade_cost_ProductionLine1 <- reactive({
        if (values$pl_levelsA[1] < 3) {
          pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[1] + 1, ]$cost
        } else {
          NA
        }
      })

      # For the second production line
      upgrade_cost_ProductionLine2 <- reactive({
        if (values$pl_levelsA[2] < 3) {
          pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[2] + 1, ]$cost
        } else {
          NA
        }
      })

      # For the third production line
      upgrade_cost_ProductionLine3 <- reactive({
        if (values$pl_levelsA[3] < 3) {
          pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[3] + 1, ]$cost
        } else {
          NA
        }
      })

      # For the fourth production line
      upgrade_cost_ProductionLine4 <- reactive({
        if (values$pl_levelsB[1] < 3) {
          pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[1] + 1, ]$cost
        } else {
          NA
        }
      })

      # For the fifth production line
      upgrade_cost_ProductionLine5 <- reactive({
        if (values$pl_levelsB[2] < 3) {
          pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[5] + 1, ]$cost
        } else {
          NA
        }
      })

      # Compute the total required solar energy
      total_required_energy <- reactive({
        total_energy_needed <- 0

        # process the first array (A)
        for (i in 1:length(values$pl_levelsA)) {
          toggleValue <- input[[paste0("toggle", i)]]
          if (toggleValue == FALSE) { # the production line is using solar energy
            total_energy_needed <- total_energy_needed + solar_consumptionA()[i]
          }
        }

        # process the second array (B)
        for (i in 1:length(values$pl_levelsB)) {
          toggleValue <- input[[paste0("toggle", i + length(values$pl_levelsA))]] # Offset i by the length of the first array
          if (toggleValue == FALSE) { # the production line is using solar energy
            total_energy_needed <- total_energy_needed + solar_consumptionB()[i]
          }
        }

        total_energy_needed
      })

      ## FUNCTIONS

      resetGame <- function() {
        values$battery_level <- 1
        values$pl_levelsA <- rep(1, 3)
        values$pl_levelsB <- rep(1, 2)
        values$day <- initial_df$day
        values$cash <- initial_df$cash
        values$emissions <- initial_df$emissions
        values$battery_value <- initial_df$batteryvalue
        values$sunlight <- rgamma(1, shape = shape, scale = scale) # This should give you a mean of 10 and a variance of 9.
        values$selected_component <- "None"
        values$summary_data <- NULL
        gameData <- reactiveVal()
        print("resetGame")
      }

      generateUI <- function(name) {
        if (name == "Battery") {
          cur_battery_stats <- battery_stats()
          next_battery_stats <- if (values$battery_level < 3) {
            battery_df[battery_df$level == values$battery_level + 1, ]
          } else {
            NULL
          }

          # Generate battery stats HTML
          battery_html <- paste0(
            "<tr>",
            "<th>", "", "</th>",
            "<th>", "Current", "</th>",
            "<th>", "Next", "</th>",
            "<th>", "Change", "</th>",
            "</tr>",
            "<tr>",
            "<td>", "Capacity", "</td>",
            "<td>", cur_battery_stats$capacity, "</td>",
            "<td>", if (!is.null(next_battery_stats)) {
              next_battery_stats$capacity
            } else {
              "N/A"
            }, "</td>",
            "<td>", if (!is.null(next_battery_stats)) {
              paste0("+", next_battery_stats$capacity - cur_battery_stats$capacity)
            } else {
              "N/A"
            }, "</td>",
            "</tr>",
            if (!is.null(next_battery_stats)) {
              paste0(
                "<tr>",
                "<td>", "Upgrade Cost", "</td>",
                "<th>", "", "</th>",
                "<td>", next_battery_stats$cost, "</td>",
                "<th>", "", "</th>",
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
                "</table>"
              )
            ),
            if (!is.null(next_battery_stats)) {
              if (values$cash >= next_battery_stats$cost) {
                actionButton(ns(paste0("upgrade_", name)), "Upgrade")
              }
            },
            actionButton(ns("cancel_upgrade"), "Cancel")
          )
        } else if (startsWith(name, "Production Line")) {
          pl_index <- as.integer(substr(name, nchar(name), nchar(name)))

          # Define the upgrade button ID here
          upgrade_button_id <- paste0("upgrade_PL", pl_index)

          if (pl_index <= 3) { # if the production line is of type A
            cur_pl_stats <- pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[pl_index], ]
            next_pl_stats <- if (values$pl_levelsA[pl_index] < 3) {
              pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[pl_index] + 1, ]
            } else {
              NULL
            }
          } else { # if the production line is of type B
            # Adjust pl_index for pl_levelsB and pl_df_typeB
            pl_index <- pl_index - length(values$pl_levelsA)

            cur_pl_stats <- pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[pl_index], ]
            next_pl_stats <- if (values$pl_levelsB[pl_index] < 3) {
              pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[pl_index] + 1, ]
            } else {
              NULL
            }
          }

          # Generate production line stats HTML
          pl_html <- paste0(
            "<tr>",
            "<th>", "", "</th>",
            "<th>", "Current", "</th>",
            "<th>", "Next", "</th>",
            "<th>", "Change", "</th>",
            "</tr>",
            "<tr>",
            "<td>", "Cash Generated", "</td>",
            "<td>", cur_pl_stats$cash_generated, "</td>",
            "<td>", if (!is.null(next_pl_stats)) {
              next_pl_stats$cash_generated
            } else {
              "N/A"
            }, "</td>",
            "<td>", if (!is.null(next_pl_stats)) {
              paste0("+", next_pl_stats$cash_generated - cur_pl_stats$cash_generated)
            } else {
              "N/A"
            }, "</td>",
            "</tr>",
            "<tr>",
            "<td>", "Emissions", "</td>",
            "<td>", cur_pl_stats$emissions, "</td>",
            "<td>", if (!is.null(next_pl_stats)) {
              next_pl_stats$emissions
            } else {
              "N/A"
            }, "</td>",
            "<td>", if (!is.null(next_pl_stats)) {
              paste0("+", next_pl_stats$emissions - cur_pl_stats$emissions)
            } else {
              "N/A"
            }, "</td>",
            "</tr>",
            "<tr>",
            "<td>", "Solar Consumption", "</td>",
            "<td>", cur_pl_stats$solar_consumption, "</td>",
            "<td>", if (!is.null(next_pl_stats)) {
              next_pl_stats$solar_consumption
            } else {
              "N/A"
            }, "</td>",
            "<td>", if (!is.null(next_pl_stats)) {
              paste0("+", next_pl_stats$solar_consumption - cur_pl_stats$solar_consumption)
            } else {
              "N/A"
            }, "</td>",
            "</tr>",
            if (!is.null(next_pl_stats)) {
              paste0(
                "<tr>",
                "<td>", "Upgrade Cost", "</td>",
                "<th>", "", "</th>",
                "<td>", next_pl_stats$cost, "</td>",
                "<th>", "", "</th>",
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
                "</table>"
              )
            ),
            if (!is.null(next_pl_stats)) {
              if (values$cash >= next_pl_stats$cost) {
                actionButton(ns(upgrade_button_id), "Upgrade")
              }
            },
            actionButton(ns("cancel_upgrade"), "Cancel")
          )
        }
      }

      # Helper function to round if numeric
      round_if_numeric <- function(x) {
        if (is.numeric(x) && !is.na(x)) {
          round(x, 1)
        } else {
          x # keep x as it is if it's not numeric or it's NA
        }
      }

      ## OUTPUT RENDERING
      # update values shown
      output$battery_value <- renderText({
        paste("Battery:", round_if_numeric(values$battery_value), "/", battery_cap())
      })
      output$day <- renderText({
        paste0("Day:\t", values$day,"/30")
      })
      output$cash <- renderText({
        paste0("Cash:\t$", values$cash)
      })
      output$emissions <- renderText({
        paste0("Emissions:\t", values$emissions, "/", carbon_limit," CO2e")
      })

      ## Information for Production line 1
      output$PL1_text1 <- renderText({
        paste0("Line 1 Level: ", values$pl_levelsA[1], " / 3")
      })

      output$PL1_text2 <- renderText({
        if (input$toggle1 == FALSE) {
          paste0("Cash: +$", pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[1], ]$cash_generated*production_nerf_factor())
        } else {
          paste0("Cash: +$", pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[1], ]$cash_generated)
        }
      })

      output$PL1_text3 <- renderText({
        if (input$toggle1 == FALSE) {
          paste0("Battery: -", pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[1], ]$solar_consumption, " Units")
        } else {
          paste0("Emission: +", pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[1], ]$emissions, " CO2e")
        }
      })

      ## Information for Production line 2
      output$PL2_text1 <- renderText({
        paste0("Line 2 Level: ", values$pl_levelsA[2], " / 3")
      })

      output$PL2_text2 <- renderText({
        if (input$toggle2 == FALSE) {
          paste0("Cash: +$", pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[2], ]$cash_generated*production_nerf_factor())
        } else {
          paste0("Cash: +$", pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[2], ]$cash_generated)
        }
      })

      output$PL2_text3 <- renderText({
        if (input$toggle2 == FALSE) {
          paste0("Battery: -", pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[2], ]$solar_consumption, " Units")
        } else {
          paste0("Emission: +", pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[2], ]$emissions, " CO2e")
        }
      })

      ## Information for Production line 3
      output$PL3_text1 <- renderText({
        paste0("Line 3 Level: ", values$pl_levelsA[3], " / 3")
      })

      output$PL3_text2 <- renderText({
        if (input$toggle3 == FALSE) {
          paste0("Cash: +$", pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[3], ]$cash_generated*production_nerf_factor())
        } else {
          paste0("Cash: +$", pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[3], ]$cash_generated)
        }
      })

      output$PL3_text3 <- renderText({
        if (input$toggle3 == FALSE) {
          paste0("Battery: -", pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[3], ]$solar_consumption, " Units")
        } else {
          paste0("Emission: +", pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[3], ]$emissions, " CO2e")
        }
      })

      ## Information for Production line 4
      output$PL4_text1 <- renderText({
        paste0("Line 4 Level: ", values$pl_levelsB[1], " / 3")
      })

      output$PL4_text2 <- renderText({
        if (input$toggle4 == FALSE) {
          paste0("Cash: +$", pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[1], ]$cash_generated*production_nerf_factor())
        } else {
          paste0("Cash: +$", pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[1], ]$cash_generated)
        }
      })

      output$PL4_text3 <- renderText({
        if (input$toggle4 == FALSE) {
          paste0("Battery: -", pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[1], ]$solar_consumption, " Units")
        } else {
          paste0("Emission: +", pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[1], ]$emissions, " CO2e")
        }
      })

      ## Information for Production line 5
      output$PL5_text1 <- renderText({
        paste0("Line 5 Level: ", values$pl_levelsB[2], " / 3")
      })

      output$PL5_text2 <- renderText({
        if (input$toggle5 == FALSE) {
          paste0("Cash: +$", pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[2], ]$cash_generated*production_nerf_factor())
        } else {
          paste0("Cash: +$", pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[2], ]$cash_generated)
        }
      })

      output$PL5_text3 <- renderText({
        if (input$toggle5 == FALSE) {
          paste0("Battery: -", pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[2], ]$solar_consumption, " Units")
        } else {
          paste0("Emission: +", pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[2], ]$emissions, " CO2e")
        }
      })

      output$productionNerfWarning <- renderText({
        if (production_nerf_factor() < 1) {
          "Warning: Battery value is low. Cash generation from solar powered production lines is halved!"
        } else {
          "" # No warning when production is not nerfed
        }
      })

      output$PL1 <- renderUI({
        PrimaryButton.shinyInput(
          inputId = ns("PL1"),
          class = "pl1-button"
        )
      })

      output$PL2 <- renderUI({
        PrimaryButton.shinyInput(
          inputId = ns("PL2"),
          class = "pl2-button"
        )
      })

      output$PL3 <- renderUI({
        PrimaryButton.shinyInput(
          inputId = ns("PL3"),
          class = "pl3-button"
        )
      })

      output$PL4 <- renderUI({
        PrimaryButton.shinyInput(
          inputId = ns("PL4"),
          class = "pl4-button"
        )
      })

      output$PL5 <- renderUI({
        PrimaryButton.shinyInput(
          inputId = ns("PL5"),
          class = "pl5-button"
        )
      })

      output$battery <- renderUI({
        if (values$battery_value / battery_cap() >= 0.8) {
          PrimaryButton.shinyInput(
            ns("battery"),
            class = "battery5"
          )
        } else if (values$battery_value / battery_cap() >= 0.6 && values$battery_value / battery_cap() < 0.8) {
          PrimaryButton.shinyInput(
            ns("battery"),
            class = "battery4"
          )
        } else if (values$battery_value / battery_cap() >= 0.4 && values$battery_value / battery_cap() < 0.6) {
          PrimaryButton.shinyInput(
            ns("battery"),
            class = "battery3"
          )
        } else if (values$battery_value / battery_cap() >= 0.2 && values$battery_value / battery_cap() < 0.4) {
          PrimaryButton.shinyInput(
            ns("battery"),
            class = "battery2"
          )
        } else if (values$battery_value / battery_cap() > 0 && values$battery_value / battery_cap() < 0.2) {
          PrimaryButton.shinyInput(
            ns("battery"),
            class = "battery1"
          )
        } else {
          PrimaryButton.shinyInput(
            ns("battery"),
            class = "battery0"
          )
        }
      })

      output$selected_component <- renderUI({
        req(values$selected_component)
        if (values$selected_component == "PL1") {
          generateUI("Production Line 1")
        } else if (values$selected_component == "PL2") {
          generateUI("Production Line 2")
        } else if (values$selected_component == "PL3") {
          generateUI("Production Line 3")
        } else if (values$selected_component == "PL4") {
          generateUI("Production Line 4")
        } else if (values$selected_component == "PL5") {
          generateUI("Production Line 5")
        } else if (values$selected_component == "Battery") {
          generateUI("Battery")
        } else if (values$selected_component == "NextDay") {
          req(values$summary_data) # Make sure summary_data exists

          if (is.null(values$summary_data)) {
            return()
          }

          # Generate row HTML strings for first 3 rows
          rows1to3 <- paste0(
            "<tr>",
            "<td>", values$summary_data[2:4, "Category"], "</td>",
            "<td>", sapply(values$summary_data[2:4, "Old.Value"], round_if_numeric), "</td>",
            "<td> &#x2B95; </td>",
            "<td>", sapply(values$summary_data[2:4, "New.Value"], round_if_numeric), "</td>",
            "<td>", ifelse(values$summary_data[2:4, "Change"] >= 0, "+", ""), sapply(values$summary_data[2:4, "Change"], round_if_numeric), "</td>",
            "</tr>",
            collapse = ""
          )

          # Generate row HTML strings for 'Solar gained' and 'Solar overflow' rows
          rows4to5 <- paste0(
            "<tr>",
            "<td>", values$summary_data[5:6, "Category"], "</td>",
            "<td>", sapply(values$summary_data[5:6, "New.Value"], round_if_numeric), "</td>",
            "<td></td>",
            "<td></td>",
            "<td></td>",
            "</tr>",
            collapse = ""
          )

          HTML(
            paste0(
              "<style>",
              "table {",
              "  margin-left: auto;",
              "  margin-right: auto;",
              "}",
              "table td, table th {",
              "  text-align: center;",
              "  vertical-align: middle;",
              "}",
              "</style>",
              '<table style="width:100%; border: none;">',
              "<tr><th>", values$summary_data[1, "Category"], "</th><th>", round_if_numeric(values$summary_data[1, "Old.Value"]), "</th><th> &#x2B95; </th><th>", round_if_numeric(values$summary_data[1, "New.Value"]), "</th><th></th></tr>",
              rows1to3,
              rows4to5,
              "</table>"
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
        if (values$day < 30 && battery_is_sufficient()) { 
          PrimaryButton.shinyInput(
            inputId = ns("next_day"),
            class = "nextday-button",
            disabled = FALSE # Ensure button is enabled again
          )
        } else if (!battery_is_sufficient()) {
          div(
            p("Insufficient Solar Energy"),
            PrimaryButton.shinyInput(
              inputId = ns("next_day"),
              class = "nextday-button",
              disabled = TRUE # Disable the button
            )
          )
        } else {
          PrimaryButton.shinyInput(
            inputId = ns("finish_game"),
            class = "finish-button"
          )
        }
      })


      ## OBSERVE EVENTS

      observeEvent(input$battery, {
        values$selected_component <- "Battery"
        print("Observe Battery Clicked")
      })

      observeEvent(input$PL1, {
        values$selected_component <- "PL1"
      })

      observeEvent(input$PL2, {
        values$selected_component <- "PL2"
      })

      observeEvent(input$PL3, {
        values$selected_component <- "PL3"
      })

      observeEvent(input$PL4, {
        values$selected_component <- "PL4"
      })

      observeEvent(input$PL5, {
        values$selected_component <- "PL5"
      })

      # Upgrade Battery
      observeEvent(input$upgrade_Battery, {
        if (values$cash >= upgrade_cost_Battery() && values$battery_level < 3) {
          values$cash <- values$cash - upgrade_cost_Battery()
          values$battery_level <- values$battery_level + 1
          values$batt_upgrade <- values$batt_upgrade + upgrade_cost_Battery()
          values$selected_component <- "Upgraded" # reset selected component
        }
      })

      # For the first production line
      observeEvent(input$upgrade_PL1, {
        upgrade_cost <- if (values$pl_levelsA[1] < 3) {
          pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[1] + 1, ]$cost
        } else {
          NA
        }

        if (!is.na(upgrade_cost) && values$cash >= upgrade_cost) {
          values$cash <- values$cash - upgrade_cost # deduct cost
          values$line_upgrade <- values$line_upgrade + upgrade_cost
          tmp <- values$pl_levelsA # Get a copy of the current levels
          tmp[1] <- tmp[1] + 1 # Increase the level of the first production line
          values$pl_levelsA <- tmp # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          values$selected_component <- "Upgraded" # reset selected component
        }
      })

      # For the second production line
      observeEvent(input$upgrade_PL2, {
        upgrade_cost <- if (values$pl_levelsA[2] < 3) {
          pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[2] + 1, ]$cost
        } else {
          NA
        }

        if (!is.na(upgrade_cost) && values$cash >= upgrade_cost) {
          values$cash <- values$cash - upgrade_cost # deduct cost
          values$line_upgrade <- values$line_upgrade + upgrade_cost
          tmp <- values$pl_levelsA # Get a copy of the current levels
          tmp[2] <- tmp[2] + 1 # Increase the level of the first production line
          values$pl_levelsA <- tmp # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          values$selected_component <- "Upgraded" # reset selected component
        }
      })

      # For the third production line
      observeEvent(input$upgrade_PL3, {
        upgrade_cost <- if (values$pl_levelsA[3] < 3) {
          pl_df_typeA[pl_df_typeA$level == values$pl_levelsA[3] + 1, ]$cost
        } else {
          NA
        }

        if (!is.na(upgrade_cost) && values$cash >= upgrade_cost) {
          values$cash <- values$cash - upgrade_cost # deduct cost
          values$line_upgrade <- values$line_upgrade + upgrade_cost
          tmp <- values$pl_levelsA # Get a copy of the current levels
          tmp[3] <- tmp[3] + 1 # Increase the level of the first production line
          values$pl_levelsA <- tmp # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          values$selected_component <- "Upgraded" # reset selected component
        }
      })

      # For the fourth production line
      observeEvent(input$upgrade_PL4, {
        upgrade_cost <- if (values$pl_levelsB[1] < 3) {
          pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[1] + 1, ]$cost
        } else {
          NA
        }

        if (!is.na(upgrade_cost) && values$cash >= upgrade_cost) {
          values$cash <- values$cash - upgrade_cost # deduct cost
          values$line_upgrade <- values$line_upgrade + upgrade_cost
          tmp <- values$pl_levelsB # Get a copy of the current levels
          tmp[1] <- tmp[1] + 1 # Increase the level of the first production line
          values$pl_levelsB <- tmp # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          values$selected_component <- "Upgraded" # reset selected component
        }
      })

      # For the fifth production line
      observeEvent(input$upgrade_PL5, {
        upgrade_cost <- if (values$pl_levelsB[2] < 3) {
          pl_df_typeB[pl_df_typeB$level == values$pl_levelsB[2] + 1, ]$cost
        } else {
          NA
        }

        if (!is.na(upgrade_cost) && values$cash >= upgrade_cost) {
          values$cash <- values$cash - upgrade_cost # deduct cost
          values$line_upgrade <- values$line_upgrade + upgrade_cost
          tmp <- values$pl_levelsB # Get a copy of the current levels
          tmp[2] <- tmp[2] + 1 # Increase the level of the first production line
          values$pl_levelsB <- tmp # Update the production levels
          # No need to update cash_generated, emissions_generated, solar_consumption as they're reactive expressions
          values$selected_component <- "Upgraded" # reset selected component
        }
      })

      # Cancel upgrade
      observeEvent(input$cancel_upgrade, {
        values$selected_component <- "None"
      })

      observeEvent(input$next_day, {
        # Get the current values
        old_day <- values$day
        old_cash <- values$cash
        old_emissions <- values$emissions
        old_battery_value <- values$battery_value

        # Generate new sunlight value for the next day
        sunlight_value <- rgamma(1, shape = shape, scale = scale)
        values$sunlight <- sunlight_value

        # Apply updates
        values$day <- values$day + 1
        # Production Line 1
        if (input$toggle1 == FALSE) {
          values$cash <- values$cash + cash_generatedA()[1] * production_nerf_factor() # Cash added
          values$battery_value <- values$battery_value - solar_consumptionA()[1] # Battery amount used
        } else if (input$toggle1 == TRUE) {
          values$cash <- values$cash + cash_generatedA()[1] # Cash added
          values$emissions <- values$emissions + emissions_generatedA()[1] # Emissions generated
        }
        # Production Line 2
        if (input$toggle2 == FALSE) {
          values$cash <- values$cash + cash_generatedA()[2] * production_nerf_factor() # Cash added
          values$battery_value <- values$battery_value - solar_consumptionA()[2] # Battery amount used
        } else if (input$toggle2 == TRUE) {
          values$cash <- values$cash + cash_generatedA()[2] # Cash added
          values$emissions <- values$emissions + emissions_generatedA()[2] # Emissions generated
        }
        # Production Line 3
        if (input$toggle3 == FALSE) {
          values$cash <- values$cash + cash_generatedA()[3] * production_nerf_factor() # Cash added
          values$battery_value <- values$battery_value - solar_consumptionA()[3] # Battery amount used
        } else if (input$toggle3 == TRUE) {
          values$cash <- values$cash + cash_generatedA()[3] # Cash added
          values$emissions <- values$emissions + emissions_generatedA()[3] # Emissions generated
        }
        # Production Line 4
        if (input$toggle4 == FALSE) {
          values$cash <- values$cash + cash_generatedB()[1] * production_nerf_factor() # Cash added
          values$battery_value <- values$battery_value - solar_consumptionB()[1] # Battery amount used
        } else if (input$toggle4 == TRUE) {
          values$cash <- values$cash + cash_generatedB()[1] # Cash added
          values$emissions <- values$emissions + emissions_generatedB()[1] # Emissions generated
        }
        # Production Line 5
        if (input$toggle5 == FALSE) {
          values$cash <- values$cash + cash_generatedB()[2] * production_nerf_factor() # Cash added
          values$battery_value <- values$battery_value - solar_consumptionB()[2] # Battery amount used
        } else if (input$toggle5 == TRUE) {
          values$cash <- values$cash + cash_generatedB()[2] # Cash added
          values$emissions <- values$emissions + emissions_generatedB()[2] # Emissions generated
        }

        # Add battery from sunlight
        added_from_sunlight <- values$sunlight
        if (values$battery_value + added_from_sunlight > battery_cap()) {
          overflow <- values$battery_value + added_from_sunlight - battery_cap()
          values$battery_value <- battery_cap()
        } else {
          values$battery_value <- values$battery_value + added_from_sunlight
          overflow <- 0
        }

        # Get the changes
        change_in_day <- values$day - old_day
        change_in_cash <- values$cash - old_cash
        change_in_emissions <- values$emissions - old_emissions
        change_in_battery <- values$battery_value - old_battery_value

        # Create data frame
        values$summary_data <- data.frame(
          Category = c("Day", "Cash", "Emissions", "Battery", "Solar gained", "Solar overflow"),
          "Old Value" = c(old_day, old_cash, old_emissions, old_battery_value, NA, NA),
          "New Value" = c(values$day, values$cash, values$emissions, values$battery_value, added_from_sunlight, overflow),
          "Change" = c(change_in_day, change_in_cash, change_in_emissions, change_in_battery, NA, NA)
        )

        # Create new row
        new_day <- data.frame(
          Day = values$day,
          Cash = values$cash,
          Emissions = values$emissions,
          Battery = values$battery_value,
          Capacity = battery_cap(),
          SolarGained = added_from_sunlight,
          SolarOverflow = overflow,
          CashGeneratedA1 = cash_generatedA()[1],
          CashGeneratedA2 = cash_generatedA()[2],
          CashGeneratedA3 = cash_generatedA()[3],
          CashGeneratedB1 = cash_generatedB()[1],
          CashGeneratedB2 = cash_generatedB()[2],
          EmissionsGeneratedA1 = if (input$toggle1 == TRUE) {
            emissions_generatedA()[1]
          } else {
            0
          },
          EmissionsGeneratedA2 = if (input$toggle2 == TRUE) {
            emissions_generatedA()[2]
          } else {
            0
          },
          EmissionsGeneratedA3 = if (input$toggle3 == TRUE) {
            emissions_generatedA()[3]
          } else {
            0
          },
          EmissionsGeneratedB1 = if (input$toggle4 == TRUE) {
            emissions_generatedB()[1]
          } else {
            0
          },
          EmissionsGeneratedB2 = if (input$toggle5 == TRUE) {
            emissions_generatedB()[2]
          } else {
            0
          },
          SolarConsumedA1 = if (input$toggle1 == FALSE) {
            solar_consumptionA()[1]
          } else {
            0
          },
          SolarConsumedA2 = if (input$toggle2 == FALSE) {
            solar_consumptionA()[2]
          } else {
            0
          },
          SolarConsumedA3 = if (input$toggle3 == FALSE) {
            solar_consumptionA()[3]
          } else {
            0
          },
          SolarConsumedB1 = if (input$toggle4 == FALSE) {
            solar_consumptionB()[1]
          } else {
            0
          },
          SolarConsumedB2 = if (input$toggle5 == FALSE) {
            solar_consumptionB()[2]
          } else {
            0
          }
        )

        # Update game state data frame
        values$game_state_df <- rbind(values$game_state_df, new_day)
        # Update summary
        values$selected_component <- "NextDay"
      })


      observeEvent(input$finish_game, {
        # Reset the game and go back to the home page
        gameData(list(
          game_state = values$game_state_df,
          final_cash = values$cash,
          final_emissions = values$emissions,
          line_upgrade = values$line_upgrade,
          batt_upgrade = values$batt_upgrade
        ))
        change_page("analysis")
        resetGame()
      })
      
      observeEvent(input$back, {
        showModal(
          modalDialog(
            title = "Confirmation",
            "Are you sure you want to go back to home? All progress will be lost.",
            footer = tagList(
              modalButton("Cancel"),
              actionButton(inputId = ns("confirm_back"), "Yes")
            )
          )
        )
      })

      # observeEvent(input$cancelButton, {
      #   removeModal()
      #   print("Cancel back to home dialog")
      # })

      observeEvent(input$confirm_back, {
        resetGame()
        removeModal()
        change_page("/")
        print("Back to home")
      })
    }
  )
}

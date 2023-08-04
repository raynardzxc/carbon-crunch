analysis_page <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "analysis-page",
    titlePanel("Analysis Page"),
    fluidRow(
      column(
        8,
        tags$div(
          class = "analysis-div",
          bsCollapse(
            open = "Game Summary",
            bsCollapsePanel(
              "Game Summary",
              plotOutput(ns("summaryPlot"), height = "400px")
            ),
            bsCollapsePanel(
              "Production Line Analysis",
              selectInput(ns("view_select"), "View:",
                choices = c("Cash Generated", "Emissions Generated", "Solar Consumption"),
                selected = "Cash Generated"
              ),
              plotOutput(ns("combinedPlot"), height = "400px")
            ),
            bsCollapsePanel(
              "Solar/Carbon Utilisation",
              plotOutput(ns("solarcarbonPlot"), height = "400px")
            ),
            bsCollapsePanel(
              "Overflow Analysis",
              plotOutput(ns("overflowPlot"), height = "400px")
            )
          )
        )
      ),
      column(
        4,
        fluidRow(
          column(
            12,
            tags$div(
              class = "score-div",
              h3("Score Breakdown"),
              p("Gross Profit: ", textOutput(ns("grossProfit"))),
              p("Battery Upgrade Cost: ", textOutput(ns("batupCost"))),
              p("Line Upgrade Cost: ", textOutput(ns("lineupCost"))),
              p("Operating Profit: ", textOutput(ns("cashValue"))),
              uiOutput(ns("pb")),
              p("Final Score: ", textOutput(ns("finalScore"))),
              p("Total Emissions: ", textOutput(ns("emissionsValue"))),
            )
          )
        ),
        fluidRow(
          tags$div(
            class = "nav-div",
            actionButton(inputId = ns("back"), label = "Back to Home", class = "general-button"),
            actionButton(inputId = ns("publish"), label = "Publish Score", class = "general-button")
          )
        )
      )
    )
  )
}


analysis_server <- function(id, gameData) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # constants
      limit <- 350

      observeEvent(input$back, change_page("/"))

      observe({
        data <- gameData()
        if (!is.null(data)) {
          game_state_df <- data$game_state
          final_cash <- data$final_cash
          final_emissions <- data$final_emissions
          final_score <- ifelse(final_emissions > limit, final_cash - 5 * (final_emissions - limit), final_cash + 10 * (limit - final_emissions))
          # Calculate penalty or bonus
          pb <- final_score-final_cash
          # Add final_score to the existing data
          data$final_score <- final_score
          gameData(data)  # Update gameData

          # Calculate cumulative values
          game_state_df$CumulativeSolarGained <- cumsum(game_state_df$SolarGained)
          game_state_df$CumulativeSolarOverflow <- cumsum(game_state_df$SolarOverflow)
          game_state_df$CumulativeCarbonEmitted <- cumsum(game_state_df$Emissions)
          game_state_df <- game_state_df %>% mutate(SolarUsed = SolarConsumedA1 + 
                                                      SolarConsumedA2 + 
                                                      SolarConsumedA3 + 
                                                      SolarConsumedB1 +
                                                      SolarConsumedB2)
          game_state_df$CumulativeSolarUsed <- cumsum(game_state_df$SolarUsed)
          game_state_df <- game_state_df %>% mutate(BatteryOverflow = SolarOverflow + Battery)

          # Render the gross profit
          output$grossProfit <- renderText({
            paste("$", format(final_cash+data$batt_upgrade+data$line_upgrade, big.mark = ","))
          }) # formatted as currency
          
          # Render the total cost of battery upgrades
          output$batupCost <- renderText({
            paste("-$", format(data$batt_upgrade, big.mark = ","))
          }) # formatted as currency
          
          # Render the total cost of line upgrades
          output$lineupCost <- renderText({
            paste("-$", format(data$line_upgrade, big.mark = ","))
          }) # formatted as currency
          
          # Render the operating profit
          output$cashValue <- renderText({
            paste("$", format(final_cash, big.mark = ","))
          }) # formatted as currency
          
          # Render the penalty/bonus
          output$pb <- renderUI({
            if (pb>=0){ # bonus for going below threshold
              p("Emissions Bonus: ",pb, style = "color: green;")
            } else { # penalty for going above threshold
              p("Emissions Penalty: ",pb, style = "color: red;")
            }
          })
          
          # Render the net profit
          output$finalScore <- renderText({
            final_score
          })
          
          # Render the total emissions
          output$emissionsValue <- renderText({
            final_emissions
          })

          # Ensure that there is data to work with
          # Plot cash, emissions, battery, and solar gained over the days
          output$summaryPlot <- renderPlot({
            plot(game_state_df$Day, game_state_df$Cash, type = "l", xlab = "Day", ylab = "", col = "black")
            lines(game_state_df$Day, game_state_df$Emissions, col = "red")
            legend("topright",
              legend = c("Cash", "Emissions"),
              col = c("black", "red"), lty = 1, cex = 0.8
            )
          })
          
          # cum solar used, cum solar gain, cum carbon emit
          output$solarcarbonPlot <- renderPlot({
            # Calculate the maximum y-value across all three series
            ymax <- max(max(game_state_df$Emissions), max(game_state_df$CumulativeSolarGained), max(game_state_df$CumulativeSolarUsed))
            
            plot(game_state_df$Day, game_state_df$Emissions,
                 type = "n",
                 xlim = c(1, 30),
                 ylim = c(0, ymax),
                 xlab = "Day",
                 ylab = "",
                 main = "Solar/Fuel Utilisation"
            )
            
            lines(game_state_df$Day, game_state_df$Emissions, col = "red")
            lines(game_state_df$Day, game_state_df$CumulativeSolarGained, col = "green")
            lines(game_state_df$Day, game_state_df$CumulativeSolarUsed, col = "blue")
            
            legend("topright",
                   legend = c("Carbon Emissions", "Solar Energy Gained", "Solar Used"),
                   col = c("red", "green", "blue"),
                   lty = 1,
                   cex = 0.8
            )
          })
          
          # Plot the relationship between battery, capacity and overflow
          output$overflowPlot <- renderPlot({
            print(game_state_df$Capacity)
            print(game_state_df$Battery)
            print(game_state_df$BatteryOverflow)
            # Calculate the maximum y-value
            ymax <- max(game_state_df$BatteryOverflow)
            
            plot(game_state_df$Day, game_state_df$BatteryOverflow,
                 type = "n",
                 xlim = c(1, 30),
                 ylim = c(0, ymax),
                 xlab = "Day",
                 ylab = "",
                 main = "Overflow Analysis"
            )
            
            lines(game_state_df$Day, game_state_df$Capacity, col = "black")
            lines(game_state_df$Day, game_state_df$Battery, col = "green")
            lines(game_state_df$Day, game_state_df$BatteryOverflow, col = "red")
            
            polygon(c(1,game_state_df$Day,30), c(0,game_state_df$BatteryOverflow,0), col = "red")
            polygon(c(1,game_state_df$Day,30), c(0,game_state_df$Capacity,0), col = "black")
            polygon(c(1,game_state_df$Day,30), c(0,game_state_df$Battery,0), col = "green")
            
            legend("topright",
                   legend = c("Battery Capacity", "Stored energy", "Solar Overflow"),
                   col = c("black", "green", "red"),
                   lty = 1,
                   cex = 0.8
            )
          })

          output$combinedPlot <- renderPlot({
            if (input$view_select == "Cash Generated") {
              cash_generated <- colSums(game_state_df[, grep("CashGenerated", names(game_state_df))])
              barplot(cash_generated,
                main = "Total Cash Generated by Each Production Line",
                ylab = "Total Cash Generated", xlab = "Production Line"
              )
            } else if (input$view_select == "Emissions Generated") {
              emissions_generated <- colSums(game_state_df[, grep("EmissionsGenerated", names(game_state_df))])
              barplot(emissions_generated,
                main = "Total Emissions Generated by Each Production Line",
                ylab = "Total Emissions Generated", xlab = "Production Line", col = "red"
              )
            } else if (input$view_select == "Solar Consumption") {
              solar_consumed <- colSums(game_state_df[, grep("SolarConsumed", names(game_state_df))])
              barplot(solar_consumed,
                main = "Total Solar Consumption by Each Production Line",
                ylab = "Total Solar Consumed", xlab = "Production Line", col = "green"
              )
            }
          })
          
          # Solar Efficiency
          output$solarEfficiencyPlot <- renderPlot({
            solar_efficiency <- game_state_df$SolarGained / (game_state_df$SolarGained + game_state_df$SolarOverflow)
            plot(game_state_df$Day, solar_efficiency, type = "l", xlab = "Day", ylab = "Solar Efficiency", col = "orange")
          })
          # Cash-Emissions Ratio
          output$cashEmissionsRatioPlot <- renderPlot({
            cash_emissions_ratio <- game_state_df$Cash / game_state_df$Emissions
            plot(game_state_df$Day, cash_emissions_ratio, type = "l", xlab = "Day", ylab = "Cash-Emissions Ratio", col = "purple")
          })
          # Cash per Emission
          output$cashPerEmissionPlot <- renderPlot({
            cash_per_emission <- game_state_df$Cash / game_state_df$Emissions
            plot(game_state_df$Day, cash_per_emission, type = "l", xlab = "Day", ylab = "Cash per Emission", col = "green")
          })
        }
      })

      observeEvent(input$publish, {
        change_page("publish")
      })
      
      
    }
  )
}

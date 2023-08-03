analysis_page <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "analysis-page",
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
              "Battery Utilisation",
              plotOutput(ns("energyPlot"), height = "400px")
            ),
            bsCollapsePanel(
              "Production Line Analysis",
              selectInput(ns("view_select"), "View:",
                choices = c("Cash Generated", "Emissions Generated", "Solar Consumption"),
                selected = "Cash Generated"
              ),
              plotOutput(ns("combinedPlot"), height = "400px")
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
              titlePanel("Analysis Page"),
              p("Final Cash: ", textOutput(ns("cashValue"))),
              p("Final Emissions: ", textOutput(ns("emissionsValue"))),
              uiOutput(ns("pb")),
              p("Final Score: ", textOutput(ns("finalScore")))
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

          # Render cash and emissions values
          output$cashValue <- renderText({
            paste("$", format(final_cash, big.mark = ","))
          }) # formatted as currency
          output$emissionsValue <- renderText({
            final_emissions
          })
          output$pb <- renderUI({
            if (pb>=0){ # bonus for going below threshold
              p("Emissions Bonus: ",pb, style = "color: green;")
            } else { # penalty for going above threshold
              p("Emissions Penalty: ",pb, style = "color: red;")
            }
          })
          output$finalScore <- renderText({
            final_score
          })

          # Ensure that there is data to work with
          # Plot cash, emissions, battery, and solar gained over the days
          output$summaryPlot <- renderPlot({
            plot(game_state_df$Day, game_state_df$Cash, type = "l", xlab = "Day", ylab = "", col = "green")
            lines(game_state_df$Day, game_state_df$Emissions, col = "red")
            legend("topright",
              legend = c("Cash", "Emissions"),
              col = c("green", "red"), lty = 1, cex = 0.8
            )
          })

          # Plot Battery, Solar Gained and Solar Overflow over the days
          output$energyPlot <- renderPlot({
            # Calculate the maximum y-value across all three series
            ymax <- max(max(game_state_df$Battery), max(game_state_df$SolarGained), max(game_state_df$SolarOverflow))

            plot(game_state_df$Day, game_state_df$Battery,
              type = "n",
              ylim = c(0, ymax),
              xlab = "Day",
              ylab = "",
              main = "Energy Utilization"
            )

            lines(game_state_df$Day, game_state_df$Battery, col = "blue")
            lines(game_state_df$Day, game_state_df$SolarGained, col = "yellow")
            lines(game_state_df$Day, game_state_df$SolarOverflow, col = "orange")

            legend("topright",
              legend = c("Battery Level", "Solar Energy Gained", "Solar Overflow"),
              col = c("blue", "yellow", "orange"),
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
                ylab = "Total Solar Consumed", xlab = "Production Line", col = "blue"
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

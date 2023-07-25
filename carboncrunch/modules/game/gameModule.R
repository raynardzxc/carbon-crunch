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
             div(class = "battery",
                 progressBar(ns("battery_bar"), value = 10, total = 15),
             ),
             
             ### Factory Floor
             div(class = "prodline",
                 fluidRow(
                   column(12,
                          PrimaryButton.shinyInput(
                            inputId = ns("PL1"),
                            class=".btn",
                            text="Upgrade 1"
                          ),
                          "Production line 1",
                          switchInput(inputId = ns("toggle1"), ## idk why this switch is not on the same level as the other elements even though they are in the same row. 
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
                          "Production line 2",
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
                          "Production line 3",
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
                          "Production line 4",
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
                          "Production line 5",
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
      battery_value <- reactiveVal(10)
      battery_cap <- reactiveVal(15)
      selected_component <- reactiveVal("None")
      summary_data <- reactiveVal() 
      
      resetGame <- function() {
        day(1)
        cash(0)
        emissions(0)
        battery_value(10)
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
      
      # update values shown
      output$battery_value <- renderText({ paste("Battery:", battery_value(),"/",battery_cap()) })
      output$day <- renderText({ paste("Day:", day()) })
      output$cash <- renderText({ paste("Cash:", cash()) })
      output$emissions <- renderText({ paste("Emissions:", emissions()) })
      
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
        } else if (selected_component() == "NextDay") {
          req(summary_data())  # Make sure summary_data exists
          
          if (is.null(summary_data())) return()
          
          HTML(
            paste0(
              '<table border="1" style="width:100%">',
              '<tr><th>Category</th><th>Old Value</th><th>New Value</th><th>Change</th></tr>',
              paste0(
                '<tr>',
                '<td>', summary_data()$Category, '</td>',
                '<td>', summary_data()$Old.Value, '</td>',
                '<td>', summary_data()$New.Value, '</td>',
                '<td>', summary_data()$Change, '</td>',
                '</tr>',
                collapse = ""
              ),
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
      
      
      output$battery_bar <- renderUI({
        updateProgressBar(
          session = session,
          id = ns("battery_bar"),
          value = battery_value()
        )
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
        
        # Apply updates
        day(day() + 1)
        for(i in 1:5) {
          toggleValue <- input[[paste0("toggle", i)]]
          if(toggleValue == FALSE) {
            cash(cash() + 10)
            battery_value(battery_value()-2)
            updateProgressBar(
              session = session,
              id = ns("battery_bar"),
              value = battery_value()
            )
          } else if(toggleValue == TRUE) {
            cash(cash() + 15)
            emissions(emissions() + 10)
          }
        }
        
        # Get the changes
        change_in_day <- day() - old_day
        change_in_cash <- cash() - old_cash
        change_in_emissions <- emissions() - old_emissions
        
        # Create data frame
        summary_data(data.frame(
          Category = c("Day", "Cash", "Emissions"), 
          "Old Value" = c(old_day, old_cash, old_emissions),
          "New Value" = c(day(), cash(), emissions()),
          "Change" = c(change_in_day, change_in_cash, change_in_emissions)
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
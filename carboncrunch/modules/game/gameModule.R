game_page <- function(id) {
  ns <- NS(id)
  div(
    titlePanel("Game Page"),
    p("This is the game page"),
    PrimaryButton.shinyInput(
      inputId = ns("back"),
      class=".btn",
      text = "Back to Home"
    )
  )
}

game_page2 <- function(id) {
  ns <- NS(id)
  div(
    titlePanel("Game Page"),
    sidebarLayout(
      sidebarPanel(
        div(class = "battery",
          progressBar(ns("battery_bar"), value = 10, total = 15),
          #uiOutput(ns("battery")),
          textOutput(ns("battery_value")),
        ),
        div(class = "prodline",
          Toggle.shinyInput(inputId = ns("toggle1"), value = TRUE),
          Toggle.shinyInput(inputId = ns("toggle2"), value = TRUE),
          Toggle.shinyInput(inputId = ns("toggle3"), value = TRUE),
          Toggle.shinyInput(inputId = ns("toggle4"), value = FALSE),
          Toggle.shinyInput(inputId = ns("toggle5"), value = FALSE),
        ),
        width = 8),
      mainPanel(
        div(class = "stats",
          textOutput(ns("day")),
          textOutput(ns("cash")),
          textOutput(ns("emissions")),
          textOutput(ns("selected_component")),
          actionButton(ns("next_day"), "Next Day"),
          actionButton(ns("back"), "Back to Home"),
        ),
        width = 4),
      position = c("left", "right"),
      fluid = TRUE
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
      
      resetGame <- function() {
        day(1)
        cash(0)
        emissions(0)
        battery_value(10)
        battery_cap(15)
        selected_component("None")
      }
      
      # update values shown
      output$battery_value <- renderText({ paste("Battery:", battery_value(),"/",battery_cap()) })
      output$day <- renderText({ paste("Day:", day()) })
      output$cash <- renderText({ paste("Cash:", cash()) })
      output$emissions <- renderText({ paste("Emissions:", emissions()) })
      output$selected_component <- renderText({ paste("Selected component:", selected_component()) })
      
      output$battery_bar <- renderUI({
        updateProgressBar(
          session = session,
          id = ns("battery_bar"),
          value = battery_value()
        )
      })
      
      observeEvent(input$battery, {
        selected_component("Battery")
        print("hi")
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
        
        # Construct the summary string and set it as the value of selected_component
        summary_string <- paste0(
          "Day: ", old_day, " -> ", day(), " (", change_in_day, ")\n",
          "Cash: ", old_cash, " -> ", cash(), " (", change_in_cash, ")\n",
          "Emissions: ", old_emissions, " -> ", emissions(), " (", change_in_emissions, ")"
        )
        selected_component(summary_string)
        
        # Check if day is 30
        if(day() > 30) {
          # route to analysis
          resetGame()
        }
        
      })
      
      #not working
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
        print("Game resetted")
        removeModal()
        change_page("/")
        print("Back to home")
      })
      
      
    }
  )
}
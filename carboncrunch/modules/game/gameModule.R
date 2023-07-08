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
        uiOutput(ns("battery")),
        textOutput(ns("battery_level")),
        div(
          id = ns("prodline"),
          Toggle.shinyInput(inputId = ns("toggle1"), value = TRUE),
          Toggle.shinyInput(inputId = ns("toggle2"), value = TRUE),
          Toggle.shinyInput(inputId = ns("toggle3"), value = TRUE),
          Toggle.shinyInput(inputId = ns("toggle4"), value = FALSE),
          Toggle.shinyInput(inputId = ns("toggle5"), value = FALSE),
        ),
        width = 8),
      mainPanel(
        textOutput(ns("day")),
        textOutput(ns("cash")),
        textOutput(ns("emissions")),
        textOutput(ns("selected_component")),
        actionButton(ns("next_day"), "Next Day"),
        actionButton(ns("confirm_back"), "Back to Home"),
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
      battery_level <- reactiveVal(10)
      battery_cap <- reactiveVal(15)
      selected_component <- reactiveVal("None")
      
      resetGame <- function() {
        day(1)
        cash(0)
        emissions(0)
        battery_level(10)
        battery_cap(15)
        selected_component("None")
      }
      
      output$battery_level <- renderText({ paste("Battery:", battery_level(),"/",battery_cap()) })
      output$day <- renderText({ paste("Day:", day()) })
      output$cash <- renderText({ paste("Cash:", cash()) })
      output$emissions <- renderText({ paste("Emissions:", emissions()) })
      output$selected_component <- renderText({ paste("Selected component:", selected_component()) })
      
      output$battery <- renderUI({
        if (battery_level() >= 0) {
          actionButton(ns("battery"), img(src = "noun-battery-5868848.png", height = 100, width = 200))
        } else {
          actionButton(ns("battery"), img(src = "noun-battery-5868850.png", height = 100, width = 200)) # replace with your image source
        }
      })
      
      observeEvent(input$battery, {
        selected_component("Battery")
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
            battery_level(battery_level()-2)
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
            isOpen = TRUE,
            title = "Confirmation",
            subText = "Are you sure you want to go back to home? All progress will be lost.",
            DialogFooter(
              DefaultButton("Cancel", id = ns("cancelButton"), shinyInput = TRUE),
              PrimaryButton("Yes", onclick = JS("function() { Shiny.setInputValue('backConfirmed', true); }"))
            ),
            # Prevent automatic dismissal
            dismissOnClickOutside = FALSE,
            dismissOnEscape = FALSE
          )
        )
      })
      
      observeEvent(input$cancelButton, {
        removeModal()
      })
      
      observeEvent(input$confirm_back, {
        removeModal()
        change_page("/")
        resetGame()
      })
      
    }
  )
}
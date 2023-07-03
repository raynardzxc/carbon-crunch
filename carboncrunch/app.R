#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("usePackages.R")
pkgnames <- c("tidyverse","shiny", "shinyjs","DBI")
loadPkgs(pkgnames)

#feature Modules
source("router/routerModule.R")

#Helper Functions
source("router/dbHelper.R")

# Define UI for application
ui <- routerModuleUI("router")

# Define server logic
server <- function(input, output) {
  routerModuleServer("router")
}

# Run the application 
shinyApp(ui = ui, server = server)

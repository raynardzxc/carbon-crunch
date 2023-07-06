game_page <- div(
  titlePanel("Game Page"),
  p("This is the game page"),
  tags$li(a(href = route_link("/"), "Back"))
)
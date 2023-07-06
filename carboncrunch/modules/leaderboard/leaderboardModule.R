leaderboard_page <- div(
  titlePanel("Leaderboard"),
  p("This is the leaderboard page"),
  tags$li(a(href = route_link("/"), "Back"))
)
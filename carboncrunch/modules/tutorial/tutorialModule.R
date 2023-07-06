tutorial_page <- div(
  titlePanel("Tutorial Page"),
  p("This is the tutorial page"),
  tags$li(a(href = route_link("/"), "Back"))
)
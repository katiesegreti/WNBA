ui <- fluidPage(
  theme = shinytheme("cyborg"),
  
  # App title
  headerPanel("WNBA players"),
  p("Let's check out some awesome WNBA players"),
  
  # team filter
  fluidRow(
    column(
      width = 4,
      uiOutput("team_selector")
    ),
    column(
      width = 4,
      uiOutput("player_selector")
    )
  ),
  
  #player bio details
  uiOutput("player_bio"),
  
  #testing
  #uiOutput("testing"),
  shiny::tags$br(),
  #player tabs
  uiOutput("player_tabs")

)
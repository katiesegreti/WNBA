function(input, output, session) {
  
  
  output$testing <- renderUI({
    #str(selected_team())
    #str(selected_bio())
    #str(selected_name())
    #str(input$player_selector)
    #str(player_stats_1())
    #str(selected_team_fullname())
    #str(input$playerstats)
    str(wnba_shooting)
  })
  
  #team selector
  output$team_selector <- renderUI({
    selectInput(
      inputId = "team_selector",
      label = "Select team:",
      choices = team_names
    )
    
  })
  #get players on selected team
  selected_team <- reactive({
    req(input$team_selector)
    wnba_bio_2021 %>%
      filter(team == input$team_selector) %>%
      select(player, pos, dob, height, weight, college)
    # wnba_today %>% 
    # filter(tm == input$team_selector,
    #        season == "2020") %>%
    #   select(player, pos, dob, height, weight, college)
  })
  #long team name
  selected_team_fullname <- reactive({
    req(input$team_selector)
    names(team_names)[team_names == input$team_selector]
  })
  
  #player selector
  output$player_selector <- renderUI({
    req(input$team_selector)
    selectInput(
      inputId = "player_selector",
      label = "Select player:",
      choices = c("", selected_team()$player)
    )
  })
  
  #text message to select a player from the table
  output$player_selection <- renderText({
    req(selected_team())
    "Select a player to see their stats"
  })
  
  
  #reactable for selected team
  output$team_table <- renderReactable({
    reactable(selected_team(),
              defaultPageSize = 20,
              onClick = "select",
              selection = "single",
              columns = list(
                player = colDef(
                  name = "Player"
                ),
                pos = colDef(
                  name = "Position",
                  maxWidth = 80
                ),
                dob = colDef(
                  name = "DOB",
                  format = colFormat(date = TRUE, locales = "en-US"),
                  maxWidth = 100
                ),
                height = colDef(
                  name = "Height",
                  maxWidth = 80
                ),
                weight = colDef(
                  name = "Weight",
                  maxWidth = 80
                ),
                college = colDef(
                  name = "College/Country"
                )
              ),
              highlight = TRUE,
              striped = TRUE,
              bordered = TRUE,
              theme = reactableTheme(
                borderColor = "#FF671F",
                stripedColor = "#f6f8fa",
                highlightColor = "lightblue"
              ))
  })
  
  selected_player <- reactive({
    req(selected_team())
    getReactableState("team_table", "selected")
  })
  selected_name <- reactive({
    req(input$player_selector)
    #selected_team()$player[selected_player()]
    input$player_selector
  })
  selected_bio <- reactive({
    req(selected_name())
    wnba_bio_2021 %>%
      filter(player == selected_name()) %>%
      select(player, team, pos, height, weight, dob, college)
  })
  
  #get stats/history for selected player
  player_stats_1 <- reactive({
    req(selected_name())
    wnba_today %>%
      filter(player == selected_name()) %>%
      select(season, tm, g, gs, avg_mp, avg_fg, avg_fga, fg_percent, avg_3p, avg_3pa, x3p_percent, 
             avg_2p, avg_2pa, x2p_percent, avg_ft, avg_fta, ft_percent, avg_orb, avg_trb, avg_ast, 
             avg_stl, avg_blk, avg_tov, avg_pf, avg_pts) %>%
      arrange(desc(season))
  })
  
  #bio details
  output$player_bio <- renderUI({
    req(input$player_selector)
    
    fluidRow(
      h3(selected_name()),
      column(
        width = 4,
        p(paste0(selected_team_fullname()), " • ", selected_bio()$pos[1])
      ),
      column(
        width = 4,
        p(paste0("Height: ", selected_bio()$height[1])),
        HTML(paste0("Birthdate:", br(), format(selected_bio()$dob[1], format = "%B %d, %Y")))
      ),
      column(
        width = 4,
        p(paste0("Weight: ", selected_bio()$weight[1])),
        HTML(paste0("College/Country:", br(), selected_bio()$college[1]))
      )
    )
  })
  
  #history chart metric selector
  output$metric_history <- renderUI({
    req(selected_name())
    selectInput(
      inputId = "metric_history",
      label = "Select stat for chart:",
      choices = c("points", "FG%", "3P%", "rebounds", "assists", "blocks", "steals")
    )
  })
  
  #comparison chart metric selector
  output$metric_compare <- renderUI({
    req(selected_name())
    selectInput(
      inputId = "metric_compare",
      label = "Select stat for charts:",
      choices = c("points", "FG%", "3P%", "rebounds", "assists", "blocks", "steals")
    )
  })
  
  metric_to_compare <- reactive({
    req(input$metric_compare)
    input$metric_compare
  })
  

  
  #make chart for player history
  output$history_chart <- renderPlot({
    req(input$metric_history)
    if(input$metric_history == "points") {
      avgpoint_chart(selected_name())
    } else if(input$metric_history == "FG%") {
      fgpct_chart(selected_name())
    } else if(input$metric_history == "3P%") {
      threepct_chart(selected_name())
    } else if(input$metric_history == "rebounds") {
      rebound_chart(selected_name())
    } else if(input$metric_history == "assists") {
      assist_chart(selected_name())
    } else if(input$metric_history == "blocks") {
      block_chart(selected_name())
    } else if(input$metric_history == "steals") {
      steal_chart(selected_name())
    }
    
  })
    
  #make team comparison chart
  output$comparison_team <- renderPlot({
    req(metric_to_compare())
    if(input$player_selector != "" & !is.null(input$player_selector)) {
      if(input$metric_compare == "points"){
        team_compare_player_pts(selected_name(), input$team_selector, selected_team_fullname())
      } else if(metric_to_compare() == "FG%") {
        team_compare_player_fg(selected_name(), input$team_selector, selected_team_fullname())
      } else if(metric_to_compare() == "3P%") {
        team_compare_player_3p(selected_name(), input$team_selector, selected_team_fullname())
      } else if(metric_to_compare() == "rebounds") {
        team_compare_player_rebounds(selected_name(), input$team_selector, selected_team_fullname())
      } else if(metric_to_compare() == "assists") {
        team_compare_player_assists(selected_name(), input$team_selector, selected_team_fullname())
      } else if(metric_to_compare() == "blocks") {
        team_compare_player_blocks(selected_name(), input$team_selector, selected_team_fullname())
      } else if(metric_to_compare() == "steals") {
        team_compare_player_steals(selected_name(), input$team_selector, selected_team_fullname())
      }
      
    }
    
  })

  #make league comparison chart
  output$comparison_league <- renderPlot({
    req(metric_to_compare())
    if(input$player_selector != "" & !is.null(input$player_selector)) {
      if(metric_to_compare() == "points") {
        league_compare_player_pts(selected_name())
      } else if(metric_to_compare() == "FG%") {
        league_compare_player_fg(selected_name())
      } else if(metric_to_compare() == "3P%") {
        league_compare_player_3p(selected_name())
      } else if(metric_to_compare() == "rebounds") {
        league_compare_player_rebounds(selected_name())
      } else if(metric_to_compare() == "assists") {
        league_compare_player_assists(selected_name())
      } else if(metric_to_compare() == "blocks") {
        league_compare_player_blocks(selected_name())
      } else if(metric_to_compare() == "steals") {
        league_compare_player_steals(selected_name())
      }
    }
  })
  
  #shots chart
  output$shots_chart <- renderPlot({
    req(selected_name())
    if(input$player_selector != "" & !is.null(input$player_selector)) {
      shooting_chart(selected_name())
    }
  })
  
  # tabs for player stats
  output$player_tabs <- renderUI({
    req(selected_name())
    tabsetPanel(
      id = "playerstats",
      tabPanel("stats history",
               shiny::tags$br(),
               fluidRow(
                 column(
                   width = 6,
                   #reactable for selected player
                   output$player_table <- renderReactable({
                     reactable(player_stats_1(),
                               columns = list(
                                 season = colDef(
                                   name = "Season",
                                   maxWidth = 65,
                                   style = sticky_style,
                                   headerStyle = sticky_style
                                 ),
                                 tm = colDef(
                                   name = "Team",
                                   maxWidth = 62
                                 ),
                                 g = colDef(
                                   name = "G",
                                   maxWidth = 40
                                 ),
                                 gs = colDef(
                                   name = "GS",
                                   maxWidth = 40
                                 ),
                                 avg_mp = colDef(
                                   name = "MP",
                                   maxWidth = 50
                                 ),
                                 avg_fg = colDef(
                                   name = "FGM",
                                   maxWidth = 50
                                 ),
                                 avg_fga = colDef(
                                   name = "FGA",
                                   maxWidth = 50
                                 ),
                                 fg_percent = colDef(
                                   name = "FG%",
                                   maxWidth = 65,
                                   format = colFormat(percent = TRUE, digits = 1)
                                 ),
                                 avg_3p = colDef(
                                   name = "3PM",
                                   maxWidth = 50
                                 ),
                                 avg_3pa = colDef(
                                   name = "3PA",
                                   maxWidth = 50
                                 ),
                                 x3p_percent = colDef(
                                   name = "3P%",
                                   maxWidth = 65,
                                   format = colFormat(percent = TRUE, digits = 1)
                                 ),
                                 avg_2p = colDef(
                                   name = "2PM",
                                   maxWidth = 50
                                 ),
                                 avg_2pa = colDef(
                                   name = "2PA",
                                   maxWidth = 50
                                 ),
                                 x2p_percent = colDef(
                                   name = "2P%",
                                   maxWidth = 65,
                                   format = colFormat(percent = TRUE, digits = 1)
                                 ),
                                 avg_ft = colDef(
                                   name = "FTM",
                                   maxWidth = 50
                                 ),
                                 avg_fta = colDef(
                                   name = "FTA",
                                   maxWidth = 50
                                 ),
                                 ft_percent = colDef(
                                   name = "FT%",
                                   maxWidth = 65,
                                   format = colFormat(percent = TRUE, digits = 1)
                                 ),
                                 avg_orb = colDef(
                                   name = "OREB",
                                   maxWidth = 55
                                 ),
                                 avg_trb = colDef(
                                   name = "DREB",
                                   maxWidth = 55
                                 ),
                                 avg_ast = colDef(
                                   name = "AST",
                                   maxWidth = 50
                                 ),
                                 avg_stl = colDef(
                                   name = "STL",
                                   maxWidth = 50
                                 ),
                                 avg_blk = colDef(
                                   name = "BLK",
                                   maxWidth = 50
                                 ),
                                 avg_tov = colDef(
                                   name = "TOV",
                                   maxWidth = 50
                                 ),
                                 avg_pf = colDef(
                                   name = "PF",
                                   maxWidth = 50
                                 ),
                                 avg_pts = colDef(
                                   name = "PTS",
                                   maxWidth = 50
                                 )
                               ),
                               highlight = TRUE,
                               striped = TRUE,
                               bordered = TRUE,
                               fullWidth = FALSE,
                               theme = reactableTheme(
                                 borderColor = "#FF671F",
                                 stripedColor = "#f6f8fa",
                                 highlightColor = "lightblue"
                               )
                     )
                   })
                   
                 )
               ),
               
               shiny::tags$br(),
               uiOutput("metric_history"),
               fluidRow(
                 column(
                   width = 10,
                   plotOutput("history_chart") 
                 )
               )
               
      ),
      tabPanel("league/team comparisons",
               #select input for comparison metrics
               uiOutput("metric_compare"),
               fluidRow(
                 column(
                   width = 10,
                   plotOutput("comparison_team")
                 )
               ),
               shiny::tags$br(),
               fluidRow(
                 column(
                   width = 10,
                   plotOutput("comparison_league")
                 )
               )
               
               
      ),
      tabPanel("shooting",
               shiny::tags$br(),
               fluidRow(
                 column(
                   width = 8,
                   plotOutput("shots_chart")
                 )
               )
               )
    )
  })
  
}
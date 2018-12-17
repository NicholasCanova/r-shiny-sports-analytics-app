
## =============================================================== ##
# Copyright (C) 2017 Nicholas Canova
# 
# This launches the server side code for the shot chart tool
## =============================================================== ##

# the logo
output$sc.logo <- renderImage({
  # When input$n is 1, filename is ./images/image1.jpeg
  filename <- 'Images/teamlogos.jpg'
  
  this.width <- session$clientData$output_sc.logo_width
  this.height <- this.width / (1365 / 1024) # the file ratio
  
  # Return a list containing the filename
  list(src = filename,
       width = this.width,
       height = this.height)
}, deleteFile = FALSE)

# the ghostplot
output$sc.ghostplot <- renderPlot({
  df <- data.frame()
  ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
})

# initialize the reactive UI for charting
# =========================================
# select player for player-season graph
output$sc.playerseason.input <- renderUI({
  if(input$sc.shotchart.input %in% c('Shot Marker Graph (Player-Season)', 'Heat Map (Player-Season)')) {
    
    all.players <- unique(shot.pbp$fullname)
    names(all.players) <- all.players

    # selectizeInput(inputId = "sc.player.id", label = "Select Player:", choices = all.players, 
    #                selected = 'Stephen Curry', multiple = TRUE, options = list(maxItems = 1))
    
    selectInput(inputId = 'sc.player.id', label = 'Select Player:', multiple = FALSE,
                choices = all.players, selected = 'Stephen Curry')
    
  } else{
    return(NULL)
  }
})

# select team for team-game marker chart
output$sc.teamgame.input <- renderUI({
  if(input$sc.shotchart.input == 'Shot Marker Graph (Team-Game)') {
    all.teams <- unique(shot.pbp$team)
    names(all.teams) <- all.teams
    selectInput(inputId = 'sc.team.id', label = 'Select Team:', multiple = FALSE,
                choices = all.teams, selected = 'GSW')
  } else{
    return(NULL)
  }
})

# select opponent and date for team-game marker chart
output$sc.teamoppgame.input <- renderUI({
  if(input$sc.shotchart.input == 'Shot Marker Graph (Team-Game)') {
    
    req(input$sc.team.id)
    
    # dates to order the inputs
    all.gamedates <- c(opp.date.df$Date[opp.date.df$Team2 == input$sc.team.id],
                       opp.date.df$Date[opp.date.df$Team1 == input$sc.team.id])
    idx <- order(as.Date(all.gamedates))
    
    all.opps <- c(paste(opp.date.df$Team1[opp.date.df$Team2 == input$sc.team.id], 
                        opp.date.df$Date[opp.date.df$Team2 == input$sc.team.id]),
                  paste(opp.date.df$Team2[opp.date.df$Team1 == input$sc.team.id],
                        opp.date.df$Date[opp.date.df$Team1 == input$sc.team.id]))
    names(all.opps) <- all.opps
    all.opps <- all.opps[idx]
    
    selectInput(inputId = 'sc.opp.id', label = 'Select Opponent:', multiple = FALSE,
                choices = all.opps, selected = "HOU 2017-10-17")
  } else{
    return(NULL)
  }
})

# highlight individual player in team-game marker chart
output$sc.playergame.input <- renderUI({
  if(input$sc.shotchart.input == 'Shot Marker Graph (Team-Game)') {
    
    # players in the game
    req(input$sc.opp.id)
    req(input$sc.team.id)
    
    game.date <- gsub("-", "", strsplit(input$sc.opp.id, split = ' ')[[1]][2])
    first.t1 <- strsplit(input$sc.opp.id, split = ' ')[[1]][1]
    second.t2 <- input$sc.team.id
    
    the.gameid <- c(paste0(game.date, '-', first.t1, '-', second.t2),
                    paste0(game.date, '-', second.t2, '-', first.t1))
    
    this.pbp <- shot.pbp[shot.pbp$team == second.t2 & 
                           shot.pbp$msf.gameID %in% the.gameid, ] 
    
    teams.players <- c("All Players", unique(this.pbp$fullname))
    names(teams.players) <- teams.players
    
    selectInput(inputId = 'sc.playergame.id', label = 'Select Player to Highlight:', multiple = FALSE,
                choices = teams.players, selected = "All Players")
  } else{
    return(NULL)
  }
})

# select team for team-season heat chart
output$sc.teamseason.input <- renderUI({
  if(input$sc.shotchart.input == 'Heat Map (Team-Season)') {
    
    all.teams <- sort(unique(shot.pbp$team))
    names(all.teams) <- all.teams
    
    selectInput(inputId = 'sc.teamhex.id', label = 'Select Team:', multiple = FALSE,
                choices = all.teams, selected = 'GSW')
    
  } else{
    return(NULL)
  }
})
# =====

# Create The Shot Charts 
output$shotchart <- renderPlotly({
  
  # 3.B Marker Graphs
  # ===-===-===-===-===
  
  # 3.B.1 The Player-Season Graph
  # ===-===-===-===-===-===-===-===
  if(input$sc.shotchart.input == 'Shot Marker Graph (Player-Season)') {
    req(input$sc.player.id)
    this.fullname <- input$sc.player.id
    window.width <- session$clientData$output_sc.ghostplot_width
    
    shot.pbp <- shot.pbp %>% filter(fullname == this.fullname)
    plotMarkerChart(shot.pbp, this_fullname = this.fullname, plot_height = 0.8*window.width, plot_width = window.width)
  }
  
  # 3.B.2. The Team-Game Graph
  # ===-===-===-===-===-===-===-===
  else if(input$sc.shotchart.input == 'Shot Marker Graph (Team-Game)') {
    
    req(input$sc.opp.id); 
    req(input$sc.team.id); 
    req(input$sc.playergame.id);
    window.width <- session$clientData$output_sc.ghostplot_width
    
    plot.date <- strsplit(input$sc.opp.id, split = ' ')[[1]][2]
    this.date <- gsub("-", "", plot.date)
    this.t1 <- strsplit(input$sc.opp.id, split = ' ')[[1]][1]
    this.t2 <- input$sc.team.id
    
    this.gameid <- c(paste0(this.date, '-', this.t1, '-', this.t2),
                     paste0(this.date, '-', this.t2, '-', this.t1))
    this.gameid <- this.gameid[this.gameid %in% shot.pbp$msf.gameID]
    
    # this.gameid is == character(0) for the split second after changing team. for this period, lets return NULL
    if(length(this.gameid) == 0) { return(NULL) }
    
    shot.pbp <- shot.pbp %>% filter(msf.gameID == this.gameid)
    plotMarkerChart(shot.pbp, game.label = this.gameid, plot_height = 0.8*window.width, plot_width = window.width)
  }
  
  # 3.C Hex Graphs
  # ===-===-===-===-===
  
  # 3.C.1. The Player-Season Graph 
  # ===-===-===-===-===-===-===-===-===
  else if(input$sc.shotchart.input == 'Heat Map (Player-Season)') {
    
    req(input$sc.player.id)
    this.fullname <- input$sc.player.id
    window.width <- session$clientData$output_sc.ghostplot_width
    
    # RIGHT HERE IS WHERE I GOTTA GET THIS WORKING
    player.heatmap <- createHexPlotDF(shot.pbp, this_fullname = this.fullname, plotheight = 0.8*window.width, divisor = 31)
    plotHexChart(player.heatmap = player.heatmap, this_fullname = this.fullname, plot_height = 0.8*window.width, plot_width = window.width)
  }
  
  # 3.C.2. The Team-Season Graph 
  # ===-===-===-===-===-===-===-===-===
  else {
    
    req(input$sc.teamhex.id)
    req(session$clientData$output_sc.ghostplot_width)
    team_name <- input$sc.teamhex.id
    window.width <- session$clientData$output_sc.ghostplot_width
    print(window.width)
    
    player.heatmap <- createHexPlotDF(shot.pbp, this_team = team_name, plotheight = 0.8*window.width, divisor = 31)
    plotHexChart(player.heatmap = player.heatmap, this_team = team_name, plot_height = 0.8*window.width, plot_width = window.width)
  }
  
})
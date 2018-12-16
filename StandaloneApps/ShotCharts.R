
# ======================
# 0. Setup 
# ======================
rm(list = ls())
setwd('/Users/Home/Dropbox/My_Documents/Not_Work_or_School/Sports Analytics/InteractiveSportsGraphs/')
Sys.setenv(TZ='EST')

# grabbing data / data manipulation
library(readr)
library(plyr)
library(dplyr)

# libaries for plotting
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)

# source charts
source('charts.R')
# ====

# ================================
# 1. Read And Preprocess Data
# ================================

# read the new CSV file, add fullnames column
all.pbp <- read_csv('Data/MSF_Feeds/NBA_gameplaybyplay_shotcharts.csv')
all.pbp$fullname <- paste(all.pbp$firstname, all.pbp$lastname)

# save a dataframe with opponents and game dates
opp.date.df <- strsplit(unique(all.pbp$msf.gameID), split = '-') %>%
  do.call(rbind, .) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  setNames(c('Date', 'Team1', 'Team2')) %>%
  mutate(Date = as.Date(Date, format = '%Y%m%d'))


# create selectinput vector of graph types
shotchart.types <- c('Shot Marker Graph (Player-Season)', 'Shot Marker Graph (Team-Game)',
                     'Heat Map (Player-Season)', 'Heat Map (Team-Season)')
names(shotchart.types) <- shotchart.types
# ====



# ========================
# 2. USER INTERFACE
# ========================
ui <- fluidPage(theme = shinytheme('united'),
                
                # 2.A Create Shiny App Title 
                # ===-===-===-===-===-===-===-===
                fluidRow(
                  column(width = 12, offset = 2,
                         h2('NBA Shot Charts and Heat Maps Application'))
                ),
                
                fluidRow(
                  
                  # 2.B Create All Widgets For App
                  # ===-===-===-===-===-===-===-===
                  column(width = 3, align = 'center',
                         
                         # header, and selectInput debugger
                         h3('Chart Type'), hr(),
                         tags$head(tags$style(HTML(".shiny-split-layout > div {
                                                   overflow: visible; } "))),
                         
                         # create permanent input for shot chart type (should be 5 options)
                         selectInput(inputId = 'shotchart.input', label = 'Select Shot Chart Type:', multiple = FALSE,
                                     choices = shotchart.types, selected = 'Heat Map (Team-Season)'),

                         uiOutput('playerseason.input'),
                         uiOutput('teamgame.input'),
                         uiOutput('teamoppgame.input'),
                         uiOutput('playergame.input'),
                         uiOutput('teamseason.input')
                         
                  ),
                         
                  # 2.C Launch the Chart
                  # ===-===-===-===-===-===
                  column(width = 8, align = 'left',
                         br(),
                         plotlyOutput("shotplot")
                  )
                )
)
# ====


# ========================
# 3. SERVER 
# ========================
server <- shinyServer(function(input, output) {

  # 3.A format the other input parameters
  # ===-===-===-===-===-===-===-===-===-===
  # select player for player-season graph
  output$playerseason.input <- renderUI({
    if(input$shotchart.input %in% c('Shot Marker Graph (Player-Season)', 'Heat Map (Player-Season)')) {
      
      all.players <- unique(all.pbp$fullname)
      names(all.players) <- all.players
      
      selectInput(inputId = 'player.id', label = 'Select Player:', multiple = FALSE,
                  choices = all.players, selected = 'Stephen Curry')
      
    } else{
      return(NULL)
    }
  })
  
  # select team for team-game marker chart
  output$teamgame.input <- renderUI({
    if(input$shotchart.input == 'Shot Marker Graph (Team-Game)') {
      all.teams <- unique(all.pbp$team)
      names(all.teams) <- all.teams
      selectInput(inputId = 'team.id', label = 'Select Team:', multiple = FALSE,
                  choices = all.teams, selected = 'GSW')
    } else{
      return(NULL)
    }
  })

  # select opponent and date for team-game marker chart
  output$teamoppgame.input <- renderUI({
    if(input$shotchart.input == 'Shot Marker Graph (Team-Game)') {

      req(input$team.id)

      # dates to order the inputs
      all.gamedates <- c(opp.date.df$Date[opp.date.df$Team2 == input$team.id],
                         opp.date.df$Date[opp.date.df$Team1 == input$team.id])
      idx <- order(as.Date(all.gamedates))
      
      all.opps <- c(paste(opp.date.df$Team1[opp.date.df$Team2 == input$team.id], 
                          opp.date.df$Date[opp.date.df$Team2 == input$team.id]),
                    paste(opp.date.df$Team2[opp.date.df$Team1 == input$team.id],
                          opp.date.df$Date[opp.date.df$Team1 == input$team.id]))
      names(all.opps) <- all.opps
      all.opps <- all.opps[idx]

      selectInput(inputId = 'opp.id', label = 'Select Opponent:', multiple = FALSE,
                  choices = all.opps, selected = "HOU 2017-10-17")
    } else{
      return(NULL)
    }
  })
  
  # highlight individual player in team-game marker chart
  output$playergame.input <- renderUI({
    if(input$shotchart.input == 'Shot Marker Graph (Team-Game)') {

      # players in the game
      req(input$opp.id)
      req(input$team.id)

      game.date <- gsub("-", "", strsplit(input$opp.id, split = ' ')[[1]][2])
      first.t1 <- strsplit(input$opp.id, split = ' ')[[1]][1]
      second.t2 <- input$team.id
      
      the.gameid <- c(paste0(game.date, '-', first.t1, '-', second.t2),
                       paste0(game.date, '-', second.t2, '-', first.t1))
      
      this.pbp <- all.pbp[all.pbp$team == second.t2 & 
                          all.pbp$msf.gameID %in% the.gameid, ] 

      teams.players <- c("All Players", unique(this.pbp$fullname))
      names(teams.players) <- teams.players
      
      selectInput(inputId = 'playergame.id', label = 'Select Player to Highlight:', multiple = FALSE,
                  choices = teams.players, selected = "All Players")
    } else{
      return(NULL)
    }
  })
  
  # select team for team-season heat chart
  output$teamseason.input <- renderUI({
    if(input$shotchart.input == 'Heat Map (Team-Season)') {
      
      all.teams <- sort(unique(all.pbp$team))
      names(all.teams) <- all.teams
      
      selectInput(inputId = 'teamhex.id', label = 'Select Team:', multiple = FALSE,
                  choices = all.teams, selected = 'GSW')
      
    } else{
      return(NULL)
    }
  })


  # Create The Shot Charts 
  output$shotplot <- renderPlotly({
    
    # 3.B Marker Graphs
    # ===-===-===-===-===
        
    # 3.B.1 The Player-Season Graph
    # ===-===-===-===-===-===-===-===
    if(input$shotchart.input == 'Shot Marker Graph (Player-Season)') {
      req(input$player.id)
      
      # f_name <- strsplit(input$player.id, split = ' ')[[1]][1]
      # l_name <- strsplit(input$player.id, split = ' ')[[1]][2]

      # all.pbp <- all.pbp %>% filter(firstname == f_name, lastname == l_name)
      # plotMarkerChart(all.pbp, lname = l_name, fname = f_name, plot_height = 800)
      
      this.fullname <- input$player.id
      
      all.pbp <- all.pbp %>% filter(fullname == this.fullname)
      plotMarkerChart(all.pbp, this_fullname = this.fullname, plot_height = 800)
    }
    
    # 3.B.2. The Team-Game Graph
    # ===-===-===-===-===-===-===-===
    else if(input$shotchart.input == 'Shot Marker Graph (Team-Game)') {
      
      req(input$opp.id); 
      req(input$team.id); 
      req(input$playergame.id);
      
      plot.date <- strsplit(input$opp.id, split = ' ')[[1]][2]
      this.date <- gsub("-", "", plot.date)
      this.t1 <- strsplit(input$opp.id, split = ' ')[[1]][1]
      this.t2 <- input$team.id
      
      this.gameid <- c(paste0(this.date, '-', this.t1, '-', this.t2),
                       paste0(this.date, '-', this.t2, '-', this.t1))
      this.gameid <- this.gameid[this.gameid %in% all.pbp$msf.gameID]
      
      # this.gameid is == character(0) for the split second after changing team. for this period, lets return NULL
      if(length(this.gameid) == 0) { return(NULL) }
      
      all.pbp <- all.pbp %>% filter(msf.gameID == this.gameid)  
      plotMarkerChart(all.pbp, game.label = this.gameid, plot_height = 800)
    }
   
    # 3.C Hex Graphs
    # ===-===-===-===-===
    
    # 3.C.1. The Player-Season Graph 
    # ===-===-===-===-===-===-===-===-===
    else if(input$shotchart.input == 'Heat Map (Player-Season)') {

      # Trevor Ariza had different zone calculations
      req(input$player.id)
      this.fullname <- input$player.id
      
      player.heatmap <- createHexPlotDF(all.pbp, this_fullname = this.fullname, plotheight = 800)
      plotHexChart(player.heatmap = player.heatmap, this_fullname = this.fullname, plot_height = 800)
      
  
            
      # req(input$player.id)
      # 
      # f_name <- strsplit(input$player.id, split = ' ')[[1]][1]
      # l_name <- strsplit(input$player.id, split = ' ')[[1]][2]
      # 
      # player.heatmap <- createHexPlotDF(all.pbp, fname = f_name, lname = l_name, plotheight = 800)
      # plotHexChart(player.heatmap = player.heatmap, lname = l_name, fname = f_name, plot_height = 800)
    }
        
    # 3.C.2. The Team-Season Graph 
    # ===-===-===-===-===-===-===-===-===
    # else if(input$shotchart.input == 'Heat Map (Team-Season)')
    else {

      req(input$teamhex.id)
      team_name <- input$teamhex.id
      
      player.heatmap <- createHexPlotDF(all.pbp, this_team = team_name, plotheight = 800)
      plotHexChart(player.heatmap = player.heatmap, this_team = team_name, plot_height = 800)
    }
    
  })
})

shinyApp(ui, server)
# ====

# name_id_map <- structure(list(team_id = c("ATL", "BOS", "BRO", "CHA", "CHI", 
#                            "CLE", "DAL", "DEN", "DET", "GSW", "HOU", "IND", "LAC", "LAL", 
#                            "MEM", "MIA", "MIL", "MIN", "NOP", "NYK", "OKL", "ORL", "PHI", 
#                            "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS"), 
#                fran_id = c("Hawks","Celtics", "Nets", "Hornets", "Bulls", "Cavaliers", "Mavericks", 
#                            "Nuggets", "Pistons", "Warriors", "Rockets", "Pacers", "Clippers", 
#                            "Lakers", "Grizzlies", "Heat", "Bucks", "Timberwolves", "Pelicans", 
#                            "Knicks", "Thunder", "Magic", "Sixers", "Suns", "Trailblazers", 
#                            "Kings", "Spurs", "Raptors", "Jazz", "Wizards")), 
#           .Names = c("team_id", "fran_id"), row.names = c(NA, -30L), class = "data.frame")


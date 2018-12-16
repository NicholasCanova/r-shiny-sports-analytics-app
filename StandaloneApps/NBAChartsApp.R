
# ======================
# 0. SETUP 
# ======================
setwd("/Users/Home/Dropbox/My_Documents/Not_Work_or_School/Sports Analytics/InteractiveSportsGraphs/")
rm(list = ls())

# libaries for plotting
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)

# connecting to the API
library(readr)
library(dplyr)
# ====


# =================================
# 1. FORMAT DATA / PREPROCESSING
# =================================

# THINK ABOUT REFORMATTING IN A TIDY MANNER
# write_csv(player.data, 'NBA_cumulativeplayerstats_helper.csv')
# player.data <- read_csv('NBA_cumulativeplayerstats_helper.csv')
# colnames(player.data) <- make.names(colnames(player.data))
# 
# # compute seconds threshold to filter players (top 300 players by time played)
# secs_threshold <- sort(player.data$stats.MinSeconds..text, 
#                        partial = length(player.data$stats.MinSeconds..text)-299)[zed-299]
# 
# player.stats <- player.data %>%
#   filter(stats.MinSeconds..text >= secs_threshold) %>%
#   group_by(player.ID) %>%
#   dplyr::summarize(lastName = min(player.LastName),
#                    firstName = min(player.FirstName),
#                    POS = min(player.Position),
#                    Team = min(team.Name),
#                    GP = stats.GamesPlayed..text,
#                    PTS36 = round(36 * 60 * stats.Pts..text / stats.MinSeconds..text, 2))

# grab the season data
# player.data <- msf_get_results(league = 'nba', 
#                                season = '2017-2018-regular',
#                                feed = 'cumulative_player_stats')
# player.data <- player.data$api_json$cumulativeplayerstats$playerstatsentry
# 
# # grab all data columns that would be used for any of the plots
# keepers <- c('player.LastName', 'player.FirstName', 'player.Position', 'team.Name', 
#              'stats.FgAtt.#text', 'stats.FtAtt.#text',
#              'stats.Reb.#text', 'stats.Ast.#text', 'stats.Pts.#text',
#              'stats.Tov.#text', 'stats.Stl.#text', 'stats.Blk.#text', 
#              'stats.MinSeconds.#text'
#              )
# player.stats <- player.data[, names(player.data) %in% keepers]
# colnames(player.stats) = c('lastName', 'firstName', 'POS', 'Team', 'FGA', 'FTA', 'REB', 'AST', 'PTS', 'TOV', 'STL', 'BLK', 'SEC')

player.stats <- read_csv('Data/MSF_Feeds/NBA_cumulativeplayerstats.csv')

# create my color palette for the graph
color.pal.df <- data.frame(colorhex = c('#006BB6', '#2C5234', '#BA0C2F', '#6F263D', '#007A33', '#D50032',
                                        '#23375B', '#C8102E', '#862633', '#201747', '#002B5C', '#724C9F',
                                        '#FF671F', '#702F8A', '#007DC5', '#0050B5', '#010101', '#418FDE', 
                                        '#041E42', '#002B5C', '#003DA5', '#CE1141', '#BA0C2F', '#B6BFBF',
                                        '#E56020', '#007DC3', '#7AC143', '#F0163A', '#FFC72D', '#0C2340'),
                           teamname = c("76ers", "Bucks", "Bulls", "Cavaliers", "Celtics", "Clippers", 
                                        "Grizzlies", "Hawks", "Heat", "Hornets", "Jazz", "Kings", 
                                        "Knicks", "Lakers", "Magic", "Mavericks", "Nets", "Nuggets", 
                                        "Pacers", "Pelicans", "Pistons", "Raptors", "Rockets", "Spurs", 
                                        "Suns", "Thunder", "Timberwolves", "Trail Blazers", "Warriors", "Wizards"), 
                           stringsAsFactors = FALSE)


# prepare the dataframe for graphing
player.stats <- player.stats %>%
  filter(as.integer(SEC) > 6000) %>%
  mutate(FGA = as.integer(FGA), FTA = as.integer(FTA),
         REB = as.integer(REB), AST = as.integer(AST), PTS = as.integer(PTS),
         TOV = as.integer(TOV), STL = as.integer(STL), BLK = as.integer(BLK), SEC = as.integer(SEC), 
         Team = as.character(Team),
         MIN = round(SEC / 60, 1),
         REB36 = round(36 * REB / MIN, 2),
         AST36 = round(36 * AST / MIN, 2),
         PTS36 = round(36 * PTS / MIN, 2),
         TOV36 = round(36 * TOV / MIN, 2),
         STL36 = round(36 * STL / MIN, 2),
         BLK36 = round(36 * BLK / MIN, 2),
         OTHER36 = round(36 * (REB + AST + STL + BLK) / MIN, 2),
         TSPCT = round(PTS / (2 * (FGA + (0.44 * FTA))), 3), 
         MarkerSize = 12) %>% 
  left_join(color.pal.df, by = c('Team' = 'teamname'))


# create widget options vectors
chart.types <- c('Offensive Efficiency', 'Ball Control', 'Blocks vs. Steals', 'Points vs. Other Stats')
names(chart.types) <- chart.types

position.options <- c('All Positions', 'PG', 'SG', 'G', 'SF', 'PF', 'F', 'C')
names(position.options) <- position.options

team.options <- sort(as.character(unique(player.stats$Team)))
names(team.options) <- team.options

player.options <- c('All Players', unique(paste(player.stats$firstName, player.stats$lastName)))
names(player.options) <- player.options
# ==== 


# ========================
# 2. USER INTERFACE 
# ========================
ui <- fluidPage(theme = shinytheme('united'),
                
                # 2.A Create Shiny App Title 
                # ===-===-===-===-===-===-===-===
                fluidRow(
                  column(width = 12, align = 'center',
                         h2('NBA Interactive Chart-Creating Application'))
                ),

                fluidRow(


                  # 2.B Create All Widgets For App
                  # ===-===-===-===-===-===-===-===
                  column(width = 4, align = 'center',
                         # header, and selectInput debugger
                         h3('Chart Type'), hr(),
                         tags$head(tags$style(HTML(".shiny-split-layout > div {
                                                   overflow: visible; } "))),

                         selectInput(inputId = 'chart.input', label = 'Select Chart Type:', multiple = FALSE,
                                     choices = chart.types, selected = 'Offensive Efficiency'),

                         # select inputs for position and team
                         selectInput(inputId = 'position.input', label = 'Select Position:', multiple = FALSE,
                                     choices = position.options, selected = 'All Positions'),
                         selectInput(inputId = 'team.input', label = 'Select Team:', multiple = FALSE,
                                     choices = team.options, selected = 'Warriors'),

                         # select inputs for specific player
                         selectInput(inputId = 'player.input', label = 'Select Player:', multiple = FALSE,
                                     choices = player.options, selected = 'All Players')),


                  # 2.C Launch the Chart
                  # ===-===-===-===-===-===
                  column(width = 8, align = 'center',
                         br(),
                         plotlyOutput("playerxy", width = "100%", height = "100%")
                  )
                )
)
# ====


# ===========================
# 3. SERVER SIDE CODE 
# ===========================

server <- shinyServer(function(input, output) {
  
  # 3. Create the chart
  # ===-===-===-===-===-===
  output$playerxy <- renderPlotly({
    
    # 3.A Filter the df using input paramters
    # ===-===-===-===-===-===-===-===-===-===-===
    # filter based on position parameter
    if(input$position.input == "All Positions") {
      player.stats <- player.stats
    } else if(input$position.input == "F") {
      player.stats <- player.stats %>%
                          filter(POS %in% c('F', 'SF', 'PF'))
    } else if(input$position.input == "G") {
      player.stats <- player.stats %>%
                          filter(POS %in% c('G', 'PG', 'SG'))
    } else if(input$position.input == "PG") {
      player.stats <- player.stats %>%
                          filter(POS %in% c('G', 'PG'))
    } else if(input$position.input == "SG") {
      player.stats <- player.stats %>%
                          filter(POS %in% c('G', 'SG'))
    } else if(input$position.input == "SF") {
      player.stats <- player.stats %>%
                          filter(POS %in% c('F', 'SF'))
    } else if(input$position.input == "PF") {
      player.stats <- player.stats %>%
                          filter(POS %in% c('F', 'PF'))
    } else {
      player.stats <- player.stats %>%
                          filter(POS %in% c('C'))
    }
    
    
    # highlight and slightly increase size based on team parameter
    player.stats$colorhex[player.stats$Team != input$team.input] <- '#d3d3d3'
    player.stats$MarkerSize[player.stats$Team == input$team.input] <- 20

    # player parameter
    if(input$player.input != "All Players") {
      # increase size based on player selected
      fname <- strsplit(input$player.input, split = ' ')[[1]][1]
      lname <- strsplit(input$player.input, split = ' ')[[1]][2]
      player.stats$MarkerSize[player.stats$lastName == lname & player.stats$firstName == fname] <- 30
      
      # reset color for player selected, if not on team selected
      players_team <- player.stats$Team[player.stats$lastName == lname & player.stats$firstName == fname]
      players_color <- color.pal.df$colorhex[color.pal.df$teamname == players_team]
      player.stats$colorhex[player.stats$lastName == lname & player.stats$firstName == fname] <- players_color
    }
    
    # create axes, annotations, etc.
    ax <- list(title = "",
               color = 'rgb(100, 100, 100)',
               showgrid = TRUE,
               zeroline = FALSE)
    
    ay <- list(title = "",
               color = 'rgb(100, 100, 100)',
               showgrid = TRUE,
               zeroline = FALSE)
    
    # Offensive Efficiency Chart
    if(input$chart.input == 'Offensive Efficiency') {
      ax$title = "PTS / 36 MINS"
      ay$title = "True Shooting %"
      
      lm.model = lm(TSPCT ~ PTS36, data = player.stats)
      
      plot_ly(player.stats) %>%
        add_trace(x = ~PTS36, y = fitted(lm.model), type = 'scatter', mode = "lines",
                  color = I('black')) %>%
        add_trace(x = ~PTS36, y = ~TSPCT,
                  type = 'scatter', mode = 'markers', 
                  marker = list(size = ~MarkerSize, opacity = 0.8,
                                color = ~colorhex,
                                line = list(color = 'rgba(50, 50, 50, .8)',
                                            width = 1)),
                  hoverinfo = 'text',
                  text = ~paste(firstName, lastName, '\n',
                                'Pts/36: ', PTS36, '\n',
                                'TS%: ', TSPCT)) %>%
        layout(title = 'Offensive Efficiency - True Shooting % vs. Points / 36 Mins',
               xaxis = ax, 
               yaxis = ay,
               showlegend = FALSE)
    } 

    # Ball Control Chart
    else if(input$chart.input == 'Ball Control') {
      ax$title = "TOV / 36 MINS"
      ay$title = "AST / 36 MINS"
      
      lm.model = lm(AST36 ~ TOV36, data = player.stats)
      
      plot_ly(player.stats) %>%
        add_trace(x = ~TOV36, y = fitted(lm.model), type = 'scatter', mode = "lines",
                  color = I('black')) %>%
        add_trace(x = ~TOV36, y = ~AST36,
                  type = 'scatter', mode = 'markers', 
                  marker = list(size = ~MarkerSize, opacity = 0.8,
                                color = ~colorhex,
                                line = list(color = 'rgba(50, 50, 50, .8)',
                                            width = 1)),
                  hoverinfo = 'text',
                  text = ~paste(firstName, lastName, '\n',
                                'Tov/36: ', TOV36, '\n',
                                'Ast/36: ', AST36)) %>%
        layout(title = 'Ball Control - Assists / 36 Mins vs. Turnovers / 36 Mins',
               xaxis = ax, 
               yaxis = ay,
               showlegend = FALSE)
    }

    # Blocks vs. Steals Chart
    else if(input$chart.input == "Blocks vs. Steals") {
      ax$title = "STL / 36 MINS"
      ay$title = "BLK / 36 MINS"

      lm.model = lm(BLK36 ~ STL36, data = player.stats)
      
      plot_ly(player.stats) %>%
        add_trace(x = ~STL36, y = fitted(lm.model), type = 'scatter', mode = "lines",
                  color = I('black')) %>%
        add_trace(x = ~STL36, y = ~BLK36,
                  type = 'scatter', mode = 'markers', 
                  marker = list(size = ~MarkerSize, opacity = 0.8,
                                color = ~colorhex,
                                line = list(color = 'rgba(50, 50, 50, .8)',
                                            width = 1)),
                  hoverinfo = 'text',
                  text = ~paste(firstName, lastName, '\n',
                                'Stl/36: ', STL36, '\n',
                                'Blk/36: ', BLK36)) %>%
        layout(title = 'Defensive Box Score Stats - Blocks / 36 Mins vs. Steals / 36 Mins',
               xaxis = ax, 
               yaxis = ay,
               showlegend = FALSE)
    }

    # Pts vs. Everything Else
    else {
      ax$title = "(REB + AST + STL + BLK) / 36 MINS"
      ay$title = "PTS / 36 MINS"

      lm.model = lm(PTS36 ~ OTHER36, data = player.stats)
      
      plot_ly(player.stats) %>%
        add_trace(x = ~OTHER36, y = fitted(lm.model), type = 'scatter', mode = "lines",
                  color = I('black')) %>%
        add_trace(x = ~OTHER36, y = ~PTS36,
                  type = 'scatter', mode = 'markers', 
                  marker = list(size = ~MarkerSize, opacity = 0.8,
                                color = ~colorhex,
                                line = list(color = 'rgba(50, 50, 50, .8)',
                                            width = 1)),
                  hoverinfo = 'text',
                  text = ~paste(firstName, lastName, '\n',
                                'Other/36: ', OTHER36, '\n',
                                'Pts/36: ', PTS36)) %>%
        layout(title = 'Doing It All On The Court - Points / 36 Mins vs. Other / 36 Mins',
               xaxis = ax, 
               yaxis = ay,
               showlegend = FALSE)
    }

  })
  
})
# ====

shinyApp(ui, server)



# ==============================================================================
# 0. SET WD, RESET THE WORKSPACE, LOAD LIBRARIES, SOURCE FUNCTIONS, SAVE SOURCES
# ==============================================================================
setwd("/Users/Home/Dropbox/My_Documents/Not_Work_or_School/Sports Analytics/InteractiveSportsGraphs/")
rm(list = ls())

# for data manipulation
library(readr)
library(dplyr)

# for data viz
library(shiny)
library(shinythemes)
library(plotly)
# ====


# ========================
# 1. plot NBA ELOs
# ========================

# read last 5 years + this year of nba.elo ratings
nba.elo <- read_csv('Data/MSF_Feeds/NBA_elo.csv') %>% 
             filter(year_id >= 2013)

color.pal.df <- data.frame(
  colorhex = c('#006BB6', '#2C5234', '#BA0C2F', '#6F263D', '#007A33', '#D50032',
               '#6189B9', '#C8102E', '#862633', '#00788C', '#002B5C', '#724C9F',
               '#FF671F', '#702F8A', '#007DC5', '#0050B5', '#010101', '#418FDE', 
               '#FDBB30', '#002B5C', '#003DA5', '#CE1141', '#BA0C2F', '#B6BFBF',
               '#E56020', '#007DC3', '#7AC143', '#F0163A', '#FFC72D', '#0C2340'),
  teamname = c("Sixers", "Bucks", "Bulls", "Cavaliers", "Celtics", "Clippers", 
               "Grizzlies", "Hawks", "Heat", "Hornets", "Jazz", "Kings", 
               "Knicks", "Lakers", "Magic", "Mavericks", "Nets", "Nuggets", 
               "Pacers", "Pelicans", "Pistons", "Raptors", "Rockets", "Spurs", 
               "Suns", "Thunder", "Timberwolves", "Trailblazers", "Warriors", "Wizards"), 
  stringsAsFactors = FALSE)

team.list <- sort(color.pal.df$teamname)
names(team.list) <- team.list

# mutate dates into X values for the plot
nba.elo$xvals <- as.integer(as.factor(nba.elo$date_game))


# =========
# 2. UI
# =========
ui <- fluidPage(theme = shinytheme('united'),
  
  fluidRow(
    column(width = 12, align = 'center',
           h2('NBA ELO Ratings - Last 5 Years'))
  ),
  
  selectInput(inputId = 'team.input', label = 'Select Team:', multiple = FALSE, 
              choices = team.list, selected = 'Warriors'),
  plotlyOutput('plot')
)
# ====


# ============
# 3. Server
# ============
server <- shinyServer(function(input, output) {

  # set x axis year labels 
  mytickvals = c(); myticktext = c();
  for(i in 1:length(unique(nba.elo$year_id))) {
    this_year = unique(nba.elo$year_id)[i]
    hmm = tail(nba.elo$xvals[nba.elo$year_id == this_year], 1)
    mytickvals = c(mytickvals, hmm)
    myticktext = c(myticktext, as.character(this_year))
  }
  
  # modify last tickval out further
  mytickvals[length(mytickvals)] = 2*mytickvals[length(mytickvals)-1] - mytickvals[length(mytickvals)-2]
  
  # set x axis
  ax <- list(title = "",
    tickmode = 'array',
    tickvals = mytickvals,
    ticktext = myticktext,
    range = c(1, max(mytickvals)+10),
    zeroline = FALSE)
  
  # set y axis
  ay <- list(title = "",
    tickmode = 'array',
    tickvals = c(1200, 1300, 1400, 1500, 1600, 1700, 1800),
    ticktext = c('1200', '1300', '1400', 'Avg', '1600', '1700', '1800')
  )
  
  # set y = 1500 line, grey playoff shades
  myshapes = list(
    list(type = 'line',
         x0 = min(nba.elo$xvals), 
         x1 = max(mytickvals),
         y0 = 1500, y1 = 1500,
         width = 1,
         fillcolor = 'rgba(0,0,0,1)', line = list(color = 'rgba(0,0,0,1)'),
         opacity = 0.5)
    )
  
  playoff_idxs = which(nba.elo$is_playoffs == 1)
  playoff_startend = sapply(split(playoff_idxs, cumsum(c(1, diff(playoff_idxs) != 1))), range)
  
  start_vals <- nba.elo$xvals[playoff_startend[1, ]]
  end_vals <- nba.elo$xvals[playoff_startend[2, ]]
  playoff.length = mean(end_vals - start_vals)
  
  start_vals <- c(start_vals, start_vals[length(start_vals)]*2 - start_vals[length(start_vals)-1])
  end_vals <- c(end_vals, start_vals[length(start_vals)] + playoff.length)
  
  for(i in 1:length(start_vals)) {
    this.rect <- list(type = 'rect',
                      x0 = start_vals[i], 
                      x1 = end_vals[i],
                      y0 = 1900, y1 = 1150, opacity = 0.1,
                      layer = 'below', fillcolor = 'rgba(50,50,50,1)')
    myshapes[[length(myshapes)+1]] <- this.rect 
  }
  
  
  # and do the plot
  output$plot <- renderPlotly({
    
    # grab team id, color
    this.id <- input$team.input
    this.color <- color.pal.df$colorhex[color.pal.df$teamname == this.id]

    # create df for max/min rating
    min.idx <- which.min(nba.elo$elo_n[nba.elo$fran_id == this.id])
    max.idx <- which.max(nba.elo$elo_n[nba.elo$fran_id == this.id])
      
    y.max <- max(nba.elo$elo_n[nba.elo$fran_id == this.id])
    x.max <- nba.elo$xvals[nba.elo$fran_id == this.id][max.idx]
    y.min <- min(nba.elo$elo_n[nba.elo$fran_id == this.id])
    x.min <- nba.elo$xvals[nba.elo$fran_id == this.id][min.idx]
    
    max.date <- nba.elo$date_game[nba.elo$fran_id == this.id][max.idx]
    min.date <- nba.elo$date_game[nba.elo$fran_id == this.id][min.idx]
    
    max.opp <- nba.elo$opp_fran[nba.elo$fran_id == this.id][max.idx]
    min.opp <- nba.elo$opp_fran[nba.elo$fran_id == this.id][min.idx]
    
    max.result <- nba.elo$game_result[nba.elo$fran_id == this.id][max.idx]
    min.result <- nba.elo$game_result[nba.elo$fran_id == this.id][min.idx]
    
    max.pts <- nba.elo$pts[nba.elo$fran_id == this.id][max.idx]
    min.pts <- nba.elo$pts[nba.elo$fran_id == this.id][min.idx]  
    max.opppts <- nba.elo$opp_pts[nba.elo$fran_id == this.id][max.idx]
    min.opppts <- nba.elo$opp_pts[nba.elo$fran_id == this.id][min.idx]  
    
    max_min.df <- data.frame(xvals = c(x.min, x.max), yvals = c(y.min, y.max),
                             yplotvals = c(y.min - 10, y.max + 10),
                             thistext = c(paste('Worst Team ELO Last 5 Years', '\n', 
                                                min.date, '\n',
                                                min.result, ' | ', min.pts, '-', min.opppts, ' vs. ', min.opp, '\n',
                                                'ELO: ', round(y.min, 1), sep = ''), 
                                          paste('Best Team ELO Last 5 Years', '\n', 
                                                max.date, '\n',
                                                max.result, ' | ', max.pts, '-', max.opppts, ' vs. ', max.opp, '\n',
                                                'ELO: ', round(y.max, 1), sep = '')))
    
    plot_ly(height = 800) %>%
      
      # add grey lines for all other teams
      add_trace(data = nba.elo[nba.elo$fran_id != this.id, ] %>% group_by(year_id, team_id), 
                x = ~xvals, y = ~elo_n, type = 'scatter', mode = 'lines',
                hoverinfo = 'none', 
                line = list(width = 1), 
                color = I('black'), 
                alpha = 0.1, 
                showlegend = FALSE) %>%
      
      # add colored line for this specific team
      add_trace(data = nba.elo[nba.elo$fran_id == this.id, ] %>% group_by(year_id), 
                x = ~xvals, y = ~elo_n, type = 'scatter', mode = 'lines',
                line = list(color = this.color),
                hoverinfo = 'text', 
                text = ~paste(date_game, '\n', 
                             game_result, ' | ', pts, '-', opp_pts, ' vs. ', opp_id, '\n',
                             'ELO: ', round(elo_n, 1),  sep = ''),
                showlegend = FALSE) %>%
      
      # add star markers for teams best / worst record
      add_trace(data = max_min.df, x = ~xvals, y = ~yplotvals, type = 'scatter', mode = 'markers',
                marker = list(symbol = 'star', size = 22, color = this.color,
                              line = list(width = 1, color = 'black')),
                hoverinfo = 'text',
                text = ~paste(thistext)) %>%
      
      layout(xaxis = ax, 
             yaxis = ay,
             shapes = myshapes)
  })
})
# ====

shinyApp(ui, server)


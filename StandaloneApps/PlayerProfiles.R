
# unused argument (c("Player", "Pos", "Tm", stat_color_map$Stats))
# Evaluation error: object 'fullname' not found.
# unused argument (c("Player", "Pos", "Tm", "OWS", "DWS"))

# ===============
# 0. Setup
# ===============

# set working directory, clear environment
setwd('/Users/Home/Dropbox/My_Documents/Not_Work_or_School/Sports Analytics/InteractiveSportsGraphs/NBAApp/')
rm(list = ls())

# libraries for data vis
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)

# library for data manip
library(readr)
library(dplyr)
library(tidyr)

# theres a shot chart here
source('charts.R')
# ====


# ==============================
# 1. Load and Preprocess Data
# ==============================

# load playerstats.main (BBR) for the percentile and winshares plots 
playerstats.main <- read_csv('Data/MSF_Feeds/NBA_BBRplayerstats.csv')

# load shots.pbp for the shot charts
shots.pbp <- read_csv('Data/MSF_Feeds/NBA_gameplaybyplay_shotcharts.csv')
shots.pbp$fullname <- paste(shots.pbp$firstname, shots.pbp$lastname)

# create stat_color mapping dataframe for percentile plot
stat_color_map <- data.frame(Stats = c('X3P.', 'X2P.', 'eFG.', 'TS.', 'FT.', 'USG.', 'TRB.', 'STL.', 'BLK.', 'AST.', 'TOV.'),
                             StatsName = c('3PT%', '2PT%', 'eFG%', 'TS%', 'FT%', 'USG%', 'REB%', 'STL%', 'BLK%', 'AST%', 'TOV%'),
                             Colors = c('#f2003a', '#0202ef', '#0bea27', '#8a0eaf', '#ff6a00', '#efd107',
                                        '#46f0f0', '#f032e6', '#d2f53c', '#fabebe', '#008080'), 
                             XVals = c(1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41),
                             stringsAsFactors = FALSE)

# create functions for 2 of 3 plots (loaded the functions already for shotcharts)
createPercentilePlot <- function(playerstats.main, this.player, this.pos, mp.threshold, plot.width, plot.height) {
  
  # A,B,C building marker dataframes
  # ==================================
  # A. filter rows by pos and mp, remove unneeded cols, set cols to %
  playerstats.df <- playerstats.main %>%
    filter(Pos == this.pos & G*MP > mp.threshold) %>%
    select(c('Player', 'Pos', 'Tm', stat_color_map$Stats)) %>%
    mutate(TRB. = TRB. / 100,
           AST. = AST. / 100,
           STL. = STL. / 100,
           BLK. = BLK. / 100,
           TOV. = TOV. / 100,
           USG. = USG. / 100)
  
  
  # B. create marker dataframes for all players and the focus player
  marker.df <- playerstats.df %>%  
    gather(key = Stats, value = StatValue, -Player, -Pos, -Tm) %>%
    left_join(stat_color_map, by = c('Stats'='Stats')) %>%
    mutate(XVals = XVals + 0.6*(runif(length(XVals))-0.5)) %>%
    filter(!is.na(StatValue))
  
  focusplayer.df <- marker.df %>%
    filter(Player == this.player) %>%
    mutate(XVals = stat_color_map$XVals + 1.5)
  
  
  # C. compute focus player's percentiles in each stat
  focus.percentiles <- c()
  for(i in 1:nrow(focusplayer.df)) {
    stat.vector <- pull(playerstats.df[playerstats.df$Player != this.player, ], stat_color_map$Stats[i])
    stat.vector <- stat.vector[!is.na(stat.vector)]
    player.pctile <- round(sum(focusplayer.df$StatValue[i] > stat.vector) / length(stat.vector), digits = 3)
    focus.percentiles <- c(focus.percentiles, player.pctile)
  }
  focusplayer.df$Percentile = focus.percentiles
  # ====
  
  
  # D. create boxplot df and shapes list
  # =====================================
  boxplot.df <- data.frame(this.min = numeric(), this.q1 = numeric(), this.mid = numeric(), this.q3 = numeric(), this.max = numeric())
  for(i in 1:nrow(stat_color_map)) {
    this.stat <- stat_color_map$Stats[i]
    its.vector <- pull(playerstats.df, this.stat)
    
    this.row <- list(this.min = min(its.vector, na.rm = TRUE),
                     this.q1 = quantile(its.vector, .25, na.rm = TRUE),
                     this.mid = mean(its.vector, na.rm = TRUE),
                     this.q3 = quantile(its.vector, .75, na.rm = TRUE),
                     this.max = max(its.vector, na.rm = TRUE))
    
    boxplot.df <- rbind(boxplot.df, this.row)
  }
  
  # initialize and fill the shapes list
  box_edgeL = 0.75; box_edgeR = 2.25    # if you change these values, change focusplayer.df XVals too
  whisk_L = 1; whisk_R = 2
  box_mid = mean(c(box_edgeL, box_edgeR))

  myshapes <- list()
  for(i in 1:nrow(boxplot.df)) {
    
    mybox <- list(type = 'rect',
                  y0 = boxplot.df$this.q1[i], 
                  y1 = boxplot.df$this.q3[i],
                  x0 = stat_color_map$XVals[i] + box_edgeL,
                  x1 = stat_color_map$XVals[i] + box_edgeR,
                  fillcolor = stat_color_map$Colors[i],
                  opacity = 0.5,
                  line = list(color = stat_color_map$Colors[i], width = 3))
    
    topline = list(type = 'line',
                   y0 = boxplot.df$this.max[i],
                   y1 = boxplot.df$this.max[i],
                   x0 = stat_color_map$XVals[i] + whisk_L,
                   x1 = stat_color_map$XVals[i] + whisk_R,
                   line = list(color = stat_color_map$Colors[i], width = 2))
    
    botline = list(type = 'line',
                   y0 = boxplot.df$this.min[i],
                   y1 = boxplot.df$this.min[i],
                   x0 = stat_color_map$XVals[i] + whisk_L,
                   x1 = stat_color_map$XVals[i] + whisk_R,
                   line = list(color = stat_color_map$Colors[i], width = 2))
    
    topvertline = list(type = 'line',
                       y0 = boxplot.df$this.q3[i],
                       y1 = boxplot.df$this.max[i],
                       x0 = stat_color_map$XVals[i] + box_mid,
                       x1 = stat_color_map$XVals[i] + box_mid,
                       line = list(color = stat_color_map$Colors[i], width = 2))
    
    botvertline = list(type = 'line',
                       y0 = boxplot.df$this.min[i],
                       y1 = boxplot.df$this.q1[i],
                       x0 = stat_color_map$XVals[i] + box_mid,
                       x1 = stat_color_map$XVals[i] + box_mid,
                       line = list(color = stat_color_map$Colors[i], width = 2))
    
    boxtopline = list(type = 'line',
                      y0 = boxplot.df$this.q3[i],
                      y1 = boxplot.df$this.q3[i],
                      x0 = stat_color_map$XVals[i] + box_edgeL,
                      x1 = stat_color_map$XVals[i] + box_edgeR,
                      line = list(color = stat_color_map$Colors[i], width = 2))
    
    boxbotline = list(type = 'line',
                      y0 = boxplot.df$this.q1[i],
                      y1 = boxplot.df$this.q1[i],
                      x0 = stat_color_map$XVals[i] + box_edgeL,
                      x1 = stat_color_map$XVals[i] + box_edgeR,
                      line = list(color = stat_color_map$Colors[i], width = 2))
    
    boxrightline = list(type = 'line',
                        y0 = boxplot.df$this.q1[i],
                        y1 = boxplot.df$this.q3[i],
                        x0 = stat_color_map$XVals[i] + box_edgeR,
                        x1 = stat_color_map$XVals[i] + box_edgeR,
                        line = list(color = stat_color_map$Colors[i], width = 2))
    
    boxleftline = list(type = 'line',
                       y0 = boxplot.df$this.q1[i],
                       y1 = boxplot.df$this.q3[i],
                       x0 = stat_color_map$XVals[i] + box_edgeL,
                       x1 = stat_color_map$XVals[i] + box_edgeL,
                       line = list(color = stat_color_map$Colors[i], width = 2))
    
    boxmidline = list(type = 'line',
                      y0 = boxplot.df$this.mid[i],
                      y1 = boxplot.df$this.mid[i],
                      x0 = stat_color_map$XVals[i] + box_edgeL,
                      x1 = stat_color_map$XVals[i] + box_edgeR,
                      line = list(color = stat_color_map$Colors[i], width = 2))
    
    myshapes[[length(myshapes)+1]] <- mybox
    myshapes[[length(myshapes)+1]] <- topline
    myshapes[[length(myshapes)+1]] <- botline
    myshapes[[length(myshapes)+1]] <- topvertline
    myshapes[[length(myshapes)+1]] <- botvertline
    myshapes[[length(myshapes)+1]] <- boxtopline
    myshapes[[length(myshapes)+1]] <- boxbotline
    myshapes[[length(myshapes)+1]] <- boxrightline
    myshapes[[length(myshapes)+1]] <- boxleftline
    myshapes[[length(myshapes)+1]] <- boxmidline
  }
  
  divisor1 <- list(type = 'line', x0 = 24, x1 = 24, y0 = 0.01, y1 = 0.99,
                   line = list(color = 'rgb(150, 150, 150)', width = 2))
  divisor2 <- list(type = 'line', x0 = 36, x1 = 36, y0 = 0.01, y1 = 0.99,
                   line = list(color = 'rgb(150, 150, 150)', width = 2))
  myshapes[[length(myshapes)+1]] <- divisor1
  myshapes[[length(myshapes)+1]] <- divisor2
  # ====
  
  
  # E. create axes and annotations
  # ================================
  ax <- list(title = "",
             tickmode = 'array',
             tickvals = stat_color_map$XVals+1,
             ticktext = stat_color_map$StatsName,
             tickfont = list(size = plot.width/92),
             color = 'rgb(100, 100, 100)',
             fixedrange = TRUE,
             showgrid = FALSE,
             zeroline = FALSE)
  
  ay <- list(title = "",
             tickfont = list(size = plot.width/92),
             showgrid = TRUE, 
             zerolinecolor = 'rgb(100, 100, 100)')
  
  myannotations <- list(
                     list(text = 'Shooting / Scoring', showarrow = FALSE, x = 11, y = 0.12,
                          font = list(color = 'rgb(100, 100, 100)', size = plot.width/60)),
                     list(text = 'Defense / Rebounding', showarrow = FALSE, x = 30, y = 0.30,
                          font = list(color = 'rgb(100, 100, 100)', size = plot.width/60)),
                     list(text = 'Ball Control', showarrow = FALSE, x = 40.5, y = 0.7,
                          font = list(color = 'rgb(100, 100, 100)', size = plot.width/60))
                   )
  # ====
  
  
  # F. create the plot
  # =====================
  plot_ly(marker.df, height = plot.height, width = plot.width) %>%
    
    # all players markers
    add_trace(x = ~XVals, y = ~StatValue, type = 'scatter', mode = 'markers',
              marker = list(color = marker.df$Colors, size = 7),
              opacity = 0.8,
              showlegend = FALSE,
              hoverinfo = 'text',
              text = ~paste(Player, '\n', StatsName, ' | ', marker.df$StatValue*100, '%', sep = '')) %>%
    
    # this players markers
    add_trace(data = focusplayer.df, 
              x = ~XVals, y = ~StatValue, type = 'scatter', mode = 'markers',
              marker = list(symbol = 'star', color = focusplayer.df$Colors, size = 15,
                            line = list(width = 1, color = I('black'))),
              hoverinfo = 'text', 
              text = ~paste(Player, '\n', 
                            StatsName, ' | ', focusplayer.df$StatValue*100, '%', '\n', 
                            focusplayer.df$Percentile*100, '% Percentile', sep = '')) %>%
    
    layout(shapes = myshapes,
           xaxis = ax, yaxis = ay,
           annotations = myannotations,
           plot_bgcolor = 'rgb(248, 248, 248)',
           paper_bgcolor = 'rgb(248, 248, 248)')
  # ====
}
createWinsharesPlot <- function(playerstats.main, this.player, this.pos, mp.threshold, plot.width, plot.height) {
  
  # A. initialize the color mapping dataframe used for focus player's point
  # ========================================================================
  color.pal.df <- data.frame(
    colorhex = c('#006BB6', '#2C5234', '#BA0C2F', '#6F263D', '#007A33', '#D50032',
                 '#6189B9', '#C8102E', '#862633', '#00788C', '#002B5C', '#724C9F',
                 '#FF671F', '#702F8A', '#007DC5', '#0050B5', '#010101', '#418FDE', 
                 '#FDBB30', '#002B5C', '#003DA5', '#CE1141', '#BA0C2F', '#B6BFBF',
                 '#E56020', '#007DC3', '#7AC143', '#F0163A', '#FFC72D', '#0C2340'),
    elo538names = c("Sixers", "Bucks", "Bulls", "Cavaliers", "Celtics", "Clippers", 
                    "Grizzlies", "Hawks", "Heat", "Hornets", "Jazz", "Kings", 
                    "Knicks", "Lakers", "Magic", "Mavericks", "Nets", "Nuggets", 
                    "Pacers", "Pelicans", "Pistons", "Raptors", "Rockets", "Spurs", 
                    "Suns", "Thunder", "Timberwolves", "Trailblazers", "Warriors", "Wizards"),
    bbrefnames = c("PHI", "MIL", "CHI", "CLE", "BOS", "LAC", 
                   "MEM", "ATL", "MIA", "CHO", "UTA", "SAC", 
                   "NYK", "LAL", "ORL", "DAL", "BRK", "DEN",  
                   "IND", "NOP", "DET", "TOR", "HOU", "SAS",   
                   "PHO", "OKC", "MIN", "POR", "GSW", "WAS"),
    stringsAsFactors = FALSE)
  # ====
  
  
  # B. create the main plotting dataframe winshares.df with marker colors and sizes
  # ================================================================================
  this.team <- playerstats.main$Tm[playerstats.main$Player == this.player]
  this.color <- color.pal.df$colorhex[color.pal.df$bbrefnames == this.team]
  
  winshares.df <- playerstats.main %>%
    filter(Pos == this.pos & G*MP > mp.threshold) %>%
    select(c('Player', 'Pos', 'Tm', 'OWS', 'DWS')) %>%
    mutate(MarkerSize = ifelse(Player == this.player, 26, 12),
           MarkerColor = ifelse(Player == this.player, this.color, '#d3d3d3'))
  # ====
  
  
  # C. create the axes, shapes and annotations
  # ============================================
  # axes
  ax <- list(title = "Season Offensive Win Shares",
             color = 'rgb(100, 100, 100)',
             showgrid = FALSE,
             zeroline = FALSE)
  
  ay <- list(title = "Season Defensive Win Shares",
             color = 'rgb(100, 100, 100)',
             showgrid = FALSE,
             zeroline = FALSE)
  
  # shapes
  vertline = list(type = 'line',
                  x0 = mean(winshares.df$OWS), x1 = mean(winshares.df$OWS),
                  y0 = min(winshares.df$DWS), y1 = max(winshares.df$DWS),
                  line = list(color = 'rgb(100, 100, 100)', width = 2)) #,
  horline = list(type = 'line',
                 x0 = min(winshares.df$OWS), x1 = max(winshares.df$OWS),
                 y0 = mean(winshares.df$DWS), y1 = mean(winshares.df$DWS),
                 line = list(color = 'rgb(100, 100, 100)', width = 2))
  myshapes2 <- list(vertline, horline)
  
  # annotations
  xbuff <- (max(winshares.df$OWS) - min(winshares.df$OWS)) / 10
  ybuff <- (max(winshares.df$DWS) - min(winshares.df$DWS)) / 10
  toplefttext <- list(text = "<b>Defensive \nStuds</b>", align = 'center',
                      font = list(color = 'rgb(100, 100, 100)', size = 20),
                      opacity = 0.8, showarrow = FALSE,
                      x = min(winshares.df$OWS) + xbuff,
                      y = max(winshares.df$DWS) - ybuff)
  botlefttext <- list(text = "<b>Bad \nPlayers</b>", align = 'center',
                      font = list(color = 'rgb(100, 100, 100)', size = 20),
                      opacity = 0.8, showarrow = FALSE,
                      x = min(winshares.df$OWS) + xbuff,
                      y = min(winshares.df$DWS) + ybuff)
  toprighttext <- list(text = "<b>Two-Way \nPlayers</b>", align = 'center',
                       font = list(color = 'rgb(100, 100, 100)', size = 20),
                       opacity = 0.8, showarrow = FALSE,
                       x = max(winshares.df$OWS) - xbuff,
                       y = max(winshares.df$DWS) - ybuff)
  botrighttext <- list(text = "<b>Offensive \nStuds</b>", align = 'center',
                       font = list(color = 'rgb(100, 100, 100)', size = 20),
                       opacity = 0.8, showarrow = FALSE,
                       x = max(winshares.df$OWS) - xbuff,
                       y = min(winshares.df$DWS) + ybuff)
  myannotations <- list(toplefttext, botlefttext, toprighttext, botrighttext)
  # ====
  
  
  # D. do the plot
  # ===============
  plot_ly(winshares.df, height = plot.height, width = plot.width) %>%
    
    # add all points
    add_trace(x = ~OWS, y = ~DWS,
              type = 'scatter', mode = 'markers', 
              marker = list(size = ~MarkerSize, opacity = 0.8,
                            color = ~MarkerColor,
                            line = list(color = 'rgba(50, 50, 50, .8)',
                                        width = 1)),
              hoverinfo = 'text',
              text = ~paste(Player, '\n',
                            'Offensive WS: ', OWS, '\n',
                            'Defensive WS: ', DWS)) %>%
    
    layout(title = 'Offensive vs. Defensive Win Shares \n Season Total',
           xaxis = ax, yaxis = ay,
           shapes = myshapes2,
           showlegend = FALSE,
           margin = list(t = '65'),
           plot_bgcolor = 'rgb(248, 248, 248)',
           paper_bgcolor = 'rgb(248, 248, 248)',
           annotations = myannotations)
  # ====
}

# create list of player names for selectInput
mp.threshold <- sort(playerstats.main$MPTotal, TRUE)[300]
player.list <- playerstats.main %>% filter(G*MP > mp.threshold) %>% pull(Player)
names(player.list) <- player.list

# create last updated date
last.updated <- substring(max(shots.pbp$msf.gameID), 1, 8)
last.updated <- paste0(substring(last.updated, 5, 6), '-', substring(last.updated, 7, 8), '-', substring(last.updated, 1, 4))


# ========================
# 2. USER INTERFACE
# ========================
ui <- fluidPage(theme = shinytheme('united'),
                
                # grab the width and height in pixels of the window
                tags$head(tags$script('
                  var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                ')),
                
                # shiny app title
                br(),
                fluidRow(width = 12, align = 'center', 
                         paste("Last Updated:", last.updated)
                         ),
                fluidRow(
                  column(width = 12, align = 'center',
                         h3('NBA Player Profiles'))
                ),
                
                # create permanent input for shot chart type (should be 5 options)                
                fluidRow(
                  column(width = 1),
                  column(width = 3, align = 'center', 
                   selectInput(inputId = 'player.input', label = 'Select Player:', multiple = FALSE,
                               choices = player.list, selected = 'Stephen Curry')),
                  column(width = 8, align = 'center', 
                         "This tool computes the selected player's ranking in 11 stat categories against all other players at the same position, 
                         displayers the players shot chart, and plots his offensive and defensive win shares to date")
                ),
    
                fluidRow(
                  # 2.C Launch the Chart
                  # ===-===-===-===-===-===
                  column(width = 12, align = 'center', 
                    plotlyOutput("PCTchart", width = 'auto', height = 'auto'))
                ),
                
                fluidRow(
                  column(width = 6, align = 'right',
                         plotlyOutput("shotchart", height = 'auto')),
                  column(width = 6, align = 'left',
                         br(),
                         plotlyOutput('WSchart'), height = 'auto')
                ),
                br()
)
# ====


# ========================
# 3. SERVER 
# ========================
server <- shinyServer(function(input, output, session) {

  # Create the 3 different plots 
  output$PCTchart <- renderPlotly({

    window.width <- input$dimension[1]
    window.height <- input$dimension[2]
    
    # set the 3 parameters
    this.player <- input$player.input
    this.pos <- playerstats.main$Pos[playerstats.main$Player == this.player]
    mp.threshold <- sort(playerstats.main$MPTotal, TRUE)[300]
    
    # and launch the plot
    # 
    createPercentilePlot(playerstats.main, this.player, this.pos, mp.threshold, 
                         plot.width = 0.9*window.width, plot.height = 0.35*window.width)
  })
  
  output$WSchart <- renderPlotly({
    
    window.width <- input$dimension[1]
    window.height <- input$dimension[2]
    
    # set the 3 parameters
    this.player <- input$player.input
    this.pos <- playerstats.main$Pos[playerstats.main$Player == this.player]
    mp.threshold <- sort(playerstats.main$MPTotal, TRUE)[300]
    
    # and launch the plot
    createWinsharesPlot(playerstats.main, this.player, this.pos, mp.threshold, plot.height = 0.35*window.width, plot.width = 0.35*1.25*window.width)
  })
  
  output$shotchart <- renderPlotly ({
    
    print(paste(input$dimension[1], input$dimension[2], input$dimension[2]/input$dimension[1]))
    
    # set the 1 parameter
    this.player <- input$player.input
    window.width <- input$dimension[1]
    
    player.heatmap <- createHexPlotDF(shots.pbp, this_fullname = this.player, plotheight = 0.395*window.width, divisor = 31)
    plotHexChart(player.heatmap = player.heatmap, this_fullname = this.player, plot_height = 0.395*window.width, plot_width = 0.395*1.25*window.width)
  })
})

shinyApp(ui, server)
# ====





# P <- plot_ly(playerstats.df) 
# for(i in 1:nrow(stat_color_map)) {
#   my_y <- pull(playerstats.df, stat_color_map$Stats[i])
#   P <- P %>% add_trace(y = my_y, jitter = 0.5, type = 'box', 
#                        name = stat_color_map$Stats[i],
#                        pointpos = -1.8, boxpoints = 'all')
# }
# P
# 




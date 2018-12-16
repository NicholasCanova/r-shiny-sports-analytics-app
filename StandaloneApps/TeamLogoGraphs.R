
# ===============
# 0. Setup
# ===============

# set working directory, clear environment
setwd("/Users/Home/Dropbox/My_Documents/Not_Work_or_School/Sports Analytics/InteractiveSportsGraphs/")
rm(list = ls())

# for reading data
library(readr)

# for data manip
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

# for the Shiny App
library(shiny)
library(shinythemes)
# ====

# =================
# 1. Read Data
# =================

team.stats <- read_csv('Data/MSF_Feeds/NBA_teamgamelogs.csv')

# load the giant team_URL_map dataframe
if(1 == 1){
# s3urls <- paste('https://s3.amazonaws.com/msfsportsdatafeeds/NBALogos/', sort(team.stats$team.Abbreviation), '.gif', sep = '')
team_URL_map <- data.frame(teamID = c("ATL", "BOS", "BRO", "CHA", "CHI", 
                                  "CLE", "DAL", "DEN", "DET", "HOU", 
                                  "IND", "LAC", "LAL", "MEM", "MIA", 
                                  "MIL", "MIN", "NOP", "NYK", "GSW", 
                                  "OKL", "ORL", "PHI", "PHX", "POR", 
                                  "SAC", "TOR", "UTA", "SAS", "WAS"),
                     imageURLs = c('https://upload.wikimedia.org/wikipedia/en/thumb/2/24/Atlanta_Hawks_logo.svg/400px-Atlanta_Hawks_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/8/8f/Boston_Celtics.svg/400px-Boston_Celtics.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/commons/thumb/4/44/Brooklyn_Nets_newlogo.svg/300px-Brooklyn_Nets_newlogo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/c/c4/Charlotte_Hornets_%282014%29.svg/400px-Charlotte_Hornets_%282014%29.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/6/67/Chicago_Bulls_logo.svg/400px-Chicago_Bulls_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/4/4b/Cleveland_Cavaliers_logo.svg/200px-Cleveland_Cavaliers_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/9/97/Dallas_Mavericks_logo.svg/400px-Dallas_Mavericks_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/7/76/Denver_Nuggets.svg/400px-Denver_Nuggets.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/1/1e/Detroit_Pistons_logo.svg/400px-Detroit_Pistons_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/2/28/Houston_Rockets.svg/400px-Houston_Rockets.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/1/1b/Indiana_Pacers.svg/400px-Indiana_Pacers.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/b/bb/Los_Angeles_Clippers_%282015%29.svg/400px-Los_Angeles_Clippers_%282015%29.svg.png', 
                                   'https://upload.wikimedia.org/wikipedia/commons/thumb/3/3c/Los_Angeles_Lakers_logo.svg/400px-Los_Angeles_Lakers_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/f/f1/Memphis_Grizzlies.svg/400px-Memphis_Grizzlies.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/f/fb/Miami_Heat_logo.svg/400px-Miami_Heat_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/4/4a/Milwaukee_Bucks_logo.svg/400px-Milwaukee_Bucks_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/c/c2/Minnesota_Timberwolves_logo.svg/400px-Minnesota_Timberwolves_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/0/0d/New_Orleans_Pelicans_logo.svg/400px-New_Orleans_Pelicans_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/2/25/New_York_Knicks_logo.svg/400px-New_York_Knicks_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/0/01/Golden_State_Warriors_logo.svg/400px-Golden_State_Warriors_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/5/5d/Oklahoma_City_Thunder.svg/400px-Oklahoma_City_Thunder.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/1/10/Orlando_Magic_logo.svg/400px-Orlando_Magic_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/0/0e/Philadelphia_76ers_logo.svg/400px-Philadelphia_76ers_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/d/dc/Phoenix_Suns_logo.svg/400px-Phoenix_Suns_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/2/21/Portland_Trail_Blazers_logo.svg/520px-Portland_Trail_Blazers_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/c/c7/SacramentoKings.svg/400px-SacramentoKings.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/3/36/Toronto_Raptors_logo.svg/400px-Toronto_Raptors_logo.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/0/04/Utah_Jazz_logo_%282016%29.svg/400px-Utah_Jazz_logo_%282016%29.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/San_Antonio_Spurs.svg/400px-San_Antonio_Spurs.svg.png',
                                   'https://upload.wikimedia.org/wikipedia/en/thumb/0/02/Washington_Wizards_logo.svg/400px-Washington_Wizards_logo.svg.png'),
                     stringsAsFactors = FALSE)
}

# create dropdown vector
chart.types <- c('Scoring Margin', 'Field Goal Percentages', '3 Point Offense', '3 Point Defense', 'Blocks and Steals', 'Create Your Own Chart')
names(chart.types) <- chart.types


# ========================
# 2. USER INTERFACE
# ========================
ui <- fillPage(theme = shinytheme('united'),
                
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
                
                # 2.A Create Shiny App Title 
                # ===-===-===-===-===-===-===-===
                fluidRow(
                  column(width = 12, offset = 2,
                         h2('Team Stats Chart Generator'))
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
                         selectInput(inputId = 'charttype.input', label = 'Select Chart Type:', multiple = FALSE,
                                     choices = chart.types, selected = 'Scoring Margin'),
                         
                         splitLayout(uiOutput('xaxisstat.input'), uiOutput('yaxisstat.input')),
                         splitLayout(uiOutput('xaxisforagainst.input'), uiOutput('yaxisforagainst.input'))
                  ),
                  
                  # 2.C Launch the Chart
                  # ===-===-===-===-===-===
                  column(width = 8, align = 'left',
                         br(),
                         plotlyOutput("teamplot", height = 'auto')
                  )
                )
)
# ====


# ========================
# 3. SERVER 
# ========================
server <- shinyServer(function(input, output, session) {
  
  # 3.A format the custom input parameters
  # ========================================
  # select player for player-season graph
  output$xaxisstat.input <- renderUI({
    if(input$charttype.input == 'Create Your Own Chart') {
      
      xaxis.options <- unique(team.stats$stat)
      names(xaxis.options) <- xaxis.options

      selectInput(inputId = 'xaxis.id', label = 'X statistic', multiple = FALSE,
                  choices = xaxis.options, selected = 'PTS')
      
    } else{
      return(NULL)
    }
  })
  
  output$xaxisforagainst.input <- renderUI({
    if(input$charttype.input == 'Create Your Own Chart') {
      
      xaxis.faoptions <- c('for', 'against')
      names(xaxis.faoptions) <- xaxis.faoptions
      
      selectInput(inputId = 'xaxis.fa', label = 'for / against', multiple = FALSE,
                  choices = xaxis.faoptions, selected = 'for')
      
    } else{
      return(NULL)
    }
  })
  
  # select team for team-game marker chart
  output$yaxisstat.input <- renderUI({
    if(input$charttype.input == 'Create Your Own Chart') {
      
      yaxis.options <- unique(team.stats$stat)
      names(yaxis.options) <- yaxis.options
      
      selectInput(inputId = 'yaxis.id', label = 'Y statistic', multiple = FALSE,
                  choices = yaxis.options, selected = 'PTS')
      
    } else{
      return(NULL)
    }
  })
  
  output$yaxisforagainst.input <- renderUI({
    if(input$charttype.input == 'Create Your Own Chart') {
      
      yaxis.faoptions <- c('for', 'against')
      names(yaxis.faoptions) <- yaxis.faoptions
      
      selectInput(inputId = 'yaxis.fa', label = 'for / against', multiple = FALSE,
                  choices = yaxis.faoptions, selected = 'against')
      
    } else{
      return(NULL)
    }
  })
  # ====
  
  # 3.B create the team logo plots 
  # =================================
  output$teamplot <- renderPlotly({
    
    # set the stat and foragainst in order to filter team.stats
    if(input$charttype.input == 'Scoring Margin') {
      x.stat = y.stat = 'PTS'; 
      x.fa = 'for'; y.fa = 'against';
    } else if(input$charttype.input == 'Field Goal Percentages') {
      x.stat = y.stat = 'FGPCT';
      x.fa = 'for'; y.fa = 'against';
    } else if(input$charttype.input == '3 Point Offense') {
      x.stat = 'X3PA'; y.stat = 'X3PCT'; 
      x.fa = 'for'; y.fa = 'for';
    } else if(input$charttype.input == '3 Point Defense') {
      x.stat = 'X3PA'; y.stat = 'X3PCT';
      x.fa = 'against'; y.fa = 'against';
    } else if(input$charttype.input == 'Blocks and Steals') {
      x.stat = 'STL'; y.stat = 'BLK'; 
      x.fa = 'for'; y.fa = 'for';
    } else {
      
      # set the req() functions here, since only here is when they're needed
      req(input$xaxis.id); req(input$yaxis.id)
      req(input$xaxis.fa); req(input$xaxis.fa)
      
      x.stat = input$xaxis.id; x.fa = input$xaxis.fa 
      y.stat = input$yaxis.id; y.fa = input$yaxis.fa
    }
    
    # use min and max x and y stat values to determine size of logos
    x.range <- range(team.stats$value[team.stats$foragainst == x.fa & team.stats$stat == x.stat])
    y.range <- range(team.stats$value[team.stats$foragainst == y.fa & team.stats$stat == y.stat])
    x.diff <- x.range[2] - x.range[1]
    y.diff <- y.range[2] - y.range[1]

    corner.df <- data.frame(xvals = c(x.range[1] - 0.05*x.diff , x.range[2] + 0.05*x.diff),
                            yvals = c(y.range[1] - 0.10*y.diff, y.range[2] + 0.05*y.diff))
    
    logo.size <- (x.range[2] - x.range[1]) / 12
      
    # create the image list w/ coordinates for logos
    all.teams <- sort(unique(team.stats$team.Abbreviation))
    images.list <- list()
    plot.df <- data.frame(xvals = rep(0, 30), yvals = rep(0, 30))
    
    for(i in 1:length(all.teams)) {
      
      this_team <- all.teams[i]
      this_x <- team.stats$value[team.stats$team.Abbreviation == this_team & team.stats$foragainst == x.fa & team.stats$stat == x.stat]
      this_y <- team.stats$value[team.stats$team.Abbreviation == this_team & team.stats$foragainst == y.fa & team.stats$stat == y.stat]
      
      
      this.list <- list(source = team_URL_map$imageURLs[team_URL_map$teamID == this_team],
                        xref = 'x', yref = 'y',
                        x = this_x,
                        y = this_y,
                        sizex = logo.size, sizey = 100,
                        opacity = 1, layer = 'above')  
      images.list[[length(images.list)+1]] <- this.list
      
      plot.df[i, ] = c(this_x, this_y)
    }

    lm.model <- lm(yvals ~ xvals, data = plot.df)
    myshapes <- list(type = 'line', 
                     x0 = x.range[1] - 0.05*x.diff,
                     x1 = x.range[2] + 0.05*x.diff,
                     y0 = lm.model$coefficients[1] + lm.model$coefficients[2] * (x.range[1] - 0.05*x.diff),
                     y1 = lm.model$coefficients[1] + lm.model$coefficients[2] * (x.range[2] + 0.05*x.diff),
                     line = list(width = 1, dash = 'dash', color = 'rgb(150, 150, 150)'))
    
    ax <- list(title = paste('Team', x.stat, x.fa, 'per game', sep = ' '),
               titlefont = list(size = 20),
               tickfont = list(size = 16),
               color = 'rgb(100, 100, 100)',
               showgrid = TRUE,
               gridcolor = 'rgb(225, 225, 225)',
               zeroline = FALSE)
    
    ay <- list(title = paste('Team', y.stat, y.fa, 'per game', sep = ' '),
               titlefont = list(size = 20),
               tickfont = list(size = 16),
               color = 'rgb(100, 100, 100)',
               showgrid = TRUE,
               gridcolor = 'rgb(225, 225, 225)',
               zeroline = FALSE)
    
    mytitle <- ifelse(input$charttype.input == 'Create Your Own Chart', 
                      paste('\n', x.stat, x.fa, 'vs.', y.stat, y.fa), 
                      paste('\n', input$charttype.input))
    
    mytitlefont <- list(size = 24, color = 'rgb(100, 100, 100)')
     
    req(input$dimension[1])
    window.width <- input$dimension[1]
    print(window.width / 2)
    
    # and finally the plot
    plot_ly(corner.df, height = 700) %>%
      add_trace(x = ~xvals, y = ~yvals, type = 'scatter', mode = 'markers',
                marker = list(size = 1)) %>%
      layout(
        title = mytitle,
             titlefont = mytitlefont,
             xaxis = ax, yaxis = ay,
             margin = list(pad = 5, l = 100, b = 50, t = 50),
             shapes = myshapes,
             images = images.list
             )
    
  })
})

shinyApp(ui, server)
# ====


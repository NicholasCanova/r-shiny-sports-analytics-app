
# Take Advantage of the
# ====== Code ===== Folding =======

# ===========
# Setup 
# ===========
rm(list = ls())

library(RCurl)
library(ggplot2)
library(shiny)
library(shinythemes)
library(plotly)
library(readr)
library(dplyr)
library(tidyr)
library(DT)

# additional packages for assist networks
library(igraph)
library(ggnetwork)
library(network)
library(sna)

# source for shot charts
# setwd() # <----------- Set a wd here on local machine !!
source('charts.R')
# ====


# ================================
# 1. Read And Preprocess Data - Shot Charts
# ================================

# read the new CSV file, add fullnames column
starttime <- Sys.time()
shot.pbp <- read_csv('Data/NBA_gameplaybyplay_shotcharts.csv')
print(paste(Sys.time() - starttime, " - Shot Chart Data")) 

# save a dataframe with opponents and game dates
opp.date.df <- strsplit(unique(shot.pbp$msf.gameID), split = '-') %>%
  do.call(rbind, .) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  setNames(c('Date', 'Team1', 'Team2')) %>%
  mutate(Date = as.Date(Date, format = '%Y%m%d'))


# create selectinput vector of graph types
shotchart.types <- c('Shot Marker Graph (Player-Season)', 'Shot Marker Graph (Team-Game)',
                     'Heat Map (Player-Season)', 'Heat Map (Team-Season)')
names(shotchart.types) <- shotchart.types
# ====


# ================================
# 2. Read And Preprocess Data - Player XY Charts
# ================================

starttime <- Sys.time()
player.stats <- read_csv('Data/NBA_cumulativeplayerstats.csv')
print(paste(Sys.time() - starttime, " - Player XY Charts Data"))
player.stats$fullName <- paste(player.stats$firstName, player.stats$lastName)

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
pxy.charttypes <- c('Offensive Efficiency', 'Ball Control', 'Blocks vs. Steals', 'Points vs. Other Stats')
names(pxy.charttypes) <- pxy.charttypes

pxy.positionoptions <- c('All Positions', 'PG', 'SG', 'G', 'SF', 'PF', 'F', 'C')
names(pxy.positionoptions) <- pxy.positionoptions

pxy.teamoptions <- sort(as.character(unique(player.stats$Team)))
names(pxy.teamoptions) <- pxy.teamoptions

pxy.playeroptions <- c('All Players', unique(player.stats$fullName))
names(pxy.playeroptions) <- pxy.playeroptions
# ====


# ================================
# 3. Read and Preprocess Data - Team XY Charts, and 4-Factors Charts
# ================================

starttime <- Sys.time()
team.stats <- read_csv('Data/NBA_teamgamelogs.csv')
print(paste(Sys.time() - starttime, " - Team XY Charts Data (and FF)"))

# create dropdown vector
txy.charttypes <- c('Scoring Margin', 'Field Goal Percentages', '3 Point Offense', '3 Point Defense', 'Blocks and Steals', 'Create Your Own Chart')
names(txy.charttypes) <- txy.charttypes

ff.teams <- unique(team.stats$team.Abbreviation)
names(ff.teams) <- ff.teams
# ====


# =====================================
# 4. Read and Preprocess Data - Game Recap Charts
# =====================================

starttime <- Sys.time()
gamerecaps.all <- read_csv('Data/NBA_gameplaybyplay_gamerecaps.csv') 
print(paste(Sys.time() - starttime, " - Game Recap Data") )

gr.teams <- unique(gamerecaps.all$hID)
names(gr.teams) <- gr.teams

gr.gameids <- unique(gamerecaps.all$msfgameid)
names(gr.gameids) <- gr.gameids 
# ====


# ==============================
# 5. Read and Preprocess Data - ELO Ratings
# ==============================

# all data earlier than last 5 years saved in another file
starttime <- Sys.time()
nba.elo <- read_csv('Data/NBA_elo.csv') 
print(paste(Sys.time() - starttime, " - ELO Data") )

elo.teamlist <- sort(color.pal.df$teamname)
names(elo.teamlist) <- elo.teamlist

# mutate dates into X values for the plot
nba.elo$xvals <- as.integer(as.factor(nba.elo$date_game))
# ====


# ================================
# 6. Read and Preprocess Data - Assist Networks
# ================================

starttime <- Sys.time()
assist.pbp <- read_csv('Data/NBA_gameplaybyplay_assistnet.csv')
print(paste(Sys.time() - starttime, " - Assist Net Data") )

# create vectors for selectInputs
an.teamlist <- sort(unique(assist.pbp$team))
names(an.teamlist) <- an.teamlist

an.nodelist <- c('auto select',2,3,4,5,6,7,8,9,10,11,12)
names(an.nodelist) <- an.nodelist
# ====


# ================================
# 7. Read and Preprocess Data - Player Percentiles
# ================================

starttime <- Sys.time()
mp.threshold <- 200
player.stats.bbr <- read_csv('Data/NBA_BBRplayerstats.csv') %>%
  filter(G*MP > mp.threshold)
print(paste(Sys.time() - starttime, " - Player Percentiles Data") )

pp.playerlist <- unique(player.stats.bbr$Player)
names(pp.playerlist) <- pp.playerlist

# ====


# ================================
# 8. Read and Preprocess Data - Outstanding Performances
# ================================

starttime <- Sys.time()
player.stats.bygame <- read_csv('Data/NBA_playerstats_bygame.csv')
print(paste(Sys.time() - starttime, " - Outstanding Perf Data") )

ip.chartlist <- c('40+ Points', '20+ Rebounds', '15+ Assists', '30+ Point Triple Double', 'Triple Double',
                  '10+ Assists, <= 1 Turnover', '7+ 3-Pointers')
names(ip.chartlist) <- ip.chartlist
# ====


# ====================
# User Interface
# ====================
ui <- shinyUI(
        navbarPage(theme = shinytheme("united"), 
           "Interactive NBA Graphing & Charting App",
           
           # first tab - Shot Charts
           # ==========================
           tabPanel("Shot Charts", sidebarLayout(
             
             sidebarPanel(width = 3,
             
               plotOutput("sc.logo", height = 'auto'),
               # tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))), # darken hr()
               br(),
               
               selectInput(inputId = 'sc.shotchart.input', label = 'Select Shot Chart Type:', multiple = FALSE,
                           choices = shotchart.types, selected = 'Heat Map (Team-Season)'),
               uiOutput('sc.playerseason.input'),
               uiOutput('sc.teamgame.input'),
               uiOutput('sc.teamoppgame.input'),
               uiOutput('sc.playergame.input'),
               uiOutput('sc.teamseason.input'),
               
               br(),
               p("The shot chart application features 2 primary chart types (heat maps and marker charts), and filters 
                 for selecting different players, teams, or individual games. Hovering 
                 over the heat maps will display a player's (or team's) ranking in that particular zone, 
                 along with their make / miss splits and shooting percentages, as well as the league's make / miss 
                 splits and shooting percentages.", align="left"),
               br(), 
               p("The style of these charts is highly motivated from similar shot charts created by", 
                 a("Kirk Goldsberry.", href = "https://twitter.com/kirkgoldsberry"), align = 'left'),
               br(),
               p(strong('Tip: '), "to avoid excessive scrolling, hit backspace to clear the name and then type the player/team 
                 whose graph you'd like to see. Use arrow keys or enter to auto-complete.", align = 'left'),
               
               br(), br(), br(), 
  
               h5("Author"),
               a(h5("The Creator"), href="https://twitter.com/going-nowhere")
             ),
             mainPanel(
               # verbatimTextOutput("clientdataText"),
               plotlyOutput("shotchart", height = 'auto'),
               plotOutput('sc.ghostplot', height = '0px')
             )
           )),
           # ====
  
           # second tab - Player XY Charts
           # ===============================
           tabPanel("Player Comparisons", sidebarLayout(
             sidebarPanel(width = 3,
               
               plotOutput("pxy.logo", height = 'auto'),
               br(), 
               
               selectInput(inputId = 'pxy.chart.input', label = 'Select Chart Type:', multiple = FALSE,
                           choices = pxy.charttypes, selected = 'Offensive Efficiency'),
               # select inputs for position and team
               selectInput(inputId = 'pxy.position.input', label = 'Select Position:', multiple = FALSE,
                           choices = pxy.positionoptions, selected = 'All Positions'),
               selectInput(inputId = 'pxy.team.input', label = 'Select Team:', multiple = FALSE,
                           choices = pxy.teamoptions, selected = 'Warriors'),
               # select inputs for specific player
               selectInput(inputId = 'pxy.player.input', label = 'Select Player:', multiple = FALSE,
                           choices = pxy.playeroptions, selected = 'James Harden'),
               br(),
               
               p("The player comparison application is a scatter plotting tool that creates marker graphs
                 based on the chart type selected. Markers can be filtered by position to get a better sense 
                 for which players perform best at their positions. Additionally, player and team inputs can be 
                 selected to emphasize the selected player / the players on the selected team. I am working on a 
                 filter that will allow users to set their own minutes-played threshold for determining which 
                 players qualify for the chart", align = 'left'),
               br(),
               p("Hovering over the individual markers on the scatter plots reveals who that player is, as well as the
                 player's statistics for the graph.", align = 'left'),
               
               br(), br(), br(), 
               h5("Author"),
               a(h5("The Creator"), href="https://twitter.com/going-nowhere")
             ),
             mainPanel(
               plotlyOutput('playerxy', height = 'auto'),
               plotOutput('pxy.ghostplot', height = '0px')
             )
           )),
           # ====
           
           # third tab - Team XY Charts
           # ============================
           tabPanel("Team Comparisons", sidebarLayout(
             sidebarPanel(width = 3,

               plotOutput("txy.logo", height = 'auto'),
               br(),        
              
               # make all the dropdowns in the create-your-own-chart display correctly
               tags$head(
                 tags$style(
                   HTML(".shiny-split-layout > div {
                        overflow: visible; } "))),
               
               # create permanent input for shot chart type (should be 5 options)
               selectInput(inputId = 'charttype.input', label = 'Select Chart Type:', multiple = FALSE,
                           choices = txy.charttypes, selected = 'Scoring Margin'),
               
               splitLayout(uiOutput('txy.xaxisstat.input'), uiOutput('txy.yaxisstat.input')),
               splitLayout(uiOutput('txy.xaxisforagainst.input'), uiOutput('txy.yaxisforagainst.input')),
               br(),
               
               p("The team comparison application is a scatter plotting tool that creates logo graphs
                 based on the chart type selected - it features 5 primary chart types, as well as the ability 
                 to create your own chart by selecting the team statistics to plot on the X and Y axes.", align = 'left'),
               br(),
               p("If the create-your-own-chart functionality is well received, I will add this into the player comparision 
                 application. I may also build additional preset charts.", align = 'left'),
               
               br(), br(), br(), 
               h5("Author"),
               a(h5("The Creator"), href="https://twitter.com/going-nowhere")
             ),
             mainPanel(
               plotlyOutput('teamxy', height = 'auto'),
               plotOutput('txy.ghostplot', height = '0px')
             )
           )),
           # ====
           
           # fourth tab - Game Recaps
           # =========================
           tabPanel("Game Recaps", sidebarLayout(
             sidebarPanel(width = 3,

               plotOutput("gr.logo", height = 'auto'),
               br(),        
              
               selectInput(inputId = 'team.input', label = 'Select Team', multiple = FALSE,
                           choices = gr.teams, selected = 'GSW'),
               uiOutput('msfid.input'),
               
               br(),
               p("The game recap application features an in game win probability (IGWP) line graph as well as a leads bar plot. Hovering over
                 the IGWP graph shows the score of the game at that moment, as well as each team's win probability. Computing a decent estimate for 
                 IGWP is actually fairly simple, requiring only the current score, number of minutes remaining, and the pregame strength of each 
                 team.", align = 'left'),
               br(),
               p("I am working on additional effects for this tool", align = 'left'),

               br(), br(), br(), 
               h5("Author"),
               a(h5("The Creator"), href="https://twitter.com/going-nowhere")
             ),
             
             mainPanel(
               plotlyOutput("leadbarplot", height = 'auto'),
               hr(), 
               plotlyOutput("winprobplot", height = 'auto'),
               plotOutput("gr.ghostplot", height = '0px'),
               br(), br()
             )
           )),
           # ====
           
           # fifth tab - ELO Ratings
           # ==========================
           tabPanel("ELO Ratings", sidebarLayout(
             sidebarPanel(width = 3,
                          
               plotOutput("elo.logo", height = 'auto'),
               br(),
               
               selectInput(inputId = 'elo.team.input', label = 'Select Team:', multiple = FALSE, 
                           choices = elo.teamlist, selected = 'Warriors'),
               
               br(), 
               p("The ELO Ratings application creates line graphs that highlight each team's historical ELO rating (over the last 5 years), with
                 markers displaying the team's highest and lowest rating over this period. This application was motivated 100% by an identical 
                 application built by", a("FiveThirtyEight - here.", href = "https://projects.fivethirtyeight.com/complete-history-of-the-nba"), align = 'left'),
               br(),
               p("In an effort to improve my sports data vis skills, I attempted to reproduce their application to the best of my ability, although
                 my graphs lagged quite a bit with >5 years displayed.", align = "left"),
               
               br(), br(), br(), 
               h5("Author"),
               a(h5("The Creator"), href="https://twitter.com/going-nowhere")
             ),
             
             mainPanel(
               plotlyOutput('eloratings', height = 'auto'),
               plotOutput('elo.ghostplot', height = '0px')
             )
           )),
           # ====
           
           # sixth tab - Assist Networks
           # ==============================
           tabPanel("Assist Networks", sidebarLayout(
             
             sidebarPanel(width = 3,
               plotOutput("an.logo", height = 'auto'),
               br(),  
               
               selectInput(inputId = 'an.team.input', label = 'Select Team:', multiple = FALSE, 
                           choices = an.teamlist, selected = 'GSW'),
               selectInput(inputId = 'an.count.input', label = '# of Players:', multiple = FALSE, 
                           choices = an.nodelist, selected = '5'),
               uiOutput("an.player.highlight"),
               br(),
               
               p("The assist networks application is a network graphing tool that displays which players on each team are assisting their teammates
                 most frequently. A filter is provided for the number of nodes to include in the network graph, as well as a player on the selected 
                 team to highlight.", align = 'left'),
               br(),
               p("Similar to the ELO Ratings chart, this is not an original idea of mine. Motivations for these graphs come 
                 from", a("PResidual", href = "https://twitter.com/presidual"), "and", a("CrumpledJumper", href = "https://twitter.com/CrumpledJumper"),
                 align = 'left'),
               
               br(), br(), br(),
               h5("Author"),
               a(h5("The Creator"), href="https://twitter.com/going-nowhere")
             ),
             mainPanel(
               # plotOutput('assistnet', height = 'auto')
               splitLayout(cellWidths=c("70%","30%"),
                           plotOutput('assistnet'),
                           tableOutput('assisttable'))
             )
           )),
           # ====
           
           # seventh tab - Four Factors
           # ============================
           tabPanel("Four Factors", sidebarLayout(
             
             sidebarPanel(width = 3,
                          plotOutput("ff.logo", height = 'auto'),
                          
                          br(),
                          radioButtons("ff.singleteamindicator.input", label = "Single team's four factors (for and against), or multiple team's four factors (against league average)",
                                       choices = list("single team for and against" = TRUE, "multiple teams against league average" = FALSE), selected = TRUE),
                          
                          br(),
                          uiOutput("ff.teamids"),
                          uiOutput("ff.teamid"),
                          
                          checkboxInput("ff.checkbox", label = "Add Text Labels?", value = FALSE),
                          br(), 
                          p("The four factors application is a bar graphing tool that displays", 
                            a("Dean Oliver's", href = 'https://twitter.com/DeanO_Lytics'), "Four Factors of Basketball Success.
                            The radio buttons toggle between two charts with a similar layout, but with different information shown", align = 'left'),
                          br(),
                          p("Hovering over the individuals bars reveals each team's stat value, as well as the team's ranking, for that factor."),
                          
                          br(), br(), br(),
                          h5("Author"),
                          a(h5("The Creator"), href="https://twitter.com/going-nowhere")
                          ),
             mainPanel(
               plotlyOutput('fourfactors', height = 'auto'),
               plotOutput('ff.ghostplot', height = '0px')
             )
           )),
           # ====
           
           # eight tab - Player Percentiles
           # =================================
           tabPanel("Player Percentiles", sidebarLayout(
             
             sidebarPanel(width = 3,
                          plotOutput("pp.logo", height = 'auto'),
                          br(),  
                          
                          selectInput(inputId = 'pp.player.input', label = 'Select Player:', multiple = FALSE, 
                                      choices = pp.playerlist, selected = 'Stephen Curry'),
                          
                          p("The player percentiles application is a box and scatter plotting tool that displays the 
                            percentile rankings of players across 11 different stats. Each marker can be hovered over to 
                            see that player and his statistics.", align = 'left'),
                          br(),
                          p(strong('Tip: '), "Use the vertical zoom to better examine the box plots for steal and block 
                            percentage.", align = 'left'),
                          
                          br(), br(), br(),
                          h5("Author"),
                          a(h5("The Creator"), href="https://twitter.com/going-nowhere")
                          ),
             mainPanel(
               plotlyOutput('playerpercentiles', height = 'auto'),
               plotOutput('pp.ghostplot', height = '0px')
             )
           )),
           # ====
           
           # ninth tab - Impressive Performances 
           # =====================================
           tabPanel("Outstanding Performances", sidebarLayout(
             
             sidebarPanel(width = 3,
                          plotOutput("ip.logo", height = 'auto'),
                          br(),  
                          
                          selectInput(inputId = 'ip.chart.input', label = 'Select Chart Type:', multiple = FALSE, 
                                      choices = ip.chartlist, selected = '40+ Points'),
                          br(),
                          
                          p("The outstanding performances application is a bubble-histogram graphing tool, motivated by graphs of a
                            similar style created by both", 
                            a("CrumpledJumper", href = "https://twitter.com/CrumpledJumper"), 'and', 
                            a("FiveThirtyEight.", href = "https://fivethirtyeight.com"), align = 'left'),
                          br(),
                          
                          p("Each player's average statistics in the table reflect that player's statistics in only those games
                            where they met the particular stat requirement that is being filtered on."),

                          br(), br(), br(),
                          h5("Author"),
                          a(h5("The Creator"), href="https://twitter.com/going-nowhere")
                          ),
             mainPanel(
               plotlyOutput('impressiveperformances', height = 'auto'),
               plotOutput('ip.ghostplot', height = '0px'),
               br(), br(),
               DT::dataTableOutput("ip.table")
             )
           ))
           # ====
  ))
# ====


# ====================
# Server Code
# ====================
server <- shinyServer(function(input, output, session) {
  
  source("server_shotcharts.R", local = TRUE)
  source("server_playerxycharts.R", local = TRUE)
  source("server_teamxycharts.R", local = TRUE)
  source("server_gamerecaps.R", local = TRUE)
  source("server_eloratings.R", local = TRUE)
  source("server_assistnet.R", local = TRUE)
  source("server_fourfactors.R", local = TRUE)
  source("server_playerpercentiles.R", local = TRUE)
  source("server_impressiveperformances.R", local = TRUE)
  
})
# ====

# client data
# cdata <- session$clientData

# Values from cdata returned as text
# output$clientdataText <- renderText({
#   cnames <- names(cdata)
#   
#   allvalues <- lapply(cnames, function(name) {
#     paste(name, cdata[[name]], sep = " = ")
#   })
#   paste(allvalues, collapse = "\n")
# })

shinyApp(ui, server)

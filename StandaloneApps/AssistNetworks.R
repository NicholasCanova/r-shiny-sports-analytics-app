
# http://kateto.net/network-visualization

# ===============
# 0. Setup
# ===============

# set working directory, clear environment
setwd('/Users/Home/Dropbox/My_Documents/Not_Work_or_School/Sports Analytics/InteractiveSportsGraphs/')
rm(list = ls())

# libraries for data vis
library(shiny)
library(shinythemes)
library(igraph)
library(ggnetwork)
library(ggplot2)

# library for data manip  
library(dplyr)
library(network)
library(sna)
# ====


# =======================
# 1. Grab Assist Data
# =======================

# print this

# Run MSFScraper.R to update the feed
assist.pbp <- read.csv('Data/MSF_Feeds/NBA_gameplaybyplay_assistnet.csv', stringsAsFactors = FALSE)

# create function that stops once a list of players to select from is created
highlightOptions <- function(assist.pbp, team.id, node.count) {
  
  # filter pbp to include only this team, only FGM that were assisted on
  team.pbp <- assist.pbp %>%
    filter(outcome == "SCORED" & 
           team == team.id & 
           !is.na(assisterlastname))
  
  # create dataframe to map names to names with linebreak \n in them
  team.map <- data.frame(name1 = sort(unique(c(paste(team.pbp$shooterfirstname, team.pbp$shooterlastname),
                                               paste(team.pbp$assisterfirstname, team.pbp$assisterlastname)))),
                         plotname = sort(unique(c(paste(team.pbp$shooterfirstname, '\n', team.pbp$shooterlastname, sep = ''),
                                                  paste(team.pbp$assisterfirstname, '\n', team.pbp$assisterlastname, sep = '')))),
                         stringsAsFactors = FALSE)
  
  
  # compute tallies, used to determine which players to include in graph
  FGMtable = table(paste(team.pbp$shooterfirstname, team.pbp$shooterlastname))
  ASTtable = table(paste(team.pbp$assisterfirstname, team.pbp$assisterlastname))
  
  team.tallies <- as.data.frame(FGMtable, stringsAsFactors = FALSE) %>% 
    left_join(as.data.frame(ASTtable, stringsAsFactors = FALSE), by = c('Var1'='Var1')) %>%
    setNames(c('Player', 'FGM', 'AST')) %>%
    mutate(Total = FGM + AST) 
  
  if(node.count == 'auto select') {
    team.tallies <- team.tallies %>% 
      filter(Total > sum(Total, na.rm = TRUE) / 20)
  } else {
    node.count = as.integer(node.count)
    team.tallies <- team.tallies %>%
      arrange(-Total) %>%
      top_n(node.count, wt = Total)
  }
  
  return(team.tallies$Player)
}

# create the entire assist network function
drawAssistNetwork <- function(assist.pbp, team.id, node.count, player.id) {
  
  # filter pbp to include only this team, only FGM that were assisted on
  team.pbp <- assist.pbp %>%
    filter(outcome == "SCORED" & 
             team == team.id & 
             !is.na(assisterlastname))
  
  # create dataframe to map names to names with linebreak \n in them
  team.map <- data.frame(name1 = sort(unique(c(paste(team.pbp$shooterfirstname, team.pbp$shooterlastname),
                                               paste(team.pbp$assisterfirstname, team.pbp$assisterlastname)))),
                         plotname = sort(unique(c(paste(team.pbp$shooterfirstname, '\n', team.pbp$shooterlastname, sep = ''),
                                                  paste(team.pbp$assisterfirstname, '\n', team.pbp$assisterlastname, sep = '')))),
                         stringsAsFactors = FALSE)
  
  
  # compute tallies, used to determine which players to include in graph
  FGMtable = table(paste(team.pbp$shooterfirstname, team.pbp$shooterlastname))
  ASTtable = table(paste(team.pbp$assisterfirstname, team.pbp$assisterlastname))
  
  team.tallies <- as.data.frame(FGMtable, stringsAsFactors = FALSE) %>% 
    left_join(as.data.frame(ASTtable, stringsAsFactors = FALSE), by = c('Var1'='Var1')) %>%
    setNames(c('Player', 'FGM', 'AST')) %>%
    mutate(Total = FGM + AST) 
  
  if(node.count == 'auto select') {
    team.tallies <- team.tallies %>% 
      filter(Total > sum(Total, na.rm = TRUE) / 20)
  } else {
    node.count = as.integer(node.count)
    team.tallies <- team.tallies %>%
      arrange(-Total) %>%
      top_n(node.count, wt = Total)
  }
  
  # initialize edges dataframe
  team.edges <- cbind(paste(team.pbp$assisterfirstname, team.pbp$assisterlastname),
                      paste(team.pbp$shooterfirstname, team.pbp$shooterlastname)) %>%
    as.data.frame(., stringsAsFactors = FALSE) %>%
    setNames(c('from', 'to')) %>%
    group_by(from, to) %>%
    dplyr::summarise(edgeweight = n()) %>%
    filter(from %in% team.tallies$Player & 
           to %in% team.tallies$Player)
  
  # initialize nodes dataframe                       
  team.nodes <- as.data.frame(unique(c(team.edges$from, team.edges$to))) %>% setNames('id')
  
  # compute tallies, which will be
  nba.net <- graph_from_data_frame(d = team.edges, vertices = team.nodes, directed = T)
  
  zed = ggnetwork(nba.net, layout = layout_in_circle(nba.net)) %>%
          dplyr::select(-one_of(c('na.x', 'na.y')))
  zed$x = round(as.vector(zed$x), digits = 3)
  zed$y = round(as.vector(zed$y), digits = 3)
  zed$xend = round(as.vector(zed$xend), digits = 3)
  zed$yend = round(as.vector(zed$yend), digits = 3)
  zed$vertex.names = as.character(zed$vertex.names)
  zed <- zed %>% left_join(team.map, by = c('vertex.names'='name1'))
  
  # add destination names (what vertex is being pointed to)
  node.df <- zed[is.na(zed$edgeweight), 1:3]
  end.df <- zed[!is.na(zed$edgeweight), 4:5]
  end.players <- apply(end.df, 1, FUN = function(x) {
    dists = (x[1] - node.df$x)^2 + (x[2] - node.df$y)^2
    end.player = node.df$vertex.names[which.min(dists)]
    return(end.player)
  })
  zed$dest.names[!is.na(zed$edgeweight)] = end.players

  # create assistcount column == edgeweights, modify edgeweights
  zed$assistcount <- zed$edgeweight
  zed$edgeweight <- (zed$edgeweight)^2
  
  # assign node colors
  my.node.pallet <- c('#bc0f39', '#2323a3', '#32993f', '#7c159b', '#d36a1f', '#d6bc0e',
                      '#46f0f0', '#f032e6', '#d2f53c', '#fabebe', '#008080', '#aa6e28')
  zed$nodecolors <- NA 
  zed$nodecolors[is.na(zed$edgeweight)] = my.node.pallet[1:length(zed$nodecolors[is.na(zed$edgeweight)])]
  
  # assign edge colors
  zed$edgecolors <- 'grey90'
  # colors going out of player.id (easy)
  focusplayer.edgeoutcolor <- zed$nodecolors[is.na(zed$edgeweight) & zed$vertex.names == player.id]
  zed$edgecolors[zed$vertex.names == player.id] = focusplayer.edgeoutcolor
  # colors coming into player.id (hard)
  players.intofocusplayer <- zed$vertex.names[which(zed$dest.names == player.id)]
  their.colors <- sapply(players.intofocusplayer, FUN = function(x) zed$nodecolors[is.na(zed$edgeweight) & zed$vertex.names == x])  
  zed$edgecolors[which(zed$dest.names == player.id)] = their.colors
  
  # and the plot
  assist.plot <- ggplot(zed, aes(x = x, y = y, xend = xend, yend = yend)) +
    
    # add edges
    geom_edges(aes(size = edgeweight), color = zed$edgecolors[which(!is.na(zed$edgeweight))], 
               curvature = 0.2,
               arrow = arrow(length = unit(12, "pt"), type = "closed"),
               alpha = 0.8) +
    
    # add nodes
    geom_nodes(data = zed[is.na(zed$edgeweight), ], 
               color = zed$nodecolors[is.na(zed$edgeweight)], 
               size = 10) +

    # add node labels 
    geom_nodetext(aes(label = factor(plotname)), fontface = "bold",
                  nudge_x = 0.03, nudge_y = 0.08, size = 6) +
    
    xlim(-0.05, 1.05) + ylim(-0.05, 1.08) +
    ggtitle(paste('Assist Network For', team.id, '- A Focus On', player.id)) +
    theme_blank() +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold", size = (21), hjust = 0.5))
  
  return(list(assist.plot, zed))
}


# create vectors for selectInputs
team.list <- sort(unique(assist.pbp$team))
names(team.list) <- team.list

node.list <- c('auto select',2,3,4,5,6,7,8,9,10,11,12)
names(node.list) <- node.list
# ====


# ====================
# 2. User Interface
# ====================
ui <- fluidPage(theme = shinytheme('united'),
                
                fluidRow(
                  column(width = 12, align = 'center',
                         h2('NBA Assist Networks'))
                ),
                
                fluidRow(
                  column(width = 1, align = 'center'),
                  column(width = 2, align = 'center',
                    selectInput(inputId = 'team.input', label = 'Select Team:', multiple = FALSE, 
                                choices = team.list, selected = 'NYK')
                  ),
                  column(width = 2, align = 'center',
                    selectInput(inputId = 'count.input', label = '# of Players:', multiple = FALSE, 
                                choices = node.list, selected = 'auto select')
                  ),
                  column(width = 2, align = 'center', 
                    uiOutput("player.highlight")
                  )
                ),

                br(),
                fluidRow(
                  column(width = 8, align = 'center',
                         plotOutput('assistnet', height = 'auto')
                  ),
                  column(width = 4, align = 'center',
                         tableOutput('assisttable'))
                )                
)
# ====


# ===========================
# 3. SERVER SIDE CODE 
# ===========================

server <- shinyServer(function(input, output, session) {
  
  output$player.highlight <- renderUI({
    
    node.count <- input$count.input
    team.id <- input$team.input
    
    these.players <- highlightOptions(assist.pbp, team.id, node.count)
    names(these.players) <- these.players
    
    selectInput(inputId = 'player.id', label = 'Player to Highlight:', multiple = FALSE,
                choices = these.players, selected = these.players[1])
  })
  
  # 3. Create the chart
  # ===-===-===-===-===-===
  output$assistnet <- renderPlot({
    
    print(session$clientData$output_assistnet_width)
    # everything tied neatly into the drawAssistNetwork function
    drawAssistNetwork(assist.pbp, input$team.input, input$count.input, input$player.id)[[1]]
    
  }, height = function() { session$clientData$output_assistnet_width })
  
  output$assisttable <- renderTable({
    assist.df <- drawAssistNetwork(assist.pbp, input$team.input, input$count.input, input$player.id)[[2]] %>%
      dplyr::select(-one_of(c('x', 'y', 'xend', 'yend', 'edgeweight', 'plotname', 'nodecolors', 'edgecolors'))) %>%
      filter(!is.na(assistcount)) %>%
      setNames(c('Passer', 'Scorer', 'Assists')) %>%
      arrange(-Assists)
  })
})
# ====

shinyApp(ui, server)


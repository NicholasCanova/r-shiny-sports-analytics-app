
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

# for the Shiny App
library(shiny)
library(shinythemes)

# ====


# ===========================
# 1. Preprocess The Data
# ===========================

gamerecaps.all <- read.csv('Data/MSF_Feeds/NBA_gameplaybyplay_gamerecaps.csv', stringsAsFactors = FALSE)

# create 2 charting functions
# add window.width to this function as well
drawGameRecapIGWPChart <- function(gamerecaps.all, this.msfid) {
  
  nba.game <- gamerecaps.all[gamerecaps.all$msfgameid == this.msfid, ]
  
  # set colors, axes, shapes, annotations, legend, etc.
  # =============================================
  home_ID <- unique(nba.game$hID)
  home_color <- color.pal.df$colorhex[color.pal.df$teamname == home_ID]
  away_ID <- unique(nba.game$aID)
  away_color <- color.pal.df$colorhex[color.pal.df$teamname == away_ID]
  
  ax <- list(title = "",
             tickfont = list(size = 14),
             # tickfont = list(size = window.width/60),
             color = 'rgb(50, 50, 50)',
             showgrid = FALSE,
             tickmode = "array",
             tickvals = c(40, 720, 1440, 2160, 2840),
             ticktext = c('<b>Start\nGame</b>', '<b>Q1</b>', '<b>Half</b>', '<b>Q3</b>', '<b>End\nGame</b>'),
             zeroline = FALSE)
  
  ay <- list(title = "",
             tickfont = list(size = 14),
             # tickfont = list(size = window.width/60),
             color = 'rgb(50, 50, 50)',
             showgrid = FALSE,
             tickmode = "array", 
             tickvals = c(0, 1),
             tickprefix = " ",
             ticksuffix = " ",
             ticktext = c('<b>0% </b>', '<b>100% </b>'),
             zeroline = FALSE)
  
  myshapes <- list(
    q0line <- list(type = 'line', x0 = 0, x1 = 0, y0 = 0, y1 = 1, 
                  line = list(width = 2, dash = 'solid',
                              color = 'rgb(100, 100, 100)')),
    q1line <- list(type = 'line', x0 = 720, x1 = 720, y0 = 0, y1 = 1, 
                  line = list(width = 1, dash = 'dash',
                              color = 'rgb(100, 100, 100)')),
    q2line <- list(type = 'line', x0 = 1440, x1 = 1440, y0 = 0, y1 = 1, 
                  line = list(width = 1, dash = 'dash',
                              color = 'rgb(100, 100, 100)')),
    q3line <- list(type = 'line', x0 = 2160, x1 = 2160, y0 = 0, y1 = 1, 
                  line = list(width = 1, dash = 'dash',
                              color = 'rgb(100, 100, 100)')),
    q4line <- list(type = 'line', x0 = 2880, x1 = 2880, y0 = 0, y1 = 1, 
                  line = list(width = 2, dash = 'solid',
                              color = 'rgb(100, 100, 100)')),
    fiftyline <- list(type = line, x0 = 10, x1 = 2870, y0 = 0.5, y1 = 0.5,
                     line = list(width = 2, dash = 'solid',
                                 color = 'rgb(100, 100, 100)')),
    topline <- list(type = line, x0 = 10, x1 = 2870, y0 = 1, y1 = 1,
                   line = list(width = 1, dash = 'solid',
                               color = 'rgb(100, 100, 100)')),
    bottomline <- list(type = line, x0 = 10, x1 = 2870, y0 = 0, y1 = 0,
                      line = list(width = 1, dash = 'solid',
                                  color = 'rgb(100, 100, 100)'))
  )
  
  # ====
  
  # make adjustments to seconds remaining for OT games
  # ====================================================
  all.quarters <- unique(nba.game$quarter)
  if(length(all.quarters) > 4) {
    quarters_beyond_4 <- all.quarters[all.quarters > 4]
    ax$ticktext[5] <- '<b>End\nReg</b>'
    
    for(ot.num in 1:length(quarters_beyond_4)) {
      # modify x axis labels
      ax$tickvals <- c(ax$tickvals, max(ax$tickvals)+300)
      ax$ticktext <- c(ax$ticktext, paste0('<b>End\nOT',ot.num,'</b>'))
      
      # fix shapes - extend horizontal line
      myshapes[[6]]$x1 <- myshapes[[6]]$x1 + 300
      myshapes[[7]]$x1 <- myshapes[[7]]$x1 + 300
      myshapes[[8]]$x1 <- myshapes[[8]]$x1 + 300
      
      # fix shapes - add new vertical line
      newOTline <- list(type = line, x0 = myshapes[[6]]$x1+10, x1 = myshapes[[6]]$x1 + 10,
                        y0 = 0, y1 = 1, line = list(width = 2, dash = 'solid', 
                                                    color = 'rgb(100, 100, 100)'))
      myshapes[[length(myshapes)+1]] <- newOTline

      # fix shapes - change prev vertical line to dotted, thinner
      if(ot.num == 1) {
        myshapes[[5]]$line$dash = 'dash'
        myshapes[[5]]$line$width = 1
      } else {
        myshapes[[length(myshapes)-1]]$line$dash = 'dash'
        myshapes[[length(myshapes)-1]]$line$width = 1
      }
    }
  }
  # ====
  
  
  # continue with annotations / legend
  # ====================================
  myannotations <- list(text = paste('<b>', nba.game$hID[1], ' (', tail(nba.game$homeScore, 1), ')', ' vs. ',
                                     nba.game$aID[1], ' (', tail(nba.game$awayScore, 1), ')</b>', sep = ''),
                        font = list(size = 24, color = 'rgb(50, 50, 50)'),
                        # font = list(size = window.width/48, color = 'rgb(50, 50, 50)'),
                        x = max(nba.game$secsRemaining) / 2,
                        y = 1.12,
                        showarrow = FALSE)
  
  mylegend <- list(orientation = 'v',
                   font = list(size = 14),
                   # font = window.width / 100,
                   x = 0.02,
                   y = 0.85)
  # ====
  
  # create plot_ly win prob object
  # ==================================
  totalSecs <- max(nba.game$secsRemaining)
  nba.game %>% mutate(secsIntoGame = totalSecs - secsRemaining,
                      winProb2 = 1 - winProb) %>%
    # plot_ly(height = 600, width = 1200) %>%
    plot_ly() %>%
    
    # add home teams win prob line
    add_trace(x = ~secsIntoGame, y = ~winProb, type = 'scatter', mode = 'lines',
              line = list(color = home_color, size = 4, width = 4),
              name = nba.game$hID[1],
              hoverinfo = "text",
              text = ~paste('Quarter: ', quarter, '\n',
                            'Mins Remaining: ', qminsRemaining %% 12, '\n',
                            'Score: ', paste0(homeScore, ' - ', awayScore), '\n',
                            'Win Prob: ', paste0(round(nba.game$winProb*100, 0), '%'))) %>%
    
    # add away teams win prob line
    add_trace(x = ~secsIntoGame, y = ~winProb2, type = 'scatter', mode = 'lines',
              line = list(color = away_color, size = 4, width = 4),
              name = nba.game$aID[1],
              hoverinfo = "text",
              text = ~paste('Quarter: ', quarter, '\n',
                            'Mins Remaining: ', qminsRemaining %% 12, '\n',
                            'Score: ', paste0(awayScore, ' - ', homeScore), '\n',
                            'Win Prob: ', paste0(round(nba.game$winProb2*100, 0), '%'))) %>%
    layout(xaxis = ax, yaxis = ay,
           shapes = myshapes,
           annotations = myannotations,
           legend = mylegend)
  # ====
  
}
drawGameRecapLeadBarsChart <- function(gamerecaps.all, this.msfid) {
  
  nba.game <- gamerecaps.all[gamerecaps.all$msfgameid == this.msfid, ]
  maxSecs <- max(nba.game$secsRemaining)
  
  # compute game stats (max lead, lead change, etc.)
  # =====================================================
  # intervals for horizontal lines (each 5 points)
  ymax <- max(plyr::round_any(abs(max(nba.game$homeLead)), accuracy = 5, f = ceiling), 20)
  yseq <- seq(-ymax,ymax,5)
  xseq <- c(720, 1440, 2160)
  
  # bar center and width values
  barwidths = c(); barcenters = c();
  for(i in 1:nrow(nba.game)) {
    
    # no bars for first/last rows
    if(i == 1) { barwidths <- c(barwidths, 0); barcenters <- c(barcenters, 0); next }
    if(i == nrow(nba.game)) { barwidths <- c(barwidths, 0); barcenters <- c(barcenters, 0); next }
    
    # edge case bars for second/second-to-last rows
    if(i == 2) {
      start_range <- nba.game$secsRemaining[1]
      end_range <- mean(nba.game$secsRemaining[2:3])
      
      this_width <- start_range - end_range
      this_center <- (start_range + end_range) / 2
      
      barwidths <- c(barwidths, this_width)
      barcenters <- c(barcenters, this_center)
      next
    }
    
    if(i == (nrow(nba.game)-1)) {
      start_range <- mean(nba.game$secsRemaining[(nrow(nba.game)-2):(nrow(nba.game)-1)])
      end_range <- nba.game$secsRemaining[nrow(nba.game)]
      
      this_width <- start_range - end_range
      this_center <- (start_range + end_range) / 2
      
      barwidths <- c(barwidths, this_width)
      barcenters <- c(barcenters, this_center)
      next
    }
    
    # everything besides the edge cases
    start_range <- mean(nba.game$secsRemaining[(i-1):i])
    end_range <- mean(nba.game$secsRemaining[i:(i+1)])
    
    this_width <- start_range - end_range
    this_center <- (start_range + end_range) / 2
    
    barwidths <- c(barwidths, this_width)
    barcenters <- c(barcenters, this_center)
  }
  barwidths <- barwidths * 1.2
  
  # compute longest run, times tied and lead changes
  compute_longest_run <- function(x) {
    
    # Collapse repetitions
    x_unique <- rle(x)$values
    
    # Compute score change
    score_change <- diff(x_unique)
    
    # Need to compute sum of all subvectors with the same sign
    run_side <- sign(score_change)
    run_id <- c(1, cumsum(diff(run_side) != 0) + 1)
    run_value <- tapply(score_change, run_id, sum)
    
    max(abs(run_value))
  }
  hL <- nba.game$homeLead
  
  # compute all 3 right here
  times_tied <- sum(hL[-1] == 0 & hL[-length(hL)] != 0)
  lead_changes <- sum(diff(sign(hL[hL != 0])) != 0)
  longest_run <- compute_longest_run(hL)
  # ====
  
  # # set colors, axes, shapes, annotations, legend, etc.
  # =======================================================
  home_ID2 <- unique(nba.game$hID)
  home_color2 <- color.pal.df$colorhex[color.pal.df$teamname == home_ID2]
  away_ID2 <- unique(nba.game$aID)
  away_color2 <- color.pal.df$colorhex[color.pal.df$teamname == away_ID2]
  
  ax2 <- list(title = "",
              color = 'rgb(100, 100, 100)',
              showgrid = FALSE,
              tickmode = "array",
              tickvals = c(40, 720, 1440, 2160, 2840),
              ticktext = c('<b>Start\nGame</b>', '<b>Q1</b>', '<b>Half</b>', '<b>Q3</b>', '<b>End\nGame</b>'),
              # tickfont = list(size = window.width/60),
              zeroline = FALSE)
  
  ay2 <- list(title = "",
              color = 'rgb(100, 100, 100)',
              showgrid = FALSE,
              tickmode = "array",
              tickvals = yseq,
              ticktext = paste('<b>', abs(yseq), '</b>', sep = ''),
              # tickfont = list(size = window.width/60),
              zeroline = FALSE)
  
  myshapes2 <- list()
  for(yval in yseq) {
    horiz_line <- list(type = 'line', x0 = 20, x1 = maxSecs, y0 = yval, y1 = yval,
                       line = list(width = 1, dash = 'solid', color = 'rgb(150, 150, 150)'))
    myshapes2[[length(myshapes2)+1]] <- horiz_line
  }
  for(xval in xseq) {
    vert_line <- list(type = 'line', x0 = xval, x1 = xval, y0 = -ymax, y1 = ymax,
                      line = list(width = 1, dash = 'dash', color = 'rgb(150, 150, 150)'))
    myshapes2[[length(myshapes2)+1]] <- vert_line
  }
  
  myshapes2[[length(myshapes2)+1]] <- list(type = 'line', x0 = 480, x1 = 2400, y0 = ymax + 15, y1 = ymax + 15,
                                           line = list(width = 2, dash = 'solid', color = 'rgb(150, 150, 150)'))
  # ====
  
  # make adjustments to seconds remaining for OT games
  # ====================================================
  all.quarters <- unique(nba.game$quarter)
  if(length(all.quarters) > 4) {
    quarters_beyond_4 <- all.quarters[all.quarters > 4]
    ax2$ticktext[5] <- '<b>End\nReg</b>'
    
    for(ot.num in 1:length(quarters_beyond_4)) {
      # modify x axis labels
      ax2$tickvals <- c(ax$tickvals, max(ax$tickvals)+300)
      ax2$ticktext <- c(ax$ticktext, paste0('<b>End\nOT',ot.num,'</b>'))
      
      # fix shapes - add new vertical line
      xval <- 2880 + (300*(ot.num-1))
      newOTline <- list(type = 'line', x0 = xval, x1 = xval, y0 = -ymax, y1 = ymax,
                        line = list(width = 1, dash = 'dash', color = 'rgb(150, 150, 150)'))
      myshapes2[[length(myshapes2)+1]] <- newOTline
    }
  }
  # ====
  
  # annotations - all of the text on the page
  myannotations2 <- list(
    # biggest lead text and values
    list(text = paste0('<b>',max(nba.game$homeLead),'</b>'), x = 1160, y = ymax + 23,
         font = list(size = 64, color = home_color2),
         showarrow = FALSE),
    list(text = paste0('<b>',abs(min(nba.game$homeLead)),'</b>'), x = 1720, y = ymax + 23,
         font = list(size = 64, color = away_color2),
         showarrow = FALSE),
    list(text = '<b>Biggest\nLead</b>', x = 1440, y = ymax + 23,
         font = list(size = 24, color = 'gray80'),
         showarrow = FALSE),
    
    # other three stat labels
    list(text = '<b>Times\nTied</b>', x = 600, y = ymax + 8,
         font = list(size = 22, color = 'gray80'),
         showarrow = FALSE),
    list(text = '<b>Longest\nRun</b>', x = 1320, y = ymax + 8,
         font = list(size = 22, color = 'gray80'),
         showarrow = FALSE),
    list(text = '<b>Lead\nChanges</b>', x = 2040, y = ymax + 8,
         font = list(size = 22, color = 'gray80'),
         showarrow = FALSE),
    
    # three stat values
    list(text = paste0('<b>', times_tied, '</b>'), x = 840, y = ymax + 8,
         font = list(size = 48, color = 'gray80'),
         showarrow = FALSE),
    list(text = paste0('<b>', longest_run, '</b>'), x = 1560, y = ymax + 8,
         font = list(size = 48, color = 'gray80'),
         showarrow = FALSE),
    list(text = paste0('<b>', lead_changes, '</b>'), x = 2280, y = ymax + 8,
         font = list(size = 48, color = 'gray80'),
         showarrow = FALSE)
  )
  
  mylegend2 <- list(orientation = 'h',
                    x = 0.7,
                    y = 1.01)
  # ====
  
  # create plot_ly lead tracker object
  # ======================================
  nba.game %>% mutate(secsIntoGame = maxSecs - secsRemaining,
                      winProb2 = 1 - winProb,
                      colorsVec = ifelse(homeLead > 0, home_color2, away_color2),
                      barWidths = barwidths,
                      barCenters = maxSecs - barcenters) %>%
    plot_ly() %>%
    
    add_bars(x = ~factor(barCenters), y = ~homeLead, width = ~barWidths,
             marker = list(color = ~colorsVec),
             hoverinfo = 'text',
             text = ~paste('Quarter: ', quarter, '\n',
                           'Mins Remaining: ', qminsRemaining %% 12, '\n',
                           'Score: ', paste0(homeScore, ' - ', awayScore))) %>%
    
    layout(xaxis = ax2,
           yaxis = ay2,
           shapes = myshapes2,
           annotations = myannotations2,
           legend = mylegend2)
}

this.msfid = '20171212-LAL-NYK'
drawGameRecapIGWPChart(gamerecaps.all, this.msfid)
drawGameRecapLeadBarsChart(gamerecaps.all, this.msfid)

gr.teams <- unique(gamerecaps.all$hID)
names(gr.teams) <- gr.teams

gr.gameids <- unique(gamerecaps.all$msfgameid)
names(gr.gameids) <- gr.gameids 

# have to address duplicates here (best with a second, backup DF of secondary team colors)
color.pal.df <- data.frame(
  colorhex = c('#006BB6', '#2C5234', '#BA0C2F', '#6F263D', '#007A33', '#D50032',
               '#23375B', '#C8102E', '#862633', '#201747', '#002B5C', '#724C9F',
               '#FF671F', '#702F8A', '#007DC5', '#0050B5', '#010101', '#418FDE', 
               '#041E42', '#002B5C', '#003DA5', '#CE1141', '#BA0C2F', '#B6BFBF',
               '#E56020', '#007DC3', '#7AC143', '#F0163A', '#FFC72D', '#0C2340'),
  teamname = c("PHI", "MIL", "CHI", "CLE", "BOS", "LAC", 
               "MEM", "ATL", "MIA", "CHA", "UTA", "SAC", 
               "NYK", "LAL", "ORL", "DAL", "BRO", "DEN", 
               "IND", "NOP", "DET", "TOR", "HOU", "SAS", 
               "PHX", "OKC", "MIN", "POR", "GSW", "WAS"), 
  stringsAsFactors = FALSE)


# ========================
# 2. User Interface
# ========================
ui <- fluidPage(theme = shinytheme('united'),
                
                # 2.A Create Shiny App Title 
                # ===-===-===-===-===-===-===-===
                fluidRow(
                  column(width = 12, align = 'center',
                         h2('NBA Game Recap / Analysis Application'))
                ),
                
                # 2.B Launch the Chart
                # ===-===-===-===-===-===
                fluidRow(
                  column(width = 12, align = 'center',
                         br(),
                         plotlyOutput("winprobplot", height = '400px'),
                         plotlyOutput("leadbarplot", height = '550px')
                  )
                ),
                  
                # 2.C Create Widgets Below App
                # ===-===-===-===-===-===-===-===
                fluidRow(
                  column(width = 12, align = 'center',
                         selectInput(inputId = 'team.input', label = 'Select Team', multiple = FALSE,
                                     choices = gr.teams, selected = gr.teams[1]),
                         uiOutput('msfid.input')
                  )
                )
)
# ====


# ========================
# 4. Server
# ========================
server <- shinyServer(function(input, output) {
  
  # custom input parameter all games vs. gr.thisteam
  output$msfid.input <- renderUI({
    
    req(input$team.input)
    
    team.input <- input$team.input
    print(team.input)
    all.gameids <- sort(unique(gamerecaps.all$msfgameid[gamerecaps.all$aID == team.input | gamerecaps.all$hID == team.input]), decreasing = TRUE)
    print(all.gameids)
    
    selectInput(inputId = 'this.msfid', label = 'Select Game ID:', multiple = FALSE,
                choices = all.gameids, selected = all.gameids[1])
  })
  
  output$winprobplot <- renderPlotly({
    
    this.msfid <- input$this.msfid
    drawGameRecapIGWPChart(gamerecaps.all, this.msfid)
    
  })
  

  output$leadbarplot <- renderPlotly({

    this.msfid <- input$this.msfid
    drawGameRecapLeadBarsChart(gamerecaps.all, this.msfid)
    
  })
})

shinyApp(ui, server)

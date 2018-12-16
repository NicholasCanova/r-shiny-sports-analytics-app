
# =============================
# 1. Shot Chart Application
# =============================

# 1A. Court Line Coordiates - hex_chart and marker_chart
drawCourt_ggplot = function(hex_chart = FALSE) {
  
  # Setup Court Diagram Points
  # ==================================
  
  # A.1. Corner 3PT Line
  # ==================================
  left.corner_x = c(3, 3)
  left.corner_y = c(0, 14)
  
  right.corner_x = c(47, 47)
  right.corner_y = c(0, 14)
  
  # A.2. Three-Point Arc
  # ==================================
  # define arc angle.
  theta_wing = seq(atan(-22.000/9.25), atan(-9.27318148/22.02381), length.out = 200)
  theta_straight = seq(atan(-9.27318148/22.02381), atan(9.27318148/22.02381), length.out = 200)
  
  # arc_one is far wing
  arc_one = mutate(data.frame(x = -23.90*sin(theta_wing) + 25, 
                              y = 23.90*cos(theta_wing) + 4.75), 
                   distance = sqrt((x - 25)^2 + (y - 4.75)^2))
  
  # arc_two is straight-away
  arc_two = mutate(data.frame(x = -23.90*sin(theta_straight) + 25, 
                              y = 23.90*cos(theta_straight) + 4.75), 
                   distance = sqrt((x - 25)^2 + (y - 4.75)^2))
  
  # arc_three is near wing
  arc_three = mutate(data.frame(x = 23.90*sin(rev(theta_wing)) + 25, 
                                y = 23.90*cos(rev(theta_wing)) + 4.75), 
                     distance = sqrt((x - 25)^2 + (y - 4.75)^2))
  
  # adjust the end-points to precisely match corner three lines. 
  arc_one$x[1] = 47.000
  arc_one$y[1] = 14.000
  
  arc_three$x[nrow(arc_three)] = 3.000
  arc_three$y[nrow(arc_three)] = 14.000
  
  # A.3. Input three-point arc coordinates
  # ==================================
  arc_x = c(rev(arc_three$x), rev(arc_two$x), rev(arc_one$x))
  arc_y = c(rev(arc_three$y), rev(arc_two$y), rev(arc_one$y))
  
  # A.4 Combine 3PT Coordinates
  # ==================================
  three_point_x = c(left.corner_x, arc_x, right.corner_x)
  three_point_y = c(left.corner_y, arc_y, right.corner_y)
  three_point_ID = rep('three_point', length(three_point_x))
  
  
  # B.1. Some Free Throw Line Starters
  # ==================================
  left.outside_x = c(17, 17)
  left.outside_y = c(0, 19)
  
  right.outside_x = c(33, 33)
  right.outside_y = c(0, 19.00)
  
  free.throw.line_x = c(17, 33)
  free.throw.line_y = c(19, 19)
  
  FT.perimeter_x = c(left.outside_x, free.throw.line_x, right.outside_x)
  FT.perimeter_y = c(left.outside_y, free.throw.line_y, right.outside_y)
  FT.perimeter_ID = rep('FT_perimeter', length(FT.perimeter_x))
  
  left.inside_x = c(19, 19)
  left.inside_y = c(0, 19)
  left.inside_ID = rep('FT_inside_left', length(left.inside_x))
  
  right.inside_x = c(31, 31)
  right.inside_y = c(0, 19)
  right.inside_ID = rep('FT_inside_right', length(right.inside_x))
  
  # B.2. Center Court Circles
  # ==================================
  num.pts = 200
  full.circle = seq(0, 2*pi, length.out = num.pts)
  
  FT_circle_x = 6*cos(full.circle) + 25
  FT_circle_y = 6*sin(full.circle) + 19
  FT_circle_ID = rep('FT_circle_detail', num.pts)
  
  # B.3. Combine FT Coordinates
  # ==================================
  FT_x = c(FT.perimeter_x, left.inside_x, right.inside_x, FT_circle_x)
  FT_Y = c(FT.perimeter_y, left.inside_y, right.inside_y, FT_circle_y)
  FT_ID = c(FT.perimeter_ID, left.inside_ID, right.inside_ID, FT_circle_ID)
  
  
  # C.1. Court Perimeter 
  # ==================================
  court.perimeter_x = c(0, 50, 50, 50, 50, 0, 0, 0)
  court.perimeter_y = c(0, 0, 0, 47, 47, 47, 47, 0)
  court.perimeter_ID = rep(c('court_perimeter_B',
                             'court_perimeter_R',
                             'court_perimeter_T',
                             'court_perimeter_L'), each = 2) 
  
  # C.2. Court Detail
  # ==================================
  court.detail_x = c(0, 3, 47, 50)
  court.detail_y = c(28, 28, 28, 28)
  court.detail_ID = rep(c('detail_left', 'detail_right'), each = 2)
  
  baseline.detail_x = c(14, 14, 36, 36)
  baseline.detail_y = c(0, 1, 0, 1)
  baseline.detail_ID = rep(c('baseline_left', 'baseline_right'), each = 2)
  
  # C.3. Center Court Circles
  # ==================================
  num.pts = 200
  half.circle = seq(pi, 2*pi, length.out = num.pts)
  
  circle_x = c(6*cos(half.circle) + 25, 2*cos(half.circle) + 25)
  circle_y = c(6*sin(half.circle) + 47, 2*sin(half.circle) + 47)
  circle_ID = rep(c('inner_circle_detail', 'outer_circle_detail'), each = num.pts)
  
  # C.4. Combine Detail Coordinates
  # ==================================
  detail_x = c(court.perimeter_x, court.detail_x, circle_x)
  detail_y = c(court.perimeter_y, court.detail_y, circle_y)
  detail_ID  =c(court.perimeter_ID, court.detail_ID, circle_ID)
  
  
  # D. Final Combnation of everything
  # ==================================
  x_coordinates = c(three_point_x, FT_x, detail_x, baseline.detail_x)
  y_coordinates = c(three_point_y, FT_Y, detail_y, baseline.detail_y)
  ID = c(three_point_ID, FT_ID, detail_ID, baseline.detail_ID)
  
  court_coordinates = data.frame(x = x_coordinates,
                                 y = y_coordinates, 
                                 group_ID = ID)
  
  # E. Filter out baseline if its not needed
  # ==============================================
  if(hex_chart) {
    court_coordinates <- court_coordinates %>%
      # , 'detial_right', 'detail_left'
      filter(!(group_ID %in% c('court_perimeter_T', 'court_perimeter_R', 'court_perimeter_L', 'detail_left', 'detail_right', 'outer_circle_detail', 'inner_circle_detail')))
    
  }
  
  # return dataframe of coordinates
  return(court_coordinates)
}

# 1B. Backboard Line Coordinates
drawBackboard_Basket_ggplot = function(){
  
  # A.1. Draw the Backboard
  # ==================================
  backboard_x = c(22, 28)
  backboard_y = c(4, 4)
  backboard_ID = rep('backboard', length(backboard_x))
  
  # A.2. Draw the Basket
  # ==================================
  basket.theta = seq(0, 2*pi, length.out = 100)
  
  basket_x = 0.75*cos(basket.theta) + 25
  basket_y = 0.75*sin(basket.theta) + 4.75
  basket_ID = rep('basket', length(basket.theta))
  
  
  # B.1. Draw the Basket
  # ==================================  
  x_coordinates = c(backboard_x, basket_x)
  y_coordinates = c(backboard_y, basket_y)
  ID = c(backboard_ID, basket_ID)
  
  court_coordinates = data.frame(x = x_coordinates,
                                 y = y_coordinates, 
                                 group_ID = ID)
  return(court_coordinates)
}

# 1C. Hardwood Floor Shapes - hex_chart and marker_chart
createFloorShapes = function(hex_chart = FALSE) {
  
  color_options <- c('rgb(248,205,144)', 'rgb(231,187,128)', 'rgb(244,197,137)', 
                     'rgb(231,189,127)', 'rgb(247,201,133', 'rgb(248,206,144)')
  lastcolor <- 'blue'
  floor_panels <- list()
  for(i in -4:54) {
    
    acolor = c(sample(235:248,1),sample(190:205,1), sample(130:142,1))
    acolor = paste0('rgb(', acolor[1], ',', acolor[2], ',', acolor[3], ')')
    
    while(acolor == lastcolor) {
      acolor = c(sample(235:248,1),sample(190:205,1), sample(130:142,1))
      acolor = paste0('rgb(', acolor[1], ',', acolor[2], ',', acolor[3], ')')
    }
    
    this_panel = list(type = "rect",
         fillcolor = sample(color_options, 1), line = list(color = "rgb(160,160,160)", width = 0.25), 
         opacity = 1, layer = 'below',
         x0 = i, x1 = i+1, xref = "x",
         y0 = -4, y1 = 51, yref = "y")
    
    floor_panels[[length(floor_panels)+1]] <- this_panel
  }

  if(hex_chart) {
    floor_panels <- list(type = 'rect', fillcolor = '#182749',
                         layer = 'below', opacity = 1,
                         x0 = -4, x1 = 54, y0 = -4, y1 = 54)
  }
  
  return(floor_panels)
}

# 1D. the createZones function, used in the two functions below
createZones <- function(xvals, yvals, sdists) {
  zonenumbers = rep(0, length(xvals))
  zonenames = rep("", length(xvals))
  
  zonenumbers[which(xvals <= 3 & yvals >= 33)] <- 1
  zonenames[which(xvals <= 3 & yvals >= 33)] <- 'Left Corner 3'
  
  zonenumbers[which(xvals >= 47 & yvals >= 33)] <- 2
  zonenames[which(xvals >= 47 & yvals >= 33)] <- 'Right Corner 3'
  
  zonenumbers[which(xvals < 15  & yvals < 33 & sdists >= 23.75)] <- 3
  zonenames[which(xvals < 15  & yvals < 33 & sdists >= 23.75)] <- 'Left Wing 3'
  
  zonenumbers[which(xvals > 35 & yvals < 33 & sdists >= 23.75)] <- 4
  zonenames[which(xvals > 35 & yvals < 33 & sdists >= 23.75)] <- 'Right Wing 3'
  
  zonenumbers[which(xvals >= 15 & xvals <= 35 & sdists >= 23.75)] <- 5
  zonenames[which(xvals >= 15 & xvals <= 35 & sdists >= 23.75)] <- 'Top of Key 3'
  
  zonenumbers[which(xvals > 3 & xvals <= 17 & yvals >= 33)] <- 6
  zonenames[which(xvals > 3 & xvals <= 17 & yvals >= 33)] <- 'Left Baseline 2'
  
  zonenumbers[which(xvals >= 33 & xvals < 47 & yvals >= 33)] <- 7
  zonenames[which(xvals >= 33 & xvals < 47 & yvals >= 33)] <- 'Right Baseline 2'
  
  zonenumbers[which(xvals > 3 & xvals <= 17 & yvals < 33 & sdists < 23.75)] <- 8
  zonenames[which(xvals > 3 & xvals <= 17 & yvals < 33 & sdists < 23.75)] <- 'Left Elbow 2'
  
  zonenumbers[which(xvals >= 33 & xvals < 47 & yvals < 33 & sdists < 23.75)] <- 9
  zonenames[which(xvals >= 33 & xvals < 47 & yvals < 33 & sdists < 23.75)] <- 'Right Elbow 2'
  
  zonenumbers[which(xvals > 17 & xvals < 33 & yvals < 28 & sdists < 23.75)] <- 10
  zonenames[which(xvals > 17 & xvals < 33 & yvals < 28 & sdists < 23.75)] <- 'Top of Key 2'
  
  zonenumbers[which(xvals > 17 & xvals < 33 & yvals >= 28)] <- 11
  zonenames[which(xvals > 17 & xvals < 33 & yvals >= 28)] <- 'In The Paint'
  
  return(data.frame(zonenumber = zonenumbers, zonename = zonenames, stringsAsFactors = FALSE))
}

# 1E. Create the player.shotdist plottable dataframe
createHexPlotDF <- function(mydf, this_team = NA, this_fullname = NA, plotheight, divisor = 31) { 
  
  # set dimensions for plot, add (again) hoop spots
  hoop_center_x <- 25
  hoop_center_y <- 47 - (4 + 0.5 + 0.75)
  
  
  # A. compute MarkerSize, add ZoneNumber and ZoneName 
  # ====================================================
  # compute percentage of shots taken at each marker, across the league
  league.marker.df <- mydf %>% 
    mutate(totalshots = nrow(.)) %>%
    group_by(xmapped, ymapped) %>% 
    dplyr::summarize(totalshots = min(totalshots),
                     LFGAxy = n()) %>%
    mutate(PctOfLeagueShots = LFGAxy / totalshots) %>%
    dplyr::select(-one_of(c('totalshots', 'LFGAxy')))
    
  # initialize dataframe that gets plotted with MarkerSize added 
  playerteam.marker.df <- mydf %>%
    # filter by team, and compute teams total number of shots
    {if(!is.na(this_team)) filter(., team == this_team) else filter(., fullname == this_fullname) } %>% 
    mutate(totalshots = nrow(.)) %>%
    # group by unique marker locations, compute shots / % of shots at each marker
    group_by(xmapped, ymapped) %>% 
    dplyr::summarize(totalshots = min(totalshots),
                     PFGAxy = n()) %>%
    mutate(PctOfPlayerShots = PFGAxy / totalshots) %>%
    as.data.frame() %>% 
    # add the league percentage of all shots
    left_join(league.marker.df, by = c('xmapped'='xmapped', 'ymapped'='ymapped')) %>%
    mutate(MarkerSize = round(ifelse(PctOfPlayerShots > 1.35*PctOfLeagueShots, 
                              plotheight/divisor, 
                              (plotheight/divisor)*(PctOfPlayerShots/(1.35*PctOfLeagueShots))), digits = 2)) %>%
    # attach zonenames, which will be needed later
    mutate(ShotDist = round(sqrt((xmapped - hoop_center_x)^2 + (ymapped - hoop_center_y)^2), digits = 2)) %>%
    mutate(ZoneNumber = createZones(xmapped, ymapped, ShotDist)$zonenumber,
           ZoneName = createZones(xmapped, ymapped, ShotDist)$zonename) %>%
    # remove excess columns
    dplyr::select(-one_of(c('totalshots', 'PFGAxy', 'PctOfPlayerShots', 'PctOfLeagueShots', 'ShotDist')))
  # ====
  
  
  # B. compute this_teams ranking in each zone (maybe this_players?)
  # =============================================
  leaguebyplayerteam.zones.df <- mydf %>% 
    {if(!is.na(this_team)) group_by(., zonenumber, team) else group_by(., zonenumber, fullname) } %>% 
    dplyr::summarize(LFGA = n(),
                     LFGM = sum(outcome == 'SCORED')) %>%
    mutate(LFGPct = round(LFGM/LFGA, 3)) 

  # compute threshold for top ~200 players at each zone (top 300 for the paint)
  if(is.na(this_team)) {
    zone.thresholds <- leaguebyplayerteam.zones.df %>%
                         group_by(zonenumber) %>%
                         dplyr::summarize(top200thplayer = sort(LFGA, TRUE)[200])
    zone.11.LFGAs <- sort(leaguebyplayerteam.zones.df$LFGA[leaguebyplayerteam.zones.df$zonenumber == 11])
    zone.11.thresh <- sort(zone.11.LFGAs, TRUE)[300]
    zone.thresholds$top200thplayer[zone.thresholds$zonenumber == 11] = zone.11.thresh
    leaguebyplayerteam.zones.df <- leaguebyplayerteam.zones.df %>%
                                     left_join(zone.thresholds, by = c('zonenumber'='zonenumber')) %>%
                                     filter(LFGA >= top200thplayer)
  }
  
  # account for team vs player rankings, player FGA thresholds by zone, etc.
  playerteamzonerank.df <- data.frame(zonenumber = 1:11, ZoneRank = rep(0, 11), ZoneTotalCt = rep(0, 11))
  for(this_zone in 1:11) {
    if(!is.na(this_team)) {
      thisteams.zonepct = leaguebyplayerteam.zones.df$LFGPct[leaguebyplayerteam.zones.df$zonenumber == this_zone & leaguebyplayerteam.zones.df$team == this_team]
      eachteams.zonepct = leaguebyplayerteam.zones.df$LFGPct[leaguebyplayerteam.zones.df$zonenumber == this_zone & leaguebyplayerteam.zones.df$team != this_team]
      
      thisteams.zonerank = sum(thisteams.zonepct < eachteams.zonepct) + 1
      playerteamzonerank.df$ZoneRank[this_zone] = thisteams.zonerank
    } else {
      thisplayers.zonepct = leaguebyplayerteam.zones.df$LFGPct[leaguebyplayerteam.zones.df$zonenumber == this_zone & 
                                                                 leaguebyplayerteam.zones.df$fullname == this_fullname]
      eachplayers.zonepct = leaguebyplayerteam.zones.df$LFGPct[leaguebyplayerteam.zones.df$zonenumber == this_zone & 
                                                                 leaguebyplayerteam.zones.df$fullname != this_fullname]
                                                               
      # have to handle players who dont have qualifying FGA in a zone
      if(length(thisplayers.zonepct) == 0) {
        thisplayers.zonerank = 9999
      } else {
        thisplayers.zonerank = sum(thisplayers.zonepct < eachplayers.zonepct) + 1
      }
      
      thiszone.qualifiedcount = length(eachplayers.zonepct) + 1
      playerteamzonerank.df$ZoneRank[this_zone] = thisplayers.zonerank
      playerteamzonerank.df$ZoneTotalCt[this_zone] = thiszone.qualifiedcount
    }
  }
  # ====
  
  
  # C. computing the MarkerColor parameter, and zone shooting numbers
  # ===================================================================
  # league average shooting by zone dataframe
  league.zone.df <- mydf %>% 
    group_by(zonenumber) %>%
    dplyr::summarize(LFGA = n(),
                     LFGM = sum(outcome == 'SCORED')) %>%
    mutate(LFGPct = round(LFGM/LFGA, 3))
  
  # this player/teams shooting by zone, with ranking
  playerteam.zone.df <- mydf %>% 
    # filter by team, and compute teams shots made / attempted / pct by zone
    {if(!is.na(this_team)) filter(., team == this_team) else filter(., fullname == this_fullname) } %>% 
    # filter(team == this_team) %>%
    group_by(zonenumber) %>%
    dplyr::summarize(PFGA = n(),
                     PFGM = sum(outcome == 'SCORED')) %>%
    mutate(PFGPct = round(PFGM/PFGA, 3)) %>% 
    # join leauge averge, and teams ranking, zone shooting numbers
    left_join(league.zone.df, by = c('zonenumber'='zonenumber')) %>%
    left_join(playerteamzonerank.df, by = c('zonenumber'='zonenumber'))
  
  # set marker color based on ranking
  if(!is.na(this_team)) {
    playerteam.zone.df$MarkerColor <- cut(playerteamzonerank.df$ZoneRank, 
                                          breaks = c(0,6.5,12.5,18.5,24.5,30.5), 
                                          labels = c('Top 20%', '20% to 40%', '40% to 60%', '60% to 80%', 'Bot 20%'))
  } else {
    PctRanks <- c()
    for(i in 1:nrow(playerteam.zone.df)) {
      if(playerteam.zone.df$ZoneRank[i] == 9999) {
        this.pct <- 1.2
      } else {
        this.pct <- playerteam.zone.df$ZoneRank[i] / playerteam.zone.df$ZoneTotalCt[i]
      }
      PctRanks <- c(PctRanks, this.pct)
    }
    playerteam.zone.df$MarkerColor <- cut(PctRanks, 
                                          breaks = c(-0.01, 0.2, 0.4, 0.6, 0.8, 1, 1.25),
                                          labels = c('Top 20%', '20% to 40%', '40% to 60%', '60% to 80%', 'Bot 20%', 'Did Not Qualify on Zone FGA'))
  } 
  # ====  
  
  
  # D. bring together, combine playerteam.marker.df with playerteam.zone.df
  # ==========================================================================
  playerteam.marker.df <- playerteam.marker.df %>% 
    left_join(playerteam.zone.df, by = c('ZoneNumber'='zonenumber'))
  
  return(playerteam.marker.df)
  # ====
}

# 1F. Plot the Heat Graph 
plotHexChart <- function(player.heatmap, this_fullname = NA, this_team = NA, plot_height, plot_width) {
  
  # 0. call earlier functions to initialize some data
  # ====================================================
  court.data2 <- drawCourt_ggplot(hex_chart = TRUE) %>% group_by(group_ID)
  hoop.data <- drawBackboard_Basket_ggplot() %>% group_by(group_ID)
  floor.shapes2 <- createFloorShapes(hex_chart = TRUE)
  
  m.size <- max(player.heatmap$MarkerSize)
  legend.data <- data.frame(x = c(2,3.5,5,6.5,8,16.6,17.85), y = c(0,0,0,0,0,0,0), 
                            msize = c(rep(m.size, 5), m.size/1.8125, m.size),
                            hovtext = c('Bot 20%', '20% - 40%', '40% - 60%', '60% - 80%', 'Top 20%', 'Not a lot', 'A lot'), 
                            stringsAsFactors = FALSE)
  
  # set the name that is passed into the title of the graph 
  this_title_name <- ifelse(is.na(this_team), this_fullname, this_team)
  this_hover_name <- ifelse(is.na(this_team), this_fullname, this_team)
  # ====
  
  
  # 1. create annotations, axes, shapes, etc.
  # ===========================================
  ax <- list(title = "",
             showticklabels = FALSE,
             color = 'rgb(100, 100, 100)',
             showgrid = FALSE,
             zeroline = FALSE,
             fixedrange = TRUE)
  
  ay <- list(title = "",
             showticklabels = FALSE,
             color = 'rgb(100, 100, 100)',
             showgrid = FALSE,
             zeroline = FALSE,
             fixedrange = TRUE)
  
  # my_annotations <- list(text = paste0("<b>", this_title_name, ' Season Heat Chart</b>'),
  #                        showarrow = FALSE, x = 25, y = 51,
  #                        font = list(color = 'white', size = m.size))
  
  my_annotations <- list(list(text = "<b>Efficiency By Location",
                              showarrow = FALSE, x = 5, y = 2,
                              font = list(color = 'white', size = m.size/2)),
                         list(text = 'Really\nBad',
                              showarrow = FALSE, x = -0.25, y = 0,
                              font = list(color = 'white', size = m.size/2)),
                         list(text = 'Really\nGood',
                              showarrow = FALSE, x = 10.3, y = 0,
                              font = list(color = 'white', size = m.size/2)),
                         list(text = "<b>Frequency",
                              showarrow = FALSE, x = 17.5, y = 2,
                              font = list(color = 'white', size = m.size/2)),
                         list(text = 'Low', showarrow = FALSE, x = 15, y = 0,
                              font = list(color = 'white', size = m.size/2)),
                         list(text = 'High', showarrow = FALSE, x = 20, y = 0,
                              font = list(color = 'white', size = m.size/2)),
                         list(text = paste0("<b>", this_title_name, ' Season Heat Chart</b>'),
                              showarrow = FALSE, x = 25, y = 51,
                              font = list(color = 'white', size = m.size)),
                         list(text = paste0("<b>By: @SmokeyCanova</b>"),
                              showarrow = FALSE, x = 47, y = 2,
                              font = list(color = 'white', size = m.size/2)),
                         list(text = paste0("<b>Data: MySportsFeeds</b>"),
                              showarrow = FALSE, x = 47.1, y = 0.5,
                              font = list(color = 'white', size = m.size/2)))
  
  my_shapes <- list(floor.shapes2, 
                    list(type = 'line', layer = 'above', 
                         line = list(color = 'white'),
                         x0 = -3, x1 = 53, y0 = 4, y1 = 4),
                    list(type = 'line', layer = 'above',
                         line = list(color = 'white'),
                         x0 = 13, x1 = 13, y0 = -3, y1 = 3))
  
  # colors for all markers and legend markers 
  legend.colors <- c('#1147FF', '#86D8FF', '#FFEF67', '#FF7D11', '#F30000', '#FF7D11', '#FF7D11')
  marker.colors <- c('#F30000', '#FF7D11', '#FFEF67', '#86D8FF', '#1147FF')
  
  # add column to dataframe for bottom row in the hover
  player.heatmap$hover.rankrow <- paste('Team ranking in zone:', player.heatmap$ZoneRank)  
  # ====
  
  
  # 2. add a few tweaks for the player graph
  if(is.na(this_team)) {
    # tweak legend markers
    addtl.legend.marker <- list(x = 24, y = 0, msize = m.size, hovtext = '<5 FGA in Zone')
    legend.data <- rbind(legend.data, addtl.legend.marker)
    
    # add additional legend annotation
    addtl.annotation <- list(text = paste0("Did Not Qualify on Zone \nFGA for Heat Coloring"),
                             showarrow = FALSE, x = 30.5, y = 0,
                             font = list(color = 'white', size = m.size/2))
    my_annotations[[length(my_annotations)+1]] <- addtl.annotation
    
    # add additional vertical white line
    addtl.line <- list(type = 'line', layer = 'above',
                       line = list(color = 'white'),
                       x0 = 22, x1 = 22, y0 = -3, y1 = 3)
    my_shapes[[length(my_shapes)+1]] <- addtl.line
    
    # add to list of legend colors
    legend.colors <- c(legend.colors, '#D3D3D3')
    marker.colors <- c(marker.colors, '#D3D3D3')
    
    # fix the hover.lastrow for players
    player.heatmap$hover.rankrow[player.heatmap$ZoneRank == 9999] <- paste('Did Not Qualify on FGA for Ranking')
    player.heatmap$hover.rankrow[player.heatmap$ZoneRank != 9999] <- paste('Player Rank:', player.heatmap$ZoneRank[player.heatmap$ZoneRank != 9999], 
                                                                           ' / ', player.heatmap$ZoneTotalCt[player.heatmap$ZoneRank != 9999], ' qualified')
  }
  
  
  # 2. draw the plot
  player.heatmap <- player.heatmap %>% filter(ymapped > 5)
  plot_ly(player.heatmap, height = plot_height, width = plot_width) %>%
    
    # levels(plot.atCoordinates$colorval2) (check this for the order of the levels to be correct)
    # draw the hexagon layer
    add_trace(x = ~xmapped, y = ~ymapped, type = 'scatter', mode = 'markers',
              # color = ~colorval2,
              # colors = c('#1147FF', '#86D8FF', '#FFEF67', '#FF7D11', '#F30000'),
              color = ~MarkerColor,
              colors = marker.colors,
              showlegend = FALSE,
              marker = list(symbol = 'hexagon', 
                            size = ~MarkerSize, 
                            opacity = 1),
              hoverinfo = 'text', 
              hovertext = ~paste('Zone: ', ZoneName, '\n', 
                                 this_hover_name, ' in zone: ', PFGPct*100, '% (', PFGM, '/', PFGA, ')', '\n', 
                                 'League in zone: ', LFGPct*100, '% (', LFGM, '/', LFGA, ')', '\n', 
                                 hover.rankrow, sep = '')) %>%

    # draw the legend layer
    add_trace(data = legend.data, x = ~x, y = ~y, type = 'scatter', mode = 'markers',
              showlegend = FALSE,
              marker = list(symbol = 'hexagon', opacity = 1, size = ~msize,
                            color = legend.colors),
              hoverinfo = 'text', 
              text = ~paste(hovtext)) %>%
    
    # draw the court and the backboard/hoop
    add_trace(data = court.data2, x = ~x, y = ~(47-y), type = 'scatter', mode = 'lines',
              color = I('white'), line = list(width = 3), hoverinfo = 'none',
              showlegend = FALSE) %>%
    add_trace(data = hoop.data, x = ~x, y = ~(47-y), type = 'scatter', mode = 'lines',
              color = I('white'), line = list(width = 3), hoverinfo = 'none',
              showlegend = FALSE) %>%
    
    # add the layout
    layout(shapes = my_shapes,
           annotations = my_annotations,
           xaxis = ax, yaxis = ay)
}

# 1G. Plot the Marker Graph
plotMarkerChart <- function(all.pbp, this_fullname = NA, game.label = NA, plot_height, plot_width) {
  
  # call earlier functions to initialize some data
  court.data <- drawCourt_ggplot(hex_chart = FALSE) %>% group_by(group_ID)
  hoop.data <- drawBackboard_Basket_ggplot() %>% group_by(group_ID)
  floor.shapes <- createFloorShapes(hex_chart = FALSE)
  
  # set the name that is passed into the title of the graph
  if(is.na(game.label)) {
    this.title <- paste0(this_fullname, '  Season Shot Chart')
  }
  else {
    gameid.split <- unlist(strsplit(game.label, '-'))
    this.title <- paste0(gameid.split[2], ' vs. ', gameid.split[3], ' Shot Chart  ',
                         substring(gameid.split[1], 5, 6), '-', substring(gameid.split[1], 7, 8), '-',
                         substring(gameid.split[1], 1, 4))
  }
  
  # if a game matchup, compute each team's FGA / FGM, 3PA / 3PM to display in corners
  if(!is.na(game.label)) {
    t1id <- gameid.split[2];
    t12PM <- sum(all.pbp$outcome == 'SCORED' & all.pbp$ptsattempt == 2 & all.pbp$team == t1id)
    t12PA <- sum(all.pbp$ptsattempt == 2 & all.pbp$team == t1id)
    t12PCT <- round(t12PM / t12PA, 3)*100
    t13PM <- sum(all.pbp$outcome == 'SCORED' & all.pbp$ptsattempt == 3 & all.pbp$team == t1id)
    t13PA <- sum(all.pbp$ptsattempt == 3 & all.pbp$team == t1id)
    t13PCT <- round(t13PM / t13PA, 3)*100
    t1annotation <- paste0(t1id, '\n', t12PM, '/', t12PA, ' | ', t12PCT, '% on 2s', '\n', t13PM, '/', t13PA, ' | ', t13PCT, '% on 3s')
    
    t2id <- gameid.split[3];
    t22PM <- sum(all.pbp$outcome == 'SCORED' & all.pbp$ptsattempt == 2 & all.pbp$team == t2id)
    t22PA <- sum(all.pbp$ptsattempt == 2 & all.pbp$team == t2id)
    t22PCT <- round(t22PM / t22PA, 3)*100
    t23PM <- sum(all.pbp$outcome == 'SCORED' & all.pbp$ptsattempt == 3 & all.pbp$team == t2id)
    t23PA <- sum(all.pbp$ptsattempt == 3 & all.pbp$team == t2id)
    t23PCT <- round(t23PM / t23PA, 3)*100
    t2annotation <- paste0(t2id, '\n', t22PM, '/', t22PA, ' | ', t22PCT, '% on 2s', '\n', t23PM, '/', t23PA, ' | ', t23PCT, '% on 3s')
  }
    
  # set axes, annotations, shapes, etc.
  ax <- list(title = "",
             showticklabels = FALSE,
             color = 'rgb(100, 100, 100)',
             showgrid = FALSE,
             zeroline = FALSE)
  
  ay <- list(title = "",
             showticklabels = FALSE,
             color = 'rgb(100, 100, 100)',
             showgrid = FALSE,
             zeroline = FALSE)
  
  myannotations <- list()
  titleannotation <- list(text = paste0('<b>', this.title, '</b>'), 
                          font = list(size = 24, color = 'white'),
                          x = 25, y = 49, 
                          showarrow = FALSE,
                          align = 'center')
  myannotations[[length(myannotations)+1]] <- titleannotation
  if(!is.na(game.label)) {
    score1annotaiton <- list(text = paste0('<b>', t1annotation, '</b>'), 
                             font = list(size = 20, color = 'white'),
                             x = 10, y = 10, 
                             showarrow = FALSE,
                             align = 'center')
    score2annotation <- list(text = paste0('<b>', t2annotation, '</b>'), 
                             font = list(size = 20, color = 'white'),
                             x = 40, y = 10, 
                             showarrow = FALSE,
                             align = 'center')
    myannotations[[length(myannotations)+1]] <- score1annotaiton
    myannotations[[length(myannotations)+1]] <- score2annotation
  }
  
  
  # set size and colors
  all.pbp$colorhex <- ifelse(all.pbp$outcome == "SCORED", '#006400', '#8b0000')
  all.pbp$sizevals <- 16
  
  
  # create plot object
  plot_ly(all.pbp, height = plot_height, width = plot_width) %>%
    
    # draw the shot markers
    add_trace(x = ~xloc, y = ~yloc, type = 'scatter', mode = 'markers',
              marker = list(size = ~sizevals, color = ~colorhex, opacity = 1), 
    hoverinfo = 'text',
    text = ~paste(this_fullname)) %>%
    
    # draw the court and the backboard/hoop
    add_trace(data = court.data, x = ~x, y = ~(47-y), type = 'scatter', mode = 'lines', 
              color = I('white'), line = list(width = 3),
              hoverinfo = 'none') %>%
    add_trace(data = hoop.data, x = ~x, y = ~(47-y), type = 'scatter', mode = 'lines', 
              color = I('white'), line = list(width = 3),
              hoverinfo = 'none') %>%
    
    # add the layout
    layout(xaxis = ax, 
           yaxis = ay, 
           shapes = floor.shapes,
           annotations = myannotations,
           showlegend = FALSE)
}
# ====


# ===================================
# 2. Player XY Charts Application
# ===================================

# 2A. Plot each of the different Player XY Graphs
drawPlayerXYChart <- function(player.stats, chart.type, player.name, player.pos, player.team, color.pal.df, window.width) {
  
  # Filter the dataframe using position input paramters
  if(player.pos == "All Positions") {
    player.stats <- player.stats
  } else if(player.pos == "F") {
    player.stats <- player.stats %>%
      filter(POS %in% c('F', 'SF', 'PF'))
  } else if(player.pos == "G") {
    player.stats <- player.stats %>%
      filter(POS %in% c('G', 'PG', 'SG'))
  } else if(player.pos == "PG") {
    player.stats <- player.stats %>%
      filter(POS %in% c('G', 'PG'))
  } else if(player.pos == "SG") {
    player.stats <- player.stats %>%
      filter(POS %in% c('G', 'SG'))
  } else if(player.pos == "SF") {
    player.stats <- player.stats %>%
      filter(POS %in% c('F', 'SF'))
  } else if(player.pos == "PF") {
    player.stats <- player.stats %>%
      filter(POS %in% c('F', 'PF'))
  } else {
    player.stats <- player.stats %>%
      filter(POS %in% c('C'))
  }
  
  # highlight and slightly increase size based on team parameter
  player.stats$colorhex[player.stats$Team != player.team] <- '#d3d3d3'
  player.stats$MarkerSize[player.stats$Team == player.team] <- 20
  
  # player parameter
  if(player.name != "All Players") {
    
    # increase size based on player selected
    # fname <- strsplit(player.name, split = ' ')[[1]][1]
    # lname <- strsplit(player.name, split = ' ')[[1]][2]
    player.stats$MarkerSize[player.stats$fullName == player.name] <- 30
    
    # reset color for player selected, if not on team selected
    players_team <- player.stats$Team[player.stats$fullName == player.name]
    players_color <- color.pal.df$colorhex[color.pal.df$teamname == players_team]
    player.stats$colorhex[player.stats$fullName == player.name] <- players_color
  }
  
  # create axes, annotations, etc.
  ax <- list(title = "",
             tickfont = list(size = window.width/60),
             titlefont = list(size = window.width/60),
             color = 'rgb(100, 100, 100)',
             showgrid = TRUE,
             zeroline = FALSE)
  
  ay <- list(title = "",
             tickfont = list(size = window.width/60),
             titlefont = list(size = window.width/60),
             color = 'rgb(100, 100, 100)',
             showgrid = TRUE,
             zeroline = FALSE)
  
  mytitlesize <- list(size = window.width/44, 
                      color = 'rgb(100, 100, 100)')
  
  # adjust marker size for window width
  player.stats$MarkerSize <- player.stats$MarkerSize * (window.width/1000)
  
  # set chart-specific values (titles, column names)
  if(chart.type == 'Offensive Efficiency') {
    ax$title <- "PTS / 36 MINS"
    ay$title <- "True Shooting %"
    xcol <- pull(player.stats, PTS36)
    ycol <- pull(player.stats, TSPCT)
    
    my.title <- '<b>Offensive Efficiency - True Shooting % vs. Points / 36 Mins</b>'
    my.hover <- paste(player.stats$fullName, '\n', 'Pts/36:', xcol, '\n', 'TS%:', ycol)
  } 
  else if(chart.type == 'Ball Control') {
    ax$title = "TOV / 36 MINS"
    ay$title = "AST / 36 MINS"
    xcol <- pull(player.stats, TOV36)
    ycol <- pull(player.stats, AST36)
    
    my.title <- '<b>Ball Control - Assists / 36 Mins vs. TOs / 36 Mins</b>'
    my.hover <- paste(player.stats$fullName, '\n', 'Tov/36', xcol, '\n', 'Ast/36:', ycol)
  } 
  else if(chart.type == "Blocks vs. Steals") {
    ax$title = "STL / 36 MINS"
    ay$title = "BLK / 36 MINS"
    xcol <- pull(player.stats, STL36)
    ycol <- pull(player.stats, BLK36)
    
    my.title <- '<b>Defensive Box Score Stats - Blocks / 36 Mins vs. Steals / 36 Mins</b>'
    my.hover <- paste(player.stats$fullName, '\n', 'Stl/36', xcol, '\n', 'Blk/36:', ycol)
  } 
  else {
    ax$title = "(REB + AST + STL + BLK) / 36 MINS"
    ay$title = "PTS / 36 MINS"
    xcol <- pull(player.stats, OTHER36)
    ycol <- pull(player.stats, PTS36)
    
    my.title <- '<b>Doing It All On The Court - Points / 36 Mins vs. Other / 36 Mins</b>'
    my.hover <- paste(player.stats$fullName, '\n', 'Other/36', xcol, '\n', 'Pts/36:', ycol)
  }
  
  # annotation for smokeycanova
  xval <- max(xcol) - 0.04*(max(xcol)-min(xcol))
  yval <- min(ycol) - 0.02*(max(ycol)-min(ycol))
  yval2 <- min(ycol) - 0.06*(max(ycol)-min(ycol))
  
  my_annotations <- list(list(text = paste0("<b>By: @SmokeyCanova</b>"),
                              showarrow = FALSE, x = xval, y = yval,
                              font = list(color = 'rgb(100,100,100)', size = window.width/80)),
                         list(text = paste0("<b>Data: MySportsFeeds</b>"),
                              showarrow = FALSE, x = xval, y = yval2,
                              font = list(color = 'rgb(100,100,100)', size = window.width/80)))
  
  # create the plot
  lm.model = lm(ycol ~ xcol)
  plot_ly(player.stats, height = 0.67*window.width) %>%
    add_trace(x = xcol, y = fitted(lm.model), type = 'scatter', mode = "lines",
              color = I('black')) %>%
    add_trace(x = xcol, y = ycol,
              type = 'scatter', mode = 'markers', 
              marker = list(size = ~MarkerSize, opacity = 0.8,
                            color = ~colorhex,
                            line = list(color = 'rgba(50, 50, 50, .8)',
                                        width = 1)),
              hoverinfo = 'text',
              text = my.hover) %>%
    layout(title = my.title,
           margin = list(t = 75, b = 75, l = 75),
           titlefont = mytitlesize,
           xaxis = ax, yaxis = ay,
           annotations = my_annotations,
           showlegend = FALSE)
}
# ====


# ===================================
# 3. Team XY Charts Application
# ===================================

# 3A. Plot each of the different Team XY Graphs
drawTeamXYChart <- function(team.stats, chart.type, window.width, xaxis.id = NA, yaxis.id = NA, xaxis.fa = NA, yaxis.fa = NA) {
  
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
  
  # set the stat and foragainst in order to filter team.stats
  if(chart.type == 'Scoring Margin') {
    x.stat = y.stat = 'PTS'; 
    x.fa = 'for'; y.fa = 'against';
  } else if(chart.type == 'Field Goal Percentages') {
    x.stat = y.stat = 'FGPCT';
    x.fa = 'for'; y.fa = 'against';
  } else if(chart.type == '3 Point Offense') {
    x.stat = 'X3PA'; y.stat = 'X3PCT'; 
    x.fa = 'for'; y.fa = 'for';
  } else if(chart.type == '3 Point Defense') {
    x.stat = 'X3PA'; y.stat = 'X3PCT';
    x.fa = 'against'; y.fa = 'against';
  } else if(chart.type == 'Blocks and Steals') {
    x.stat = 'STL'; y.stat = 'BLK'; 
    x.fa = 'for'; y.fa = 'for';
  } else {
    
    # set the req() functions here, since only here is when they're needed
    x.stat = xaxis.id; x.fa = xaxis.fa 
    y.stat = yaxis.id; y.fa = yaxis.fa
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
  
  xmid <- mean(team.stats$value[team.stats$foragainst == x.fa & team.stats$stat == x.stat])
  ymid <- mean(team.stats$value[team.stats$foragainst == y.fa & team.stats$stat == y.stat])
  xmin <- corner.df$xvals[1] - 0.05*(corner.df$xvals[2]-corner.df$xvals[1])
  xmax <- corner.df$xvals[2] + 0.05*(corner.df$xvals[2]-corner.df$xvals[1])
  ymin <- corner.df$yvals[1] - 0.05*(corner.df$yvals[2]-corner.df$yvals[1])
  ymax <- corner.df$yvals[2] + 0.05*(corner.df$yvals[2]-corner.df$yvals[1])
  
  myshapes <- list(
    list(type = 'line', layer = 'below',
          x0 = xmid, x1 = xmid, y0 = ymin, y1 = ymax,
          line = list(width = 2, dash = 'longdash', color = 'rgb(100, 100, 100')),
    list(type = 'line', layer = 'below',
          x0 = xmin, x1 = xmax, y0 = ymid, y1 = ymid,
          line = list(width = 2, dash = 'longdash', color = 'rgb(100, 100, 100'))
  )
  
  ax <- list(title = paste('Team', x.stat, x.fa, 'per game', sep = ' '),
             titlefont = list(size = window.width/48),
             tickfont = list(size = window.width/60),
             color = 'rgb(100, 100, 100)',
             showgrid = FALSE,
             fixedrange = TRUE,
             zeroline = FALSE)
  
  ay <- list(title = paste('Team', y.stat, y.fa, 'per game', sep = ' '),
             titlefont = list(size = window.width/48),
             tickfont = list(size = window.width/60),
             color = 'rgb(100, 100, 100)',
             showgrid = FALSE,
             fixedrange = TRUE,
             zeroline = FALSE)
  
  mytitle <- ifelse(chart.type == 'Create Your Own Chart', 
                    paste('<b>\n', x.stat, x.fa, 'vs.', y.stat, y.fa, '</b>'), 
                    paste('<b>\n', chart.type, '</b>'))
  
  mytitlefont <- list(size = window.width/40, 
                      color = 'rgb(100, 100, 100)')
  
  xval <- corner.df$xvals[2] - 0.04*(corner.df$xvals[2] - corner.df$xvals[1])
  yval <- corner.df$yvals[1] - 0.02*(corner.df$yvals[2] - corner.df$yvals[1])
  yval2 <- corner.df$yvals[1] - 0.06*(corner.df$yvals[2] - corner.df$yvals[1])
  
  my_annotations <- list(list(text = paste0("<b>By: @SmokeyCanova</b>"),
                              showarrow = FALSE, x = xval, y = yval,
                              font = list(color = 'rgb(100,100,100)', size = window.width/80)),
                         list(text = paste0("<b>Data: MySportsFeeds</b>"),
                              showarrow = FALSE, x = xval, y = yval2,
                              font = list(color = 'rgb(100,100,100)', size = window.width/80)))
  
  # and finally the plot
  plot_ly(corner.df, height = 0.67*window.width) %>%
    add_trace(x = ~xvals, y = ~yvals, type = 'scatter', mode = 'markers',
              marker = list(size = 1)) %>%
    layout(
      title = mytitle,
      titlefont = mytitlefont,
      xaxis = ax, yaxis = ay,
      margin = list(pad = 5, l = 100, b = 70, t = 50),
      shapes = myshapes,
      annotations = my_annotations,
      images = images.list
    )
}
# ====


# ===================================
# 4. Game Recaps Application
# ===================================

# 4A. Plot the in game win prob graphs
drawGameRecapIGWPChart <- function(gamerecaps.all, this.msfid, color.pal.df, window.width) {
  
  nba.game <- gamerecaps.all[gamerecaps.all$msfgameid == this.msfid, ]
  
  # set colors, axes, shapes, annotations, legend, etc.
  # =============================================
  home_ID <- unique(nba.game$hID)
  home_color <- color.pal.df$colorhex[color.pal.df$teamid == home_ID]
  away_ID <- unique(nba.game$aID)
  away_color <- color.pal.df$colorhex[color.pal.df$teamid == away_ID]
  
  ax <- list(title = "",
             tickfont = list(size = window.width/60),
             color = 'rgb(100, 100, 100)',
             showgrid = FALSE,
             tickmode = "array",
             tickvals = c(40, 720, 1440, 2160, 2800),
             ticktext = c('<b>Start\nGame</b>', '<b>Q1</b>', '<b>Half</b>', '<b>Q3</b>', '<b>End\nGame</b>'),
             zeroline = FALSE)
  
  ay <- list(title = "",
             tickfont = list(size = window.width/60),
             color = 'rgb(100, 100, 100)',
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
                        # font = list(size = 24, color = 'rgb(50, 50, 50)'),
                        font = list(size = window.width/48, color = 'rgb(100, 100, 100)'),
                        x = max(nba.game$secsRemaining) / 2,
                        y = 1.12,
                        showarrow = FALSE)
  
  mylegend <- list(orientation = 'v',
                   # font = list(size = 14),
                   font = list(size = window.width / 100),
                   x = 0.02,
                   y = 0.85)
  # ====
  
  # create plot_ly win prob object
  # ==================================
  totalSecs <- max(nba.game$secsRemaining)
  nba.game <- nba.game %>% 
    mutate(secsIntoGame = totalSecs - secsRemaining,
    winProb2 = 1 - winProb)
  
  # plot_ly(height = 600, width = 1200) %>%
  plot_ly(nba.game, height = 0.5*window.width) %>%
  
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
         legend = mylegend,
         margin = list(b = 75, l = 80))
  # ====
  
}

# 4B. Plot the lead bars graph
drawGameRecapLeadBarsChart <- function(gamerecaps.all, this.msfid, color.pal.df, window.width) {
  
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
  times_tied <- ifelse(times_tied < 10, paste0('0',times_tied), times_tied)
  lead_changes <- sum(diff(sign(hL[hL != 0])) != 0)
  lead_changes <- ifelse(lead_changes < 10, paste0('0',lead_changes), lead_changes)
  longest_run <- compute_longest_run(hL)
  longest_run <- ifelse(longest_run < 10, paste0('0',longest_run), longest_run)
  # ====
  
  # # set colors, axes, shapes, annotations, legend, etc.
  # =======================================================
  home_ID2 <- unique(nba.game$hID)
  home_color2 <- color.pal.df$colorhex[color.pal.df$teamid == home_ID2]
  away_ID2 <- unique(nba.game$aID)
  away_color2 <- color.pal.df$colorhex[color.pal.df$teamid == away_ID2]
  
  ax2 <- list(title = "",
              color = 'rgb(100, 100, 100)',
              showgrid = FALSE,
              tickmode = "array",
              tickvals = c(40, 720, 1440, 2160, 2800),
              ticktext = c('<b>Start\nGame</b>', '<b>Q1</b>', '<b>Half</b>', '<b>Q3</b>', '<b>End\nGame</b>'),
              tickfont = list(size = window.width/60),
              zeroline = FALSE)
  
  ay2 <- list(title = "",
              color = 'rgb(100, 100, 100)',
              showgrid = FALSE,
              tickmode = "array",
              tickvals = yseq,
              ticksuffix = "  ",
              ticktext = paste('<b>', abs(yseq), '</b>', sep = ''),
              tickfont = list(size = window.width/60),
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
  
  myshapes2[[length(myshapes2)+1]] <- list(type = 'line', x0 = maxSecs / 8, x1 = maxSecs / 1.15, y0 = ymax + 15, y1 = ymax + 15,
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
      ax2$tickvals <- c(ax2$tickvals, max(ax2$tickvals)+300)
      ax2$ticktext <- c(ax2$ticktext, paste0('<b>End\nOT',ot.num,'</b>'))
      
      # fix shapes - add new vertical line
      xval <- 2880 + (300*(ot.num-1))
      newOTline <- list(type = 'line', x0 = xval, x1 = xval, y0 = -ymax, y1 = ymax,
                        line = list(width = 1, dash = 'dash', color = 'rgb(150, 150, 150)'))
      myshapes2[[length(myshapes2)+1]] <- newOTline
    }
  }
  # ====
  
  # annotations - all of the text on the page
  hMaxLead <- ifelse(max(nba.game$homeLead) < 10, paste0('0',max(nba.game$homeLead)), max(nba.game$homeLead))
  aMaxLead <- ifelse(abs(min(nba.game$homeLead)) < 10, paste0('0',abs(min(nba.game$homeLead))), abs(min(nba.game$homeLead)))
  
  myannotations2 <- list(
    # maxSecs / 1020
    
    # biggest lead text and values
    list(text = paste0('<b>',hMaxLead,'</b>'), x = maxSecs / 2.82, y = ymax + 22,
         font = list(size = window.width/14, color = home_color2),
         showarrow = FALSE),
    list(text = paste0('<b>',aMaxLead,'</b>'), x = maxSecs / 1.55, y = ymax + 22,
         font = list(size = window.width/14, color = away_color2),
         showarrow = FALSE),
    list(text = '<b>Largest\nLead</b>', x = maxSecs / 2, y = ymax + 22,
         font = list(size = window.width/35, color = 'rgb(100,100,100)'),
         showarrow = FALSE),
    
    # other three stat labels
    list(text = '<b>Times\nTied</b>', x = maxSecs / 5.54, y = ymax + 9,
         font = list(size = window.width/42, color = 'rgb(100,100,100)'),
         showarrow = FALSE),
    list(text = '<b>Longest\nRun</b>', x = maxSecs / 2.20, y = ymax + 9,
         font = list(size = window.width/42, color = 'rgb(100,100,100)'),
         showarrow = FALSE),
    list(text = '<b>Lead\nChanges</b>', x = maxSecs / 1.37, y = ymax + 9,
         font = list(size = window.width/42, color = 'rgb(100,100,100)'),
         showarrow = FALSE),
    
    # three stat values
    list(text = paste0('<b>', times_tied, '</b>'), x = maxSecs / 3.69, y = ymax + 9,
         font = list(size = window.width/21, color = 'rgb(100,100,100)'),
         showarrow = FALSE),
    list(text = paste0('<b>', longest_run, '</b>'), x = maxSecs / 1.80, y = ymax + 9,
         font = list(size = window.width/21, color = 'rgb(100,100,100)'),
         showarrow = FALSE),
    list(text = paste0('<b>', lead_changes, '</b>'), x = maxSecs / 1.20, y = ymax + 9,
         font = list(size = window.width/21, color = 'rgb(100,100,100)'),
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
    plot_ly(height = 0.57*window.width) %>%
    
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
           legend = mylegend2,
           margin = list(b = 50, l = 75))
}
# ====


# ==================================
# 5. ELO Ratings Application
# ==================================

# 5A. The ELO Ratings chart
drawELORatings <- function(nba.elo, this.id, color.pal.df, window.width) {
  
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
             ticksuffix = " ",
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
  
  # grab team id, color
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
  
  plot_ly(height = 0.67*window.width) %>%
    
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
              line = list(color = this.color, width = 3),
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
}
# ====


# ===================================
# 6. Assist Networks Application
# ===================================

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
drawAssistNetwork <- function(assist.pbp, team.id, node.count, window.width, player.id) {
  
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
                  nudge_x = 0.03, nudge_y = 0.08, size = window.width / 116,
                  color = '#646464') +
    
    xlim(-0.05, 1.05) + ylim(-0.05, 1.08) +
    ggtitle(paste('Assist Network For', team.id, '\n A Focus On', player.id)) +
    theme_blank() +
    theme(legend.position = 'none',
          plot.title = element_text(face = "bold", size = window.width / 32, hjust = 0.5, color = '#646464'))
  
  return(list(assist.plot, zed))
}
# ====


# ===================================
# 7. Four Factors Application
# ===================================

# team.ids = 'GSW'; single_team_evaluation = TRUE; write_labels = TRUE; window.width = 800;
# team.ids = c('GSW', 'HOU', 'NYK'); single_team_evaluation = FALSE; write_labels = TRUE; window.width = 800;

# 7A. create the Four Factors bar graph
drawFourFactorsChart <- function(team.stats, team.ids, single_team_evaluation, write_labels, color.pal.df, window.width) {
  
  # rank each team in each stat
  # =============================
  rankings.df <- team.stats %>% 
    filter(foragainst == 'for') %>% 
    filter(stat %in% c('eFG%', 'ORB%', 'FTR', 'TOV%')) %>%
    group_by(stat) %>% 
    mutate(rank = min_rank(-value)) %>%
    select(team.Abbreviation, stat, rank)
  
  opp.rankings.df <- team.stats %>% 
    filter(foragainst == 'against') %>% 
    filter(stat %in% c('eFG%', 'ORB%', 'FTR', 'TOV%')) %>%
    group_by(stat) %>% 
    mutate(rank = min_rank(value)) %>%
    select(team.Abbreviation, stat, rank)
  
  # modify the rankings for TOV%
  rankings.df$rank[rankings.df$stat == 'TOV%'] <- 31 - rankings.df$rank[rankings.df$stat == 'TOV%']
  opp.rankings.df$rank[opp.rankings.df$stat == 'TOV%'] <- 31 - opp.rankings.df$rank[opp.rankings.df$stat == 'TOV%']
  # ====
  
  # for single team evaluation, do this graph
  if(single_team_evaluation == TRUE) {
    
    # filter the dataframe, add rankings, set colors
    # ================================================
    team.for.stats <- team.stats %>% 
      filter(stat %in% c('eFG%', 'ORB%', 'FTR', 'TOV%')) %>%
      filter(team.Abbreviation == team.ids) %>%
      filter(foragainst == 'for') %>%
      left_join(rankings.df, by = c('team.Abbreviation'='team.Abbreviation', 'stat'='stat')) %>%
      mutate(colorhex = color.pal.df$colorhex[color.pal.df$teamid == team.ids],
             darkerhex = color.pal.df$darkerhex[color.pal.df$teamid == team.ids]) %>%
      arrange(stat)
    
    team.against.stats <- team.stats %>% 
      filter(stat %in% c('eFG%', 'ORB%', 'FTR', 'TOV%')) %>%
      filter(team.Abbreviation == team.ids) %>%
      filter(foragainst == 'against') %>%
      left_join(opp.rankings.df, by = c('team.Abbreviation'='team.Abbreviation', 'stat'='stat')) %>%
      mutate(colorhex = '#d3d3d3',
             darkerhex = '#969696') %>%
      arrange(stat)
    
    plot.df <- rbind(team.against.stats, team.for.stats)
    plot.df$hovercol <- paste0(plot.df$team.Abbreviation, ' ', plot.df$stat, ': ', round(plot.df$value*100, 1), '%', '\n',
                               'Four Factors Rank: ', plot.df$rank)
    # ====

    # create layout - axes, annotations, etc.
    # =========================================
    
    # arrange the dataframe (needed for labels)
    # plot.df <- plot.df %>% arrange(foragainst, stat)
    
    # axes
    ax <- list(title = "",
               tickfont = list(size = window.width/60),
               titlefont = list(size = window.width/60),
               color = 'rgb(100, 100, 100)',
               showgrid = FALSE,
               zeroline = FALSE)
    
    ay <- list(title = "",
               tickfont = list(size = window.width/60),
               titlefont = list(size = window.width/60),
               color = 'rgb(100, 100, 100)',
               showgrid = TRUE,
               zeroline = TRUE, 
               zerolinewidth = 3,
               zerolinecolor = 'rgb(100, 100, 100)')
    
    title_name <- paste0(team.ids, ' Four Factors (Own vs. Opp)')
    title_height <- max(plot.df$value) + 0.08 
    myannotations <- list(
      list(text = paste0("<b>", title_name, '</b>'),
           showarrow = FALSE, x = 1.5, y = title_height, 
           font = list(color = 'rgb(100, 100, 100)', size = window.width/40))
    )
    
    # add text labels
    bargap = 0.2
    if(write_labels) {
      numfactors = 4
      numteams = 2
      barwidth = (1 - bargap) / numteams 
      
      deltas <- seq(barwidth, barwidth*numteams, by = barwidth)
      deltas <- deltas - mean(deltas)
      
      thisfont = 10 * numfactors * numteams
      
      # myannotations2 <- list()
      for(i in 1:numteams) {
        this.foragainst <- unique(plot.df$foragainst)[i]
        for(j in 1:numfactors) {
          this.rank <- plot.df$rank[plot.df$foragainst == this.foragainst][j]
          this.stat <- plot.df$stat[plot.df$foragainst == this.foragainst][j]
          this.val <- plot.df$value[plot.df$foragainst == this.foragainst][j]
          this.text <- paste0(round(plot.df$value[plot.df$foragainst == this.foragainst][j]*100,1),'%')
          
          this.xval <- (j-1)+deltas[i]
          this.label <- list(text = paste0('<b>', this.foragainst, '\n', this.stat, ': ', this.text, '\n', 'Rank: ', this.rank, '</b>'),
                             x = this.xval, y = this.val + 0.03, showarrow = FALSE,
                             font = list(color = 'rgb(100, 100, 100)', size = window.width/thisfont))
          myannotations[[length(myannotations)+1]] <- this.label
        }
      }
    } 
    
    # ====
    
    # do the plot
    # ============
    plot_ly(height = 0.67*window.width) %>%
      add_trace(data = plot.df, x = ~stat, y = ~value, type = 'bar', color = ~foragainst,
                marker = list(color = ~colorhex,
                              line = list(color = ~darkerhex, width = 3)),
                showlegend = FALSE,
                hoverinfo = 'text',
                text = ~hovercol) %>%
      layout(
        xaxis = ax, yaxis = ay, 
        annotations = myannotations,
        barmode = 'group',
        bargap = bargap,
        bargroupgap = 0.08
      )
    # ====
  }
  
  else {
    
    # compute league averages (for) Four Factors
    # ============================================
    lg.averages <- team.stats %>% 
      filter(foragainst == 'for') %>%
      filter(stat %in% c('FTR', 'eFG%', 'TOV%', 'ORB%')) %>%
      group_by(stat) %>%
      dplyr::summarise(value = mean(value)) %>%
      mutate(team.Abbreviation = 'ALL', 
             foragainst = 'for')
    # ====
    
    # grab (for) Four Factors for all teams in team.ids
    # ====================================================
    plot.df <- team.stats %>% 
      filter(team.Abbreviation %in% team.ids) %>%
      filter(stat %in% c('FTR', 'eFG%', 'TOV%', 'ORB%')) %>%
      filter(foragainst == 'for') %>%
      rbind(., lg.averages) %>% 
      left_join(color.pal.df, by = c('team.Abbreviation'='teamid')) %>%
      left_join(rankings.df, by = c('team.Abbreviation'='team.Abbreviation', 'stat'='stat'))
    
    # add the hovertext on
    plot.df <- plot.df %>%
      mutate(hovercol = paste0(team.Abbreviation, ' ', stat, ': ', round(value*100, 1), '%', '\n',
                               'Four Factors Rank: ', rank))
    
    # modify hovertext for ALL
    plot.df$hovercol[plot.df$team.Abbreviation == 'ALL'] <- paste0('League Average ', plot.df$stat[plot.df$team.Abbreviation == 'ALL'], 
                                                                   ': ', round(plot.df$value[plot.df$team.Abbreviation == 'ALL']*100, 1), '%')
    # add the colors for ALL
    plot.df$colorhex[plot.df$team.Abbreviation == 'ALL'] = '#d3d3d3'
    plot.df$darkerhex[plot.df$team.Abbreviation == 'ALL'] = '#969696'
    # ====
    
    # create layout - axes, annotations, etc.
    # =========================================
    
    # arrange plot first (will matter for annotations)
    plot.df <- plot.df %>% arrange(team.Abbreviation, stat)
    
    # axes
    ax <- list(title = "",
               tickfont = list(size = window.width/60),
               titlefont = list(size = window.width/60),
               color = 'rgb(100, 100, 100)',
               showgrid = FALSE,
               zeroline = FALSE)
    
    ay <- list(title = "",
               tickfont = list(size = window.width/60),
               titlefont = list(size = window.width/60),
               color = 'rgb(100, 100, 100)',
               showgrid = TRUE,
               zeroline = TRUE, 
               zerolinewidth = 3,
               zerolinecolor = 'rgb(100, 100, 100)')
    
    # annotations
    title_name <- paste0('Four Factors Against League Average')
    title_height <- max(plot.df$value) + 0.08 
    myannotations <- list(
      list(text = paste0("<b>", title_name, '</b>'),
                          showarrow = FALSE, x = 1.5, y = title_height, 
                          font = list(color = 'rgb(100, 100, 100)', size = window.width/40))
    ) 
    
    bargap = 0.2
    if(write_labels) {
      numfactors = 4
      numteams = length(unique(plot.df$team.Abbreviation))
      barwidth = (1 - bargap) / numteams 
      
      deltas <- seq(barwidth, barwidth*numteams, by = barwidth)
      deltas <- deltas - mean(deltas)
      
      thisfont = 10 * numfactors * numteams
      
      # myannotations2 <- list()
      for(i in 1:numteams) {
        this.team <- unique(plot.df$team.Abbreviation)[i]
        for(j in 1:numfactors) {
          this.rank <- plot.df$rank[plot.df$team.Abbreviation == this.team][j]
          this.stat <- plot.df$stat[plot.df$team.Abbreviation == this.team][j]
          this.val <- plot.df$value[plot.df$team.Abbreviation == this.team][j]
          this.text <- paste0(round(plot.df$value[plot.df$team.Abbreviation == this.team][j]*100,1),'%')
          
          this.xval <- (j-1)+deltas[i]
          this.label <- list(text = paste0('<b>', this.stat, ': ', this.text, '\n', 'Rank: ', this.rank, '</b>'),
                             x = this.xval, y = this.val + 0.02, showarrow = FALSE,
                             font = list(color = 'rgb(100, 100, 100)', size = window.width/thisfont))
          myannotations[[length(myannotations)+1]] <- this.label
        }
      }
    }
    # ====
    
    # do the plot
    # ============
    plot_ly(height = 0.67*window.width) %>%
      add_trace(data = plot.df, x = ~stat, y = ~value, type = 'bar', color = ~team.Abbreviation,
                marker = list(color = ~colorhex,
                              line = list(color = ~darkerhex, width = 3)),
                showlegend = FALSE,
                hoverinfo = 'text',
                text = ~hovercol) %>%
      layout(
        xaxis = ax, yaxis = ay,
        annotations = myannotations,
        barmode = 'group',
        bargap = bargap,
        bargroupgap = 0.08
      )
    # ====
  }
}
# ====



# ===================================
# 8. Player Percentiles
# ===================================

# 8A. draw the chart
drawPlayerPercentilesChart <- function(player.stats.bbr, this.player, this.pos, mp.threshold, window.width) {
  
  stat_color_map <- data.frame(Stats = c('X3P.', 'X2P.', 'eFG.', 'TS.', 'FT.', 'USG.', 'TRB.', 'STL.', 'BLK.', 'AST.', 'TOV.'),
                               StatsName = c('3PT%', '2PT%', 'eFG%', 'TS%', 'FT%', 'USG%', 'REB%', 'STL%', 'BLK%', 'AST%', 'TOV%'),
                               Colors = c('#f2003a', '#0202ef', '#0bea27', '#8a0eaf', '#ff6a00', '#efd107',
                                          '#46f0f0', '#f032e6', '#d2f53c', '#fabebe', '#008080'), 
                               XVals = c(1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41),
                               stringsAsFactors = FALSE)
  
  # A,B,C building marker dataframes
  # ==================================
  # A. filter rows by pos and mp, remove unneeded cols, set cols to %
  playerstats.df <- player.stats.bbr %>%
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
             tickfont = list(size = window.width/60),
             color = 'rgb(100, 100, 100)',
             fixedrange = TRUE,
             showgrid = FALSE,
             zeroline = FALSE)
  
  ay <- list(title = "",
             tickfont = list(size = window.width/60),
             showgrid = TRUE, 
             color = 'rgb(100, 100, 100)',
             zerolinecolor = 'rgb(100, 100, 100)',
             zerolinewidth = 2)
  
  myannotations <- list(
    list(text = 'Shooting / Scoring', showarrow = FALSE, x = 11, y = 0.12,
         font = list(color = 'rgb(100, 100, 100)', size = window.width/60)),
    list(text = 'Defense / Rebounding', showarrow = FALSE, x = 30, y = 0.30,
         font = list(color = 'rgb(100, 100, 100)', size = window.width/60)),
    list(text = 'Ball Control', showarrow = FALSE, x = 40.5, y = 0.7,
         font = list(color = 'rgb(100, 100, 100)', size = window.width/60)),
    list(text = paste0("<b>", this.player, ' Statistical Rankings Amongst All ', this.pos, 's</b>'),
         showarrow = FALSE, x = 21, y = 1.1,
         font = list(color = 'rgb(100, 100, 100)', size = window.width/44))
  )
  # ====
  
  
  # F. create the plot
  # =====================
  plot_ly(marker.df, height = 0.75*window.width) %>%
    
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
    
    layout(
      shapes = myshapes,
      xaxis = ax, yaxis = ay,
      annotations = myannotations
      # plot_bgcolor = 'rgb(248, 248, 248)',
      # paper_bgcolor = 'rgb(248, 248, 248)'
    )
  # ====
}
# ====


# =========================================
# 9. Impressive Performances Application
# =========================================

# 9A. another great chart added
drawImpressivePerformancesChart <- function(player.stats.bygame, chart.type, color.pal.df, window.width) {
  
  # create column for filtering rows based on stat threshold
  # ==========================================================
  if(chart.type == '40+ Points') {
    filter_col <- ifelse(player.stats.bygame$PTS >= 40, 1, 0)
  } else if(chart.type == '20+ Rebounds') {
    filter_col <- ifelse(player.stats.bygame$REB >= 20, 1, 0)
  } else if(chart.type == '15+ Assists') {
    filter_col <- ifelse(player.stats.bygame$AST >= 15, 1, 0)
  } else if(chart.type == 'Triple Double') {
    stat_cols <- c('PTS', 'REB', 'AST', 'STL', 'BLK')
    filter_col <- apply(player.stats.bygame[, names(player.stats.bygame) %in% stat_cols], 1, FUN = function(x) sum(x >= 10) >= 3)
  } else if(chart.type == '30+ Point Triple Double') {
    stat_cols <- c('PTS', 'REB', 'AST', 'STL', 'BLK')
    filter_col <- apply(player.stats.bygame[, names(player.stats.bygame) %in% stat_cols], 1, 
                        FUN = function(x) sum(x >= 10) >= 3 & x[1] >= 30)
  } else if(chart.type == '10+ Assists, <= 1 Turnover') {
    filter_col <- ifelse(player.stats.bygame$AST >= 10 & player.stats.bygame$TOV <= 1, 1, 0) 
  } else if(chart.type == '7+ 3-Pointers') {
    filter_col <- ifelse(player.stats.bygame$X3PM >= 7, 1, 0)
  } else {
    # dont worry about this for now
    print('woah')
    break
  }
  player.stats.bygame$filter_col <- filter_col
  # ====
  
  # filter to ip.plot.df, and create output table
  # ==================================================
  ip.plot.df <- player.stats.bygame %>%
    filter(filter_col == 1) %>%
    mutate(weekNum = ceiling(as.integer(gameDate - as.Date('2017-10-16')) / 7)) %>%
    group_by(weekNum) %>% 
    mutate(yaxis = row_number(),
           playerInitials = paste0(substring(firstName, 1, 1), '.', substring(lastName, 1, 1), '.'),
           hoverCol = paste0('Date: ', gameDate, '\n', fullName, '\n', 'Pts-Reb-Ast-Stl-Blk', '\n', PTS, ' - ', REB, ' - ', AST, ' - ', STL, ' - ', BLK)) %>%
    left_join(color.pal.df, by = c('teamAbb'='teamid')) 
  
  output.df <- ip.plot.df %>% 
    group_by(fullName) %>%
    dplyr::summarize(Games = n(),
                     `Avg Points` = round(mean(PTS), 1),
                     `Avg Assists` = round(mean(AST), 1),
                     `Avg Rebounds` = round(mean(REB), 1),
                     `Avg Steals` = round(mean(STL), 1),
                     `Avg Blocks` = round(mean(BLK), 1)) %>%
    arrange(-Games)
  
  colnames(output.df)[1] = 'Player Name'
  # ====
  
  # create month labels on xaxis
  # ==============================
  xlabels.df <- data.frame(
    month.num = c(10, 11, 12, 1, 2, 3, 4, 5, 6),
    month.name = c('October', 'November', 'December', 'January', 'February', 'March', 'April', 'May', 'June'),
    month.pos = c(1.5, 5, 9, 13, 17, 21, 25, 29, 33),
    stringsAsFactors = FALSE)

  this.month <- as.integer(substring(max(ip.plot.df$gameDate), 6, 7))
  xlabels.df <- xlabels.df[1:which(xlabels.df$month.num == this.month), ]
  
  # create layout - axes, annotations, etc.
  ax <- list(title = "",
             tickvals = xlabels.df$month.pos,
             ticktext = xlabels.df$month.name,
             tickfont = list(size = window.width/48),
             titlefont = list(size = window.width/60),
             color = 'rgb(100, 100, 100)',
             fixedrange = TRUE,
             showgrid = FALSE,
             zeroline = FALSE)
  
  ay <- list(title = "",
             tickvals = "",
             showgrid = FALSE,
             zeroline = TRUE, 
             fixedrange = TRUE,
             zerolinewidth = 3,
             zerolinecolor = 'rgb(100, 100, 100)')
  
  my_annotations <- list(
    list(text = paste0("<b>", chart.type, '</b>'),
         showarrow = FALSE, x = 4.5, y = max(ip.plot.df$yaxis + 1), 
         font = list(color = 'rgb(100, 100, 100)', size = window.width/40)),
    list(text = paste0("<b>No. of\nGames</b>"),
         showarrow = FALSE, x = 0, y = 0.67*max(ip.plot.df$yaxis + 1),
         font = list(color = 'rgb(100, 100, 100)', size = window.width/40))
  )
  for(i in 1:nrow(ip.plot.df)) {
    this.label <- list(x = ip.plot.df$weekNum[i], y = ip.plot.df$yaxis[i],
                       text = ip.plot.df$playerInitials[i],
                       xref = 'x', yref = 'y',
                       showarrow = FALSE,
                       font = list(color = 'white', size = window.width / 60))
    my_annotations[[length(my_annotations)+1]] <- this.label
  }
  
  my_shapes <- list(type = 'line', x0 = 0.25, x1 = max(ip.plot.df$weekNum)+0.75, y0 = 0.1, y1 = 0.1,
                    line = list(color = 'rgb(100, 100, 100)', width = 3))
  # ====
  
  # and the plot
  # ==============
  
  # height too big when x == 8
  # height right size when x == 9
  # height going to be too small when x == 10
  
  # marker sizes a function of the number of weeks
  markerSizes <- window.width / (1.5 + (1.5*max(ip.plot.df$weekNum)))
  
  # height of plot is quite a pain here - needed because of varying number of markers by row and column
  height_adj1 <- ((7 / max(ip.plot.df$weekNum))^0.75)/1.05
  height_adj2 <- ((2+max(ip.plot.df$yaxis)) / 7)
  plotHeight <- height_adj1*height_adj2*1.3*(window.width^0.89)
  
  
  output.plot <- plot_ly(ip.plot.df, height = plotHeight) %>%
    add_trace(x = ~weekNum, y = ~yaxis, type = 'scatter', mode = 'markers',
              marker = list(size = markerSizes, color = ~colorhex,
                            line = list(color = ~darkerhex,
                                        width = 2)),
              hoverinfo = 'text',
              text = ~hoverCol) %>%
    layout(
      xaxis = ax, yaxis = ay,
      annotations = my_annotations,
      shapes = my_shapes
    )
  
  # return(list(output.plot))
  return(list(output.plot, output.df))
  # ====
  
}

# ====



# The Single Color Mapping Dataframe
color.pal.df <- data.frame(
  colorhex = c('#006BB6', '#2C5234', '#BA0C2F', '#6F263D', '#007A33', '#D50032',
               '#6189B9', '#C8102E', '#862633', '#00788C', '#002B5C', '#724C9F',
               '#FF671F', '#702F8A', '#007DC5', '#0050B5', '#232323', '#418FDE', 
               '#FDBB30', '#002B5C', '#003DA5', '#CE1141', '#BA0C2F', '#B6BFBF',
               '#E56020', '#007DC3', '#7AC143', '#F0163A', '#FFC72D', '#0C2340'),
  darkerhex = c('#004677', '#1c3d23', '#9b0120', '#4f1024', '#00471d', '#990023',
                '#345e91', '#91051c', '#6d111e', '#004a56', '#001328', '#471f77',
                '#bc4005', '#460c5e', '#014770', '#003170', '#010101', '#114f8e',
                '#b57d09', '#001328', '#002059', '#870123', '#9b0120', '#B6BFBF',
                '#9b3a0c', '#015584', '#518e22', '#910018', '#9b750d', '#829696'),
  teamname = c("76ers", "Bucks", "Bulls", "Cavaliers", "Celtics", "Clippers", 
               "Grizzlies", "Hawks", "Heat", "Hornets", "Jazz", "Kings", 
               "Knicks", "Lakers", "Magic", "Mavericks", "Nets", "Nuggets", 
               "Pacers", "Pelicans", "Pistons", "Raptors", "Rockets", "Spurs", 
               "Suns", "Thunder", "Timberwolves", "Trail Blazers", "Warriors", "Wizards"),
  teamid = c("PHI", "MIL", "CHI", "CLE", "BOS", "LAC", 
             "MEM", "ATL", "MIA", "CHA", "UTA", "SAC", 
             "NYK", "LAL", "ORL", "DAL", "BRO", "DEN", 
             "IND", "NOP", "DET", "TOR", "HOU", "SAS", 
             "PHX", "OKL", "MIN", "POR", "GSW", "WAS"),
  stringsAsFactors = FALSE
)


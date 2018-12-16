
# ==============
# 0. Setup
# ==============

rm(list = ls())
# setwd() # Set your working directory here 
Sys.setenv(TZ='America/Los_Angeles')

library(mysportsfeedsR)
library(dplyr)
library(readr)
library(rvest)
library(tidyr)
library(lubridate)

library(mongolite)

.MySportsFeedsEnv <- new.env()
.MySportsFeedsEnv$data <- list(v1_0_username <- NULL,
                               v1_0_password <- NULL)

# Get a free non-commercial API key from MySportsFeeds to work these functions
# Note That I Use an OUTDATED (v1) version of MySportsFeeds - they have recently released v2.

# ELO update helper functions
movMultiplier <- function(w_pregame_ELO, l_pregame_ELO, margin_of_win, did_home_team_win) { 
  adjustment <- ifelse(did_home_team_win, 100, -100)
  final_multiplier_NBA = ((margin_of_win+3)^0.8) / (7.5+0.006*(w_pregame_ELO - l_pregame_ELO + adjustment))
  return(final_multiplier_NBA)
}
basicELOUpdater <- function(w_pregame_ELO, l_pregame_ELO, multiplier, K) {
  
  # K = 20; multiplier = .5385191; w_pregame_ELO = 1725; l_pregame_ELO = 1607
  
  # transform the ratings into their exponential form
  w_transformed_ELO = 10 ^ (w_pregame_ELO / 400) 
  l_transformed_ELO = 10 ^ (l_pregame_ELO / 400) 
  
  # use transformed ELO to calculate each team's pregame expected winning percentage
  w_winpct = w_transformed_ELO / (w_transformed_ELO + l_transformed_ELO)
  l_winpct = l_transformed_ELO / (w_transformed_ELO + l_transformed_ELO)
  
  # use expected win pct and multiplier to calculate updated ELO ratings for each team
  w_postgame_ELO = w_pregame_ELO + K * multiplier * (1 - w_winpct)
  l_postgame_ELO = l_pregame_ELO + K * multiplier * (0 - l_winpct)
  
  # return the updated ELO ratings, winner then loser  
  return(c(w_postgame_ELO, l_postgame_ELO))
}
# ====


# ======================================
# 1. NBA game_playbyplay (ShotCharts) 
# ======================================

# read in the data previously scraped
shotcharts.all <- read.csv('NBA_gameplaybyplay_shotcharts.csv', stringsAsFactors = FALSE)

# create vector of all the game IDs
nba.schedule <- msf_get_results(league = 'nba', season = '2017-2018-regular',
                                feed = 'full_game_schedule', verbose = TRUE)
nba.schedule <- nba.schedule$api_json$fullgameschedule$gameentry

# create the vector, filter out dates in the future, and games that have already been scraped
game.ids <- paste0(gsub('-', '', nba.schedule$date), '-', nba.schedule$awayTeam.Abbreviation, '-', nba.schedule$homeTeam.Abbreviation)
idx1 <- as.integer(gsub('-', '', nba.schedule$date)) < as.integer(gsub('-', '', Sys.Date()))
idx2 <- !(game.ids %in% shotcharts.all$msf.gameID)
game.ids <- game.ids[idx1 & idx2]

# loop game_playbyplay for each game, grabbing shots data
for(i in 1:length(game.ids)) {
  
  # sleep will prevent errors from being thrown
  print(i)
  Sys.sleep(1); # change back to 2
  
  # grab this games pbp dataframe, set msf ID 
  shotcharts.pbp <- msf_get_results(league = 'nba',
                              season = '2017-2018-regular',
                              feed = 'game_playbyplay',
                              params = list(gameid = game.ids[i]))
  shotcharts.pbp <- shotcharts.pbp$api_json$gameplaybyplay$plays$play
  shotcharts.pbp$api_json$gameplaybyplay$plays
  shotcharts.pbp$msf.gameID <- game.ids[i]
  
  # columns to keep (filtering columns helps with speed of the app / reading CSV files)
  keepers <- c("quarter", "time", "msf.gameID",
               "fieldGoalAttempt.teamAbbreviation", 
               "fieldGoalAttempt.distanceFeet",
               "fieldGoalAttempt.Points",
               "fieldGoalAttempt.outcome",
               "fieldGoalAttempt.shootingPlayer.LastName",
               "fieldGoalAttempt.shootingPlayer.FirstName", 
               "fieldGoalAttempt.shootingPlayer.Position", 
               "fieldGoalAttempt.shotLocation.x",
               "fieldGoalAttempt.shotLocation.y")
  
  newnames <- c('quarter', 'time', 'msf.gameID', 'team', 'shotdist', 'ptsattempt', 'outcome', 'lastname', 
                'firstname', 'pos', 'yloc', 'xloc')

  # assess which keeper columns were present, keep them, and set the names correctly to the column-filtered dataframe
  keeper_cols_present <- keepers[which(keepers %in% names(shotcharts.pbp))]
  shotcharts.pbp <- shotcharts.pbp[, names(shotcharts.pbp) %in% keeper_cols_present]
  setnames(shotcharts.pbp, old = keepers[keepers %in% names(shotcharts.pbp)], new = newnames[keepers %in% names(shotcharts.pbp)])
  
  # if theres no coordinate data, skip this game
  if(!('xloc' %in% names(shotcharts.pbp))) { next }
  
  # filter by xlocation being present
  shotcharts.pbp <- shotcharts.pbp %>% 
    filter(!is.na(xloc)) %>%
    mutate(xloc = as.integer(xloc), yloc = as.integer(yloc))
  
  # map from random scale to 0:50 / 0:94
  mapCoords <- function(shotcharts.pbp, xlocs, ylocs) {
    
    xmapped <- shotcharts.pbp$xloc
    ymapped <- shotcharts.pbp$yloc

    # map the y coordinate first (remember napkin drawing)
    ymapped[shotcharts.pbp$yloc > 470] = round((ymapped[shotcharts.pbp$yloc > 470] - 470) / 10, 1)
    ymapped[shotcharts.pbp$yloc <= 470] = round((470 - ymapped[shotcharts.pbp$yloc <= 470]) / 10, 1)
    
    # map the x coordinate second 
    xmapped[shotcharts.pbp$yloc > 470] = round(shotcharts.pbp$xloc[shotcharts.pbp$yloc > 470] / 10, 1)
    xmapped[shotcharts.pbp$yloc <= 470] = round((500 - shotcharts.pbp$xloc[shotcharts.pbp$yloc <= 470]) / 10, 1)
    
    # update the columns in the dataframe, and return entire dataframe
    shotcharts.pbp$xloc = xmapped
    shotcharts.pbp$yloc = ymapped
    return(shotcharts.pbp)
    
    # old mapping equations, which I think are incorrect
    # # flip to one side of court
    # xmapped[shotcharts.pbp$yloc > 470] = 500 - xmapped[shotcharts.pbp$yloc > 470]
    # ymapped[shotcharts.pbp$yloc < 470] = 940 - ymapped[shotcharts.pbp$yloc < 470]
    # 
    # # bring down to 50 / 94 scale
    # xmapped = round(xmapped / 10, 1)
    # ymapped = round((ymapped - 470) / 10, 1)
    # 
    # # flip the y values one last time
    # ymapped = ymapped
    # 
    # # return both vectors
    # return(as.data.frame(cbind(xmapped, ymapped)))
  }    
  shotcharts.pbp <- mapCoords(shotcharts.pbp)

  # add hexagonal mapping value and zones names
  # addZonesAndMapPoints --> MapPointsAndAddZones
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
  addZonesAndMapPoints <- function(mydf, markersize = 'medium') {
    
    # A. discretize coordinates for the hexagon markers
    if(markersize == 'small') {
      hex.coords <- data.frame(xspots = c(seq(0.5, 49.5,by=1),
                                          rep(c(seq(1,49,by=1),seq(0.5,49.5,by=1)), 23)),
                               yspots = rep(x = (1:47)-0.5,
                                            c(rep(c(50,49),23),50)))
      divisor <- length((1:47)-0.5)
    }
    
    else if(markersize == 'medium') {
      hex.coords <- data.frame(xspots = c(seq(1,49,by=1.5),
                                          rep(c(seq(1.75,48.25,by=1.5),seq(1,49,by=1.5)), 15)),
                               yspots = rep(x = seq(1,46,by=1.5),
                                            times = c(rep(c(33,32),15),33)))
      divisor <- length(seq(1,46,by=1.5))
    }
    
    else if(markersize == 'large') {
      hex.coords <- data.frame(xspots = c(seq(1,49,by=2),
                                          rep(c(seq(2,48,by=2),seq(1,49,by=2)), 11)),
                               yspots = rep(x = seq(2,46,by=2),
                                            c(rep(c(25,24),11),25)))
      divisor <- length(seq(2,46,by=2))
    }
    
    
    # B. Set hoop center
    hoop_center_x <- 25
    hoop_center_y <- 47 - (4 + 0.5 + 0.75)
    
    
    # C. map playbyplay coordinates to nearest discretized point
    xcol <- which(names(mydf) == 'xloc')
    ycol <- which(names(mydf) == 'yloc')
    
    mydf <- apply(mydf[, c(xcol, ycol)], 1, FUN = function(x) {
      zed <- (x[1] - hex.coords$xspots)^2 + (x[2] - hex.coords$yspots)^2
      return(c(hex.coords$xspots[which.min(zed)], hex.coords$yspots[which.min(zed)]))
    }) %>%
      t() %>%
      as.data.frame() %>%
      setNames(c('xmapped', 'ymapped')) %>%
      cbind(mydf, .)
    
    # calculate shot distances AFTER mapping points (actually using xmapped!)
    mydf$shotdist <- round(sqrt((mydf$xmapped - hoop_center_x)^2 + (mydf$ymapped - hoop_center_y)^2), digits = 1)
    
    # D. create zonenames, zonenumbers, return the dataframe
    mydf <- cbind(mydf, createZones(mydf$xmapped, mydf$ymapped, mydf$shotdist))
    return(mydf)
  }  
  shotcharts.pbp <- addZonesAndMapPoints(shotcharts.pbp, markersize = 'medium')
  
  # modify classes
  shotcharts.pbp$quarter <- as.integer(shotcharts.pbp$quarter)
  shotcharts.pbp$ptsattempt <- as.integer(shotcharts.pbp$ptsattempt)
  shotcharts.pbp$zonenumber <- as.integer(shotcharts.pbp$zonenumber)
  
  # add full name, remove individual names
  shotcharts.pbp$fullname <- paste(shotcharts.pbp$firstname, shotcharts.pbp$lastname)
  shotcharts.pbp <- shotcharts.pbp %>% 
    dplyr::select(-one_of(c('firstname', 'lastname')))
  
  # rbind.fill the dataframe to the all.pbp main dataframe
  shotcharts.all <- plyr::rbind.fill(shotcharts.all, shotcharts.pbp)
}

# and write updated CSV
write_csv(shotcharts.all, 'NBA_gameplaybyplay_shotcharts.csv')
# ====


# ===========================================
# 2. NBA game_playbyplay (AssistNetworks)
# ===========================================

# read in the data previously scraped
assistnet.all <- read.csv('NBA_gameplaybyplay_assistnet.csv', stringsAsFactors = FALSE)

# create vector of all the game IDs
nba.schedule <- msf_get_results(league = 'nba', season = '2017-2018-regular',
                                feed = 'full_game_schedule', verbose = TRUE)
nba.schedule <- nba.schedule$api_json$fullgameschedule$gameentry

# create the vector, filter out dates in the future, and games that have already been scraped
game.ids <- paste0(gsub('-', '', nba.schedule$date), '-', nba.schedule$awayTeam.Abbreviation, '-', nba.schedule$homeTeam.Abbreviation)
idx1 <- as.integer(gsub('-', '', nba.schedule$date)) < as.integer(gsub('-', '', Sys.Date()))
idx2 <- !(game.ids %in% assistnet.all$msf.gameID)
game.ids <- game.ids[idx1 & idx2]

# loop game_playbyplay for each game, grabbing shots data
if(length(game.ids) > 0) {
  
  # loop through game.ids, making updates
  for(i in 1:length(game.ids)) {
    
    # sleep will prevent errors from being thrown
    Sys.sleep(1); print(paste(i, game.ids[i]))
  
    # the msf query
    assistnet.pbp <- msf_get_results(league = 'nba',
                                season = '2017-2018-regular',
                                feed = 'game_playbyplay',
                                params = list(gameid = game.ids[i]))
    assistnet.pbp <- assistnet.pbp$api_json$gameplaybyplay$plays$play
    assistnet.pbp$msf.gameID <- game.ids[i]
    
    
    # columns to keep (filtering columns helps with speed of the app / reading CSV files)
    keepers <- c("quarter", "time", "msf.gameID",
                 "fieldGoalAttempt.teamAbbreviation", 
                 "fieldGoalAttempt.outcome",
                 "fieldGoalAttempt.shootingPlayer.LastName",
                 "fieldGoalAttempt.shootingPlayer.FirstName", 
                 "fieldGoalAttempt.assistingPlayer.LastName",
                 "fieldGoalAttempt.assistingPlayer.FirstName")
    
    newnames <- c('quarter', 'time', 'msf.gameID', 'team', 'outcome', 'shooterlastname', 'shooterfirstname', 'assisterlastname', 'assisterfirstname')
    
    # assess which keeper columns were present, keep them, and set
    # the names correctly to the column-filtered dataframe
    keeper_cols_present <- keepers[which(keepers %in% names(assistnet.pbp))]
    assistnet.pbp <- assistnet.pbp[, names(assistnet.pbp) %in% keeper_cols_present]
    setnames(assistnet.pbp, old = keepers[keepers %in% names(assistnet.pbp)], new = newnames[keepers %in% names(assistnet.pbp)])
    
    # filter rows that dont feature an assister
    assistnet.pbp <- assistnet.pbp %>%
      filter(!is.na(assisterlastname))
    
    # correct class types
    assistnet.pbp$quarter <- as.integer(assistnet.pbp$quarter)
      
    # rbind.fill the dataframe to the all.pbp main dataframe
    assistnet.all <- plyr::rbind.fill(assistnet.all, assistnet.pbp)
  }
  
  # only write the new CSV if updates were made
  write_csv(assistnet.all, 'NBA_gameplaybyplay_assistnet.csv')
}
# ====


# ==================================
# 3. NBA ELO Ratings (ELORatings)
# ==================================

# NBA ELO Ratings has to be updated before Game Recaps, since ELOs are needed for IGWP calcs

# start with what 538 'gave' us + whats already scraped
nba.elo <- read_csv('NBA_elo.csv')

# # fix the year being 2068 
# all.years <- lubridate::year(nba.elo$date_game)
# all.months <- lubridate::month(nba.elo$date_game)
# all.days <- lubridate::day(nba.elo$date_game)
# 
# all.years[all.years > 2017] <- all.years[all.years > 2017] - 100
# nba.elo$date_game <- as.Date(paste(all.years, '-', all.months, '-', all.days, sep = ''))

# one time fix stuff
{
  # # one time fix of tickers to MSF tickers
  # unique(paste(nba.elo$team_id[nba.elo$year_id > 2010], nba.elo$fran_id[nba.elo$year_id > 2010]))
  # 
  # # CHA CHO Hornets (msf == CHA)
  # nba.elo$team_id[nba.elo$team_id == 'CHO'] = 'CHA'
  # nba.elo$opp_id[nba.elo$opp_id == 'CHO'] = 'CHA'
  # 
  # # NOH NOP Pelicans (msf == NOP)
  # nba.elo$team_id[nba.elo$team_id == 'NOH'] = 'NOP'
  # nba.elo$opp_id[nba.elo$opp_id == 'NOH'] = 'NOP'
  # 
  # # BRO BRK Nets (msf == BRO)
  # nba.elo$team_id[nba.elo$team_id == 'BRK'] = 'BRO'
  # nba.elo$opp_id[nba.elo$opp_id == 'BRK'] = 'BRO'
  # 
  # # PHO PHX Suns (msf == PHX)
  # nba.elo$team_id[nba.elo$team_id == 'PHO'] = 'PHX'
  # nba.elo$opp_id[nba.elo$opp_id == 'PHO'] = 'PHX'
  # 
  # # OKL OKC Thunder (msf == OKL)
  # nba.elo$team_id[nba.elo$team_id == 'OKC'] = 'OKL'
  # nba.elo$opp_id[nba.elo$opp_id == 'OKC'] = 'OKL'
  # 
  # 
  # # add the entire 2015-2016 and 2016-2017 seasons to nba.elo
  # nba1516.regular <- msf_get_results(league = 'nba', season = '2015-2016-regular',
  #                                 feed = 'full_game_schedule', verbose = TRUE)
  # nba1516.regular <- nba1516.regular$api_json$fullgameschedule$gameentry
  # 
  # nba1516.playoff <- msf_get_results(league = 'nba', season = '2016-playoff',
  #                                    feed = 'full_game_schedule', verbose = TRUE)
  # nba1516.playoff <- nba1516.playoff$api_json$fullgameschedule$gameentry
  # 
  # nba1617.regular <- msf_get_results(league = 'nba', season = '2016-2017-regular',
  #                                    feed = 'full_game_schedule', verbose = TRUE)
  # nba1617.regular <- nba1617.regular$api_json$fullgameschedule$gameentry
  # 
  # nba1617.playoff <- msf_get_results(league = 'nba', season = '2017-playoff',
  #                                     feed = 'full_game_schedule', verbose = TRUE)
  # nba1617.playoff <- nba1617.playoff$api_json$fullgameschedule$gameentry
  # 
  # nba1517.all <- rbind(nba1516.regular, nba1516.playoff, nba1617.regular, nba1617.playoff)
}

# query the entire season, grab dates (a) before today and (b) not already in nba.elo
nba1718.regular <- msf_get_results(league = 'nba', season = '2017-2018-regular',
                                   feed = 'full_game_schedule', verbose = TRUE)
nba1718.regular <- nba1718.regular$api_json$fullgameschedule$gameentry
nba1718.regular$msfID <- paste0(gsub('-', '', nba1718.regular$date), '-', nba1718.regular$awayTeam.Abbreviation, '-', nba1718.regular$homeTeam.Abbreviation) 

# grab all dates where at least 1 game is missing from nba.elo
all.dates <- as.Date(unique(nba1718.regular$date))
# date is before today
idx1 <- all.dates < Sys.Date()
# dates of all game IDs not in nba.elo
idx2 <- all.dates %in% as.Date(unique(nba1718.regular$date[!(nba1718.regular$msfID %in% nba.elo$msf_game_id)]))
all.dates <- all.dates[idx1 & idx2]

if(length(all.dates) > 0) {

  # use the scoreboard, loop over each date
  for(i in 1:length(all.dates)) {
    
    print(paste(i, all.dates[i]))
    is.playoff <- 0
    this.date <- gsub('-', '', all.dates[i])
  
    # query the game
    day.scoreboard <- msf_get_results(league = 'nba', season = '2017-2018-regular', 
                                      feed = 'scoreboard', params = list(fordate = this.date))
    day.scoreboard <- day.scoreboard$api_json$scoreboard$gameScore
    
    # loop through each game in the day's scoreboard
    for(j in 1:nrow(day.scoreboard)) {
      
      # grab team Ids, team scores, and msf id
      hID = day.scoreboard$game.homeTeam.Abbreviation[j] 
      aID = day.scoreboard$game.awayTeam.Abbreviation[j]
      hFran = day.scoreboard$game.homeTeam.Name[j]
      aFran = day.scoreboard$game.awayTeam.Name[j]
      hScore = as.integer(day.scoreboard$homeScore[j])
      aScore = as.integer(day.scoreboard$awayScore[j])
      msf_id <- paste0(this.date, '-', aID, '-', hID) 
    
      # skip this game if already accounted for in ELO    
      if(msf_id %in% nba.elo$msf_game_id) { next }
       
      # grab month and year
      this_month <- as.integer(substring(this.date, 5, 6))
      this_year <- ifelse(this_month >8, 
                          as.integer(substring(this.date, 1, 4))+1, 
                          as.integer(substring(this.date, 1, 4)))
      
      # compute seasongame numbers
      hGameNum <- sum(nba.elo$year_id == this_year & nba.elo$team_id == hID) + 1
      aGameNum <- sum(nba.elo$year_id == this_year & nba.elo$team_id == aID) + 1
      
      # grab old elo, revert back 25% if first GameNum == 1
      hELOi <- tail(nba.elo$elo_n[nba.elo$team_id == hID], 1)
      hELOi <- ifelse(hGameNum == 1, 1500 + (hELOi - 1500)*.75, hELOi)
      aELOi <- tail(nba.elo$elo_n[nba.elo$team_id == aID], 1)
      aELOi <- ifelse(aGameNum == 1, 1500 + (aELOi - 1500)*.75, aELOi)
      
      # compute updated elo
      margin_of_victory <- abs(hScore - aScore)
      wELO <- ifelse(hScore > aScore, hELOi, aELOi)
      lELO <- ifelse(hScore > aScore, aELOi, hELOi)
      did_home_win_indicator <- ifelse(hScore > aScore, TRUE, FALSE)
      
      this_multiplier <- movMultiplier(wELO, lELO, margin_of_victory, did_home_win_indicator)
      updated_ELOs <- basicELOUpdater(wELO, lELO, this_multiplier, K = 20)
      
      hELOn <- round(ifelse(hScore - aScore > 0, updated_ELOs[1], updated_ELOs[2]), digits = 2)
      aELOn <- round(ifelse(hScore - aScore > 0, updated_ELOs[2], updated_ELOs[1]), digits = 2)
      if(hScore - aScore > 0) { this_result = c('W', 'L') } else { this_result = c('L', 'W') }
      
      # create the row (as a list), and rbind it to the main dataframe
      nba.elo.row <- list(gameorder = as.integer(rep(max(nba.elo$gameorder) + 1, 2)),
                          game_id = rep(paste0(this.date, '0', hID), 2),
                          lg_id = rep('NBA', 2),
                          is_copy = as.integer(c(0, 1)),
                          year_id = as.integer(rep(this_year, 2)),
                          date_game = rep(as.Date(all.dates[i]), 2),  
                          seasongame = as.integer(c(hGameNum, aGameNum)),
                          is_playoffs = as.integer(rep(is.playoff, 2)),
                          team_id = c(hID, aID),
                          fran_id = c(hFran, aFran),
                          pts = c(hScore, aScore),
                          elo_i = c(hELOi, aELOi),
                          elo_n = c(hELOn, aELOn),
                          opp_id = c(aID, hID),
                          opp_fran = c(aFran, hFran),
                          opp_pts = c(aScore, hScore),
                          opp_elo_i = c(aELOi, hELOi),
                          opp_elo_n = c(aELOn, hELOn),
                          game_location = c('H', 'A'),
                          game_result = this_result, 
                          msf_game_id = rep(msf_id, 2))
      
      nba.elo <- rbind(nba.elo, nba.elo.row)
    }
  }
  
  sum(is.na(nba.elo[, !(names(nba.elo) %in% 'msf_game_id')]))
  
  write_csv(nba.elo, 'NBA_elo.csv')
}
# ====


# ========================================
# 4. NBA game_playbyplay (GameRecaps)
# ========================================

# 1.A read nba.elo and the gamerecaps file already made
nba_elo <- read_csv('NBA_elo.csv')
gamerecaps.all <- read.csv('NBA_gameplaybyplay_gamerecaps.csv', stringsAsFactors = FALSE)

# create vector of all the game IDs
nba.schedule <- msf_get_results(league = 'nba', season = '2017-2018-regular',
                                feed = 'full_game_schedule', verbose = TRUE)
nba.schedule <- nba.schedule$api_json$fullgameschedule$gameentry

# 1.B create the vector, filter out dates in the future, and games that have already been scraped
game.ids <- paste0(gsub('-', '', nba.schedule$date), '-', nba.schedule$awayTeam.Abbreviation, '-', nba.schedule$homeTeam.Abbreviation)
idx1 <- as.integer(gsub('-', '', nba.schedule$date)) < as.integer(gsub('-', '', Sys.Date()))
idx2 <- !(game.ids %in% gamerecaps.all$msfgameid)
game.ids <- game.ids[idx1 & idx2]
gamedates.for.newgames <- unique(substring(game.ids, 1, 8))

if(length(gamedates.for.newgames) > 0) {

  # 1.C query scoreboard for each game from the season append msf gameID onto the output dataframe
  all.scores <- c()
  for(i in 1:length(gamedates.for.newgames)) {
    scores <- msf_get_results(league = 'nba', season = '2017-2018-regular',
                              feed = 'scoreboard', params = list(fordate = gamedates.for.newgames[i])) # todays_date
    scores <- scores$api_json$scoreboard$gameScore
    all.scores <- rbind(all.scores, scores)
  }
  
  all.scores$msf.gameID <- paste(gsub('-', '', all.scores$game.date),
                                 "-", all.scores$game.awayTeam.Abbreviation,
                                 "-", all.scores$game.homeTeam.Abbreviation, sep = "")
  
  # 1.D query and format the play by play for every single NBA game that has happened
  
  # initialize the functions used in the loop
  grabPlayByPlay <- function(game.id, hID, aID, hScore, aScore) {
  
    # query the game, grab the dataframe
    nba.game <- msf_get_results(league = 'nba', season = '2017-2018-regular',
                                feed = 'game_playbyplay', params = list(gameid = game.id),
                                verbose = FALSE)
    nba.game <- nba.game$api_json$gameplaybyplay$plays$play
  
    # initialize score columns, format quarter column
    nba.game$quarter <- as.integer(nba.game$quarter)
    nba.game$homeScore = nba.game$awayScore = nba.game$homePts = nba.game$awayPts = 0
  
    # fill homePts and awayPts with scores
    for(j in 1:nrow(nba.game)) {
  
      # made 2-pt or 3-pt field goal
      testFGM <- (!is.na(nba.game$fieldGoalAttempt.teamAbbreviation[j]) & nba.game$fieldGoalAttempt.outcome[j] == 'SCORED')
      if(testFGM) {
        tmID <- nba.game$fieldGoalAttempt.teamAbbreviation[j]
        tmPts <- as.integer(nba.game$fieldGoalAttempt.Points[j])
        if(tmID == hID) {
          nba.game$homePts[j] = tmPts
        } else {
          nba.game$awayPts[j] = tmPts
        }
      }
  
      # made free throw
      testFTM <- (!is.na(nba.game$freeThrowAttempt.teamAbbreviation[j]) & nba.game$freeThrowAttempt.outcome[j] == 'SCORED')
      if(testFTM) {
        tmID <- nba.game$freeThrowAttempt.teamAbbreviation[j]
        if(tmID == hID) {
          nba.game$homePts[j] = 1
        } else {
          nba.game$awayPts[j] = 1
        }
      }
    }
  
    # cumsum homePts and awayPts to get homeScore and awayScore
    nba.game$homeScore = cumsum(nba.game$homePts)
    nba.game$awayScore = cumsum(nba.game$awayPts)
  
    # create game secs, game minutes, quarter secs remaining cols
    nba.game <- cbind(nba.game, as.data.frame(matrix(as.integer(unlist(strsplit(nba.game$time, split = ':'))), ncol = 2, byrow = TRUE)))
    colnames(nba.game)[(length(nba.game)-1):length(nba.game)] = c('mins', 'secs')
  
    nba.game$quarter_remaining <- ifelse(nba.game$quarter <= 4, 
                                         (11 - nba.game$mins)*60 + (60 - nba.game$secs),
                                         (4 - nba.game$mins)*60 + (60 - nba.game$secs))
    
    nba.game$secsRemaining <- ifelse(nba.game$quarter <= 3,
                                     (4 - nba.game$quarter)*720 + nba.game$quarter_remaining,
                                     nba.game$quarter_remaining)
    
    nba.game$minsRemaining <- plyr::round_any(nba.game$secsRemaining + 29.5, 60) / 60
    nba.game$qminsRemaining <- plyr::round_any(nba.game$quarter_remaining + 29.5, 60) / 60
    
    # add gameid, aID and hID all to dataframe
    nba.game$msfgameid = game.id
    nba.game$aID = aID
    nba.game$hID = hID
  
    keepers <- c('quarter', 'time', 'homeScore', 'awayScore', 'secsRemaining', 'minsRemaining', 'qminsRemaining', 'msfgameid', 'aID', 'hID')
    nba.game <- nba.game[, names(nba.game) %in% keepers]
  
    # Check For Correct Score
    awayComputedCurrentScore <- tail(nba.game$awayScore, 1)
    homeComputedCurrentScore <- tail(nba.game$homeScore, 1)
  
    if((awayComputedCurrentScore != aScore) | (homeComputedCurrentScore != hScore)) {
      print(i)
      print('Diff Scores - add them to print statement?')
      break
    }
  
    # change column types (numerics to integers) - don't use type convert due to time column
    nba.game$awayScore <- as.integer(nba.game$awayScore)
    nba.game$homeScore <- as.integer(nba.game$homeScore)
    nba.game$secsRemaining <- as.integer(nba.game$secsRemaining)
    nba.game$minsRemaining <- as.integer(nba.game$minsRemaining)
    nba.game$qminsRemaining <- as.integer(nba.game$qminsRemaining)
    
    return(nba.game)
  }
  addEloToPBP <- function(nba_game, hID, aID, gameDate, winsELO.df) {
  
    # grab team ELO ratings as of the date of the game
    print(as.Date(gameDate) %in% winsELO.df$date_game[winsELO.df$team_id == aID])
  
    aELO <- winsELO.df$elo_i[winsELO.df$team_id == aID & winsELO.df$date_game == as.Date(gameDate)]
    hELO <- winsELO.df$elo_i[winsELO.df$team_id == hID & winsELO.df$date_game == as.Date(gameDate)]
  
    # compute pregame point spread (home spread, so can be negative if home == underdog)
    homeSpread <- round(((hELO + 100) - aELO) / 28, 1)
    nba_game$homeSpread <- homeSpread
  
    return(nba_game)
  }
  computeIGWP <- function(nba_game, c1, t1, c2, t2) {
  
    # try with the second equation
    ZZ <- (c1*(nba_game$homeLead))/(nba_game$minsRemaining^t1) + (c2*nba_game$pps)/(nba_game$minsRemaining^t2)
    nba_game$winProb <- round(exp(ZZ) / (1 + exp(ZZ)), digits = 3)
  
    return(nba_game)
  }
  
  for(i in 1:nrow(all.scores)) {
    
    # sleep to prevent errors in EC2 instance;
    Sys.sleep(1); print(i)
    
    # skip this game if already included in gamerecaps.all
    this.msfID <- all.scores$msf.gameID[i]
    if(this.msfID %in% gamerecaps.all$msfgameid) { next }
    
    # grab parameters needed for the functions below
    hID <- all.scores$game.homeTeam.Abbreviation[i]
    aID <- all.scores$game.awayTeam.Abbreviation[i]
    hScore <- all.scores$homeScore[i]
    aScore <- all.scores$awayScore[i]
    gameDate <- all.scores$game.date[i]
  
    # grab and format this game's playbyplay data
    nba.game <- grabPlayByPlay(this.msfID, hID, aID, hScore, aScore)
  
    # add ELO ratings, proportional point spread
    nba.game <- addEloToPBP(nba.game, hID, aID, gameDate, nba_elo)
    nba.game$pps <- round(nba.game$homeSpread * (nba.game$minsRemaining/48), digits = 3)
    nba.game$homeLead <- nba.game$homeScore - nba.game$awayScore
  
    # hardcode the coefficients, and compute win probs
    c1 = 0.734; t1 = 0.573; c2 = 1.362; t2 = 0.607;
    nba.game <- computeIGWP(nba.game, c1, t1, c2, t2)
    
    # now you can change the time for OT
    if(length(unique(nba.game$quarter)) > 4) {
      extra.quarters <- unique(nba.game$quarter)[unique(nba.game$quarter) > 4]
      
      for(i in 1:length(extra.quarters)) {
        this.OT <- extra.quarters[i]
        nba.game$secsRemaining[nba.game$quarter < this.OT] <- nba.game$secsRemaining[nba.game$quarter < this.OT] + 300
        nba.game$minsRemaining[nba.game$quarter < this.OT] <- nba.game$minsRemaining[nba.game$quarter < this.OT] + 5
      }
    } 
    
    gamerecaps.all <- rbind(gamerecaps.all, nba.game)
  }
  
  # correct classes
  gamerecaps.all$secsRemaining <- as.integer(gamerecaps.all$secsRemaining)
  gamerecaps.all$minsRemaining <- as.integer(gamerecaps.all$minsRemaining)
  
  # check for no errors
  sum(is.na(gamerecaps.all[, 1:13]))
  
  # write to CSV
  write_csv(gamerecaps.all, 'NBA_gameplaybyplay_gamerecaps.csv') 
}
# ====


# =================================================
# 5. NBA cumulativeplayerstats (PlayerXYCharts)
# =================================================

# grab the season data
player.data <- msf_get_results(league = 'nba', 
                               season = '2017-2018-regular',
                               feed = 'cumulative_player_stats')
player.data <- player.data$api_json$cumulativeplayerstats$playerstatsentry

# grab all data columns that would be used for any of the plots
keepers <- c('player.LastName', 'player.FirstName', 'player.Position', 'team.Name', 
             'stats.FgAtt.#text', 'stats.FtAtt.#text',
             'stats.Reb.#text', 'stats.Ast.#text', 'stats.Pts.#text',
             'stats.Tov.#text', 'stats.Stl.#text', 'stats.Blk.#text', 
             'stats.MinSeconds.#text')
player.data <- player.data[, names(player.data) %in% keepers]
colnames(player.data) = c('lastName', 'firstName', 'POS', 'Team', 'FGA', 'FTA', 'REB', 'AST', 'PTS', 'TOV', 'STL', 'BLK', 'SEC')

# correct column types
player.data <- player.data %>% type_convert()

write_csv(player.data, 'NBA_cumulativeplayerstats.csv')
# ====


# =======================================
# 6. NBA teamgamelogs (TeamXYCharts)
# =======================================

# 6.A grab the data from MSF
nba.schedule <- msf_get_results(league = 'nba', season = '2017-2018-regular', feed = 'full_game_schedule')
all.teams <- unique(nba.schedule$api_json$fullgameschedule$gameentry$awayTeam.Abbreviation)

all.gamelogs <- c()
for(this.team in all.teams) {
  
  # sleep prevent errors
  Sys.sleep(1);
  
  this.gamelog = msf_get_results(league = 'nba', season = '2017-2018-regular',
                                 feed = 'team_gamelogs', params = list(team = this.team))
  this.gamelog <- this.gamelog$api_json$teamgamelogs$gamelogs
  all.gamelogs <- rbind(all.gamelogs, this.gamelog)
}

# automatically format columns correctly, update colnames
all.gamelogs <- all.gamelogs %>% readr::type_convert()
colnames(all.gamelogs) <- make.names(colnames(all.gamelogs))

# set opponent ID column, in order to group by stats allowed
all.gamelogs$oppteam.Abbreviation <- ifelse(all.gamelogs$team.Abbreviation == all.gamelogs$game.homeTeam.Abbreviation, 
                                            all.gamelogs$game.awayTeam.Abbreviation, 
                                            all.gamelogs$game.homeTeam.Abbreviation) 

# 6.B create main dataframe
# the teamfor.stats dataframe
teamfor.stats <- all.gamelogs %>%
  group_by(team.Abbreviation) %>%
  dplyr::summarise(GP = n(),
                   PTS = round(sum(stats.Pts..text) / n(), 1),
                   REB = round(sum(stats.Reb..text) / n(), 1),
                   ORB = round(sum(stats.OffReb..text) / n(), 1),
                   DRB = round(sum(stats.DefReb..text) / n(), 1),
                   AST = round(sum(stats.Ast..text) / n(), 1),
                   BLK = round(sum(stats.Blk..text) / n(), 1),
                   STL = round(sum(stats.Stl..text) / n(), 1),
                   TOV = round(sum(stats.Tov..text) / n(), 1),
                   X2PM = round(sum(stats.Fg2PtMade..text) / n(), 1),
                   X2PA = round(sum(stats.Fg2PtAtt..text) / n(), 1),
                   X3PM = round(sum(stats.Fg3PtMade..text) / n(), 1), 
                   X3PA = round(sum(stats.Fg3PtAtt..text) / n(), 1),
                   FTM = round(sum(stats.FtMade..text) / n(), 1),
                   FTA = round(sum(stats.FtAtt..text) / n(), 1)) %>%
  mutate(X2PCT = round(X2PM / X2PA, 3),
         X3PCT = round(X3PM / X3PA, 3),
         FTPCT = round(FTM / FTA, 3),
         FGPCT = round((X2PM + X3PM) / (X2PA + X3PA), 3)) %>%
  gather(key = stat, value = value, -team.Abbreviation) %>%
  mutate(foragainst = 'for')

# the teamagainst.stats dataframe
teamagainst.stats <- all.gamelogs %>%
  group_by(oppteam.Abbreviation) %>%
  dplyr::summarise(GP = n(),
                   PTS = round(sum(stats.Pts..text) / n(), 1),
                   REB = round(sum(stats.Reb..text) / n(), 1),
                   ORB = round(sum(stats.OffReb..text) / n(), 1),
                   DRB = round(sum(stats.DefReb..text) / n(), 1),
                   AST = round(sum(stats.Ast..text) / n(), 1),
                   BLK = round(sum(stats.Blk..text) / n(), 1),
                   STL = round(sum(stats.Stl..text) / n(), 1),
                   TOV = round(sum(stats.Tov..text) / n(), 1),
                   X2PM = round(sum(stats.Fg2PtMade..text) / n(), 1),
                   X2PA = round(sum(stats.Fg2PtAtt..text) / n(), 1),
                   X3PM = round(sum(stats.Fg3PtMade..text) / n(), 1), 
                   X3PA = round(sum(stats.Fg3PtAtt..text) / n(), 1),
                   FTM = round(sum(stats.FtMade..text) / n(), 1),
                   FTA = round(sum(stats.FtAtt..text) / n(), 1)) %>%
  mutate(X2PCT = round(X2PM / X2PA, 3),
         X3PCT = round(X3PM / X3PA, 3),
         FTPCT = round(FTM / FTA, 3),
         FGPCT = round((X2PM + X3PM) / (X2PA + X3PA), 3)) %>%
  gather(key = stat, value = value, -oppteam.Abbreviation) %>%
  mutate(foragainst = 'against') %>%
  setNames(c('team.Abbreviation', 'stat', 'value', 'foragainst'))

# combine into one dataframe
team.stats <- rbind(teamfor.stats, teamagainst.stats)

# 6.C compute possessions, and the 4 factors
four.factors.df <- lapply(all.teams, FUN = function(x) {
  
  # team eFG percent (for and against)
  team_2fgm <- team.stats$value[team.stats$stat == 'X2PM' & team.stats$foragainst == 'for' & team.stats$team.Abbreviation == x]
  team_3fgm <- team.stats$value[team.stats$stat == 'X3PM' & team.stats$foragainst == 'for' & team.stats$team.Abbreviation == x]
  team_fga <- team.stats$value[team.stats$stat == 'X2PA' & team.stats$foragainst == 'for' & team.stats$team.Abbreviation == x] +
                team.stats$value[team.stats$stat == 'X3PA' & team.stats$foragainst == 'for' & team.stats$team.Abbreviation == x]
  team_efg <- (team_2fgm + 1.5*team_3fgm) / (team_fga)

  opp_2fgm <- team.stats$value[team.stats$stat == 'X2PM' & team.stats$foragainst == 'against' & team.stats$team.Abbreviation == x]
  opp_3fgm <- team.stats$value[team.stats$stat == 'X3PM' & team.stats$foragainst == 'against' & team.stats$team.Abbreviation == x]
  opp_fga <- team.stats$value[team.stats$stat == 'X2PA' & team.stats$foragainst == 'against' & team.stats$team.Abbreviation == x] +
               team.stats$value[team.stats$stat == 'X3PA' & team.stats$foragainst == 'against' & team.stats$team.Abbreviation == x]
  opp_efg <- (opp_2fgm + 1.5*opp_3fgm) / (opp_fga)
  
  # team turnover percent (for and against)
  team_tov <- team.stats$value[team.stats$stat == 'TOV' & team.stats$foragainst == 'for' & team.stats$team.Abbreviation == x]
  team_fta <- team.stats$value[team.stats$stat == 'FTA' & team.stats$foragainst == 'for' & team.stats$team.Abbreviation == x]
  team_tov_pct <- team_tov / (team_fga + 0.44*team_fta + team_tov)
  
  opp_tov <- team.stats$value[team.stats$stat == 'TOV' & team.stats$foragainst == 'against' & team.stats$team.Abbreviation == x]
  opp_fta <- team.stats$value[team.stats$stat == 'FTA' & team.stats$foragainst == 'against' & team.stats$team.Abbreviation == x]
  opp_tov_pct <- opp_tov / (opp_fga + 0.44*opp_fta + opp_tov)
  
  # team offensive rebound percent (for and against)
  team_orb <- team.stats$value[team.stats$stat == 'ORB' & team.stats$foragainst == 'for' & team.stats$team.Abbreviation == x]
  opp_drb <- team.stats$value[team.stats$stat == 'DRB' & team.stats$foragainst == 'against' & team.stats$team.Abbreviation == x]
  team_orb_pct <- team_orb / (team_orb + opp_drb)
  
  opp_orb <- team.stats$value[team.stats$stat == 'ORB' & team.stats$foragainst == 'against' & team.stats$team.Abbreviation == x]
  team_drb <- team.stats$value[team.stats$stat == 'DRB' & team.stats$foragainst == 'for' & team.stats$team.Abbreviation == x]
  opp_orb_pct <- opp_orb / (opp_orb + team_drb)
  
  # team free throw rate (for and against)
  team_ftm <- team.stats$value[team.stats$stat == 'FTM' & team.stats$foragainst == 'for' & team.stats$team.Abbreviation == x]
  team_ft_pct <- team_ftm / team_fga
  
  opp_ftm <- team.stats$value[team.stats$stat == 'FTM' & team.stats$foragainst == 'against' & team.stats$team.Abbreviation == x]
  opp_ft_pct <- opp_ftm / opp_fga
  
  
  # create 8 rows to enter into the team.stats dataframe
  c('eFG%', 'ORB%', 'FTR', 'TOV%')
  four.factors.rows <- data.frame(team.Abbreviation = rep(x, 8),
                                  stat = c('eFG%', 'ORB%', 'FTR', 'TOV%', 'eFG%', 'ORB%', 'FTR', 'TOV%'),
                                  value = c(team_efg, team_orb_pct, team_ft_pct, team_tov_pct,
                                            opp_efg, opp_orb_pct, opp_ft_pct, opp_tov_pct),
                                  foragainst = c('for', 'for', 'for', 'for', 'against', 'against', 'against', 'against'),
                                  stringsAsFactors = FALSE)
  
  # team.stats <- rbind(team.stats, four.factors.rows)
  return(four.factors.rows)
}) %>%
  do.call('rbind', .)

team.stats <- rbind(team.stats, four.factors.df)

# 6.D write to CSV
write_csv(team.stats, 'NBA_teamgamelogs.csv')
# ====


# ===============================================================
# 7. BBR PLayer Stats [per game / advanced] (PlayerProfiles)
# ===============================================================

# use rvest to grab the two dataframes
bbr_pergame_url <- 'https://www.basketball-reference.com/leagues/NBA_2018_per_game.html'
pergame_page <- read_html(bbr_pergame_url)
pergame_df <- html_table(html_nodes(pergame_page, "table"), fill = TRUE)[[1]] %>%
                filter(Player != 'Player') 
players_traded <- unique(pergame_df$Player[pergame_df$Tm == 'TOT'])
pergame_df <- pergame_df %>% 
                filter(!(Player %in% players_traded) | Tm == 'TOT')

bbr_advanced_url <- 'https://www.basketball-reference.com/leagues/NBA_2018_advanced.html'
advanced_page <- read_html(bbr_advanced_url)
advanced_df <- html_table(html_nodes(advanced_page, "table"), fill = TRUE)[[1]] 
players_traded <- unique(advanced_df$Player[advanced_df$Tm == 'TOT'])
advanced_df <- advanced_df[advanced_df$Player != 'Player', names(advanced_df) != ''] %>%
  filter(!(Player %in% players_traded) | Tm == 'TOT') %>%
  dplyr::select(-one_of(c('Rk', 'Pos', 'Age', 'Tm', 'G', 'MP')))

# combine into one, fix col names, add total minutes, and write to CSV
BBR_player_df <- pergame_df %>% left_join(advanced_df, by = c('Player'='Player'))
names(BBR_player_df) <- make.names(colnames(BBR_player_df))
BBR_player_df$MPTotal <- as.integer(BBR_player_df$MP) * as.integer(BBR_player_df$G)

# convert class types
BBR_player_df <- BBR_player_df %>% type_convert()

# fix team abbreviations
BBR_player_df$Tm[BBR_player_df$Tm == 'OKC'] = 'OKL'
BBR_player_df$Tm[BBR_player_df$Tm == 'BRK'] = 'BRO'
BBR_player_df$Tm[BBR_player_df$Tm == 'PHO'] = 'PHX'
BBR_player_df$Tm[BBR_player_df$Tm == 'CHO'] = 'CHA'

write_csv(BBR_player_df, 'NBA_BBRplayerstats.csv')
# ====


# ================================================
# 8. Player Game Stats (Outstanding Performances)
# ================================================

player.stats.bygame <- read_csv('NBA_playerstats_bygame.csv')

# grab the entire NBA schedule, and all gamedates < todays date
nba1718.regular <- msf_get_results(
  league = 'nba', season = '2017-2018-regular',
  feed = 'full_game_schedule')$api_json$fullgameschedule$gameentry

# all gameids that havent happened yet
game.ids <- paste0(gsub('-', '', nba1718.regular$date), '-', nba1718.regular$awayTeam.Abbreviation, '-', nba1718.regular$homeTeam.Abbreviation)
idx1 <- as.integer(gsub('-', '', nba1718.regular$date)) < as.integer(gsub('-', '', Sys.Date()))
idx2 <- !(game.ids %in% player.stats.bygame$msf.gameID)
game.ids <- game.ids[idx1 & idx2]

if(length(game.ids) > 0) {

  # loop the daily player stats feed
  for(i in 1:length(game.ids)) {
  
    # grab this game
    this.id <- game.ids[i]; print(paste(i, this.id))
    
    # query the game stats (not yet available for some reason... ) - API key!
    game.player.stats <- msf_get_results(league = 'nba', season = '2017-2018-regular',
                                           feed = 'player_gamelogs', params = list(game = this.id))
    game.player.stats <- game.player.stats$api_json$playergamelogs$gamelogs
    
    # add msf.id and gamedate
    game.player.stats$msf.gameID <- this.id
    game.player.stats$gameDate <- as.Date(paste0(substring(this.id,1,4),'-',substring(this.id,5,6),'-',substring(this.id,7,8)))
    
    # fix column names, grab keepers, filter 
    colnames(game.player.stats) <- make.names(colnames(game.player.stats))
    keepers <- c('player.LastName', 'player.FirstName', 'player.Position', 'team.Name', 'team.Abbreviation', 
                 'stats.Pts..text', 'stats.Reb..text', 'stats.Ast..text', 'stats.Stl..text', 'stats.Blk..text',
                 'stats.Tov..text', 'stats.Fg3PtMade..text', 'msf.gameID', 'gameDate')
    newnames <- c('lastName', 'firstName', 'Pos', 'teamName', 'teamAbb', 'PTS', 'REB', 
                  'AST', 'STL', 'BLK', 'TOV', 'X3PM', 'msf.gameID', 'gameDate')
    game.player.stats <- game.player.stats %>% 
      dplyr::select(one_of(keepers)) %>%
      setnames(keepers, newnames) %>%
      mutate(fullName = paste(firstName, lastName)) %>%
      type_convert()
    
    # rbind onto the main dataframe
    player.stats.bygame <- plyr::rbind.fill(player.stats.bygame, game.player.stats)
  }
  
  write_csv(player.stats.bygame, 'NBA_playerstats_bygame.csv')
}
# ====


# ===================================================
# 9. Connect to Mongo DB and write this information
# ====

collections <- c(
  "NBA_gameplaybyplay_shotcharts",
  "NBA_gameplaybyplay_assistnet",
  "NBA_elo",
  "NBA_gameplaybyplay_gamerecaps",
  "NBA_cumulativeplayerstats",
  "NBA_teamgamelogs",
  "NBA_BBRplayerstats",
  "NBA_playerstats_bygame"
)

my.dataframes <- c(
  "shotcharts.all",
  "assistnet.all",
  "nba.elo",
  "gamerecaps.all",
  "player.data",
  "team.stats",
  "BBR_player_df", 
  "player.stats.bygame"
)

# clean up some column name issues
colnames(shotcharts.all)[colnames(shotcharts.all) == "msf.gameID"] = "msfgameID"
colnames(assistnet.all)[colnames(assistnet.all) == "msf.gameID"] = "msfgameID"
colnames(team.stats)[colnames(team.stats) == "team.Abbreviation"] = "teamAbbreviation"

# colnames(shotcharts.all)[colnames(shotcharts.all) == "msf.gameID"] = "msfgameID"


for(i in 1:8) {
  # connect to the collection (db table)
  con <- mongolite::mongo(collection = collections[i],
                          db = "intersportsgraphs")
  
  # add data to the table, thats all
  print(paste('Prev Dim: ', nrow(get(my.dataframes[i])), '  and current row count: ', con$count()))
  con$remove('{}')
  con$insert(get(my.dataframes[i]))
  con$count()
  print(paste('Updated Dim: ', dim(get(my.dataframes[i])), '  and still current row count'))
  
  # print success
  print(paste("Updated the ", collections[i], " collection in the intersportsgraphs DB"))
}


# # Count data, Read Data, and Drop the collection
# con$count()
# con$find('{}')
# con$drop()
# 
# # Print a first observation
# con$iterate()$one()


# ====


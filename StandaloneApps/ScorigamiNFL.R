
# =================================================================================
# 0. SET WD, RESET THE WORKSPACE, LOAD LIBRARIES, SOURCE FUNCTIONS, SAVE SOURCES
# =================================================================================

# set working directory, clear environment
setwd("/Users/Home/Dropbox/My_Documents/Not_Work_or_School/Sports Analytics/InteractiveSportsGraphs/")
rm(list = ls())

# for grabbing data
library(mysportsfeedsR)
library(rvest)

# for data viz
library(plotly)
library(ggplot2)

# for data manip
library(dplyr)
library(stringr)
library(readr)

# connecting to the API
.MySportsFeedsEnv <- new.env()
.MySportsFeedsEnv$data <- list(v1_0_username <- NULL,
                               v1_0_password <- NULL)
authenticate_v1_0('NicholasCanova', 'thisis1strongsportsfeedpassword')
# ====



# ============================================
# 1. GRAB ALL HISTORICAL NFL GAME SCORES
# ============================================

# # create the output dataframe that will contain all scores
# scores_df <- data.frame(Team1 = character(0), Score1 = integer(0),
#                         Team2 = character(0), Score2 = integer(0),
#                         Week = character(0), Year = integer(0))
# 
# # connect to page
# site_URL <- 'http://www.jt-sw.com/football/pro/results.nsf'
# page = read_html(site_URL)
# 
# # grab vector of all years with NFL games
# all_years = html_text(html_nodes(page, "li"), trim = TRUE)
# 
# # loop through individual years
# for(i in 1:length(all_years)) {
#   print(all_years[i])
#   year_URL <- paste('http://www.jt-sw.com/football/pro/results.nsf/By/Season?OpenDocument&Season=', all_years[i], sep = '')
#   year_page <- read_html(year_URL)
# 
#   allweeks_thisyear = html_text(html_nodes(year_page, "li"), trim = TRUE)
#   these_idx <- grep(pattern = 'Week|Wild Card Playoffs|Divisional Playoffs|Conference Championship|League Championship|Superbowl', x = allweeks_thisyear)
#   allweeks_thisyear <- allweeks_thisyear[these_idx]
# 
#   allweeks_thisyear <- trimws(gsub("Week", "", allweeks_thisyear))
# 
#   # fine tune the week label formatting
#   for(j in 1:length(allweeks_thisyear)) {
#     if(nchar(allweeks_thisyear[j]) == 1) {
#       allweeks_thisyear[j] = paste0('0', allweeks_thisyear[j])
#     }
#     if(allweeks_thisyear[j] == "Wild Card Playoffs") {
#       allweeks_thisyear[j] = 'wc'
#     }
#     if(allweeks_thisyear[j] == "Divisional Playoffs") {
#       allweeks_thisyear[j] = 'div'
#     }
#     if(allweeks_thisyear[j] == "Conference Championship") {
#       allweeks_thisyear[j] = 'conf'
#     }
#     if(allweeks_thisyear[j] == "League Championship") {
#       allweeks_thisyear[j] = 'sb'
#     }
#     if(allweeks_thisyear[j] == "Superbowl") {
#       allweeks_thisyear[j] = 'sb'
#     }
#   }
# 
#   # paste the year on front
#   end_URLs <- paste0(all_years[i], '-', allweeks_thisyear)
# 
#   # loop through individual seasons
#   for(j in 1:length(allweeks_thisyear)) {
#     week_URL <- paste0('http://www.jt-sw.com/football/pro/results.nsf/Weeks/', end_URLs[j])
#     week_page <- read_html(week_URL)
# 
#     allgames_thisweek <- html_text(html_nodes(week_page, "li"), trim = TRUE)
# 
#     week_df <- allgames_thisweek %>%
#       str_replace("(?<=\\d)\\s.*--.+$", "") %>%
#       str_replace("(?<=\\d)\\sat\\s.+$", "") %>%
#       str_replace("(?<=\\d)\\s.+$", "") %>%
#       str_replace_all("\\s(?=\\d+\\b)", ",") %>%
#       strsplit(",") %>%
#       do.call(rbind, .) %>%
#       data.frame() %>%
#       setNames(c("team1", "team1score", "team2", "team2score"))
#     week_df$Week <- allweeks_thisyear[j]
#     week_df$Year <- all_years[i]
#     week_df
# 
#     scores_df <- rbind(scores_df, week_df)
#   }
# }
# 
# # format columns
# scores_df$Year <- as.integer(scores_df$Year)
# scores_df$team1 <- as.character(scores_df$team1)
# scores_df$team2 <- as.character(scores_df$team2)
# scores_df$team1score <- as.integer(as.character(scores_df$team1score)) # warning good, converts bye weeks to NAs
# scores_df$team2score <- as.integer(as.character(scores_df$team2score))
# 
# scores_df <- scores_df[!is.na(scores_df$team1score), ]
# write_csv(scores_df, 'Data/scorigamiNFL.csv')
# ====


# ============================================
# 2. ADD 2017 & ALL NEW GAMES TO DATAFRAME 
# ============================================

# read csv
scores_df <- read_csv('Data/scorigamiNFL.csv')

# set 2 days before first game of 2017
first_tues <- as.Date('2017-09-05')

# grab NFL schedule
nfl_schedule <- msf_get_results(league = 'nfl', season = '2017-2018-regular',
                feed = 'full_game_schedule', verbose = TRUE)
nfl_schedule <- nfl_schedule$api_json$fullgameschedule$gameentry

# grab dates of games to add to scores_df
nfl_schedule$date <- as.Date(nfl_schedule$date)
nfl_schedule <- nfl_schedule[nfl_schedule$date <= Sys.Date(), ]
dates_to_add <- unique(gsub('-', '', unique(nfl_schedule$date)))

for(i in 1:length(dates_to_add)) {
  this_date <- dates_to_add[i]
  scores <- msf_get_results(league = 'nfl', season = '2017-2018-regular',
                            feed = 'scoreboard', params = list(fordate = this_date))
  scores <- scores$api_json$scoreboard$gameScore
  
  scores$team1 <- paste(scores$game.homeTeam.City, scores$game.homeTeam.Name)
  scores$team1score <- as.integer(scores$homeScore)
  scores$team2 <- paste(scores$game.awayTeam.City, scores$game.awayTeam.Name)
  scores$team2score <- as.integer(scores$awayScore)
  scores$Year <- as.integer(2017)
  scores$Week <- as.character(ceiling((as.Date(this_date, format = '%Y%m%d') - first_tues) / 7))
  scores$Week <- ifelse(nchar(scores$Week) == 1, paste0('0', scores$Week), scores$Week)
  
  scores <- scores %>% dplyr::select(one_of(c('team1', 'team1score', 'team2', 'team2score', 'Week', 'Year')))
  
  scores_df <- rbind(scores_df, scores)
}

# add order col to scores_df
scores_df$order = 1:nrow(scores_df)
scores_df = scores_df[order(-scores_df$order), ]
# ====


# ============================================
# 3. COMPUTE SCORIGAMI GRID AND LABELS
# ============================================

nrows <- 73
scorigami_df <- expand.grid(y = 0:nrows, x = 0:nrows)

# add new columns
scorigami_df$maxminusy <- (nrows-1) - scorigami_df$y
scorigami_df$category <- ""
scorigami_df$winteam <- ""
scorigami_df$loseteam <- ""
scorigami_df$week <- ""
scorigami_df$year <- NA

# loop scores_df, check each score in the scores_df
for(i in 1:nrow(scores_df)) {

  # did team1 win?  
  t1win <- scores_df$team1score[i] > scores_df$team2score[i]
  
  # x is the winning team's score, y is the losing team's score
  this_x <- ifelse(t1win, scores_df$team1score[i], scores_df$team2score[i])
  this_y <- ifelse(!t1win, scores_df$team1score[i], scores_df$team2score[i])
  
  # grab and update scorigami row
  scorigami_row <- scorigami_df[scorigami_df$x == this_x & scorigami_df$y == this_y, ]
  scorigami_row$winteam <- ifelse(t1win, scores_df$team1[i], scores_df$team2[i])
  scorigami_row$loseteam <- ifelse(!t1win, scores_df$team1[i], scores_df$team2[i])
  scorigami_row$week = scores_df$Week[i]
  scorigami_row$year = scores_df$Year[i]
  scorigami_row$category = "Yes"

  # add back to the scorigami dataframe
  scorigami_df[scorigami_df$x == this_x & scorigami_df$y == this_y, ] <- scorigami_row
}

# check scorigami output
scorigami_df$category <- ifelse(scorigami_df$y > scorigami_df$x, "Black", scorigami_df$category)
scorigami_df$category[scorigami_df$category == ""] = "No"
head(scorigami_df[scorigami_df$category == 'Yes', ])

# add black for a few additional scores
scorigami_df$category[scorigami_df$x %in% c(0:5,7) & scorigami_df$y == 1] = "Black"
scorigami_df$category[scorigami_df$x == 1 & scorigami_df$y == 0] = "Black"

# create the colors
min_year <- min(as.integer(scorigami_df$year), na.rm = TRUE)
max_year <- max(as.integer(scorigami_df$year), na.rm = TRUE)

# compute categoryNumber for color of box
scorigami_df$catNum <- 0
scorigami_df$catNum[scorigami_df$category == "No"] <- 3
scorigami_df$catNum[scorigami_df$category == "Black"] <- 4

zed = scorigami_df$year[scorigami_df$category == "Yes"]
box_colors <- (((zed - min_year) / (max_year - min_year)) / 2) + 2
scorigami_df$catNum[scorigami_df$category == "Yes"] <- box_colors
scorigami_df$catNum[which(scorigami_df$year == 2017)] <- 1
# ====


# ============================================
# 4. PLOT SCORIGAMI GRID
# ============================================

# create zed matrix, then modify
zed <- matrix(data = scorigami_df$catNum, nrow = 74, ncol = 74)
plot_text <- matrix(data = paste('Score: ', scorigami_df$x, '-', scorigami_df$y, '\n',
                                 'Winner: ', scorigami_df$winteam, '\n', 
                                 'Loser: ', scorigami_df$loseteam, '\n',
                                 'Year: ', scorigami_df$year, '\n',
                                 'Week: ', scorigami_df$week, '\n'),
                    nrow = 74, ncol = 74)

ax <- list(
  tickmode = "array",
  tickvals = 0:73,
  # mirror = TRUE,
  zeroline = FALSE,
  ticks = "",
  side = 'top',
  title = ""
)

ay <- list(
  ticks = "",
  title = "",
  zeroline = FALSE,
  showticklabels = FALSE,
  autorange = "reversed"
) 

myshapes <- list()
for(i in 0:74) {
  # horizontal lines
  this_list <- list(type = 'line', fillcolor = 'black', 
                    x0 = -0.5, x1 = 73.5,
                    y0 = i-0.5, y1 = i-0.5, 
                    line = list(width = 1, color = 'rgb(50, 50, 50)'))
  myshapes[[length(myshapes)+1]] <- this_list
  
  # vertical lines
  this_list <- list(type = 'line', fillcolor = 'black', 
                    x0 = i-0.5, x1 = i-0.5,
                    y0 = -0.5, y1 = 73.5, 
                    line = list(width = 1, color = 'rgb(50, 50, 50)'))
  myshapes[[length(myshapes)+1]] <- this_list
}

plot_ly(z = zed, 
        type = 'heatmap', 
        colors = colorRamp(c("green3", "blue3", "white", "black")),
        showscale = FALSE,
        hoverinfo = 'text',
        text = plot_text) %>%

  add_annotations(x = 0:72,
                  y = 1:73,
                  text = as.character(1:73),
                  font = list(color = 'white',
                              size = 10),
                  showarrow = FALSE) %>%
  
  layout(xaxis = ax,
         yaxis = ay,
         shapes = myshapes)

# ====



## Plot
# myplot <- ggplot(scorigami_df, aes(x = x, y = y, fill = category)) + 
#   geom_tile(color = "black", size = 0.2) +
#   
#   # display correct tick marks
#   scale_x_continuous(position = 'top', breaks = 0:73) +
#   scale_y_reverse() +
#   scale_fill_manual(values = c("gray5", "white", "green3")) +
#   
#   # annotate diagonal, add labels
#   annotate('text', x = 0:72, y = 1:73, label = as.character(1:73), color = 'white', size = 2.65) +
#   labs(x = 'Points Scored by Winner', y = 'Points Scored by Loser') + 
#   
#   theme(legend.position = 'none',
#         axis.ticks = element_blank(),
#         axis.title.y = element_text(vjust = -20, hjust = 0.91),
#         axis.title.x = element_text(vjust = 5, hjust = 0.08),
#         axis.text.y = element_blank(),
#         axis.text.x.top = element_text(margin = margin(b = -28, unit = "pt")),
#         panel.background = element_rect(fill = 'white'))
# 
# # running ggplotly on this takes quite a while... consider re-plotting using plot_ly.
# ggplotly(myplot)
# 


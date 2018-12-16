
# That popular CFB imperialism map
# https://cfb.stakked.io/
# https://mapchart.net/
# https://docs.google.com/document/d/15e76nV1TNb5ef22Q2y7j3rfIq3D7L2L02tmJFjCSqVE/edit

# http://rmaps.github.io/

# =================================================================================
# 0. SET WD, RESET THE WORKSPACE, LOAD LIBRARIES, SOURCE FUNCTIONS, SAVE SOURCES
# =================================================================================

# set working directory, clear environment
setwd("/Users/Home/Dropbox/My_Documents/Not_Work_or_School/Sports Analytics/InteractiveSportsGraphs/")
rm(list = ls())

# for grabbing data
library(geojsonio)
library(mysportsfeedsR)
library(httr)
library(jsonlite)

# for data viz
library(plotly)
library(ggplot2)
library(maps)

# for data manip
library(dplyr)

# set mapbox environment
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoibmlja2Nhbm92YSIsImEiOiJjajhtZG93MzAweTRsMzNtenZucTEwZHlzIn0.WZXiIRam0L4RfnfBpoYZ2A')

# connecting to the API
.MySportsFeedsEnv <- new.env()
.MySportsFeedsEnv$data <- list(v1_0_username <- NULL,
                               v1_0_password <- NULL)
authenticate_v1_0('NicholasCanova', 'thisis1strongsportsfeedpassword')

# ====


# ==================================
# 1. CREATE THE DATA FOR THE MAPS
# ==================================

# 1.A Grab the initial county, city, and nba city data
# ===-===-===-===-===-===-===-===-===-===-===-===-===-===
# map_data("world", "canada") %>%
county.data <- map(database = "county", fill = TRUE, plot = FALSE) %>%
  map_data()

# grab the 27 NBA cities (2 LA, 2 NY, 1 Canada)
city.data <- maps::us.cities
nba.city.data <- city.data %>%
  filter(name %in% c("Atlanta GA", "Boston MA", "Charlotte NC", "Chicago IL", "Cleveland OH", 
                     "Dallas TX", "Denver CO", "Detroit MI", "Houston TX", "Indianapolis IN", 
                     "Los Angeles CA", "Memphis TN", "Miami FL", "Milwaukee WI", "Minneapolis MN", 
                     "New Orleans LA", "New York NY", "Oakland CA", "Oklahoma City OK", "Orlando FL", 
                     "Philadelphia PA", "Phoenix AZ", "Portland OR", "Sacramento CA", "Salt Lake City UT", 
                     "San Antonio TX", "WASHINGTON DC")) %>%
  arrange(name)


# 1.B add a landowner column to the nba.city.data df
# ===-===-===-===-===-===-===-===-===-===-===-===-===-===
nba.city.data$landowner <- c("ATL", "BOS", "CHA", "CHI", "CLE", 
                             "DAL", "DEN", "DET", "HOU", "IND", 
                             "LA?", "MEM", "MIA", "MIL", "MIN", 
                             "NOP", "NY?", "GSW", "OKL", "ORL", 
                             "PHI", "PHX", "POR", "SAC", "UTA", 
                             "SAS", "WAS")

nba.city.data$landowner[11] = ifelse(runif(1) < 0.5, "LAL", "LAC")
nba.city.data$landowner[17] = "NYK" # ifelse(runif(1) < 0.5, "NYK", "BRO")


# 1.C create the color / image mapping df
# ===-===-===-===-===-===-===-===-===-===-===
colormap <- data.frame(teamID = c("ATL", "BOS", "BRO", "CHA", "CHI", 
                                  "CLE", "DAL", "DEN", "DET", "HOU", 
                                  "IND", "LAC", "LAL", "MEM", "MIA", 
                                  "MIL", "MIN", "NOP", "NYK", "GSW", 
                                  "OKL", "ORL", "PHI", "PHX", "POR", 
                                  "SAC", "TOR", "UTA", "SAS", "WAS"),
                       colorHex = c('#E13A3E', '#28B463', '#061922', '#008CA8', '#CE1141',
                                    '#860038', '#007DC5', '#4D90CD', '#ED174C', '#CE1141',
                                    '#00275D', '#ED174C', '#552582', '#0F586C', '#F9A01B',
                                    '#00471B', '#005083', '#B4975A', '#DC7633', '#FDB927',
                                    '#007DC3', '#C4CED3', '#ED174C', '#E56020', '#E03A3E',
                                    '#724C9F', '#B4975A', '#002B5C', '#BAC3C9', '#002B5C'),
                       colorName = c('gray30', 'green3', 'gray10', 'darkcyan', 'red3',
                                    'firebrick4', 'dodgerblue2', 'deepskyblue3', 'red', 'red',
                                    'yellow1', 'red', 'purple', 'cadetblue4', 'darkorange',
                                    'darkgreen', 'springgreen3', 'goldenrod4', 'orangered', 'gold1',
                                    'royalblue2', 'royalblue3', 'royalblue3', 'darkorange3', 'gray20',
                                    'darkorchid4', 'gray65', 'orange', 'gray65', 'blue3'),
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


# 1.D find nearest city for each coordinate - find center of each city as well
# ===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===-===
county.grouped = county.data %>% 
  group_by(group) %>%
  summarize(meanlong = mean(long),
            meanlat = mean(lat))

# add landowning team and color to county.grouped
nearestcity <- apply(county.grouped, 1, FUN = function(x) {
                 thisdist = (as.numeric(x[2]) - nba.city.data$long)^2 +
                            (as.numeric(x[3]) - nba.city.data$lat)^2
                 thisdist = which.min(sqrt(thisdist))
               })
county.grouped$landowner <- nba.city.data$landowner[nearestcity]
 
# get center of county region (diff than city center), add to nba.city.data
region.centers <- county.grouped %>% 
  group_by(landowner) %>%
  summarize(regionlong = round(mean(meanlong),2),
            regionlat = round(mean(meanlat),2))
nba.city.data <- nba.city.data %>% left_join(region.centers, by = c('landowner'='landowner'))

# add these onto county.data
county.data <- county.data %>%
                 left_join(county.grouped[, c(1,4)], by = c('group'='group')) %>%
                 left_join(colormap, by = c('landowner'='teamID')) %>%
                 mutate(landowner = as.factor(landowner))

colorrange = c()
a = levels(county.data$landowner)
for(i in 1:length(a)) {
  this_col = colormap$colorName[colormap$teamID == a[i]]
  colorrange = c(colorrange, this_col)
}

# create image list
image_list = apply(nba.city.data, 1, FUN = function(x) {
  this_team = x[7]
  list(source = colormap$imageURLs[colormap$teamID == this_team],
       xref = 'x', yref = 'y',
       x = as.numeric(x[8])-1,
       y = as.numeric(x[9])+1,
       sizex = 2.5, sizey = 2.5,
       opacity = 1, layer = 'above')  
})
# ====


# ==================================
# 2. CREATE THE MAPS
# ==================================

# create geo
geo <- list(
  scope = 'usa',
  showland = TRUE,
  landcolor = toRGB("gray95"),
  countrycolor = toRGB("gray80")
)

ax <- list(
  showgrid = FALSE,
  title = "",
  showticklabels = FALSE
)

ay <- list(
  showgrid = FALSE,
  title = "",
  showticklabels = FALSE
)

# create the plot with the colors
p1 <- county.data %>%
        group_by(group) %>%
        plot_ly(x = ~long, y = ~lat,
                color = ~landowner, colors = colorrange) %>%
        add_polygons(line = list(width = 0.4),
                     opacity = 1) %>%
        add_polygons(fillcolor = 'transparent', 
                     line = list(color = 'black', width = 0.5),
                     showlegend = FALSE,
                     ids = ~landowner) %>%
        layout(title = 'NBA Imperialism Map',
               titlefont = list(size = 16,
                                color = 'blue4'),
               plot_bgcolor = 'gray90',
               showlegend = FALSE,
               hovermode = 'closest',
               geo = geo,
               images = image_list,
               xaxis = ax,
               yaxis = ay)

p1

# ====


# ==================================
# 3. OTHER MAPS
# ==================================
ggplot(mtcars, aes(x = cyl, y = mpg, fill = factor(cyl))) + 
  geom_bar(stat = 'identity') + 
  scale_fill_manual('legend', values = c('red3', 'blue3', 'green3'))


p2 <- county.data %>%
  group_by(group) %>%
  plot_geo(x = ~long, y = ~lat,
           color = ~landowner, colors = colorrange,
           text = ~landowner, hoverinfo = "text") %>%
  add_polygons(line = list(width = 0.4)) %>%
  layout(showlegend = FALSE,
         title = "Land Owned by Each Team",
         geo = geo)    




# Figure 2.20: Three different ways to render a map. On the top left is plotly’s default 
#  cartesian coordinate system, on the top right is plotly’s custom geographic layout, 
# and on the bottom is mapbox.
dat <- map_data("county") %>% 
  mutate(mycol = sample(c(1,2,3,4), nrow(map_data("county")), replace = TRUE)) %>%
  group_by(group)

# grabbing dataframe with the map_data option seems good
map1 <- plot_ly(dat, x = ~long, y = ~lat) %>% 
  add_paths(size = I(1)) %>%
  # add_polygons(hoverinfo = "text", text = ~region) %>%
  add_segments(x = -120, xend = -100, y = 30, 50)

# using plot_mapbox seems a lot tougher
map2 <- plot_mapbox(dat, x = ~long, y = ~lat) %>% 
  add_paths(size = I(2)) %>%
  add_segments(x = -120, xend = -100, y = 30, 40) %>%
  add_polygons(color = ~factor(mycol), colors = c('#000000', '#BBBBBB', '#DDDDDD', '#FF3311')) %>%
  layout(mapbox = list(zoom = 2,
                       center = list(lat = ~median(lat), lon = ~median(long))
  ))

# geo() is the only object type which supports different map projections
map3 <- plot_geo(dat, x = ~long, y = ~lat) %>% 
  add_markers(size = I(1)) %>%
  add_segments(x = -100, xend = -50, y = 50, 75) %>%
  layout(geo = list(projection = list(type = "mercator")))

subplot(map1, map2) %>%
  subplot(map3, nrows = 2) %>% 
  hide_legend()



# Figure 2.21: A map of U.S. population density using the state.x77 data from the datasets package.
density <- state.x77[, "Population"] / state.x77[, "Area"]

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white')
)

plot_geo() %>%
  add_trace(
    z = ~density, text = state.name,
    locations = state.abb, locationmode = 'USA-states'
  ) %>%
  add_markers(
    x = state.center[["x"]], y = state.center[["y"]], 
    size = I(2), symbol = I(8), color = I("white"), hoverinfo = "none"
  ) %>%
  layout(geo = g)
# ====
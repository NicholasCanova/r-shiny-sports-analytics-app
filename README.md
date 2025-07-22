# R Shiny Sports Analytics App

An interactive R Shiny application for visualizing NBA data with multiple analytics features.

## Features

- **Shot Charts**: Heat maps and marker charts for players and teams
- **Player Comparisons**: Scatter plots comparing player statistics
- **Team Comparisons**: Team performance analytics and custom chart creation
- **Game Recaps**: In-game win probability and lead tracking
- **ELO Ratings**: Historical team strength ratings over time
- **Assist Networks**: Network graphs showing player assist patterns
- **Four Factors**: Dean Oliver's basketball success metrics
- **Player Percentiles**: Statistical percentile rankings across 11 metrics
- **Outstanding Performances**: Bubble histograms of exceptional individual games

## Installation & Setup

1. **Install Required Libraries**
   ```r
   install.packages(c("shiny", "shinythemes", "plotly", "readr", "dplyr", "tidyr", "DT", "igraph", "ggnetwork", "network", "sna", "ggplot2", "RCurl"))
   ```

2. **Run the Application**
   ```r
   setwd('path/to/r-shiny-sports-analytics-app/')
   shiny::runApp()
   ```

## Screenshots

<img src="https://github.com/user-attachments/assets/302ad5f4-169c-4bd9-a530-1675704b36fa" width="500" alt="Shot Charts">
<img src="https://github.com/user-attachments/assets/83663602-666f-4f56-ace8-e4a8754d9c4d" width="500" alt="Player Comparisons">

<img src="https://github.com/user-attachments/assets/c5a8cbf0-287d-479d-8c29-5381a70d28b1" width="535" alt="Team Analytics">
<img src="https://github.com/user-attachments/assets/7541359f-ca20-4c9c-8e38-ae68baab1359" width="450" alt="Game Recaps">


## Data Sources

The app uses NBA data from various sources including:
- MySportsFeeds API
- Basketball Reference
- Historical ELO ratings

## License

This project is open source and available under the [MIT License](LICENSE).

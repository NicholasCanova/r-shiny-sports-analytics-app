
## ======================================================== ##
# Copyright (C) 2017 Nicholas Canova
# 
# This launches the server side code for the 
# ERO Ratings charts tool
## ======================================================== ##

# the logo
output$elo.logo <- renderImage({
  filename <- 'Images/nbalogo.png'
  
  this.width <- session$clientData$output_elo.logo_width
  this.height <- this.width / (1250 / 588) # the file ratio
  
  # Return a list containing the filename
  list(src = filename,
       width = this.width,
       height = this.height)
}, deleteFile = FALSE)

# the ghostplot
output$elo.ghostplot <- renderPlot({
  df <- data.frame()
  ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
})

# and do the plot
output$eloratings <- renderPlotly({
  
  window.width <- session$clientData$output_elo.ghostplot_width
  this.id <- input$elo.team.input
  drawELORatings(nba.elo, this.id, color.pal.df, window.width)
  
})

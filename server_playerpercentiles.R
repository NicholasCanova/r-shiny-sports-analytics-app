

## ======================================================== ##
# Copyright (C) 2017 Nicholas Canova
# 
# This launches the server side code for the 
# Player Percentiles chart tool
## ======================================================== ##

# the logo
output$pp.logo <- renderImage({
  filename <- 'Images/teamlogos.jpg'
  
  this.width <- session$clientData$output_sc.logo_width
  this.height <- this.width / (1365 / 1024) # the file ratio
  
  # Return a list containing the filename
  list(src = filename,
       width = this.width,
       height = this.height)
}, deleteFile = FALSE)

# the ghostplot
output$pp.ghostplot <- renderPlot({
  df <- data.frame()
  ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
})

# and do the plot
output$playerpercentiles <- renderPlotly({
  
  req(input$pp.player.input)

  window.width <- session$clientData$output_pp.ghostplot_width
  this.player <- input$pp.player.input
  print(this.player)
  this.pos <- player.stats.bbr$Pos[player.stats.bbr$Player == this.player]
  mp.threshold <- 200
  
  # window.width = 800
  # this.player = 'Stephen Curry'
  # this.pos = 'PG'
  # mp.threshold = 200 
  # plot.width = 800 
  # plot.height = 600
  
  drawPlayerPercentilesChart(player.stats.bbr, this.player, this.pos, mp.threshold, window.width)
  
})

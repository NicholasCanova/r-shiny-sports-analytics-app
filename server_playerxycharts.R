
## ======================================================== ##
# Copyright (C) 2017 Nicholas Canova
# 
# This launches the server side code for the 
# player xy charts tool
## ======================================================== ##

# the logo
output$pxy.logo <- renderImage({
  # When input$n is 1, filename is ./images/image1.jpeg
  filename <- 'Images/teamlogos.jpg'
  
  this.width <- session$clientData$output_sc.logo_width
  this.height <- this.width / (1365 / 1024) # the file ratio
  
  # Return a list containing the filename
  list(src = filename,
       width = this.width,
       height = this.height)
}, deleteFile = FALSE)

# the ghostplot
output$pxy.ghostplot <- renderPlot({
  df <- data.frame()
  ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
})

# 3. Create the chart
# ===-===-===-===-===-===
output$playerxy <- renderPlotly({
  
  # set the parameters to pass to the charting function
  window.width <- session$clientData$output_pxy.ghostplot_width
  chart.type <- input$pxy.chart.input
  player.pos <- input$pxy.position.input
  player.team <- input$pxy.team.input
  player.name <- input$pxy.player.input
  
  drawPlayerXYChart(player.stats, chart.type, player.name, player.pos, player.team, color.pal.df, window.width)
})


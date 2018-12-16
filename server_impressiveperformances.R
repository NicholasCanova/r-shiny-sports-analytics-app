
## ======================================================== ##
# Copyright (C) 2017 Nicholas Canova
# 
# This launches the server side code for the 
# Impressive Performances chart tool
## ======================================================== ##

# the logo
output$ip.logo <- renderImage({
  filename <- 'Images/nbalogo.png'
  
  this.width <- session$clientData$output_ip.logo_width
  this.height <- this.width / (1250 / 588) # the file ratio
  
  # Return a list containing the filename
  list(src = filename,
       width = this.width,
       height = this.height)
}, deleteFile = FALSE)

# the ghostplot
output$ip.ghostplot <- renderPlot({
  df <- data.frame()
  ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
})

# and do the plot
output$impressiveperformances <- renderPlotly({
  
  window.width <- session$clientData$output_ip.ghostplot_width
  chart.type <- input$ip.chart.input

  drawImpressivePerformancesChart(player.stats.bygame, chart.type, color.pal.df, window.width)[[1]]
})

# the table counting the performances
output$ip.table <- renderDataTable({

  window.width <- session$clientData$output_ip.ghostplot_width
  chart.type <- input$ip.chart.input

  drawImpressivePerformancesChart(player.stats.bygame, chart.type, color.pal.df, window.width)[[2]]
})
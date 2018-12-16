
## ======================================================== ##
# Copyright (C) 2017 Nicholas Canova
# 
# This launches the server side code for the 
# Game Recaps charts tool
## ======================================================== ##

# the logo
output$gr.logo <- renderImage({
  # When input$n is 1, filename is ./images/image1.jpeg
  filename <- 'Images/nbalogo.png'
  
  this.width <- session$clientData$output_gr.logo_width
  this.height <- this.width / (1250 / 588) # the file ratio
  
  # Return a list containing the filename
  list(src = filename,
       width = this.width,
       height = this.height)
}, deleteFile = FALSE)

# the ghostplot
output$gr.ghostplot <- renderPlot({
  df <- data.frame()
  ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
})

# the reactive UI for game IDs
output$msfid.input <- renderUI({
  
  req(input$team.input)
  
  team.input <- input$team.input
  print(team.input)
  all.gameids <- sort(unique(gamerecaps.all$msfgameid[gamerecaps.all$aID == team.input | gamerecaps.all$hID == team.input]), decreasing = TRUE)
  print(all.gameids)
  
  selectInput(inputId = 'this.msfid', label = 'Select Game ID:', multiple = FALSE,
              choices = all.gameids, selected = all.gameids[1])
})

# the win probability line graph
output$winprobplot <- renderPlotly({
  
  req(input$this.msfid)
  this.msfid <- input$this.msfid
  window.width <- session$clientData$output_gr.ghostplot_width
  drawGameRecapIGWPChart(gamerecaps.all, this.msfid, color.pal.df, window.width)
  
})

# the leads bar plot
output$leadbarplot <- renderPlotly({
  
  req(input$this.msfid)
  this.msfid <- input$this.msfid
  window.width <- session$clientData$output_gr.ghostplot_width
  drawGameRecapLeadBarsChart(gamerecaps.all, this.msfid, color.pal.df, window.width)
  
})
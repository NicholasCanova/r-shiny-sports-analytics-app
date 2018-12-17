
## ======================================================== ##
# Copyright (C) 2017 Nicholas Canova
# 
# This launches the server side code for the 
# player xy charts tool
## ======================================================== ##

# the logo
output$txy.logo <- renderImage({
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
output$txy.ghostplot <- renderPlot({
  df <- data.frame()
  ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
})

# initialize the reactive UI for X, Y values
# ========================================
# select player for player-season graph
output$txy.xaxisstat.input <- renderUI({
  if(input$charttype.input == 'Create Your Own Chart') {
    
    xaxis.options <- unique(team.stats$stat)
    names(xaxis.options) <- xaxis.options
    
    selectInput(inputId = 'xaxis.id', label = 'X statistic', multiple = FALSE,
                choices = xaxis.options, selected = 'PTS')
    
  } else{
    return(NULL)
  }
})

output$txy.xaxisforagainst.input <- renderUI({
  if(input$charttype.input == 'Create Your Own Chart') {
    
    xaxis.faoptions <- c('for', 'against')
    names(xaxis.faoptions) <- xaxis.faoptions
    
    selectInput(inputId = 'xaxis.fa', label = 'for / against', multiple = FALSE,
                choices = xaxis.faoptions, selected = 'for')
    
  } else{
    return(NULL)
  }
})

# select team for team-game marker chart
output$txy.yaxisstat.input <- renderUI({
  if(input$charttype.input == 'Create Your Own Chart') {
    
    yaxis.options <- unique(team.stats$stat)
    names(yaxis.options) <- yaxis.options
    
    selectInput(inputId = 'yaxis.id', label = 'Y statistic', multiple = FALSE,
                choices = yaxis.options, selected = 'PTS')
    
  } else{
    return(NULL)
  }
})

output$txy.yaxisforagainst.input <- renderUI({
  if(input$charttype.input == 'Create Your Own Chart') {
    
    yaxis.faoptions <- c('for', 'against')
    names(yaxis.faoptions) <- yaxis.faoptions
    
    selectInput(inputId = 'yaxis.fa', label = 'for / against', multiple = FALSE,
                choices = yaxis.faoptions, selected = 'against')
    
  } else{
    return(NULL)
  }
})
# ====

# create the team logo plots 
# =============================
output$teamxy <- renderPlotly({
  
  window.width <- session$clientData$output_txy.ghostplot_width
  chart.type <- input$charttype.input
  
  if(chart.type == 'Create Your Own Chart') {
    
    req(input$xaxis.id); req(input$yaxis.id);
    req(input$xaxis.fa); req(input$xaxis.fa);
    
    xaxis.id <- input$xaxis.id
    yaxis.id <- input$yaxis.id
    xaxis.fa <- input$xaxis.fa
    yaxis.fa <- input$yaxis.fa
    
    drawTeamXYChart(team.stats, chart.type, window.width, xaxis.id, yaxis.id, xaxis.fa, yaxis.fa)
  }
  else { 
    drawTeamXYChart(team.stats, chart.type, window.width)
  }
  

  # ====
})

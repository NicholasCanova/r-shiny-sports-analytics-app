
## ======================================================== ##
# Copyright (C) 2017 Nicholas Canova
# 
# This launches the server side code for the 
# player xy charts tool
## ======================================================== ##

# the logo
output$ff.logo <- renderImage({
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
output$ff.ghostplot <- renderPlot({
  df <- data.frame()
  ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100)
})


# the reactive UI based on singleteam or not
output$ff.teamid <- renderUI({
  if(input$ff.singleteamindicator.input == TRUE) {
    
    selectizeInput("ff.team.input", label = "Select Team:", choices = ff.teams, 
                   selected = 'GSW', multiple = FALSE)
  } else{
    return(NULL)
  }
})

output$ff.teamids <- renderUI({
  if(input$ff.singleteamindicator.input == FALSE) {
    
    selectizeInput("ff.teams.input", label = "Select Team(s):", choices = ff.teams, 
                   selected = 'GSW', multiple = TRUE, options = list(maxItems = 4))
  } else{
    return(NULL)
  }
})


# the four factors chart 
output$fourfactors <- renderPlotly({

  # set the input values
  req(input$ff.singleteamindicator.input); # req(input$ff.checkbox)
  
  single_team_evaluation <- input$ff.singleteamindicator.input
  window.width <- session$clientData$output_ff.ghostplot_width
  show.labels <- input$ff.checkbox
  
  print(input$ff.checkbox)
  print(show.labels)
  
  if(single_team_evaluation == TRUE) {
    req(input$ff.team.input)
    team.ids <- input$ff.team.input
  } else {
    req(input$ff.teams.input)
    team.ids <- input$ff.teams.input
  }

  # call the chart
  drawFourFactorsChart(team.stats, team.ids, single_team_evaluation, show.labels, color.pal.df, window.width)
})
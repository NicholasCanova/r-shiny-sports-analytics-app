
## ======================================================== ##
# Copyright (C) 2017 Nicholas Canova
# 
# This launches the server side code for the 
# assist network charting tool
## ======================================================== ##

# the logo
output$an.logo <- renderImage({
  filename <- 'Images/nbalogo.png'
  
  this.width <- session$clientData$output_an.logo_width
  this.height <- this.width / (1250 / 588) # the file ratio
  
  # Return a list containing the filename
  list(src = filename,
       width = this.width,
       height = this.height)
}, deleteFile = FALSE)

# create selectInput based on team / number of nodes selected, 
output$an.player.highlight <- renderUI({
  
  node.count <- input$an.count.input
  team.id <- input$an.team.input
  
  these.players <- highlightOptions(assist.pbp, team.id, node.count)
  names(these.players) <- these.players
  
  selectInput(inputId = 'player.id', label = 'Player to Highlight:', multiple = FALSE,
              choices = these.players, selected = these.players[1])
})

# 3. Create the chart
# ===-===-===-===-===-===
output$assistnet <- renderPlot({
  
  window.width <- session$clientData$output_assistnet_width
  
  # everything tied neatly into the drawAssistNetwork function
  drawAssistNetwork(assist.pbp, input$an.team.input, input$an.count.input, window.width, input$player.id)[[1]]
}, height = function() { session$clientData$output_assistnet_width })

output$assisttable <- renderTable({
  
  window.width <- session$clientData$output_assistnet_width
  
  assist.df <- drawAssistNetwork(assist.pbp, input$an.team.input, input$an.count.input, window.width, input$player.id)[[2]] %>%
    dplyr::select(-one_of(c('x', 'y', 'xend', 'yend', 'edgeweight', 'plotname', 'nodecolors', 'edgecolors'))) %>%
    filter(!is.na(assistcount)) %>%
    setNames(c('Passer', 'Scorer', 'Assists')) %>%
    arrange(-Assists)
})
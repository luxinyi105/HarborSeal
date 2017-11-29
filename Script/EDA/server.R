library(geosphere)
library(leaflet)
library(shiny)
load('seals.Rdata')
source('functions.R')

id.ls <- c('PV98TUG01', 'PV98TUG02', 'PV98TUG04', 'PV98TUG10', 'PV98TUG11', 'PV98TUG13', 
           'PV98TUG14', 'PV98TUG18')

server <- function(input, output){
  ## Select data
  selectedData <- reactive({
    if (input$cat == 'All') selectedData <- seals
    else selectedData <- seals[seals$LocationQuality == input$cat, ]
    
    if (input$id != 'All') selectedData <- selectedData[selectedData$DeployID == input$id, ]
    
    if (input$satellite != 'All') selectedData <- selectedData[selectedData$Satellite == input$satellite, ]
    
    selectedData
    # dateFrom <- as.POSIXct(as.Date(paste(as.character(1900 + input$year), '-', as.character(input$month), '-', as.character(input$day), sep = '')), tz = 'UTC')
    # dateTo <- dateFrom + 24*3600*30*3
    # selectedData[(selectedData$Date > dateFrom) & (selectedData$Date < dateTo), ]
  })
  
  
  
  ## Map
  output$plot1 <- renderLeaflet({
    pal <- colorFactor(palette = topo.colors(length(unique(selectedData()$DeployID))) , levels = unique(selectedData()$DeployID))
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = selectedData(), lng = ~Longitude, lat = ~Latitude,
                       color = ~pal(DeployID), radius = 3, stroke = FALSE, fillOpacity = 1) %>%
      addLegend('bottomright', pal = pal, values = unique(selectedData()$DeployID))
  })
  
  ## Location difference
  library(ggplot2)
  output$plot2 <- renderPlot({
    ggplot(selectedData(), aes(x = dx, y = dy, colour = factor(col))) +
      theme_classic() +
      geom_point(alpha = 0.75) +
      scale_colour_manual(values = seq(1, 8), labels = id.ls) + 
      theme(legend.title = element_blank()) + 
      xlim(-2*10^5, 2*10^5) +
      ylim(-10^5, 10^5) +
      geom_hline(yintercept = 0) +
      geom_vline(xintercept = 0) +
      labs(x = 'Distance in Longitude', y = 'Distance in Latitude')
  })
  
  # dates <- eventReactive(input$satellite, {
  #   if (input$satellite != 'All') dfc[dfc$Satellite == input$satellite, ]$Date
  #   else dfc$Date
  # })
  # 
  # output$plot3 <- renderPlot({
  #   dates <- as.Date(dates())
  #   plot(table(dates), xlab = 'Date', ylab = 'Counts')
  # })
  # 
  # ## Trajectory
  # library(ggplot2)
  # dfc.id <- reactive({
  #   if (input$id == 'All') dfc.id <- dfc
  #   else dfc.id <- dfc[dfc$DeployID == input$id, ]
  # })
  # 
  # output$plot4 <- renderPlot({
  #   ggplot(selectedData(), aes(x = Date, y = Longitude, shape = LocationQuality, color = DeployID)) +
  #     geom_point() + 
  #     coord_cartesian(ylim = c(min(dfc.id()$Longitude), max(dfc.id()$Longitude)))
  # })
  # 
  # output$plot5 <- renderPlot({
  #   ggplot(selectedData(), aes(x = Date, y = Latitude, shape = LocationQuality, color = DeployID)) +
  #     geom_point() +
  #     coord_cartesian(ylim = c(min(dfc.id()$Latitude), max(dfc.id()$Latitude)))
  # })
  
}

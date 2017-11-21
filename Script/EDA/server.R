server <- function(input, output){
  library(geosphere)
  library(leaflet)
  library(shiny)
  load('dfc.Rdata')
  source('functions.R')
  
  ## Select data
  selectedData <- reactive({
    if (input$cat == 'All') selectedData <- dfc
    else selectedData <- dfc[dfc$LocationQuality == input$cat, ]
    
    if (input$id != 'All') selectedData <- selectedData[selectedData$DeployID == input$id, ]
    
    if (input$satellite != 'All') selectedData <- selectedData[selectedData$Satellite == input$satellite, ]
    
    dateFrom <- as.POSIXct(as.Date(paste(as.character(1900 + input$year), '-', as.character(input$month), '-', as.character(input$day), sep = '')), tz = 'UTC')
    dateTo <- dateFrom + 24*3600*14
    selectedData[(selectedData$Date > dateFrom) & (selectedData$Date < dateTo), ]
    # selectedData
  })
  
  
  
  ## Map
  output$plot1 <- renderLeaflet({
    pal <- colorFactor(palette = topo.colors(length(unique(selectedData()$DeployID))) , levels = unique(selectedData()$DeployID))
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = selectedData(), lng = ~Longitude, lat = ~Latitude, 
                       color = ~pal(DeployID), radius = 3, stroke = FALSE, fillOpacity = 0.8)
  })
  
  # ## Location difference
  # output$plot2 <- renderPlot({
  #   lon <- selectedData()$Longitude
  #   lat <- selectedData()$Latitude
  #   k <- length(lon)
  #   dx <- rep(NA, k*(k - 1)/2)
  #   dy <- rep(NA, k*(k - 1)/2)
  #   ct <- 1
  #   for (i in 1:(k - 1)){
  #     for (j in (i + 1):k){
  #       hd <- get_hd(c(lon[i], lon[j]), c(lat[i], lat[j]))
  #       dx[ct] <- hd$dx
  #       dy[ct] <- hd$dy
  #       ct <- ct + 1
  #     }
  #   }
  #   
  #   plot(dx, dy, xlim = c(-10^5, 10^5), ylim = c(-10^5, 10^5), col = 'red', cex = 0.6, 
  #        xlab = expression(paste(Delta, 'x (m)')), ylab = expression(paste(Delta, 'y (m)')))
  #   abline(v = 0, col = 'blue')
  #   abline(h = 0, col = 'blue')
  # })
  
  dates <- eventReactive(input$satellite, {
    if (input$satellite != 'All') dfc[dfc$Satellite == input$satellite, ]$Date
    else dfc$Date
  })

  output$plot3 <- renderPlot({
    dates <- as.Date(dates())
    plot(table(dates), xlab = 'Date', ylab = 'Counts')
  })
  
  ## Trajectory
  library(ggplot2)
  dfc.id <- reactive({
    if (input$id == 'All') dfc.id <- dfc
    else dfc.id <- dfc[dfc$DeployID == input$id, ]
  })
  
  output$plot4 <- renderPlot({
    ggplot(selectedData(), aes(x = Date, y = Longitude, shape = LocationQuality, color = DeployID)) +
      geom_point() + 
      coord_cartesian(ylim = c(min(dfc.id()$Longitude), max(dfc.id()$Longitude)))
  })
  
  output$plot5 <- renderPlot({
    ggplot(selectedData(), aes(x = Date, y = Latitude, shape = LocationQuality, color = DeployID)) +
      geom_point() +
      coord_cartesian(ylim = c(min(dfc.id()$Latitude), max(dfc.id()$Latitude)))
  })
  
}

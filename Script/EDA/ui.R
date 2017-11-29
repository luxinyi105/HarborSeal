id.ls <- c('PV98TUG01', 'PV98TUG02', 'PV98TUG04', 'PV98TUG10', 'PV98TUG11', 'PV98TUG13', 
           'PV98TUG14', 'PV98TUG18')

ui <- fluidPage(
  # img(src = 'harbor_seal.jpg'),
  sidebarLayout(
    sidebarPanel(
      selectInput('cat', 'Location Quality', choices = c('All', '0', '1', '2', '3', 'A', 'B')),
      
      selectInput('id', 'Seal ID', choices = c('All', id.ls)),
      
      selectInput('satellite', 'Satellite', choices = c('All', 'D', 'H', 'J', 'K')),
       
      # fluidRow(
      #   column(6, actionButton('allDates', 'All Dates')),
      #   column(6, actionButton('oneDay', 'One Day'))
      # ),
      # 
      # fluidRow(
      #   column(4, numericInput('year', 'Year', value = 98, min = 97, max = 99, step = 1)),
      #   column(4, numericInput('month', 'Month', value = 1, min = 1, max = 12, step = 1)),
      #   column(4, numericInput('day', 'Day', value = 1, min = 1, max = 30, step = 1))
      # ),
      
      width = 4,
      fluid = FALSE
    ),
    mainPanel(
      
      tabsetPanel(
        tabPanel('Map', leafletOutput('plot1')),
        tabPanel('Location Difference', plotOutput('plot2'), plotOutput('plot3'))
        # tabPanel('Trajectory', plotOutput('plot4'), plotOutput('plot5'))
      )
    )
  )
  
)
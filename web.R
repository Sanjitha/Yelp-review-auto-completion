#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("wordcloud") 
#install.packages("RColorBrewer")
library(shiny)
library(shinythemes)
library(wordcloud)
library(RColorBrewer)


shinyUI = (fluidPage(
  
  # Theme
  theme = shinytheme("darkly"),
  
  # Application title
  titlePanel("Auto complete tool"),
  
  # Sidebar ####    
  sidebarLayout(
     
    sidebarPanel(
      fluidRow(column(12, offset=0, 
      # Wordcloud output
      plotOutput('wordcloud')
     ))),
    
    # Mainpanel ####
    
    mainPanel(
       
      # Text input
      textInput("text", label = ('Please enter your review'), value = ''),
      
      # Table output
      dataTableOutput('table')
     
    ) 
  )
)
)


shinyServer = (function(input, output) {
  
  # Reactive statement for prediction function when user input changes ####
  prediction =  reactive( {
    
    # Get input
    inputText = input$text
    
    # Call to predict function
    prediction = predictionGiven3Words(inputText)
  })
  
  # Output data table ####
  output$table = renderDataTable(prediction(),
                                 option = list(pageLength = 5,
                                               lengthMenu = list(c(5, 10, 100), c('5', '10', '100')),
                                               columnDefs = list(list(visible = F, targets = 2))
                                               #searching = F
                                 )
  )
  
  # Output word cloud ####
  wordcloud_rep = repeatable(wordcloud)
  output$wordcloud = renderPlot(
    wordcloud_rep(
      prediction()$nextTerm,
      prediction()$frequency,
      colors = brewer.pal(8, 'Dark2'),
      scale=c(4, 0.5),
      max.words = 50
    )
  )
})


shinyApp(ui=shinyUI, server = shinyServer)

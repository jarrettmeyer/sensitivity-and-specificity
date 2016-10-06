library(shiny)


ui <- shinyUI(fluidPage(
   
  # Application title
  titlePanel("Sensitivity and Specificity"),
   
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("sensitivity", "Sensitivity (%):", value = 99, min = 0, max = 100, step = 0.1),
      numericInput("specificity", "Specificity (%):", value = 99, min = 0, max = 100, step = 0.1),
      numericInput("prevalence", "Disease Prevalence (%):", value = 10, min = 0, max = 100, step = 0.1),
      numericInput("population", "Population:", value = 1000, min = 10, max = 1e12, step = 1)
    ),
      
    mainPanel(
      tableOutput('table'),
      textOutput('accuracy'),
      textOutput('posPredictionValue'),
      textOutput('negPredictionValue')
    )
  )
))


server <- shinyServer(function(input, output) {
  
  getStats <- reactive({
    posPop <- round(input$population * input$prevalence / 100)
    negPop <- input$population - posPop
    truePos <- round(posPop * input$sensitivity / 100)
    falseNeg <- posPop - truePos
    trueNeg <- round(negPop * input$specificity / 100)
    falsePos <- negPop - trueNeg
    list(posPop = posPop, negPop = negPop, truePos = truePos, falseNeg = falseNeg, falsePos = falsePos, trueNeg = trueNeg)
  })
  
  getStatsDf <- reactive({
    stats <- getStats()
    df <- data.frame(c(stats$truePos, stats$falseNeg, stats$posPop), 
                     c(stats$falsePos, stats$trueNeg, stats$negPop))
    df[,1] <- as.integer(df[,1])
    df[,2] <- as.integer(df[,2])
    rownames(df) <- c("Pos.Test", "Neg.Test", "Total")
    colnames(df) <- c("Pos.Subject", "Neg.Subject")
    df
  })
  
  getAccuracy <- reactive({
    stats <- getStats()
    accuracy <- (stats$truePos + stats$trueNeg) / (stats$truePos+ stats$trueNeg + stats$falsePos + stats$falseNeg) * 100
    paste0("Accuracy: ", format(accuracy, nsmall = 2), "%")
  })
  
  getPosPredictionValue <- reactive({
    stats <- getStats()
    ppv <- stats$truePos / (stats$truePos + stats$falsePos) * 100
    paste0("Positive Prediction Value: ", round(ppv, 2), "%")
  })
  
  getNegPredictionValue <- reactive({
    stats <- getStats()
    npv <- stats$trueNeg / (stats$trueNeg + stats$falseNeg) * 100
    paste0("Negative Prediction Value: ", round(npv, 2), "%")
  })
  
  output$table <- renderTable(getStatsDf(), rownames = TRUE)
  output$accuracy <- renderText(getAccuracy())
  output$posPredictionValue <- renderText(getPosPredictionValue())
  output$negPredictionValue <- renderText(getNegPredictionValue())
})


# Run the application 
shinyApp(ui = ui, server = server)


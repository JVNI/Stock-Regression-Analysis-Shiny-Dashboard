library(shiny)
library(shinythemes)
library(quantmod)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  
  titlePanel("Linear Regression Modeling - Immanuel Berger"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Choose a category:",
                  choices = c("Tech", "Banks", "Healthcare", "Consumer Discretionary", "Energy")),
      uiOutput("stockUI"),
      uiOutput("benchmarkUI"),
      dateRangeInput("dates",
                     "Date range",
                     start = "2015-01-01",
                     end = as.character(Sys.Date())),
      radioButtons("returnType", "Return Type:", choices = c("Simple Returns", "Log Returns")),
      selectInput("movingAverage", "Moving Average:", 
                  choices = c("None", "10-day", "20-day", "30-day", "10-day EMA", "20-day EMA", "30-day EMA")),
      selectInput("differencing", "Order of Differencing:", 
                  choices = c("None", "1", "2", "3")),
      actionButton("fitModel", "Fit Model"),
      textOutput("modelOutput"),
      actionButton("runResiduals", "Run Residual Diagnostics"),
      textOutput("residualOutput")
    ),
    mainPanel(
      p("Welcome to the Linear Regression Modeling Shiny Web App, from the course: R Language and Environment for Statistical Computing. This app has been created by Immanuel Berger, student number: 515532, under the guidance of Professor Gabriele Cantaluppi. The focus of this application lies in modeling stocks within specific industries and benchmark indexes."),
      plotOutput("graph"),
      verbatimTextOutput("summary"),
      plotOutput("secondgraph")
    )
  )
)

server <- function(input, output, session) {
  
  stockCategories <- list(
    "Tech" = list(
      stocks = c("MSFT", "AAPL", "GOOGL", "AMZN", "TSLA", "META", "NFLX", "NVDA", "INTC"),
      benchmark = "^IXIC"  # NASDAQ 
    ),
    "Banks" = list(
      stocks = c("JPM", "BAC", "WFC", "C", "GS", "MS", "USB", "PNC", "TFC", "BK"),
      benchmark = "^GSPC"  # SP500
    ),
    "Healthcare" = list(
      stocks = c("JNJ", "PFE", "MRK", "ABBV", "TMO", "UNH", "MDT", "ABT", "DHR", "LLY"),
      benchmark = "XLV"  # SP500 For Healthcare
    ),
    "Consumer Discretionary" = list(
      stocks = c("HD", "MCD", "NKE", "SBUX", "AMZN", "TSLA", "BKNG", "LOW", "ROST", "TJX"),
      benchmark = "XLY"  # SP500 Consumer Discretionary Index
    ),
    "Energy" = list(
      stocks = c("XOM", "CVX", "COP", "SLB", "PSX", "VLO", "BKR", "MPC", "OXY", "HAL"),
      benchmark = "XLE"  # SP500 Energy Index Sector
    )
  )
  
  stockData <- reactive({
    req(input$stock)
    stock <- getSymbols(input$stock, src = "yahoo", from = input$dates[1], to = input$dates[2], auto.assign = FALSE)
    stock <- Cl(stock)
    if (input$returnType == "Simple Returns") {
      stock <- dailyReturn(stock)
    } else {
      stock <- dailyReturn(stock, type = "log")
    }
    stock
  })
  
  benchmarkData <- reactive({
    req(input$category)
    benchmark <- getSymbols(stockCategories[[input$category]]$benchmark, src = "yahoo", from = input$dates[1], to = input$dates[2], auto.assign = FALSE)
    benchmark <- Cl(benchmark)
    if (input$returnType == "Simple Returns") {
      benchmark <- dailyReturn(benchmark)
    } else {
      benchmark <- dailyReturn(benchmark, type = "log")
    }
    benchmark
  })
  
  transformData <- function(data, movingAvg, differencing) {
    if (movingAvg != "None")
      if (movingAvg == "10-day") {
      data <- rollapply(data, width = 10, FUN = mean, fill = NA)
    } else if (movingAvg == "20-day") {
      data <- rollapply(data, width = 20, FUN = mean, fill = NA)
    } else if (movingAvg == "30-day") {
      data <- rollapply(data, width = 30, FUN = mean, fill = NA)
    } else if (movingAvg == "10-day EMA") {
      data <- EMA(data, n = 10)
    } else if (movingAvg == "20-day EMA") {
      data <- EMA(data, n = 20)
    } else if (movingAvg == "30-day EMA") {
      data <- EMA(data, n = 30)
    }
    if (differencing != "None") {
      d <- as.numeric(differencing)
      data <- diff(data, differences = d)
    }
    data
  }
  
  observeEvent(input$fitModel, {
    req(input$stock, input$benchmark)
    
    stock <- stockData()
    benchmark <- benchmarkData()
    
    stock <- na.omit(transformData(stock, input$movingAverage, input$differencing))
    benchmark <- na.omit(transformData(benchmark, input$movingAverage, input$differencing))
    
    data <- na.omit(cbind(stock, benchmark))
    colnames(data) <- c("Stock", "Benchmark")
    
    formula <- as.formula("Stock ~ Benchmark")
    
    model <- lm(formula, data = data)
    
    output$graph <- renderPlot({
      regression_vals <- predict(model, newdata = data)
      
      layout(matrix(1:4, 2, 2, byrow = TRUE))
      plot(index(data), data$Stock, col = "blue", main = "Stock Return", xlab = "Time", ylab = "Stock Return")
      plot(index(data), data$Benchmark, col = "green", main = "Benchmark Return", xlab = "Time", ylab = "Index Return")
      plot(index(data), regression_vals, col = "red", main = "Regression Line", xlab = "Time", ylab = "Return")
      plot(index(data), data$Stock, col = "blue", main = "Linear Regression Model Fit", xlab = "Time", ylab = "Stock Return")
      lines(index(data), data$Benchmark, col = "green")
      lines(index(data), regression_vals, col = "red")
    })
    
    output$summary <- renderPrint({
      summary(model)
    })
  })
  
  output$modelOutput <- renderText({
    result <- "This button above is used to fit the Stock with the respective Benchmark Index and run a regression and plot its model"
    return(result)
  })
  
  observeEvent(input$runResiduals, {
    req(input$stock, input$benchmark)
    
    stock <- stockData()
    benchmark <- benchmarkData()
    
    stock <- na.omit(transformData(stock, input$movingAverage, input$differencing))
    benchmark <- na.omit(transformData(benchmark, input$movingAverage, input$differencing))
    
    data <- na.omit(cbind(stock, benchmark))
    colnames(data) <- c("Stock", "Benchmark")
    
    formula <- as.formula("Stock ~ Benchmark")
    
    model <- lm(formula, data = data)
    
    output$secondgraph <- renderPlot({
      layout(matrix(1:4, 2, 2, byrow = TRUE))
      plot(model)
    })
    
    output$residualOutput <- renderText({
      result <- "This button above plots an overview of residual diagnostics including: [1] Residual Deviations from fitted values, [2] Q-Q Plot, [3] Scale-Location Plot (Homoskedasticity), [4] Residuals vs Leverage Plot (Disproportions in regression results)"
      return(result)
    })
  })
  
  output$stockUI <- renderUI({
    req(input$category)
    selectInput("stock", "Choose a stock (Response Variable):", choices = stockCategories[[input$category]]$stocks)
  })
  
  output$benchmarkUI <- renderUI({
    req(input$category)
    selectInput("benchmark", "Choose a Benchmark (Predictor Variable)", choices = stockCategories[[input$category]]$benchmark)
  })
}

shinyApp(ui = ui, server = server)

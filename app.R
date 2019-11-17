library(shiny)
library(ggplot2)
library(dplyr)
data(AirPassengers)
library(shiny)
library(datasets)
library(forecast)



#load the module code
source("server.r")
source("ui.r")

ui <- shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Timeseries Forecasting"),
  
  # Sidebar with controls to select the dataset and forecast ahead duration
  sidebarPanel(
    selectInput("variable", "Variable:",
                list("Air Passengers" = "AirPassengers", 
                     "Australian total wine sales" = "wineind",
                     "Australian monthly gas production" = "gas")),
    numericInput("ahead", "Months to Forecast Ahead:", 12),
    
    submitButton("Update View")
  ),
  
  
  
  # Show the caption and forecast plots
  mainPanel(
    h3(textOutput("caption")),
    
    tabsetPanel(
      tabPanel("Exponetial Smoothing (ETS) Forecast", plotOutput("etsForecastPlot")), 
      tabPanel("Arima Forecast", plotOutput("arimaForecastPlot")),
      tabPanel("Timeseries Decomposition", plotOutput("dcompPlot"))
    )
  )
))
  
  
  
  
  

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  getDataset <- reactive({
    if (input$variable=="AirPassengers")
    {
      return(AirPassengers)
    }
    else if (input$variable=="gas")
    {
      return(gas)
    }
    else
    {
      return(wineind)
    }
  })
  
  output$caption <- renderText({
    paste("Dataset: ", input$variable)
  })
  
  output$dcompPlot <- renderPlot({
    ds_ts <- ts(getDataset(), frequency=12)
    f <- decompose(ds_ts)
    plot(f)
  })
  
  output$arimaForecastPlot <- renderPlot({
    fit <- auto.arima(getDataset())
    plot(forecast(fit, h=input$ahead))
  })
  
  output$etsForecastPlot <- renderPlot({
    fit <- ets(getDataset())
    plot(forecast(fit, h=input$ahead))
  })
  
})
  
  
  
  


require(rsconnect)



rsconnect::addAuthorizedUser()

rsconnect::addConnectServer()

setAccountInfo(name='mohcinemadkour', token='6CC1A67FFB2C3F8FC7B80707606694F1', secret='0GrenldtSp3BYHAeySMqL8sexElxG2uhuNve0Eum')




# Loading libraries
library(shiny)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(plotly)
library(reactable)
library(readxl)
library(writexl)
library(DT)
library(anomalize)
interactive <- TRUE

# The user interface section of the app.
ui <- fluidPage(
  titlePanel("",
             windowTitle = "Fully Automated Forecasting App."),
  tabsetPanel(
    tabPanel("Help",
             fluid = TRUE,
             mainPanel(
               h3("This is a short demo:"),
               HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/PXI_bsHRkjg" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
               tags$hr(),
               width = 9
             )
    ),
    tabPanel("Forecasting",
             fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 tags$div(img(src = "logo.png")),
                 
                 tags$hr(),
                 downloadButton(outputId = 'downloadTemplate',
                                label    = 'Download template',
                                style    = 'width:100%; color:green; font-size:100%;'
                 ),
                 tags$hr(),
                 fileInput(inputId     = "file",
                           label       = "Upload your file please:",
                           buttonLabel = list(icon("bar-chart-o"))
                 ),
                 
                 selectInput("category", "select category to forecast:", choices = NULL),
                 sliderInput(inputId  = "Train",
                             label    = "Adjust % of Train data:",
                             min = 0.75, max = 1, value = 0.8, step = 0.05),
                 tags$hr(),
                 sliderInput(inputId  = "h",
                             label    = "Modify # of future days to forecast?",
                             min = 1, max = 30, value = 7, step = 1),
                 tags$div(
                   tags$p("From ",
                          strong(downloadLink(outputId = "downloadData",
                                              label    = "HERE",
                                              style    = 'color:red;')
                          ),
                          "you can download an excel file with forecasted values."
                   )
                 ), width = 2), 
               mainPanel(
                 fluidRow(
                   column(6, plotOutput("distPlot")),
                   column(6, reactableOutput("table"))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("testforecast1")),
                   column(6, plotlyOutput("testforecast"))
                 )
                 , width = 10)
             )
    )
  )
)

# The server section of the app
server <- function(input, output, session) {
  
  output$downloadTemplate <- downloadHandler(
    filename = function() {
      paste("temp", ".xlsx", sep = "")
    },
    content  = function(file) {
      temp <- data.frame(cat = "", date = "", value = "")
      write_xlsx(temp, file)
    }
  )
  
  m <- reactive({
    inFile <- input$file
    if (is.null(inFile)) 
      return(NULL)
    m <- read_excel(inFile$datapath,
                    col_names = c("cat", "date", "value"),
                    skip = 1)
  })
  
  observeEvent(m(),
               {
                 updateSelectInput(session, "category", choices = unique(m()$cat))
               }
               )
  
m750 <- reactive({
  m750 <- m() %>%
    filter(cat %in% input$category & value > 0) %>%
    arrange(date) %>%
    time_decompose(value) %>%
    anomalize(remainder, method = "gesd", max_anoms = 0.1) %>%
    clean_anomalies() %>%
    select(date, value = observed_cleaned) %>%
    as_tibble()
  })
  
  train <- reactive({
    train <- training(initial_time_split(m750(), prop = input$Train))
  })
  
  test <- reactive({
    test <- testing(initial_time_split(m750(), prop = input$Train))
  })
  
  model_fit_arima_no_boost <-  reactive({
    
    model_fit_arima_no_boost <- arima_reg() %>%
      set_engine(engine = "auto_arima") %>%
      fit(value ~ date, data = train())
    
  })
  
  model_fit_arima_boosted <- reactive({
    model_fit_arima_boosted <- arima_boost(
      min_n = 2,
      learn_rate = 0.015
    ) %>%
      set_engine(engine = "auto_arima_xgboost") %>%
      fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
          data = train())
  })
  
  model_fit_ets <- reactive({
    
    model_fit_ets <- exp_smoothing() %>%
      set_engine(engine = "ets") %>%
      fit(value ~ date, data = train())
  })
  
  model_fit_prophet <- reactive({
    
    model_fit_prophet <- prophet_reg() %>%
      set_engine(engine = "prophet") %>%
      fit(value ~ date, data = train())
  })
  
  models_tbl <- reactive({
    models_tbl <- modeltime_table(
            model_fit_arima_no_boost(),
            model_fit_arima_boosted(),
            model_fit_ets(),
            model_fit_prophet())
  })
  
  calibration_tbl <- reactive({
    calibration_tbl <- models_tbl() %>%
      modeltime_calibrate(new_data = test())
  })
  
  refit_tbl <- reactive({
    refit_tbl <- calibration_tbl() %>%
      modeltime_refit(data = m750())
  })
  
  output$distPlot <- renderPlot({
    
    m750() %>% 
      plot_time_series(date, value, .title = "Time Series Plot with Trend line",.interactive = FALSE)
  })
  
  output$testforecast1 <- renderPlotly({
    calibration_tbl() %>%
      modeltime_forecast(
        new_data    = test(),
        actual_data = m750()
      ) %>%
      plot_modeltime_forecast(
        .title = paste("Forecasted plot of '", input$category, "' based on ", input$Train, " of data as Training data.", sep = ""),
        .legend_max_width = 25, # For mobile screens
        .interactive      = interactive
      )
  })
  
  output$table <- renderReactable({
    
    calibration_tbl() %>%
      modeltime_accuracy() %>%
      table_modeltime_accuracy(.interactive = interactive)
  })
  
  output$testforecast <- renderPlotly({
    
    refit_tbl() %>%
      modeltime_forecast(h = paste(input$h, " day", sep = ""), actual_data = m750()) %>%
      plot_modeltime_forecast(
        .title = paste("Forecasted plot of '", input$category, "' for future ", input$h, " days", sep = ""),
        .legend_max_width = 25, # For mobile screens
        .interactive      = interactive)
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste(input$category,"_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(refit_tbl() %>%
                   modeltime_forecast(h = "2 week", actual_data = m750()), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

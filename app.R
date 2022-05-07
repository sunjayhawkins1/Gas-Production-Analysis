library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(fpp3)
library(shinydashboard)
library(ggplot2)
library(seasonal)


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Australian Gas Production Analysis",
                  titleWidth = 300),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Introduction", tabName = "Intro"),
      menuItem("Total Time-Series", tabName = "FTS"),
      menuItem("Seasonality", tabName = "Seas"),
      menuItem("Decomposition", tabName = "Decomp"),
      menuItem("AutoCorrelation", tabName = "ACF"),
      menuItem("Forecasting", tabName = "FC"),
      menuItem("Naive", tabName = "simple"),
      menuItem("Exponential Smoothing", tabName = "smooth"),
      menuItem("ARIMA", tabName = "arima"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Intro",  box(verbatimTextOutput("Text1"),width=12)),
              
      
      
      tabItem(tabName = "FTS",  box(plotOutput("plot1"), width = 12, 
                                status = "primary", 
                                title = "Gas Production",
                                solidHeader = TRUE),
                                helpText("Gas production grew rapidly early on and has been increasing steadily sense")),
              
      tabItem(tabName = "Seas",          box(plotOutput("plot2"), width = 12, 
                                                    status = "primary", 
                                                    title = "Gas Production",
                                                    solidHeader = TRUE),
              helpText("Gas production increases between Q2 and Q3. These quarters correlate with the Australian winter seasons.")),
              
    tabItem(tabName = "Decomp",          box(plotOutput("plot3"), width = 12, 
                                          status = "primary", 
                                          title = "Gas Production",
                                          solidHeader = TRUE),
                                          helpText("This visualization further breaks down the trends and seasonality in Gas production")),
    
    tabItem(tabName = "ACF",          box(plotOutput("plot4"), width = 12, 
                                             status = "primary", 
                                             title = "Gas Production",
                                             solidHeader = TRUE),
    helpText("All of these spikes are above the blue line which indicates there is no white-noise in this series")),
    
              
    tabItem(tabName = "FC",          box(plotOutput("plot5"), width = 12, 
                                          status = "primary", 
                                          title = "Gas Production Forecasted Into Future",
                                          solidHeader = TRUE),
            helpText("Gas Production is forecasted to remain steady in the future")),
    tabItem(tabName = "simple", 
            radioButtons(inputId = "selected_model",
                         label = "Select your model",
                         choices = list("Naive" = 1, "Seasonal Naive" = 2, "Mean" = 3,"Drift" = 4)),
            box(plotOutput("plot6"), width = 12, 
            status = "primary", 
            title = "Simple Models",
            solidHeader = TRUE),
           ),
    
    tabItem(tabName = "smooth", radioButtons(inputId = "selected_ETS",
                                             label = "Select your model",
                                             choices = list("Holt" = 1, "Holt and Winters" = 2)),      
            box(plotOutput("plot7"), width = 12, 
                                         status = "primary", 
                                         title = "Gas Production Forecasted Into Future",
                                         solidHeader = TRUE)
    
      
),   tabItem(tabName = "arima", radioButtons(inputId = "selected_param",
                                             label = "Select your parameters",
                                             choices = list("Auto" = 1, "ARIMA 210" = 2,"ARIMA 013" = 3)),      
            box(plotOutput("plot8"), width = 12, 
                status = "primary", 
                title = "ARIMA",
                solidHeader = TRUE)
            
            
))))
server <- function(input, output) {
  #IntroScreen
  output$Text1<- renderText("Welcome! This application displays the full time series analysis of Gas production, seasonality, decomposition, auto correlation and multiple forecasts into future Gas production")
  #feature 1
  output$plot1 <- renderPlot({
    autoplot(aus_production,Gas)
    
  })
  #feature 2
  output$plot2 <- renderPlot({
    gg_season(aus_production, Gas)
  
  })
  #feature3
  output$plot3 <- renderPlot({
    aus_production %>%
      model(X_13ARIMA_SEATS(Gas ~ seats())) %>%
      components() %>%
      autoplot()
    
  })
  #feature4
  output$plot4 <- renderPlot({  aus_production%>% ACF(Gas) %>%
      autoplot() + labs(title="Australian Gas production")
  })
  output$plot5 <- renderPlot({  
    aus_production %>%
      model(TSLM(Gas ~ trend() + season())) %>%
      forecast(h=4) %>%
      autoplot(aus_production)
  })
  
  output$plot6 <- renderPlot({  
    if (input$selected_model==1) {
      aus_production %>%
        model(NAIVE(Gas))%>%
        forecast(h=4)%>%
        autoplot(aus_production)}
    
    else if (input$selected_model==3) {
      aus_production %>%
        model(MEAN(Gas))%>%
        forecast(h=4)%>%
        autoplot(aus_production) }
    else if (input$selected_model==2) {
      aus_production %>%
        model(SNAIVE(Gas))%>%
        forecast(h=4)%>%
        autoplot(aus_production) }
    else if (input$selected_model==4) {
      aus_production %>%
        model(NAIVE(Gas~drift()))%>%
        forecast(h=4)%>%
        autoplot(aus_production) }
    
})
  output$plot7 <- renderPlot({
    if (input$selected_ETS==1) {
      aus_production %>%
        model(AAN = ETS(Gas ~ error("A") + trend("A") + season("N")))%>%
        forecast(h=4) %>%
        autoplot(aus_production)}
 
    else if (input$selected_ETS==2) {
      aus_production %>%
        model(
          additive = ETS(Gas ~ error("A") + trend("A") +season("A")),
          multiplicative = ETS(Gas ~ error("M") + trend("A") +season("M")))%>%
            forecast(h=4) %>%
            autoplot(aus_production)
         }})
  
  output$plot8<-renderPlot({
    if (input$selected_param==1) {
      aus_production %>%
        model(search = ARIMA(Gas, stepwise=FALSE))%>%
        forecast(h=12) %>%
        autoplot(aus_production)
    }
    else if (input$selected_param==2){
      aus_production %>%
        model(arima210 = ARIMA(Gas ~ pdq(2,1,0)))%>%
        forecast(h=12) %>%
        autoplot(aus_production)}
    
    else if (input$selected_param==3){
      aus_production %>%
        model(arima013 = ARIMA(Gas ~ pdq(0,1,3)))%>%
        forecast(h=12) %>%
        autoplot(aus_production)}
      
  })
  
  
}

shinyApp(ui, server)
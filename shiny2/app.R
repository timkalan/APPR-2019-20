# v namen raziskovanja podatkov, ni zares vključeno v poročilo,
# ker nismo našli nobene posebne povezave


ui <- fluidPage(
  titlePanel("Države sveta po skupinah"),
  sidebarLayout(
    position = "right",
    sidebarPanel(h3("Parametri za graf"),
                 selectInput(inputId = "podatek",
                             label = "1. Podatek o državi",
                             choices = c("BDP" = "BDP",
                                         "BDP per capita" = "BDPpc",
                                         "populacija" = "populacija",
                                         "izobraženost" = "izobrazenost",
                                         "HDI (human development index)" = "HDI"),
                             selected = "BDP"),
                 br(),
                 selectInput(inputId = "podatek2",
                             label = "1. Podatek o državi",
                             choices = c("BDP" = "BDP",
                                         "BDP per capita" = "BDPpc",
                                         "populacija" = "populacija",
                                         "izobraženost" = "izobrazenost",
                                         "HDI (human development index)" = "HDI"),
                             selected = "BDP"),
                 br(),
                 radioButtons(inputId = "skupina",
                              label = "Skupina",
                              choices = c("skupinaE", "skupinaI"))
    ),
    mainPanel(plotOutput("grafk", height = "560px", width = "102%"))
  )
)



server <- function(input, output) {
  y <- reactive({
    ggplot(povprecje, aes(x = povprecje[, input$podatek], 
                          y = emigracija, 
                          color = povprecje[, input$skupina])) + 
      geom_point() + scale_x_log10() + scale_y_log10() + 
      xlab(input$podatek)
  })
  output$grafk <- renderPlot(y())
}



shinyApp(ui = ui, server = server)

ui <- fluidPage(
  titlePanel("Migracija glede na podatke o državah"),
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
                 # selectInput(inputId = "skala",
                 #             label = "2. Skala za podatke",
                 #             choices = c("x in y logaritmirana" = "xy",
                 #                         "x logaritmiran" = "x",
                 #                         "y logaritmiran" = "y",
                 #                         "normalno" = "n"), selected = "xy"),
                 br(),
                 radioButtons(inputId = "emiimi",
                              label = "3. Vstop ali izstop",
                              choices = c("imigracija", "emigracija")),
                 # br(),
                 # actionButton(inputId = "reg",
                 #              label = "4. Regresijska premica",
                 #              icon = "regresija")

                 ),
    mainPanel(plotOutput("grafk"))
  )
)


server <- function(input, output) {
  output$grafk <- renderPlot(
    ggplot(master, aes(x = master[, input$podatek], 
                       y = master[, input$emiimi])) + 
      geom_point() + 
      scale_x_log10() + scale_y_log10()
  )
}



shinyApp(ui = ui, server = server)

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
                 radioButtons(inputId = "emiimi",
                              label = "2. Vstop ali izstop",
                              choices = c("imigracija", "emigracija")),
                 br(),
                 selectInput(inputId = "reg",
                              label = "3. Prileganje podatkom", 
                             choices = c("Brez regresije" = "br", 
                                         "Linearna regresija" = "lm", 
                                         "LOESS" = "loess", 
                                         "GAM" = "gam"), 
                             selected = "br"),
                 br(),
                 # selectInput(inputId = "skala",
                 #             label = "4. Skala za podatke",
                 #             choices = c("x in y logaritmirana" = "xy",
                 #                         "x logaritmiran" = "x",
                 #                         "y logaritmiran" = "y",
                 #                         "normalno" = "n"), selected = "xy")
                 checkboxGroupInput(inputId = "skala",
                                    label = "4. Skala za podatke",
                                    choices = c("x logaritmiran" = "x", 
                                                "y logaritmiran" = "y"), 
                                    selected = c("x", "y"))
                 ),
    mainPanel(plotOutput("grafk"))
  )
)


server <- function(input, output) {
  output$grafk <- renderPlot(
    ggplot(master, aes(x = master[, input$podatek], 
                       y = master[, input$emiimi])) + 
      geom_point() + xlab(input$podatek) + ylab(input$emiimi) +
      ggtitle(paste0(
        input$emiimi, 
        " glede na ", 
        input$podatek, 
        " (vsaka točka predstavlja eno državo sveta)  ")) +
      scale_x_log10() + scale_y_log10() + 
      geom_smooth(method = input$reg)
  )
}



shinyApp(ui = ui, server = server)
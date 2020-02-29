
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
                 selectInput(inputId = "cajt",
                             label = "5. Časovno obdobje",
                             choices = c("Skupaj" = "sku",
                                         2019, 2015, 2010, 2005, 2000, 1995, 1990),
                             selected = "sku"),
                 br(),
                 checkboxGroupInput(inputId = "skala",
                                    label = "4. Skala za podatke",
                                    choices = c("x logaritmiran" = "x", 
                                                "y logaritmiran" = "y"), 
                                    selected = c("x", "y")),
                 br(),

                 selectInput(inputId = "reg",
                             label = "3. Prileganje podatkom", 
                             choices = c("Brez regresije" = "br", 
                                         "Linearna regresija" = "lm", 
                                         "LOESS" = "loess", 
                                         "GAM" = "gam"), 
                             selected = "br")
                 ),
    mainPanel(plotOutput("grafk", height = "560px", width = "102%"), 
              tableOutput("tabelca"))
  )
)



server <- function(input, output) {
  x <- reactive({input$skala})
  
  podatkici <- reactive({
    if (input$cajt == "sku") {
      master 
    }
    else {
      master %>% filter(leto == input$cajt)
    }
  })
  
  y <- reactive({
    pod <- podatkici()
    if (length(x()) == 2) {
      ggplot(data = pod, aes(x = pod[, input$podatek],
                         y = pod[, input$emiimi])) +
        geom_point() + xlab(input$podatek) + ylab(input$emiimi) +
        ggtitle(paste0(
          input$emiimi,
          " glede na ",
          input$podatek,
          " (vsaka točka predstavlja eno državo sveta)  ")) +
        geom_smooth(method = input$reg) + scale_x_log10() + 
        scale_y_log10()
    }
    else if (length(x()) == 1) {
      if (x() == "x") {
        ggplot(pod, aes(x = pod[, input$podatek],
                           y = pod[, input$emiimi])) +
          geom_point() + xlab(input$podatek) + ylab(input$emiimi) +
          ggtitle(paste0(
            input$emiimi,
            " glede na ",
            input$podatek,
            " (vsaka točka predstavlja eno državo sveta)  ")) +
          geom_smooth(method = input$reg) + scale_x_log10()
      }
      else {
        ggplot(pod, aes(x = pod[, input$podatek],
                           y = pod[, input$emiimi])) +
          geom_point() + xlab(input$podatek) + ylab(input$emiimi) +
          ggtitle(paste0(
            input$emiimi,
            " glede na ",
            input$podatek,
            " (vsaka točka predstavlja eno državo sveta)  ")) +
          geom_smooth(method = input$reg) + scale_y_log10()
      }
    }
    else {
      ggplot(pod, aes(x = pod[, input$podatek],
                         y = pod[, input$emiimi])) +
        geom_point() + xlab(input$podatek) + ylab(input$emiimi) +
        ggtitle(paste0(
          input$emiimi,
          " glede na ",
          input$podatek,
          " (vsaka točka predstavlja eno državo sveta)  ")) +
        geom_smooth(method = input$reg)
    }
  })
  output$grafk <- renderPlot(y())
  
  output$tabelca <- renderTable({
    pod <- podatkici()
    pod %>% 
      summarise("Korelacijski koeficient" = cor(pod[, input$podatek], 
                                                y = pod[, input$emiimi], use = "na.or.complete"),
                "Povprečje" = mean(pod[, input$emiimi], na.rm = TRUE),
                "Minimum" = min(pod[, input$emiimi], na.rm = TRUE),
                "Maksimum" = max(pod[, input$emiimi], na.rm = TRUE)
                )
    })
}


  
shinyApp(ui = ui, server = server)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

data_loaded <- read.csv("/Volumes/WD_BLACK/Rebeca Rodriguez/Curso taller de programacion en R/Curso 2/CETAV/shiny_apps/datos/datos_libertad.csv")

ui <- dashboardPage(
  dashboardHeader(
    title = div(
      tags$i(class = "fa fa-globe"), " World Freedoms", 
      style = "text-align: left;"
    ),  # Encabezado con icono de globo centrado a la izquierda
    titleWidth = 230
  ),
  
  dashboardSidebar(
    tags$style(HTML(".sidebar .selectize-input {font-size: 16px !important;}")),  # Ajustar tamaño del texto en el sidebar
    selectInput("pais", "Selecciona un País", choices = unique(data_loaded$pais)),
    sliderInput("ano", "Selecciona un Año:", min = 2008, max = 2016, value = c(2008, 2016)),
    radioButtons("metrica", "Elige Visualización:", choices = c("Puntaje", "Ranking")),
    div(style = "text-align: center;", downloadButton("descargarPDF", "Descargar en PDF", style = "color: black;")),  # Botón centrado
    div(style = "text-align: center;", class = "text-center")
  ),
  
  dashboardBody(
    tabsetPanel(
      tabPanel("Libertad Humana", plotOutput("plotHumana")),
      tabPanel("Libertad Personal", plotOutput("plotPersonal")),
      tabPanel("Libertad Económica", plotOutput("plotEconomica"))
    )
  ),
  skin = "blue"
)

server <- function(input, output) {
  
  datos_filtrados <- reactive({
    filter(data_loaded, pais == input$pais, anio >= input$ano[1] & anio <= input$ano[2])
  })
  
  render_graph <- function(metrica, titulo) {
    ggplot(datos_filtrados(), aes(x = anio, y = .data[[metrica]], group = pais)) +
      geom_line(color = "#367fa9", size = 1.5) +  # Línea más gruesa y azul
      geom_point(color = "#367fa9", size = 3) +  # Puntos azules y más grandes
      ggtitle(paste("Evolución de", titulo, "-", input$metrica)) +
      labs(color = "País") +
      theme_minimal()
  }
  
  output$plotHumana <- renderPlot({
    metrica <- if(input$metrica == "Puntaje") "libertad_humana_puntaje" else "libertad_humana_ranking"
    render_graph(metrica, "Libertad Humana")
  })
  
  output$plotPersonal <- renderPlot({
    metrica <- if(input$metrica == "Puntaje") "libertad_personal_puntaje" else "libertad_personal_ranking"
    render_graph(metrica, "Libertad Personal")
  })
  
  output$plotEconomica <- renderPlot({
    metrica <- if(input$metrica == "Puntaje") "libertad_economica_puntaje" else "libertad_economica_ranking"
    render_graph(metrica, "Libertad Económica")
  })
  
  output$descargarPDF <- downloadHandler(
    filename = function() {paste("datos_", input$pais, ".pdf", sep = "")},
    content = function(file) {
      metrica <- if(input$metrica == "Puntaje") paste("libertad", input$metrica, sep = "_") else paste("libertad", input$metrica, sep = "_")
      pdf(file)
      print(render_graph(metrica, "Libertad"))
      dev.off()
    }
  )
}

shinyApp(ui, server)




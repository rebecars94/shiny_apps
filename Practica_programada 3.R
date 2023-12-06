library(shiny)
library(plotly)
library(readr)
library(dplyr)
library(shinydashboard)
library(DT)

datos <- read_delim("datos/spotify_2000_2023.csv", delim = ";", col_types = cols())

ui <- dashboardPage(
  dashboardHeader(
    title = "Análisis Spotify",
    titleWidth = 229 
  ),
  dashboardSidebar(
    selectInput("year", "Selecciona el año:",
                choices = unique(datos$year)),
    selectInput("genre", "Selecciona el género:",
                choices = unique(datos$`top genre`)),
    column(
      width = 12,
      align = "center",  
      actionButton("updateTable", "Actualizar tabla", style = "background-color: #008000; color: white; border-color: #008000;"),
      tags$br(),  
      downloadButton("downloadData", "Descargar datos", style = "background-color: #008000; color: white; border-color: #008000;")
    )
  ),
  dashboardBody(
    class = "skin-purple",
    fluidRow(
      box(
        title = "Popularidad de los artistas",
        plotlyOutput("featurePlot")
      ),
      box(
        title = "Tabla de Datos",
        DTOutput("table")
      )
    )
  )
)

server <- function(input, output, session) {
  filteredData <- reactive({
    datos %>%
      filter(year == input$year, `top genre` == input$genre)
  })
  
  output$featurePlot <- renderPlotly({
    plot_ly(filteredData(), x = ~artist, y = ~`popularity`, 
            size = ~popularity, color = ~`top genre`,
            type = "scatter", mode = "markers")
  })
  
  output$table <- renderDT({
    filteredData()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("spotify_data_", input$year, input$genre, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$updateTable, {
    output$table <- renderDT({
      filteredData()
    })
  })
}

shinyApp(ui = ui, server = server)









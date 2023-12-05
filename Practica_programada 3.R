library(shiny)
library(plotly)
library(readr)
library(dplyr)
library(shinydashboard)

datos <- read_delim("datos/spotify_2000_2023.csv", delim = ";", col_types = cols())

ui <- dashboardPage(
  dashboardHeader(
    title = "Análisis de Canciones Top en Spotify",
    titleWidth = 300,

    tags$li(class = "dropdown", style = "background-color: green;",
            title = "Dashboard", actionButton("toggleSidebar", icon("bars"))),
    tags$li(class = "dropdown", style = "background-color: green;",
            title = "Otro elemento")
  ),
  dashboardSidebar(
    selectInput("year", "Selecciona el año:",
                choices = unique(datos$year)),
    selectInput("genre", "Selecciona el género:",
                choices = unique(datos$`top genre`)),
    column(width = 12, align = "center", downloadButton("downloadData", "Descargar datos"))
  ),
  dashboardBody(
    skin = "red",
    fluidRow(
      box(
        title = "Popularidad de los artistas",
        plotlyOutput("featurePlot")
      )
    )
  )
)

server <- function(input, output) {
  
  filteredData <- reactive({
    datos %>%
      filter(year == input$year, `top genre` == input$genre)
  })
  
  output$featurePlot <- renderPlotly({
    plot_ly(filteredData(), x = ~artist, y = ~`popularity`, 
            size = ~popularity, color = ~`top genre`,
            type = "scatter", mode = "markers")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("spotify_data_", input$year, input$genre, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
}


shinyApp(ui = ui, server = server)






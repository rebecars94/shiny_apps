library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

data_loaded <- read.csv("/Volumes/WD_BLACK/Rebeca Rodriguez/Curso taller de programacion en R/Curso 2/CETAV/shiny_apps/datos/datos_empleo_genero.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Trabajo y Género en Latinoamérica y el Caribe", titleWidth = 470),
  dashboardSidebar(
    selectInput("pais", "Selecciona un país o región", choices = unique(data_loaded$pais_region)),
    selectInput("genero", "Selecciona un género", choices = c("Hombres", "Mujeres")),
    sliderInput("ano_inicio", "Año de inicio", min = 2000, max = max(data_loaded$anyo), value = 2000),
    div(style = "display: flex; justify-content: center;",  
        actionButton("actualizar", "Actualizar"))
  ),
  dashboardBody(
    skin = "blue",
    fluidRow(
      box(
        title = "Empleadores según género",
        plotOutput("plot_empleadoras", height = "400px")
      ),
      box(
        title = "Autoempleo según género",
        plotOutput("plot_autoempleo", height = "400px")
      ),
      box(
        title = "Desempleo según género",
        plotOutput("plot_desempleo", height = "400px")
      ),
      box(
        title = "Datos de empleo",
        width = 6,  
        fluidRow(
          column(12, DTOutput("tabla", height = "351px"))  
        ),
        style = "margin-top: 5px;"
      )
    )
  )
)

server <- function(input, output) {
  datos_filtrados <- eventReactive(input$actualizar, {
    data_loaded %>%
      filter(pais_region == input$pais, anyo >= input$ano_inicio)
  })
  
  output$plot_empleadoras <- renderPlot({
    genero_variable <- ifelse(input$genero == "Hombres", "empleadores_hombres", "empleadoras_mujeres")
    ggplot(datos_filtrados(), aes(x = anyo, y = !!sym(genero_variable))) +
      geom_line(color = "#367fa9", size = 2) +
      geom_point(color = "#FF5733", size = 3) + 
      labs(title = paste("Porcentaje de", input$genero, "empleadores (as) en", input$pais),
           y = "Porcentaje",
           x = "Año")
  })
  
  output$plot_autoempleo <- renderPlot({
    genero_variable <- ifelse(input$genero == "Hombres", "autoempleo_hombres", "autoempleo_mujeres")
    ggplot(datos_filtrados(), aes(x = anyo, y = !!sym(genero_variable))) +
      geom_line(color = "#367fa9", size = 2) +
      geom_point(color = "#FF5733", size = 3) + 
      labs(title = paste("Porcentaje de", input$genero, "en autoempleo en", input$pais),
           y = "Porcentaje",
           x = "Año")
  })
  
  
  
  output$plot_desempleo <- renderPlot({
    genero_variable <- ifelse(input$genero == "Hombres", "desempleo_hombres", "desempleo_mujeres")
    ggplot(datos_filtrados(), aes(x = anyo, y = !!sym(genero_variable))) +
      geom_line(color = "#367fa9", size = 2) +
      geom_point(color = "#FF5733", size = 3) +  
      labs(title = paste("Porcentaje de", input$genero, "desempleo en", input$pais),
           y = "Porcentaje",
           x = "Año")
  })
  
  output$tabla <- renderDT({
    datatable(datos_filtrados(), options = list(pageLength = 7, scrollX = TRUE))
  })
}

shinyApp(ui, server)
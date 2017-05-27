fluidPage(
  
  # Application title
  #headerPanel("Distribución de Viajes Linea 2 Centro - La Hechicera Trolebús Mérida"),
  fluidRow(
    column(2, 
           headerPanel(title = div(img(src = "logo-tromerca.jpg",
                                       height = 70,
                                       width = "auto")),
                       windowTitle = "Distribución de Viajes Linea 2 Centro - La Hechicera Trolebús Mérida")),
    column(10,
           br(),
           br(),
           titlePanel("Distribución de Viajes Linea 2 Centro - La Hechicera Trolebús Mérida"))
  ),
  
  sidebarPanel(
    sliderInput("rangoTiempo", label = "Rango de tiempo:",
                min = as.POSIXct("2017-05-19 7:00am", format = "%Y-%m-%d %I:%M%p", tz = "GMT"),
                max = as.POSIXct("2017-05-19 6:00pm", format = "%Y-%m-%d %I:%M%p", tz = "GMT"),
                value = c(
                  as.POSIXct("2017-05-19 7:00am", format = "%Y-%m-%d %I:%M%p", tz = "GMT"),
                  as.POSIXct("2017-05-19 6:00pm", format = "%Y-%m-%d %I:%M%p", tz = "GMT")
                ),
                step = 600,
                timeFormat = "%I:%M%p",
                timezone = "+0000"
    ),
   
    sliderInput("coberturaTransportePublico", label = "Cobertura de transporte público:",
                min = 0,
                max = 100,
                value = 100,
                step = 1,
                post = "%"
    ),
    
    sliderInput("aumentoUsoTransporte", label = "Aumento de uso de transporte público:",
                min = 0,
                max = 100,
                value = 0,
                step = 1,
                post = "%"
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Matriz de Viajes",
               br(),
               htmlOutput("leyendaMatriz"),
               tableOutput("matrizViajes"),
               fluidRow(align = "center", plotOutput("grafoCompleto"))
      ),
      
      tabPanel("Flujos",
               br(),
               fluidRow(align = "center", htmlOutput("tituloFlujos")),
               fluidRow(align = "center", uiOutput("grafoSubidaRedim")),
               fluidRow(align = "center", uiOutput("grafoBajadaRedim"))
      ),
      
      tabPanel("Conteo Buses",
               br(),
               selectInput("seleccionNodos", "Seleccione un nodo:",
                           choices = c()),
               plotOutput("busesSubida"),
               plotOutput("busesBajada")
      ),
      
      tabPanel("Estadísticas",
               br(),
               htmlOutput("tituloEstOrigenes"),
               tableOutput("estadisticasOrigenes"),
               htmlOutput("tituloEstDestinos"),
               tableOutput("estadisticasDestinos")
      )
    )
  )
)
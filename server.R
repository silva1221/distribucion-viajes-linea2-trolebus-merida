library(readr)
library(igraph)

source('matriz_viajes.R', local = TRUE)
source('calculo_rangos.R', local = TRUE)

viajesData <- read_csv("~/Documentos/odiseo/r-shiny/time-range-slider/viajes.csv")
conteoBuses <- read_csv("~/Documentos/odiseo/r-shiny/time-range-slider/conteo_buses.csv")

source('inicializar_nodos.R', local = TRUE)

flujo <- NULL

function(input, output, session) {
  observe({
    opcionesNodos = c()
    for (i in 1:numNodos) {
      opcion = paste("Nodo ", i)
      opcionesNodos = c(opcionesNodos, opcion)
    }
    
    updateSelectInput(session, "seleccionNodos", choices = opcionesNodos)
  })
  
  esRango <- reactive({
    desde <-as.character.POSIXt(input$rangoTiempo[1], format = "%I:%M")
    hasta <- as.character.POSIXt(input$rangoTiempo[2], format = "%I:%M")

    return(desde != hasta)
  })
  
  viajesInfo <- reactive({
    if (esRango()) {
      rango <- calcularRango(input)
      origenes <- c()
      destinos <- c()
      
      for (i in 1:numNodos){
        origenes[i] <- sum(viajesData[rango$desde:rango$hasta, i*2])
        destinos[i] <- sum(viajesData[rango$desde:rango$hasta, i*2 + 1])
      }
      
      porcentajeCobertura <- input$coberturaTransportePublico/100
      porcentajeAmento <- input$aumentoUsoTransporte/100
      factorAumento <- porcentajeCobertura * (1 + porcentajeAmento)
      
      origenes <- ceiling(origenes * factorAumento)
      destinos <- ceiling(destinos * factorAumento)
      
      viajes <- matrizViajes(origenes, destinos)
      viajesMarco <- viajes
      
      nombres = c()
      for (i in 1:numNodos) {
        nombre = paste("Nodo ", i)
        nombres = c(nombres, nombre)
      }
      
      colnames(viajesMarco) <- nombres
      rownames(viajesMarco) <- nombres
      
      return(list(
        viajes = viajes,
        origenes = origenes,
        destinos = destinos,
        viajesMarco = viajesMarco
      ))
    }
  })
  
  output$leyendaMatriz <- renderText({
    if (esRango()) {
      paste("<b><font size=\"3\">Matriz de Viajes ",
            as.character.POSIXt(input$rangoTiempo[1],
                                format = "%I:%M%p"),
            " - ",
            as.character.POSIXt(input$rangoTiempo[2],
                                format = "%I:%M%p"),
            "</font></b>")
    }
  })
  
  output$matrizViajes <- renderTable({
    viajesInfo()$viajesMarco
  },  rownames = TRUE, colnames = TRUE,
      digits = 0, width = "100%")
  
  output$tituloFlujos <- renderText({
    if (esRango()) {
      paste("<b><font size=\"3\">Flujo entre pares consecutivos de nodos ",
            as.character.POSIXt(input$rangoTiempo[1],
                                format = "%I:%M%p"),
            " - ",
            as.character.POSIXt(input$rangoTiempo[2],
                                format = "%I:%M%p"),
            "</font></b>")
    }
  })
  
  source('dibujar_grafo_total.R', local = TRUE)
  
  output$grafoCompleto <- renderPlot({
    if (esRango()) {
      plot(grafoCompleto(), 
           edge.label = paste(E(grafoCompleto())$suma),
           vertex.label.dist=0,
           vertex.label.cex=2, 
           vertex.size = 20, 
           edge.width = E(grafoCompleto())$suma*0.0018,
           edge.curved= T, 
           edge.loop.angle=pi/4,
           edge.loop.angle2=pi/4,
           edge.arrow.size = 1.5,
           edge.label.cex = 1.2,
           layout=layout.circle)
      title(main = "Red de Flujo Linea 2")
    }
  }, height = 700, width = 700)
  
  source('dibujo_grafos.R', local = TRUE)
  
  output$grafoSubida <- renderPlot({
    if (esRango()) {
      grafo <- grafos()$grafoSubiendo
      
      plot.igraph(grafo, 
           edge.label = paste(E(grafo)$FlujoS), 
           vertex.label.dist = 0,
           vertex.label.cex = 2, 
           vertex.size = 35, 
           vertex.color = 'dodgerblue',
           edge.width = E(grafo)$FlujoS/quantile(E(grafo)$FlujoS)[2],
           edge.curved = 0,
           edge.arrow.size = 2,
           edge.label.cex = 1.2
      )
      title(main = "Sentido Centro - La Hechicera")
    }
  })
  
  output$grafoSubidaRedim <- renderUI({
    plotOutput("grafoSubida", height = 700, width = 700)
  })
  
  output$grafoBajada <- renderPlot({
    if (esRango()) {
      grafo <- grafos()$grafoBajando
      
      plot.igraph(grafo, 
                  edge.label = paste(E(grafo)$FlujoB), 
                  vertex.label.dist = 0,
                  vertex.label.cex = 2, 
                  vertex.size = 35, 
                  vertex.color = 'dodgerblue',
                  edge.width = E(grafo)$FlujoB/quantile(E(grafo)$FlujoB)[2],
                  edge.curved = 0,
                  edge.arrow.size = 2,
                  edge.label.cex = 1.2
      )
      title(main = "Sentido La Hechicera - Centro")
    }
  })
  
  output$grafoBajadaRedim <- renderUI({
    plotOutput("grafoBajada", height = 700, width = 700)
  })
  
  buses <- reactive({
    seleccion <- input$seleccionNodos
    nodo <- c()
    
    for (i in 1:numNodos) {
      if (seleccion == paste("Nodo ", i)) {
        nodo <- i
        break
      }
    }
    
    rango <- calcularRango(input)
    buses <- c()
    
    busesSubida <- conteoBuses[rango$desde:rango$hasta, i*2]
    busesSubida <- as.vector(as.matrix(busesSubida))
    busesBajada <- NULL
    if (i != 8) {
      busesBajada <- conteoBuses[rango$desde:rango$hasta, i*2 + 1]
      busesBajada <- as.vector(as.matrix(busesBajada))
    }
    intervalos <- conteoBuses[rango$desde:rango$hasta, 1]
    
    return(list(
      busesSubida = busesSubida,
      busesBajada = busesBajada,
      intervalos = as.vector(as.matrix(intervalos)),
      rango = rango
    ))
  })
  
  output$busesSubida <- renderPlot({
    if (esRango()) {
      ejeX <- buses()$rango$desde:buses()$rango$hasta
      plot(ejeX,
           buses()$busesSubida,
           type = "o",
           xlab = "",
           ylab = "Número de buses",
           axes = FALSE)
      axis(2)
      axis(1,
           at = ejeX,
           labels = buses()$intervalos,
           las = 2)
      box()
      title(main = "Conteo de buses en sentido Centro - La Hechicera")
    }
  })
  
  output$busesBajada <- renderPlot({
    if (esRango() & !is.null(buses()$busesBajada)) {
      ejeX <- buses()$rango$desde:buses()$rango$hasta
      plot(ejeX,
           buses()$busesBajada,
           type = "o",
           xlab = "",
           ylab = "Número de buses",
           axes = FALSE)
      axis(2)
      axis(1,
           at = ejeX,
           labels = buses()$intervalos,
           las = 2)
      box()
      title(main = "Conteo de buses en sentido La Hechicera - Centro")
    }
  })
  
  estadisticas <- reactive({
    if (esRango()) {
      rango <- calcularRango(input)
      viajes <- viajesData[rango$desde:rango$hasta, 2:(2*numNodos+1)]
      
      estadisticasOrigenes <- NULL
      estadisticasDestinos <- NULL
      
      for (i in 1:numNodos) {
        origenes <- as.vector(as.matrix(viajes[, 2*i-1]))
        destinos <- as.vector(as.matrix(viajes[, 2*i]))
        estadisticasOrigenes <- rbind(estadisticasOrigenes, c(paste(i),
                                                              sum(origenes),
                                                              min(origenes),
                                                              max(origenes),
                                                              round(mean(origenes), 2),
                                                              round(var(origenes), 2),
                                                              round(sd(origenes), 2)
        ))
        
        estadisticasDestinos <- rbind(estadisticasDestinos, c(paste(i),
                                                              sum(destinos),
                                                              min(destinos),
                                                              max(destinos),
                                                              round(mean(destinos), 2),
                                                              round(var(destinos), 2),
                                                              round(sd(destinos), 2)
        ))
      }
      
      nombresColumnas <- c("Nodo", 
                           "Total", 
                           "Mínimo", 
                           "Máximo", 
                           "Media",
                           "Varianza",
                           "Desviación Estándar")
      colnames(estadisticasOrigenes) <- nombresColumnas
      colnames(estadisticasDestinos) <- nombresColumnas
      
      return(list(
        origenes = estadisticasOrigenes,
        destinos = estadisticasDestinos
      ))
    }
  })
  
  output$tituloEstOrigenes <- renderText({
    if (esRango()) {
      paste("<b><font size=\"3\">Estadísticas sobre viajes de origen ",
            as.character.POSIXt(input$rangoTiempo[1],
                                format = "%I:%M%p"),
            " - ",
            as.character.POSIXt(input$rangoTiempo[2],
                                format = "%I:%M%p"),
            "</font></b>")
    }
  })
  
  output$estadisticasOrigenes <- renderTable({
    estadisticas()$origenes
  }, width = "100%")
  
  output$tituloEstDestinos <- renderText({
    if (esRango()) {
      paste("<b><font size=\"3\">Estadísticas sobre viajes de destino ",
            as.character.POSIXt(input$rangoTiempo[1],
                                format = "%I:%M%p"),
            " - ",
            as.character.POSIXt(input$rangoTiempo[2],
                                format = "%I:%M%p"),
            "</font></b>")
    }
  })
  
  output$estadisticasDestinos <- renderTable({
    estadisticas()$destinos
  }, width = "100%")
}

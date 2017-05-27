dibujarGrafos <- function(MatrizViajes) {
  numNodos
  
  FlujoS <- c()
  FlujoB <- c()
  
  #Contrucción de Pesos Suben y Bajan
  
  for(i in 1:sum(1:numNodos-1)){
    FlujoS[i] <- MatrizViajes[NodoOS[i], NodoDS[i]]
    FlujoB[i] <- MatrizViajes[NodoOB[i], NodoDB[i]]
  }
  
  # Grafo Subiendo
  
  DataGrafoS <- data.frame(NodoOS, NodoDS, FlujoS)
  GrafoS <- graph.data.frame(DataGrafoS, directed = T)
  
  # plot(GrafoS, edge.label = paste(E(GrafoS)$FlujoS), vertex.label.dist=0
  #      ,vertex.label.cex=0, vertex.size = 30, edge.width = E(GrafoS)$FlujoS/100,
  #      vertex.color='blue1', edge.curved=0.1)
  
  # Grafo Bajando
  
  DataGrafoB <- data.frame(NodoOB, NodoDB, FlujoB)
  GrafoB <- graph.data.frame(DataGrafoB, directed = T)
  
  # plot(GrafoB, edge.label = paste(E(GrafoB)$FlujoB), vertex.label.dist=0
  #      ,vertex.label.cex=0, vertex.size = 30, edge.width = E(GrafoB)$FlujoB/50,
  #      vertex.color='blue1', edge.curved=0.1)
  
  return(list(
    grafoSubiendo = GrafoS,
    grafoBajando = GrafoB
  ))
}

grafos <- reactive({
  return(dibujarGrafos(viajesInfo()$viajes))
})

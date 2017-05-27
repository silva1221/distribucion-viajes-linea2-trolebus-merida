dibujarGrafoCompleto <- function(MatrizViajes) {
  suma <- c()
  for(i in 1:length(NodoO)){
    if( i <= (length(NodoO)/2))
      suma[i] <- sum(MatrizViajes[NodoO[i], NodoD[i]:numNodos])
    else
      suma[i] <- sum(MatrizViajes[NodoO[i], NodoD[i]:1])
  }
  
  for(i in 2:(numNodos-1)){
    suma[i] <- suma[i-1] + suma[i] - sum(MatrizViajes[1:i,i])
  }
  
  aux <- (numNodos+1)
  for(i in (numNodos-1):2){
    suma[aux] <- suma[aux-1] + suma[aux] - sum(MatrizViajes[numNodos:i, i])
    aux <- aux +1
  }
  
  #GRafo:
  
  DataGrafo <- data.frame(NodoO, NodoD, suma)
  
  # Se Crea el objeto de tipo grafo
  grafo <- graph.data.frame(DataGrafo, directed = T)
  
  return(grafo)
}

grafoCompleto <- reactive({
  return(dibujarGrafoCompleto(viajesInfo()$viajes))
})
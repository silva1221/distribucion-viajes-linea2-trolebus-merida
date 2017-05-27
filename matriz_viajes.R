#Funcion Calcula Matriz OD
matrizViajes <- function(Origen, Destino){
  
  if(length(Origen) != length(Destino)) {
    #print('Las restricciones de Origen Destino son incorrectas')
  }
  else {
    #Factor Correctivo: Satisfacer Origenes=Destinos  
    if(sum(Origen) != sum(Destino)) {
      factorcor <- sum(Destino)/sum(Origen)
      Origen<-Origen*factorcor
      for(i in 1:sum(Origen-trunc(Origen)))
        Origen[i]<-Origen[i]+1
      Origen<-trunc(Origen)
    }
    
    #Declaracion de Matriz Semilla de 1nos y restricciones de diagonal nula
    NroNodos<-length(Origen)
    MatrizViajes <- matrix(1,NroNodos,NroNodos)
    for (i in 1:NroNodos)
      MatrizViajes[i,i] = 0
    
    #Inicializacion de vectores de expansion de iteraciones
    ExpansionFilas <- c(rep(1, NroNodos))
    ExpansionColumna <- c(rep(1, NroNodos))
    ExpansionFilasAnt <- c(rep(0, NroNodos))
    ExpansionColumnaAnt <- c(rep(0, NroNodos))
    SumasFilas <- c()
    SumasColumnas <- c()
    
    #Repetir hasta lograr convergencia
    while (sum(MatrizViajes) != sum(Origen)) {
      for(i in 1:NroNodos)
        SumasFilas[i] <- sum(MatrizViajes[i,])
      
      for(i in 1:NroNodos)  
        if((SumasFilas[i])==0)
          ExpansionFilas[i] <- 0
        else
          ExpansionFilas[i]<-Origen[i]/SumasFilas[i]
        
        for(i in 1:NroNodos)
          MatrizViajes[i,]<-(MatrizViajes[i,]*ExpansionFilas[i])
        
        
        for(i in 1:NroNodos)
          SumasColumnas[i] <- sum(MatrizViajes[,i])
        
        for(i in 1:NroNodos)  
          if((SumasColumnas[i])==0)
            ExpansionColumna[i] <- 0
          else
            ExpansionColumna[i]<-Destino[i]/SumasColumnas[i]
          
          for(i in 1:NroNodos){
            MatrizViajes[,i]<-(MatrizViajes[,i]*ExpansionColumna[i])
          }
          
          if (all(ExpansionFilas == ExpansionFilasAnt) & all(ExpansionColumna == ExpansionColumnaAnt))
            break 
          
          MatrizViajes <- trunc(MatrizViajes)
          ExpansionFilasAnt <- ExpansionFilas
          ExpansionColumnaAnt <- ExpansionColumna
    }
    
    MatrizViajes <- trunc(MatrizViajes)
    
    for(i in 1:NroNodos){
      for(j in 1:NroNodos){
        if (i == j)
          next
        
        if(sum(MatrizViajes[i,]) == Origen[i])
          break
        
        if((sum(MatrizViajes[i,])<Origen[i]) & (sum(MatrizViajes[,j])<Destino[j]))
          MatrizViajes[i,j] = MatrizViajes[i,j] + 1
      }
    }
    return(MatrizViajes)
  }
}
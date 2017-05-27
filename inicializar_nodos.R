numNodos <- (length(viajesData) - 1)/2

NodoOS <- NULL
NodoDS <- NULL
NodoOB <- NULL
NodoDB <- NULL
NodoO <- c(seq(1,numNodos-1), seq(numNodos,2))
NodoD <- c(seq(2,numNodos), seq(numNodos-1,1))

#Contruccion de los Vertices Origen Destino
for(i in 1:(numNodos-1)){
  NodoOS <- c(NodoOS, rep(i,numNodos-i))
  NodoDS <- c(NodoDS, seq(i+1,numNodos))
  NodoOB <- c(NodoOB, rep(numNodos+1-i, numNodos-i))
  NodoDB <- c(NodoDB, seq(1,(numNodos-i))) 
}
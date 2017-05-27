calcularRango <- function(input) {
  horaDesde <- strtoi(as.character.POSIXt(input$rangoTiempo[1],
                                          format = "%H"),
                      base = 10)
  minutoDesde <- strtoi(as.character.POSIXt(input$rangoTiempo[1],
                                            format = "%M"),
                        base = 10)
  horaHasta <- strtoi(as.character.POSIXt(input$rangoTiempo[2],
                                          format = "%H"),
                      base = 10)
  minutoHasta <- strtoi(as.character.POSIXt(input$rangoTiempo[2],
                                            format = "%M"),
                        base = 10)
  
  desde <- (horaDesde - 7)*6 + minutoDesde/10 + 1
  hasta <- (horaHasta - 7)*6 + minutoHasta/10
  
  return(list(
    desde = desde,
    hasta = hasta
  ))
}
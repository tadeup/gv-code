xablau <- function(){
  
  map <- map <-readOGR(dsn='www/data/shapefiles/BRA/BRUFE250GC_SIR.shp', 
                       encoding = "UTF-8", stringsAsFactors = F)
  votes <- votes(year = 2014, #input$anoplot, #TESTAR COM ESSES PRIMEIRO DPS DEIXAR SO GENERICO
                 position = "Presidente",#input$pol, 
                 regional_aggregation = "Estado") #input$agregGeo)
  votes <- votes[votes$UF != 'ZZ',]
  
  coord <- as.data.frame(coordinates(map))
  names(coord) <- c("lng", "lat")
  
  map@data$SIGLAS <- c("AC", "AL", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MT", 
                       "MS", "MG", "PA", "PB", "PE", "PI", "RJ", "RN", "RS", "RR", 
                       "SC", "SP", "SE", "TO", "PR", "RO", "AM")
  
  map@data$CD_GEOCUF <- as.numeric(map@data$CD_GEOCUF)
  map@data$lng <- coord$lng
  map@data$lat <- coord$lat
  
  votes13 <- votes[votes$NUMERO_CANDIDATO == 13 & votes$NUM_TURNO == 1,][,8:12]
  votes16 <- votes[votes$NUMERO_CANDIDATO == 16 & votes$NUM_TURNO == 1,][,8:12]
  votes20 <- votes[votes$NUMERO_CANDIDATO == 20 & votes$NUM_TURNO == 1,][,8:12]
  votes21 <- votes[votes$NUMERO_CANDIDATO == 21 & votes$NUM_TURNO == 1,][,8:12]
  votes27 <- votes[votes$NUMERO_CANDIDATO == 27 & votes$NUM_TURNO == 1,][,8:12]
  votes28 <- votes[votes$NUMERO_CANDIDATO == 28 & votes$NUM_TURNO == 1,][,8:12]
  votes29 <- votes[votes$NUMERO_CANDIDATO == 29 & votes$NUM_TURNO == 1,][,8:12]
  votes40 <- votes[votes$NUMERO_CANDIDATO == 40 & votes$NUM_TURNO == 1,][,8:12]
  votes43 <- votes[votes$NUMERO_CANDIDATO == 43 & votes$NUM_TURNO == 1,][,8:12]
  votes45 <- votes[votes$NUMERO_CANDIDATO == 45 & votes$NUM_TURNO == 1,][,8:12]
  votes50 <- votes[votes$NUMERO_CANDIDATO == 50 & votes$NUM_TURNO == 1,][,8:12]
  votes95 <- votes[votes$NUMERO_CANDIDATO == 95 & votes$NUM_TURNO == 1,][,8:12]
  votes96 <- votes[votes$NUMERO_CANDIDATO == 96 & votes$NUM_TURNO == 1,][,8:12]
  
  parties <- c(13,16,20,21,27,28,29,40,43,45,50,95,96)
  lista <- list(votes13, votes16, votes20, votes21, votes27, votes28, votes29, votes40,
                votes43, votes45, votes50, votes95, votes96)
  
  vec <- rep(0,13)
  for (i in 1:13){
    vote <- lista[[i]][,1]
    vec <- vec + vote
  }
  
  matrizona <- votes13[,2:5]
  
  for(i in 1:13){
    matrizona <- cbind(matrizona, lista[[i]][,1])
  }
  
  colnames(matrizona) = c("CODIGO_MACRO","NOME_MACRO", "UF",
                         "NOME_UF","p13","p16","p20","p21","p27","p28","p29","p40","p43",
                         "p45","p50","p95","p96")
  
  matrizona$soma <- vec
  
  map@data <- left_join(map@data, matrizona, by = c('SIGLAS' = 'UF'))
  map@data <- map@data[,3:23]
  
  porcentagem <- rep(0,27)
  for(i in 1:27){
    porc <- sum(map@data[i,8:20])/map@data[i,21]
    porcentagem[i] = porc
  }  

  for(i in 8:20){
    b <- map@data[,i]/map@data[,21]
    map@data <- cbind(map@data, b)
  }
  
  colnames(map@data)[22:34] <-c("p13%","p16%","p20%","p21%","p27%","p28%","p29%","p40%","p43%",
                                 "p45%","p50%","p95%","p96%")
  
  leaflet() %>% 
    addTiles() %>% 
    addMinicharts(lng = map@data$lng, lat = map@data$lat,
                  chartdata = map@data[,8:20], type = "pie",
                  colorPalette = c("red",
                                   "dodgerblue",
                                   "firebrick",
                                   "forestgreen",
                                   "gold",
                                   "black",
                                   "#A7A7A7",
                                   "orange",
                                   "yellow",
                                   "blue",
                                   "magenta",
                                   "purple",
                                   "gray"),
                  showLabels = FALSE, legend = TRUE,
                  width = 30)
}

  
  
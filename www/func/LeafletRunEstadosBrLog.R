leaflet_run_estados_br <- function(){
  leaflet(PoligonData()) %>% 
    addTiles() %>% 
    addMarkers(lng = ~lng, lat = ~lat, popup = ~paste(sprintf("<h3>%s</h3>",NOME_UF),
                                                     "<b>Numero de Votos: </b>",
                                                     log(QTDE_VOTOS)))
}

dfCepesp <- cepespdata(year = 2014,
                       columns_list = list("NUM_TURNO","NUMERO_CANDIDATO","QTDE_VOTOS","NOME_UF", "UF","NUMERO_PARTIDO", "COD_MUN_IBGE"),
                       state = "AM",
                       regional_aggregation = "Municipio",
                       political_aggregation = "Partido",
                       party = 13)


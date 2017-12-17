leaflet_run_estados_br <- function(){
  pal <- colorNumeric(
    palette = "Blues",
    domain = PoligonData()$QTDE_VOTOS) #MUDAR ISSO QUANDO FOR PLOTAR OUTRAS COISAS
  labels <- sprintf(
    "<strong>%s</strong><br/>%g votos",
    PoligonData()$NOME_UF, PoligonData()$QTDE_VOTOS) %>% 
    lapply(htmltools::HTML)
  
  leaflet(PoligonData()) %>% 
    addTiles() %>% 
    addMarkers(lng = ~lng, lat = ~lat, popup = ~paste(sprintf("<h3>%s</h3>",NOME_UF),
                                                     "<b>Numero de Votos: </b>",
                                                     QTDE_VOTOS),
               group = "Markers") %>% 
    
    addPolygons(stroke = T,color = "black",weight = 1,fillColor = ~pal(QTDE_VOTOS), fillOpacity = 0.9,opacity = 0.9, smoothFactor = 0.5,
                highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto"),
                group = "Polygons") %>% 
    
    setMaxBounds(PoligonData()@bbox[1],PoligonData()@bbox[2],PoligonData()@bbox[3],PoligonData()@bbox[4]) %>%
    
    addLegend(position = "bottomright",pal = pal,values = ~QTDE_VOTOS) %>%
    
    addEasyButton(easyButton(
      icon="fa-globe", title="Zoom to Level 1",
      onClick=JS("function(btn, map){ map.setZoom(4); }"))) %>%
    
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>% 
    
    addLayersControl(overlayGroups = c("Markers","Polygons"),
                     options = layersControlOptions(collapsed = TRUE))
}

dfCepesp <- cepespdata(year = 2014,
                       columns_list = list("NUM_TURNO","NUMERO_CANDIDATO","QTDE_VOTOS","NOME_UF", "UF","NUMERO_PARTIDO", "COD_MUN_IBGE"),
                       state = "AM",
                       regional_aggregation = "Municipio",
                       political_aggregation = "Partido",
                       party = 13)


plotBrMunPib <- function(){
  pal <- colorNumeric(
    palette = "Blues",
    domain = PoligonData()$QTDE_VOTOS/PoligonData()$PIB) #MUDAR ISSO QUANDO FOR PLOTAR OUTRAS COISAS
  labels <- sprintf(
    "<strong>%s</strong><br/>%g votos / populacao",
    PoligonData()$NOME_MUNICIPIO, PoligonData()$QTDE_VOTOS/PoligonData()$PIB) %>% 
    lapply(htmltools::HTML)
  
  leaflet(PoligonData(), options = leafletOptions(zoomControl = T,minZoom = 4,maxZoom = 10)) %>%
    
    addPolygons(stroke = T,color = "black",weight = 1,fillColor = ~pal(QTDE_VOTOS/PIB), fillOpacity = 0.9,opacity = 0.9, smoothFactor = 0.5,
                highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px",direction = "auto")) %>%
    
    setMaxBounds(PoligonData()@bbox[1],PoligonData()@bbox[2],PoligonData()@bbox[3],PoligonData()@bbox[4]) %>%
    
    addLegend(position = "bottomright",pal = pal,values = ~QTDE_VOTOS/PIB) %>%
    
    addEasyButton(easyButton(
      icon="fa-globe", title="Zoom to Level 1",
      onClick=JS("function(btn, map){ map.setZoom(4); }"))) %>%
    
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="Locate Me",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  
}

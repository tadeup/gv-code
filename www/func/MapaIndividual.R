mapaIndividual <- function(){
  #prefeito ou presidente
  output$cargo <- renderUI({
    if(input$ano %in% c(2000,2004,2008,2012)){
      selectInput("cargo", "Cargo",cargos1)
    } else {
      selectInput("cargo", "Cargo",cargos2)
    }
  })
  
  #party or candidate
  output$partyORcandidate <- renderUI({
    if(input$polAgr == "Candidato"){
      textInput("poc","Numero do candidato", value = 43)
    } else {
      textInput("poc","Numero do Partido", value = 43)
    }
  })
  
  #regAgr
  output$regAgr <- renderUI({
    if(input$estado == "BRASIL"){
      selectInput(inputId = "regAgr", label = "Agregacao regional", choices = regAgreg1)
    } else {
      selectInput(inputId = "regAgr", label = "Agregacao regional", choices = regAgreg2)
    }
  })
  
  #akira_beg
  output$AgregGeo <- renderUI({
    if(input$local == "BRASIL"){
      selectInput(inputId = "agregGeo", label = "Agregacao geografica", choices = "Estado")
    } else {
      selectInput(inputId = "agregGeo", label = "Agregacao geografica", choices = "Municipio")
    }
  })
  #akira_end
  
  #montando o dataframe
  PoligonData <- reactive({
    
    input$goMap
    
    isolate({
      if(input$regAgr == "Estado"){
        dataExtra <- pibse[[which(anos==input$ano)]]
        colunas <- colunas2
      } else {
        dataExtra <- pibs[[which(anos==input$ano)]]
        colunas <- colunas1
      }
      if(input$estado == "BRASIL"){
        estado <- "all"
      } else {
        estado <- tolower(input$estado)
      }
      if(input$polAgr == "Candidato"){
        party <- NULL
        candidate <- input$poc
      } else {
        candidate <- NULL
        party <- input$poc
      }
      dfCepesp <- cepespdata(year = input$ano,
                             columns_list = colunas,
                             state = estado,
                             regional_aggregation = input$regAgr,
                             political_aggregation = input$polAgr,
                             position = input$cargo,
                             party = party,
                             candidate_number = candidate)
      dfCepesp <- dfCepesp[dfCepesp$NUM_TURNO == input$turno,]
      dfCepesp <- left_join(dfCepesp, estados, by="UF")
      if(nrow(dfCepesp)==0){
        return(T)
      }
      if(input$estado == "BRASIL"){
        if(input$regAgr == "Estado"){
          PoligonData <-readOGR("www\\data\\shapefiles\\BRA\\BRUFE250GC_SIR.shp", stringsAsFactors = F)
          PoligonData@data$CD_GEOCUF <- as.numeric(PoligonData@data$CD_GEOCUF)
          #akira_beg
          PoligonData@data$lng <- coordinates(PoligonData)[,1]
          PoligonData@data$lat <- coordinates(PoligonData)[,2]
          #akira_end
          PoligonData@data <- left_join(PoligonData@data, dfCepesp, by=c('CD_GEOCUF' = 'codigo')) %>%
            left_join(dataExtra, by=c('UF'))
        } else {
          PoligonData <-readOGR("www\\data\\shapefiles\\BRA\\BRMUE250GC_SIR.shp", stringsAsFactors = F)
          PoligonData@data$CD_GEOCMU <- as.numeric(PoligonData@data$CD_GEOCMU)
          #akira_beg
          PoligonData@data$lng <- coordinates(PoligonData)[,1]
          PoligonData@data$lat <- coordinates(PoligonData)[,2]
          #akira+end
          PoligonData@data <- left_join(PoligonData@data, dfCepesp,by=c('CD_GEOCMU' = 'COD_MUN_IBGE') ) %>%
            left_join(dataExtra, by=c('CD_GEOCMU' = 'COD_MUN_IBGE'))
        }
      } else {
        PoligonData <-readOGR(sprintf("www\\data\\shapefiles\\%s\\%sMUE250GC_SIR.shp",input$estado,estados[estados==input$estado,][[2]]),stringsAsFactors = F)
        PoligonData@data$CD_GEOCMU <- as.numeric(PoligonData@data$CD_GEOCMU)
        #akira_beg
        PoligonData@data$lng <- coordinates(PoligonData)[,1]
        PoligonData@data$lat <- coordinates(PoligonData)[,2]
        #akira_end
        PoligonData@data <- left_join(PoligonData@data, dfCepesp,by=c('CD_GEOCMU' = 'COD_MUN_IBGE') ) %>%
          left_join(dataExtra, by=c('CD_GEOCMU' = 'COD_MUN_IBGE'))
      }
      
      return(PoligonData)
    })
  })
  
  #contuindo o mapa
  output$erroMapa1 <- renderText({
    if(class(PoligonData()) == "logical"){
      return("nao foi possivel encontrar nenhum partido ou candidato com estas especificacoes")}
  })
  
  output$chartmapa <- renderLeaflet({
    if (input$local == "BRASIL"){
        source("www/func/PieChartEstado.r", local = T)
        xablau()
    }
  })

  output$mapa <- renderLeaflet({
    input$goMap
    if(class(PoligonData()) == "logical"){
      return()
    } else {
      isolate({
        if(input$regAgr == "Estado" ){
          if(input$metrica == "total de votos"){
            #akira_beg
            source("www/func/LeafletRunEstadosBr.r", local = T)
            #leaflet_run_estados_br()
            source("www/func/plotBrEstados.r", local = T)
            leaflet_run_estados_br()
            #akira_end
          } else if(input$metrica == "votos/populacao") {
            source("www/func/plotBrEstadosPop.r", local = T)
            plotBrEstadosPop()
          } else if(input$metrica == "votos/pib") {
            source("www/func/plotBrEstadosPib.r", local = T)
            plotBrEstadosPib()
          } else {
            source("www/func/plotBrEstadosLog.r", local = T)
            plotBrEstadosLog()

            #source("www/func/LeafletRunEstadosBrLog.r", local = T)
            #leaflet_run_estados_br()
          }
          
        } else if(input$regAgr == "Municipio"){
          if(input$metrica == "total de votos"){
            #source("www/func/plotBrMun.r", local = T)
            #plotBrMun()
            
            #akira_beg
            source("www/func/LeafletRunMunicipiosBr.r", local = T)
            leaflet_run_municipios()
            #akira_end
          } else if(input$metrica == "votos/populacao") {
            source("www/func/plotBrMunPop.r", local = T)
            plotBrMunPop()
          } else if(input$metrica == "votos/pib") {
            source("www/func/plotBrMunPib.r", local = T)
            plotBrMunPib()
          } else {
            #source("www/func/plotBrMunLog.r", local = T)
            #plotBrMunLog()
            
            source("www/func/LeafletRunMunicipiosBrLog.r", local = T)
            leaflet_run_municipios_log()
          }
        }
      })
    }
  })
}

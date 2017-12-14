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
      textInput("poc","Numero do candidato", value = 13)
    } else {
      textInput("poc","Numero do Partido", value = 13)
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
          PoligonData@data <- left_join(PoligonData@data, dfCepesp, by=c('CD_GEOCUF' = 'codigo')) %>%
            left_join(dataExtra, by=c('UF'))
        } else {
          PoligonData <-readOGR("www\\data\\shapefiles\\BRA\\BRMUE250GC_SIR.shp", stringsAsFactors = F)
          PoligonData@data$CD_GEOCMU <- as.numeric(PoligonData@data$CD_GEOCMU)
          PoligonData@data <- left_join(PoligonData@data, dfCepesp,by=c('CD_GEOCMU' = 'COD_MUN_IBGE') ) %>%
            left_join(dataExtra, by=c('CD_GEOCMU' = 'COD_MUN_IBGE'))
        }
      } else {
        PoligonData <-readOGR(sprintf("www\\data\\shapefiles\\%s\\%sMUE250GC_SIR.shp",input$estado,estados[estados==input$estado,][[2]]),stringsAsFactors = F)
        PoligonData@data$CD_GEOCMU <- as.numeric(PoligonData@data$CD_GEOCMU)
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
  output$mapa <- renderLeaflet({
    input$goMap
    if(class(PoligonData()) == "logical"){
      return()
    } else {
      isolate({
        if(input$regAgr == "Estado" ){
          if(input$metrica == "total de votos"){
            source("www/func/PlotBrEstados.R", local = T)
            plotBrEstados()
          } else if(input$metrica == "votos/populacao") {
            source("www/func/PlotBrEstadosPop.R", local = T)
            plotBrEstadosPop()
          } else if(input$metrica == "votos/pib") {
            source("www/func/PlotBrEstadosPib.R", local = T)
            plotBrEstadosPib()
          } else {
            source("www/func/PlotBrEstadosLog.R", local = T)
            plotBrEstadosLog()
          }
          
        } else if(input$regAgr == "Municipio"){
          if(input$metrica == "total de votos"){
            source("www/func/PlotBrMun.R", local = T)
            plotBrMun()
          } else if(input$metrica == "votos/populacao") {
            source("www/func/PlotBrMunPop.R", local = T)
            plotBrMunPop()
          } else if(input$metrica == "votos/pib") {
            source("www/func/PlotBrMunPib.R", local = T)
            plotBrMunPib()
          } else {
            source("www/func/PlotBrMunLog.R", local = T)
            plotBrMunLog()
          }
        }
      })
    }
  })
}
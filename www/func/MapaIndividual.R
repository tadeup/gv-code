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
  
  #opções de índices
  output$chartSpawn<-renderUI({
    if(input$indices %in% c("Gini Brasil") && input$cargo %in% c("Presidente")){
      box(plotlyOutput("gini"), title = "Gini e Partido", status = "primary")
    }else if(input$indices %in% c("Gini Estados") && input$cargo %in% c("Governador")){
    } else if (input$indices %in% c("Renda Per Capita") && input$cargo %in% c("Presidente")){
      box(plotlyOutput("desemprego"), title = "Renda per capita", status = "primary")
    } else if (input$indices %in% c("Desemprego") && input$cargo %in% c("Governador")){
      box(plotlyOutput("desemprego"), title = "Desemprego e Partido", status = "primary")
    } else if ((input$indices %in% c("PEA") && input$cargo %in% c("Presidente"))){
      box(plotlyOutput("extrema_pobreza"), title = "PEA", status = "primary")
    }else {
      box(plotlyOutput("extrema_pobreza"), title = "Extrema pobreza e Partido", status = "primary")
    }
  })
  
  #âmbito nacional ou estadual
  output$estado <- renderUI({
    if(input$cargo == "Presidente"){
      selectInput("estado", "Estado", "BR")}
    else{
      selectInput("estado", "Estado", estadosdiv )
    }
  })
  output$indices<-renderUI({
    if(input$cargo == "Presidente"){
      selectInput("indices", "Indices", indicesbr)
    }else if(input$cargo == "Governador"){
      selectInput("indices", "Indices", indicesestado)
    }
  })
  
  #------------------------------------------------------------------
  index <- reactive({
    
    
    index<-input$indices
    
  })
  
  #--------------------------------------------------------------------
  #plot
  
  output$chart <- renderPlotly({
    
    if(index()=="Gini Estados"){ 
      igini_estado<-subset(igini, subset=(Sigla ==input$estado))
      igini1<-igini_estado[,3]
      plot_ly(x=anos,y=igini1,type="scatter", mode='marker', marker = list(size = 10, color = 'blue', opacity = 0.8))
    }else if(index()=="Desemprego"){
      idesemprego_estado<-subset(idesemprego, subset=(Sigla ==input$estado))
      idesemprego1<-idesemprego_estado[,3]
      plot_ly(x=anos,y=idesemprego1, type="scatter", mode='marker', marker = list(size = 10, color = 'blue', opacity = 0.8))
      
    }else if(index()=="Pobreza"){
      ipobreza_estado<-subset(ipobreza, subset=(Sigla ==input$estado))
      ipobreza1<-ipobreza_estado[,3]   
      plot_ly(x=anos,y=ipobreza1, type="scatter", mode='marker', marker = list(size = 10, color = 'blue', opacity = 0.8))
    }else if(index()=="Gini Brasil"){
      
      iginibr1<-iginibr_1[,2]
      iginibr2<-iginibr_2[,2]
      iginibr3<-iginibr_3[,2]
      iginibr4<-iginibr_4[,2]
      
      
      chart<-plot_ly(x=anos_ginibr,y=iginibr1,type="scatter", mode='lines',line = list(color = color1,width=4),connectgaps = FALSE)
      chart<-add_trace(chart, y = iginibr2, name = 'trace 1', mode = 'lines',line = list(color = color2,width=4),connectgaps = FALSE)
      chart<-add_trace(chart, y = iginibr3, name = 'trace 2', mode = 'lines',line = list(color = color2,width=4),connectgaps = FALSE)
      chart<-add_trace(chart, y = iginibr4, name = 'trace 3', mode = 'lines',line = list(color = color2,width=4),connectgaps = FALSE)        
    }else if(index()=="Pib per capita"){
      
      ppbr1<-ppbr_1[,2]
      ppbr2<-ppbr_2[,2]
      ppbr3<-ppbr_3[,2]
      ppbr4<-ppbr_4[,5]
      
      chart<-plot_ly(x=anos_pp,y=ppbr1, type="scatter", mode='lines',line = list(color = color1,width=4), connectgaps = FALSE)  
      chart<-add_trace(chart, y = ppbr2, name = 'trace 1', mode = 'lines',line = list(color = color2,width=4),connectgaps = FALSE)
      chart<-add_trace(chart, y = ppbr3, name = 'trace 2', mode = 'lines',line = list(color = color2,width=4),connectgaps = FALSE)
      chart<-add_trace(chart, y = ppbr4, name = 'trace 3', mode = 'lines',line = list(color = color2,width=4),connectgaps = FALSE)
      
    }else if(index()=="PEA"){
      
      peabr1<-peabr[,2]
      plot_ly(x=anos_pea,y=peabr1, mode='lines') 
      
    }
    
  })
}

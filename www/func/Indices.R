indicesfunc<-function(){
  #indices
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
  
  #nacional ou estadual
  output$estado_escolhido <- renderUI({
    if(input$cargo == "Presidente"){
      selectInput("estado_escolhido", "Estado", "BR")}
    else{
      selectInput("estado_escolhido", "Estado", estadosdiv )
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
      # igini_estado<-subset(igini, subset=(Sigla ==input$estado_escolhido))
      # igini1<-igini_estado[,3]
      # plot_ly(x=anos_estados,y=igini1, type="scatter", mode='marker', marker = list(size = 10, color = 'blue', opacity = 0.8))
      partido_eleito <- vector()
      
      eleitos <- dados_k1[dados_k1$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[1] <- as.character(eleitos[eleitos$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      eleitosa <-  dados_k2[dados_k2$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[2] <- as.character(eleitosa[eleitosa$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      eleitosb <- dados_k3[dados_k3$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[3] <- as.character(eleitosb[eleitosb$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      eleitosC <- dados_k4[dados_k4$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[4] <- as.character(eleitosC[eleitosC$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      eleitosD <- dados_k5[dados_k5$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[5] <- as.character(eleitosD[eleitosD$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      igini_estado <- igini[igini$Sigla == input$estado_escolhido,]
      igini_estado <- igini_estado[igini_estado$Ano != 2004,]
      igini_estado <- igini_estado[igini_estado$Ano != 2008,]
      igini1<-igini_estado$Coeficiente_de_Gini
      #proxy 2012 para 2010
      an <- c(1998, 2002, 2006, 2012, 2014)
      aaa <- as.data.frame(matrix(0,length(an),2))
      aaa[,1] <- igini1
      aaa[,2] <- an
      aaa$partido_eleito <- as.factor(partido_eleito)
      chart <- plot_ly(aaa, x=an,y= igini1,type="scatter", mode='marker', marker = list(size = 30), color =~ partido_eleito, colors =  'Paired')
     chart <- layout(chart, title = "Indice Gini - Estadual", xaxis = list(title = "Anos", autorange = T), yaxis= list(title = "Indice Gini"), width = 700, height = 400)
    }
    else if(index()=="Desemprego"){
      partido_eleito <- vector()
      
      eleitos <- dados_k1[dados_k1$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[1] <- as.character(eleitos[eleitos$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      eleitosa <-  dados_k2[dados_k2$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[2] <- as.character(eleitosa[eleitosa$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      eleitosb <- dados_k3[dados_k3$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[3] <- as.character(eleitosb[eleitosb$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      eleitosC <- dados_k4[dados_k4$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[4] <- as.character(eleitosC[eleitosC$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      eleitosD <- dados_k5[dados_k5$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[5] <- as.character(eleitosD[eleitosD$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      idesemprego_estado<- idesemprego[idesemprego$Sigla == input$estado_escolhido,]
      idesemprego_estado <- idesemprego_estado[idesemprego_estado$Ano != 2004,]
      idesemprego_estado <- idesemprego_estado[idesemprego_estado$Ano != 2008,]

      idesemprego1<-idesemprego_estado$Taxa.de.Desemprego
      an <- c(1998, 2002, 2006, 2012, 2014)
      aaa <- as.data.frame(matrix(0,length(an),2))
      aaa[,1] <- idesemprego1
      aaa[,2] <- an
      aaa$partido_eleito <- as.factor(partido_eleito)
      chart <- plot_ly(aaa, x=an,y= idesemprego1,type="scatter", mode='marker', marker = list(size = 30), color =~ partido_eleito, colors =  'Paired')
      chart <- layout(chart, title = "Taxa de desemprego- Estadual", xaxis = list(title = "Anos", autorange = T), yaxis = list(title = "Taxa de desemprego"),width = 700, height = 400 )
  
    }else if(index()=="Pobreza"){
      partido_eleito <- vector()
      
      eleitos <- dados_k1[dados_k1$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[1] <- as.character(eleitos[eleitos$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      eleitosa <-  dados_k2[dados_k2$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[2] <- as.character(eleitosa[eleitosa$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      eleitosb <- dados_k3[dados_k3$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[3] <- as.character(eleitosb[eleitosb$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      eleitosC <- dados_k4[dados_k4$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[4] <- as.character(eleitosC[eleitosC$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)
      
      eleitosD <- dados_k5[dados_k5$DESC_SIT_TOT_TURNO == "ELEITO",]
      partido_eleito[5] <- as.character(eleitosD[eleitosD$SIGLA_UF == input$estado_escolhido,]$SIGLA_PARTIDO)

      ipobreza_estado <- ipobreza[ipobreza$Sigla == input$estado_escolhido,]
      ipobreza_estado <- ipobreza_estado[ipobreza_estado$Ano != 2004,]
      ipobreza_estado <- ipobreza_estado[ipobreza_estado$Ano != 2008,]
      
      ipobreza_estado1 <-ipobreza_estado[,3]
      an <- c(1998, 2002, 2006, 2012, 2014)
      aaa <- as.data.frame(matrix(0,length(an),2))
      aaa[,1] <- ipobreza_estado1
      aaa[,2] <- an
      aaa$partido_eleito <- as.factor(partido_eleito)
      chart <- plot_ly(aaa, x=an,y= ipobreza_estado1,type="scatter", mode='marker', marker = list(size = 30), color =~ partido_eleito, colors =  'Paired')
      chart <- layout(chart,  title = "Indice de Pobreza- Estadual", xaxis = list(title = "Anos"), yaxis = list(title = "Indice de Pobreza"), width = 700, height = 400)
      
    }else if(index()=="Gini Brasil"){
      
      iginibr1<-iginibr_1[,2]
      iginibr2<-iginibr_2[,2]
      iginibr3<-iginibr_3[,2]
      iginibr4<-iginibr_4[,2]
      
      
      chart<-plot_ly(x=anos_ginibr,y=iginibr1,type="scatter", name=partido_name1998, mode='lines+markers',line = list(color = color1,width=4),connectgaps = FALSE)
      chart<-add_trace(chart, y = iginibr2, name = partido_name2002, mode = 'lines+markers',line = list(color = color2,width=4),connectgaps = FALSE)
      chart<-add_trace(chart, y = iginibr3, name = partido_name2006, mode = 'lines+markers',line = list(color = color2,width=4),connectgaps = FALSE)
      chart<-add_trace(chart, y = iginibr4, name = partido_name2010, mode = 'lines+markers',line = list(color = color2,width=4),connectgaps = FALSE)        
      chart <- layout(chart, title = "Indice Gini - Brasil", width = 700, height = 400 )
  
      }else if(index()=="Pib per capita"){
      
      ppbr1<-ppbr_1[,2]
      ppbr2<-ppbr_2[,2]
      ppbr3<-ppbr_3[,2]
      ppbr4<-ppbr_4[,5]
      
      chart<-plot_ly(x=anos_pp,y=ppbr1, type="scatter", name = partido_name1998, mode='lines',line = list(color = color1,width=4), connectgaps = FALSE)  
      chart<-add_trace(chart, y = ppbr2, name = partido_name2002, mode = 'lines',line = list(color = color2,width=4),connectgaps = FALSE)
      chart<-add_trace(chart, y = ppbr3, name = partido_name2006, mode = 'lines',line = list(color = color2,width=4),connectgaps = FALSE)
      chart<-add_trace(chart, y = ppbr4, name = partido_name2010, mode = 'lines',line = list(color = color2,width=4),connectgaps = FALSE)
      chart <- layout(chart, title = "PIB per capita", width = 700, height = 400)
    }else if(index()=="PEA"){
      
      peabr1<-peabr[,2]
      chart <- plot_ly(x=anos_pea,y=peabr1, mode='lines') 
      chart <- layout(chart, title = "Populacao Economicamente Ativa", width = 700, height = 400)
    }
    
  })
  
  output$textinho <- renderTable({
    
    
    c1 <- c("Coeficiente Gini", "Populacao Economicamente Ativa (PEA)", "Domicilios com Extrema Pobreza", "Taxa de Desemprego", "PIB per capita") 
    c2 <- c("Consiste em um numero entre 0 e 1, com 0 representando completa igualdade e 1 completa desigualdade da distribuicao de renda.", "Parcela da populacao em idade ativa classificadas como ocupadas ou desocupadas.", "Domicilios com renda abaixo da linha da pobreza, considerada como uma estimativa do valor de uma cesta de alimentos com o minimo de calorias necessarias para suprir uma pessoa", "Consiste na divisao entre a populacao desempregada e a populacao economicamente ativa.", "Consiste na divisao entre o Produto Interno Bruto e a populacao do pais.")
    t12 <- as.data.frame(cbind(c1, c2))
    colnames(t12) <- c("Indices", "Info")
    textinho <- t12
    
  })
}
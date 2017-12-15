plotCandidatos2 <- function(){
  comparativo <- reactive({
    #primeiro partido
    nome_cand <- vector()
    cp <- vector()
    
    dfpt <- hpd1y1[hpd1y1$NUMERO_CANDIDATO == input$partido1,]
    cp[1] <- sum(dfpt$QTDE_VOTOS)*100/sum(hpd1y1$QTDE_VOTOS)
    v <- as.numeric(which(hpd2y1$NUMERO_CANDIDATO == input$partido1))[1] 
    nome_cand[1] <- hpd2y1$NOME_URNA_CANDIDATO[v]
    
    
    dfpt <- hpd1y2[hpd1y2$NUMERO_CANDIDATO == input$partido1,]
    cp[2] <- sum(dfpt$QTDE_VOTOS)*100/sum(hpd1y2$QTDE_VOTOS)
    v <- as.numeric(which(hpd2y2$NUMERO_CANDIDATO == input$partido1))[1] 
    nome_cand[2] <- hpd2y2$NOME_URNA_CANDIDATO[v]
    
    
    dfpt <- hpd1y3[hpd1y3$NUMERO_CANDIDATO == input$partido1,]
    cp[3] <- sum(dfpt$QTDE_VOTOS)*100/sum(hpd1y3$QTDE_VOTOS)
    v <- as.numeric(which(hpd2y3$NUMERO_CANDIDATO == input$partido1))[1] 
    nome_cand[3] <- hpd2y3$NOME_URNA_CANDIDATO[v]
    
    
    dfpt <- hpd1y4[hpd1y4$NUMERO_CANDIDATO == input$partido1,]
    cp[4] <- sum(dfpt$QTDE_VOTOS)*100/sum(hpd1y4$QTDE_VOTOS)
    v <- as.numeric(which(hpd2y4$NUMERO_CANDIDATO == input$partido1))[1] 
    nome_cand[4] <- hpd2y4$NOME_URNA_CANDIDATO[v]
    
    
    
    dfpt <- hpd1y5[hpd1y5$NUMERO_CANDIDATO == input$partido1,]
    cp[5] <- sum(dfpt$QTDE_VOTOS)*100/sum(hpd1y5$QTDE_VOTOS)
    v <- as.numeric(which(hpd2y5$NUMERO_CANDIDATO == input$partido1))[1] 
    nome_cand[5] <- hpd2y5$NOME_URNA_CANDIDATO[v]
    
    #para o outro partido
    
    cp2 <- vector()
    nome_cand2 <- vector()
    dfpsdb <- hpd1y1[hpd1y1$NUMERO_CANDIDATO == input$partido2,]
    cp2[1] <- sum(dfpsdb$QTDE_VOTOS)*100/sum(hpd1y1$QTDE_VOTOS)
    v <- as.numeric(which(hpd2y1$NUMERO_CANDIDATO == input$partido2))[1] 
    nome_cand2[1] <- hpd2y1$NOME_URNA_CANDIDATO[v]
    
    
    dfpsdb <- hpd1y2[hpd1y2$NUMERO_CANDIDATO == input$partido2,]
    cp2[2] <- sum(dfpsdb$QTDE_VOTOS)*100/sum(hpd1y2$QTDE_VOTOS)
    v <- as.numeric(which(hpd2y2$NUMERO_CANDIDATO == input$partido2))[1] 
    nome_cand2[2] <- hpd2y2$NOME_URNA_CANDIDATO[v]
    
    
    dfpsdb <- hpd1y3[hpd1y3$NUMERO_CANDIDATO == input$partido2,]
    cp2[3] <- sum(dfpsdb$QTDE_VOTOS)*100/sum(hpd1y3$QTDE_VOTOS)
    v <- as.numeric(which(hpd2y3$NUMERO_CANDIDATO == input$partido2))[1] 
    nome_cand2[3] <- hpd2y3$NOME_URNA_CANDIDATO[v]
    
    
    dfpsdb <- hpd1y4[hpd1y4$NUMERO_CANDIDATO == input$partido2,]
    cp2[4] <- sum(dfpsdb$QTDE_VOTOS)*100/sum(hpd1y4$QTDE_VOTOS)
    v <- as.numeric(which(hpd2y4$NUMERO_CANDIDATO == input$partido2))[1] 
    nome_cand2[4] <- hpd2y4$NOME_URNA_CANDIDATO[v]
    
    
    
    dfpsdb <- hpd1y5[hpd1y5$NUMERO_CANDIDATO == input$partido2,]
    cp2[5] <- sum(dfpsdb$QTDE_VOTOS)*100/sum(hpd1y5$QTDE_VOTOS)
    v <- as.numeric(which(hpd2y5$NUMERO_CANDIDATO == input$partido2))[1] 
    nome_cand2[5] <- hpd2y5$NOME_URNA_CANDIDATO[v]
    
    anos <-
      c(2014,2010, 2006,2002, 1998)
    cp2 <- round((cp2),1)
    cp <- round((cp),1)
    
    
    comparativo <- data.frame(cbind(cp, cp2,nome_cand, nome_cand2, anos))
    
    return(comparativo)
    
  })
  
  output$aa <- renderPlotly({
    
    
    
    aa <- plot_ly(comparativo(), x = comparativo()[,5], y = comparativo()[,1], name = input$partido1, type = 'scatter', mode = 'markers', hoverinfo = "y + text",
                 text = paste("Candidato:", comparativo()[,3]), marker = list( symbol = 'circle', size = data$cp*10, sizemode = 'area', color = "#E89B9B"),
                 mode = "markers" ) 
    
    
    aa <- add_trace(aa, y = comparativo()[,2], name = input$partido2, hoverinfo = "y + text",
                   text = paste("Candidato:", comparativo()[,4]),
                   marker = list(symbol = 'circle', size = comparativo()[,2]*10, sizemode = 'area', 
                                 color = "#3fcafc"), type = 'scatter',mode = 'markers', type = 'scatter' )
    aa <- layout(aa, title = "Percentual de votos por partido ", 
                xaxis = list( title = 'Anos',
                              autorange = T, 
                              type = "linear"
                ), 
                yaxis = list( title = "Percentual de votos por partido",
                              range = c(0, 60)
                ), width = 700, height = 450, legend = list(
                  
                )) 
    
  })
  
  #Tabela 1 
  tabela_comparativo1 <- reactive({
    despesap1 <- vector()
    mediadespesa <- vector()
    #partido 1
    df1 <- candidates(year = 2014, position = 'President', columns_list = list("NUMERO_PARTIDO", "DESPESA_MAX_CAMPANHA", "NOME_URNA_CANDIDATO", "DESC_SIT_TOT_TURNO"))
    d1 <- df1[df1$DESPESA_MAX_CAMPANHA != -1,]
    data_partido1 <- d1[d1$NUMERO_PARTIDO == input$partido1,]
    despesap1[1] <- data_partido1$DESPESA_MAX_CAMPANHA
    mediadespesa[1] <- data_partido1$DESPESA_MAX_CAMPANHA
    
    
    df2 <- candidates(year = 2014, position = 'Governor', columns_list = list("NUMERO_PARTIDO", "DESPESA_MAX_CAMPANHA", "NOME_URNA_CANDIDATO", "DESC_SIT_TOT_TURNO"))
    d2 <- df2[df2$DESPESA_MAX_CAMPANHA != -1,]
    data_2partido1 <- d2[d2$NUMERO_PARTIDO == input$partido1,]
    despesap1[2] <- sum(data_2partido1$DESPESA_MAX_CAMPANHA)
    mediadespesa[2] <- mean(data_2partido1$DESPESA_MAX_CAMPANHA)
    
    df3 <- candidates(year = 2014, position = 'Senador', columns_list = list("NUMERO_PARTIDO", "DESPESA_MAX_CAMPANHA", "NOME_URNA_CANDIDATO", "DESC_SIT_TOT_TURNO"))
    d3 <- df3[df3$DESPESA_MAX_CAMPANHA != -1,]
    data_3partido1 <- d3[d3$NUMERO_PARTIDO == input$partido1,]
    despesap1[3] <- sum(data_3partido1$DESPESA_MAX_CAMPANHA)
    mediadespesa[3] <- mean(data_3partido1$DESPESA_MAX_CAMPANHA)
    
    df4 <- candidates(year = 2014, position = 'Deputado Estadual', columns_list = list("NUMERO_PARTIDO", "DESPESA_MAX_CAMPANHA", "NOME_URNA_CANDIDATO", "COD_SIT_TOT_TURNO"))
    d4 <- df4[df4$DESPESA_MAX_CAMPANHA != -1,]
    data_4partido1 <- d4[d4$NUMERO_PARTIDO == input$partido1,]
    despesap1[4] <- sum(data_4partido1$DESPESA_MAX_CAMPANHA)
    mediadespesa[4] <- mean(data_4partido1$DESPESA_MAX_CAMPANHA)
    
    
    df5 <- candidates(year = 2014, position = 'Deputado Federal', columns_list = list("NUMERO_PARTIDO", "DESPESA_MAX_CAMPANHA", "NOME_URNA_CANDIDATO", "COD_SIT_TOT_TURNO"))
    d5 <- df5[df5$DESPESA_MAX_CAMPANHA != -1,]
    data_5partido1 <- d5[d5$NUMERO_PARTIDO == input$partido1,]
    despesap1[5] <- sum(data_5partido1$DESPESA_MAX_CAMPANHA)
    mediadespesa[5] <- mean(data_5partido1$DESPESA_MAX_CAMPANHA)
    
    
    #Para prefeito e vereador nao temos esses dados.. retorna 0 ou NA  a depender do ano
    
    Cargos <- c("Presidente", "Governador", "Senador", "Deputado Estadual", "Deputado Federal")
    
    #Partido 2
    despesap2 <- vector()
    mediadespesa2 <- vector()
    
    
    data_partido2 <- d1[d1$NUMERO_PARTIDO == input$partido2,]
    despesap2[1] <- data_partido2$DESPESA_MAX_CAMPANHA
    mediadespesa2[1] <- data_partido2$DESPESA_MAX_CAMPANHA 
    
    
    data_2partido2 <- d2[d2$NUMERO_PARTIDO == input$partido2,]
    despesap2[2] <- sum(data_2partido2$DESPESA_MAX_CAMPANHA)
    mediadespesa2[2] <- mean(data_2partido2$DESPESA_MAX_CAMPANHA)
    
    data_3partido2 <- d3[d3$NUMERO_PARTIDO == input$partido2,]
    despesap2[3] <- sum(data_3partido2$DESPESA_MAX_CAMPANHA)
    mediadespesa2[3] <- mean(data_3partido2$DESPESA_MAX_CAMPANHA)
    
    
    data_4partido2 <- d4[d4$NUMERO_PARTIDO == input$partido2,]
    despesap2[4] <- sum(data_4partido2$DESPESA_MAX_CAMPANHA)
    mediadespesa2[4] <- mean(data_4partido2$DESPESA_MAX_CAMPANHA)
    
    
    data_5partido2 <- d5[d5$NUMERO_PARTIDO == input$partido2,]
    despesap2[5] <- sum(data_5partido2$DESPESA_MAX_CAMPANHA)
    mediadespesa2[5] <- mean(data_5partido1$DESPESA_MAX_CAMPANHA)
    
    
    mediadespesa2 <- round(mediadespesa2,1)
    despesa2 <- round(despesap2,1)
    despesa1 <- round(despesap1,1)
    mediadespesa <- round(mediadespesa,1)
    
    tabela_comparativo1 <- cbind(Cargos, despesa1, despesa2,mediadespesa, mediadespesa2)
    return(tabela_comparativo1)
  })
  
  output$tabelacomp1 <- renderTable({
    tab <- as.data.frame(tabela_comparativo1())
    colnames(tab) <- c("Cargos","Despesa media em campanha -Partido 1", "Despesa media em campanha - Partido 2", "Despesa total em campanha - Partido 1", "Despesa total em campanha - Partido 2")
    tabelacomp1 <- tab
  })
  
  tabela_comparativo2 <- reactive({
    Cargos <- c("Presidente", "Governador", "Senador", "Deputado Estadual", "Deputado Federal")
    
    prop_eleitos1 <- vector()
    df1 <- candidates(year = 2014, position = 'President', columns_list = list("NUMERO_PARTIDO", "DESPESA_MAX_CAMPANHA", "NOME_URNA_CANDIDATO", "DESC_SIT_TOT_TURNO"))
    d1 <- df1[df1$DESPESA_MAX_CAMPANHA != -1,]
    data_partido1 <- d1[d1$NUMERO_PARTIDO == input$partido1,]
    eleitos1 <- df1[df1$DESC_SIT_TOT_TURNO == "ELEITO",]
    eleitos1 <- eleitos1[eleitos1$NUMERO_PARTIDO == input$partido1,]
    prop_eleitos1[1] <- length(eleitos1$DESC_SIT_TOT_TURNO)/ length(data_partido1$DESC_SIT_TOT_TURNO)
    
    df2 <- candidates(year = 2014, position = 'Governor', columns_list = list("NUMERO_PARTIDO", "DESPESA_MAX_CAMPANHA", "NOME_URNA_CANDIDATO", "DESC_SIT_TOT_TURNO"))
    d2 <- df2[df2$DESPESA_MAX_CAMPANHA != -1,]
    eleitos1 <- df2[df2$DESC_SIT_TOT_TURNO == "ELEITO",]
    eleitos1 <- eleitos1[eleitos1$NUMERO_PARTIDO == input$partido1,]
    data_2partido1 <- d2[d2$NUMERO_PARTIDO == input$partido1,]
    prop_eleitos1[2] <- length(eleitos1$DESC_SIT_TOT_TURNO)/ length(data_2partido1$DESC_SIT_TOT_TURNO)
    
    df3 <- candidates(year = 2014, position = 'Senador', columns_list = list("NUMERO_PARTIDO", "DESPESA_MAX_CAMPANHA", "NOME_URNA_CANDIDATO", "DESC_SIT_TOT_TURNO"))
    d3 <- df3[df3$DESPESA_MAX_CAMPANHA != -1,]
    data_3partido1 <- d3[d3$NUMERO_PARTIDO == input$partido1,]
    eleitos1 <- df3[df3$DESC_SIT_TOT_TURNO == "ELEITO",]
    eleitos1 <- eleitos1[eleitos1$NUMERO_PARTIDO == input$partido1,]
    prop_eleitos1[3] <- length(eleitos1$DESC_SIT_TOT_TURNO)/ length(data_3partido1$DESC_SIT_TOT_TURNO)
    
    df4 <- candidates(year = 2014, position = 'Deputado Estadual', columns_list = list("NUMERO_PARTIDO", "DESPESA_MAX_CAMPANHA", "NOME_URNA_CANDIDATO", "COD_SIT_TOT_TURNO"))
    d4 <- df4[df4$DESPESA_MAX_CAMPANHA != -1,]
    data_4partido1 <- d4[d4$NUMERO_PARTIDO == input$partido1,]
    eleitos1 <- df4[df4$COD_SIT_TOT_TURNO == 2 | df4$COD_SIT_TOT_TURNO == 3,]
    eleitos1 <- eleitos1[eleitos1$NUMERO_PARTIDO == input$partido1,]
    prop_eleitos1[4] <- length(eleitos1$COD_SIT_TOT_TURNO)/ length(data_4partido1$COD_SIT_TOT_TURNO)
    
    df5 <- candidates(year = 2014, position = 'Deputado Federal', columns_list = list("NUMERO_PARTIDO", "DESPESA_MAX_CAMPANHA", "NOME_URNA_CANDIDATO", "COD_SIT_TOT_TURNO"))
    d5 <- df5[df5$DESPESA_MAX_CAMPANHA != -1,]
    data_5partido1 <- d5[d5$NUMERO_PARTIDO == input$partido1,]
    eleitos1 <- df5[df5$COD_SIT_TOT_TURNO == 2| df5$COD_SIT_TOT_TURNO == 3,]
    eleitos1 <- eleitos1[eleitos1$NUMERO_PARTIDO == input$partido1,]
    prop_eleitos1[5] <- length(eleitos1$COD_SIT_TOT_TURNO)/ length(data_5partido1$COD_SIT_TOT_TURNO)
    
    prop_eleitos1 <- prop_eleitos1*100
    prop_eleitos1 <- round(prop_eleitos1,2)
    #partido 2
    
    prop_eleitos2 <- vector()
    
    data_partido2 <- d1[d1$NUMERO_PARTIDO == input$partido2,]
    eleitos2 <- df1[df1$DESC_SIT_TOT_TURNO == "ELEITO",]
    eleitos2 <- eleitos2[eleitos2$NUMERO_PARTIDO == input$partido2,]
    prop_eleitos2[1] <- length(eleitos2$DESC_SIT_TOT_TURNO)/ length(data_partido2$DESC_SIT_TOT_TURNO)
    
    
    data_2partido2 <- d2[d2$NUMERO_PARTIDO == input$partido2,]
    eleitos2 <- df2[df2$DESC_SIT_TOT_TURNO == "ELEITO",]
    eleitos2 <- eleitos2[eleitos2$NUMERO_PARTIDO == input$partido2,]
    prop_eleitos2[2] <- length(eleitos2$DESC_SIT_TOT_TURNO)/ length(data_2partido2$DESC_SIT_TOT_TURNO)
    
    data_3partido2 <- d3[d3$NUMERO_PARTIDO == input$partido2,]
    eleitos2 <- df3[df3$DESC_SIT_TOT_TURNO == "ELEITO",]
    eleitos2 <- eleitos2[eleitos2$NUMERO_PARTIDO == input$partido2,]
    prop_eleitos2[3] <- length(eleitos2$DESC_SIT_TOT_TURNO)/ length(data_3partido2$DESC_SIT_TOT_TURNO)
    
    data_4partido2 <- d4[d4$NUMERO_PARTIDO == input$partido2,]
    eleitos2 <- df4[df4$COD_SIT_TOT_TURNO == 2| df4$COD_SIT_TOT_TURNO == 3,]
    eleitos2 <- eleitos2[eleitos2$NUMERO_PARTIDO == input$partido2,]
    prop_eleitos2[4] <- length(eleitos2$COD_SIT_TOT_TURNO)/ length(data_4partido2$COD_SIT_TOT_TURNO)
    
    
    data_5partido2 <- d5[d5$NUMERO_PARTIDO == input$partido2,]
    eleitos2 <- df5[df5$COD_SIT_TOT_TURNO == 2| df5$COD_SIT_TOT_TURNO == 3,]
    eleitos2 <- eleitos2[eleitos2$NUMERO_PARTIDO == input$partido2,]
    prop_eleitos2[5] <- length(eleitos2$COD_SIT_TOT_TURNO)/ length(data_5partido2$COD_SIT_TOT_TURNO)
    
    
    prop_eleitos2 <- round(prop_eleitos2*100,2)
    
    tabela_comparativo2 <- cbind(Cargos, prop_eleitos1, prop_eleitos2)
    return(tabela_comparativo2)
    
  })
  
  output$tabelacomp2 <- renderTable({
    tab <- as.data.frame(tabela_comparativo2())
    colnames(tab) <- c("Cargos", "% de candidatos eleitos - Partido 1", "% de candidatos eleitos - Partido 2")
    tabelacomp2 <- tab
  })

  #__________________________________________________Plot comparando efeito de um partido em outro
  chartdata <- reactive({
    
    hpd1y1 <- hpd1y1[hpd1y1$UF != "ZZ",]
    hpd1y1 <- hpd1y1[hpd1y1$NUM_TURNO == 1,]
    a <- hpd1y1[hpd1y1$NUMERO_CANDIDATO == input$partido1,]
    a <- as.data.frame(a)
    qtd1 <- a$QTDE_VOTOS
    b <- hpd1y1[hpd1y1$NUMERO_CANDIDATO == input$partido2,]
    b <- as.data.frame(b)
    qtd2 <- b$QTDE_VOTOS
    #soma de votos por estado
    estados <- a$UF
    somaest <- vector()
    for ( i in 1: length(estados)){
      e1 <- as.data.frame(hpd1y1[hpd1y1$UF == estados[i],])
      somaest[i] <- sum(e1$QTDE_VOTOS)
    }
    
    chartdata <- as.data.frame(matrix(0,27,4))
    chartdata[,1] <- estados
    chartdata[,2] <- qtd1
    chartdata[,3] <- qtd2
    chartdata[,4] <- somaest
    
    q1 <- rep(0,27)
    q2 <- rep(0,27)
    
    for(i in 1: length(chartdata$qtd1)){
      q1[i] <- chartdata$qtd1[i]/chartdata$somaest[i]
      q2[i] <- chartdata$qtd2[i]/chartdata$somaest[i]
    }
    
    chartdata[,5] <- q1
    chartdata[,6] <- q2
    
    colnames(chartdata) <- c("estados", "qtd1", "qtd2", "somaest", "q1", "q2")
    return(chartdata)
  })
  
  output$ak <- renderPlot({

    ak <- ggplot(chartdata(), aes(chartdata()[,5], chartdata()[,6])) +
      geom_point( aes(colour = chartdata()[,5]), size = log(chartdata()[,4]/50)) +
      geom_text(size = 2.8, aes(label = paste(round(chartdata()[,5],2), '%'))) +
      scale_color_gradient(high = "red", low = "rosybrown1") +
      xlab(sprintf("Porcentagem de votos no partido %s",input$partido1)) +
      ylab(sprintf("Porcentagem de votos no partido %s",input$partido2)) +
      ggtitle("Relacao do % de votos entre os partidos") 
    
  })
  
  output$gg2 <- renderPlotly({
    
    gg2 <- plot_ly(chartdata(), x = chartdata()$estados, y = chartdata()[,5]*100, name = input$partido1, type = 'scatter', mode = 'markers', hoverinfo = "text",
                   text = paste("Percentual de votos:", round(chartdata()[,5]*100,2), "%", "<br>", "Estado:", chartdata()[,1]),  marker = list( symbol = 'circle', size = chartdata()[,5]*1000, sizemode = 'area', color = "#965f8a")) 
    
    
    gg2 <- add_trace(gg2, y = chartdata()[,6]*100, name = input$partido2, mode ='markers' , hoverinfo = "text",
                     text = paste("Percentual de votos:",round(chartdata()[,6]*100,2), "%", "<br>", "Estado:" ,chartdata()[,1]),
                     marker = list(symbol = 'circle', size =chartdata()[,6]*1000, sizemode = 'area', 
                                   color = "#4ac6b7"), type = 'scatter')
    gg2 <- layout(gg2, title = "% de votos nos partidos por estado  ", 
                  xaxis = list( title = 'Estado',
                                autorange = T, 
                                type = "linear"
                  ), 
                  yaxis = list( title = "Percentual de votos por partido"), width = 700, height = 450)
  })
}

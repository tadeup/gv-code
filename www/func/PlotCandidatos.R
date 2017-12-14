plotCandidatos <- function(){
  colors <- c('rgb(244, 223, 100)', 'rgb(72, 177, 219)')
  
  output$anoPlot <- renderUI({
    if(input$cargoPlot %in% cargos1) {
      selectInput("anoPlot", "Ano", anos1)
    } else{
      selectInput("anoPlot", "Ano", anos2)
    }
  })
  
  output$estadoPlot <- renderUI({
    if(input$cargoPlot == "Presidente"){
      selectInput("estadoPlot", "Estado", "BR")}
    else{
      selectInput("estadoPlot", "Estado", estadosdiv )
    }
  })
  
  output$nomemunicipio <- renderUI({
    if(input$estadoPlot == "BR"){
      selectInput("nomemunicipio", "Municipio", choices = "all")
    } else {
      
      #filtrando o data frame pelo estado selecionado
      columns <- list("UF", "NOME_MUNICIPIO")
      df <- cepespdata(year = input$anoPlot,state = input$estadoPlot, position = input$cargoPlot, columns_list = columns)
      df <- df[df$UF == input$estadoPlot,]
      choices <- df$NOME_MUNICIPIO
      selectInput("nomemunicipio", "Municipio", choices = choices)
    }
  })
  
  output$generoSpawn <- renderUI({
    if(input$cargoPlot != "Prefeito" & input$cargoPlot != "Vereador"){
      box(plotlyOutput("genero"), title = "Distribuicao dos candidatos por genero", status = "primary")
    }
  })
  
  output$piespawn <- renderUI({
    if(input$cargoPlot %in% c("Presidente", "Governador","Senador")){
      box(plotlyOutput("pie"), title = paste(" Percentual de votos por candidato a:", input$cargoPlot), status = "primary")
    } else if (input$cargoPlot %in% c("Deputado Federal","Deputado Estadual", "Vereador")){
      box(plotlyOutput("pie2"), title = paste("Percentual de votos por partido para candidatos a:", input$cargoPlot), status = "primary")
    } else {
      box(plotlyOutput("pie3"), title = paste("Percentual de votos por partido para candidatos a:", input$cargoPlot), status = "primary")
    }
  })
  
  output$tabela <- renderUI({
    if(input$cargoPlot %in% c("Presidente", "Governador","Senador")){
      box(tableOutput("tabela1"), title = "Resultados eleicao", status = "warning")
    } else if (input$cargoPlot %in% c("Deputado Federal","Deputado Estadual", "Vereador")){
      box(tableOutput("tabela2"), title = " Resultados eleicao", status = "warning")
    } else {
      box(tableOutput("tabela3"), title = "Resultados eleicao", status = "warning")
    }
  })
  
  
  #para a segunda ABA
  
  
  #Baixando dados de candidato e limpando por estado
  
  dfcepesp <- reactive({

    # Para inputs de prefeito e vereador o grafico da distribuicao de genero nao sera realizado, pois os dados nao estao
    #disponiveis pela API, a solucao e mostrar composicao de homens e mulheres no estado
    
    columns <- list("SIGLA_UF","NUMERO_CANDIDATO", "NOME_URNA_CANDIDATO", "NUMERO_PARTIDO","SIGLA_PARTIDO","COMPOSICAO_LEGENDA", "DESCRICAO_SEXO", "DESPESA_MAX_CAMPANHA")
    dfcepesp <- as.data.frame(candidates(year = input$anoPlot, position = input$cargoPlot, columns_list = columns))
    dfcepesp <- dfcepesp[dfcepesp[8] != -1,]
    dfcepesp <- dfcepesp[dfcepesp[1] == input$estadoPlot,]
    
    return(dfcepesp)
  })

  #Plot de genero
  output$genero <- renderPlotly({
    dadosgenero <- prop.table(table(dfcepesp()[7]))
    pd <- plot_ly( labels = names(dadosgenero), values = as.vector(dadosgenero), type = "pie",
                   textposition = "inside", 
                   textinfo = 'label+percent',
                   insidetextfont = list(color = 'rgb(1, 14, 20)'),
                   hoverinfo = 'text',
                   text = paste(  names(dadosgenero), ":", as.vector(dadosgenero)),
                   marker = list(colors = colors))
    
  })
  
  #_________________________________________________________________
  #Plot para proporcao de candidato e partido. 
  #Pegando os dados
  
  #PIE chart para presidente governador e senador
  dfpie <- reactive({
    columns <- list( "NUMERO_CANDIDATO", "UF", "QTDE_VOTOS", "NOME_URNA_CANDIDATO", "SIGLA_PARTIDO", "NOME_MUNICIPIO")
    dfpie <- cepespdata(year = input$anoPlot, position = input$cargoPlot, regional_aggregation = "Municipality", columns_list = columns)
    
    if(input$estadoPlot == "BR"){
      dfpie <- as.data.frame(dfpie)
    } else { 
      dfpie <- as.data.frame(dfpie)
      dfpie <- dfpie[dfpie$UF  == input$estadoPlot,]}
    return(dfpie)
  })
  
  #calculo da proporcao 
  prop <- reactive({
    pp <- vector()
    v <- vector()
    nomes <- vector()
    tabela <- as.data.frame(table(dfpie()$NUMERO_CANDIDATO))
    
    for(i in 1: length(tabela$Var1)){
      a <- dfpie()[dfpie()$NUMERO_CANDIDATO == tabela$Var1[i],]
      pp[i] <- sum(a$QTDE_VOTOS)/ sum(dfpie()$QTDE_VOTOS)
      v <- as.numeric(which(dfpie()$NUMERO_CANDIDATO == tabela$Var1[i])[1])
      if(is.na(v) == F){
        nomes[i] <- dfpie()$NOME_URNA_CANDIDATO[v]}
    }
    pp <- round(pp,2)
    prop <- as.data.frame(cbind(nomes, pp))
    return(prop)
  })
  
  #coligacao
  dadoscolig <- reactive({
    
    a <- vector()
    colig <- vector()
    partido <- vector()
    nomes <- prop()$nomes  
    for(i in 1: length(dfcepesp()$COMPOSICAO_LEGENDA)){
      a <- as.numeric(which(nomes == dfcepesp()$NOME_URNA_CANDIDATO[i])[1])
      if(is.na(a) == F){
        colig[a] <- dfcepesp()$COMPOSICAO_LEGENDA[i]
        partido[a] <- dfcepesp()$SIGLA_PARTIDO[i]
      }}
    
    dadoscolig <- as.data.frame(cbind(colig, partido))
    return(dadoscolig)
  }) 
  
  #plotando o grafico
  output$pie <- renderPlotly({
    
    if (length(prop()$nomes) == length(dadoscolig()$partido)){
      pie <- plot_ly(labels = prop()$nomes, values = prop()$pp, type = "pie",
                     textposition = "inside",
                     textinfo = "label+percent",
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste("<b> Coligacao: </b>", dadoscolig()$colig, "<br>", "<b> Partido do candidato: </b>", dadoscolig()$partido, sep = " "),
                     marker = list(colors = colors,
                                   line= list(color = '#FFFFFF', width=1 )),
                     showlegend = T)
    }
    else{ 
      pie <- plot_ly(labels = prop()$nomes, values = prop()$pp, type = "pie",
                     textposition = "inside",
                     textinfo = "label+percent",
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     marker = list(colors = colors,
                                   line= list(color = '#FFFFFF', width=1 )),
                     showlegend = T)}
  })    
  
  #tabela1
  
  output$tabela1 <- renderTable({
    dados <- cbind(prop(), dadoscolig())
    colnames(dados) <- c("nome candidato", "Proporcao de votos", "Coligacao", "Partido")
    tabela1 <- as.data.frame(dados)
  })
  #______________________________________________
  #para deputados federais estaduais e vereador 
  
  prop2 <- reactive({
    pp <- vector()
    v <- vector()
    nomes <- vector()
    tabela <- as.data.frame(table(dfpie()$SIGLA_PARTIDO))
    
    for(i in 1: length(tabela$Var1)){
      a <- dfpie()[dfpie()$SIGLA_PARTIDO == tabela$Var1[i],]
      pp[i] <- sum(a$QTDE_VOTOS)/ sum(dfpie()$QTDE_VOTOS)
      v <- as.numeric(which(dfpie()$SIGLA_PARTIDO == tabela$Var1[i])[1])
      if(is.na(v) == F){
        nomes[i] <- dfpie()$SIGLA_PARTIDO[v]
      }
    }
    pp <- round(pp,2)
    prop2 <- as.data.frame(cbind(pp, nomes))
    return(prop2)
  })
  
  #plot
  output$pie2 <- renderPlotly({  
    pie2 <- plot_ly(labels = prop2()$nomes, values = prop2()$pp, type = "pie",
                    textposition = "inside",
                    textinfo = "label+percent",
                    insidetextfont = list(color = '#FFFFFF'),
                    hoverinfo = 'text',
                    marker = list(colors = colors,
                                  line= list(color = '#FFFFFF', width=1 )),
                    showlegend = T) 
  })
  
  #tabela2
  output$tabela2 <- renderTable({
    dados <- prop2()
    colnames(dados) <- c("Partido", "Proporcao de votos por partido")
    tabela2 <- dados
  })
  
  #__________________________________________________________
  #Para prefeito 
  
  dfmun <- reactive({
    
    if(input$nomemunicipio != "all"){
      dfmun <- dfpie()[dfpie()$UF  == input$estadoPlot,]
      dfmun <- dfpie()[dfpie()$NOME_MUNICIPIO == input$nomemunicipio,]}
    else{
      dfmun <- dfpie()}
    return(dfmun)
  })
  
  #Proporcao 
  
  # para prefeito utilizando cepespdata nao e possivel retornar o nome do candidato ou do partido, retorna #NE portanto
  #a proporcao de votos ficou pelo numero do candidato
  
  prop3 <- reactive({
    
    pp <- vector()
    v <- vector()
    tabela <- as.data.frame(table(dfmun()$NUMERO_CANDIDATO))
    for(i in 1: length(tabela$Var1)){
      a <- dfmun()[dfmun()$NUMERO_CANDIDATO == tabela$Var1[i],]
      pp[i] <- sum(a$QTDE_VOTOS)/ sum(dfmun()$QTDE_VOTOS)  
    }
    prop3 <- round(pp,2)
    return(prop3)
  }) 
  
  #plotando o grafico
  output$pie3 <- renderPlotly({
    
    pie3 <- plot_ly(labels = names(table(dfmun()$NUMERO_CANDIDATO)), values = prop3(), type = "pie",
                    textposition = "inside",
                    textinfo = "label+percent",
                    insidetextfont = list(color = '#FFFFFF'),
                    hoverinfo = 'text',
                    marker = list(colors = colors,
                                  line= list(color = '#FFFFFF', width=1 )),
                    showlegend = T)
  })
  
  #tabela3
  output$tabela3 <- renderTable({
    nomes <- names(table(dfmun()$NUMERO_CANDIDATO))
    dados <- cbind(nomes, prop3())
    colnames(dados) <- c("Numero do candidato", "Proporcao de votos")
    tabela3 <- dados
  })
  
}
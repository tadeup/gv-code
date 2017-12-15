library(plotly)
library(shiny)
library(shinydashboard)
library(leaflet)
#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("Cepesp-Fgv/cepesp-r")
library(cepespR)
library(rgdal)
library(dplyr)
library(randomcoloR)
options(scipen = 999)

# setwd("C:\\Users\\victor-pc\\Desktop\\New folder\\gv-code")

#pre-codigo
estados <- data.frame(stringsAsFactors = F,
                      UF = c("BRASIL","AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"),
                      codigo = c(00,12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17))
anos <- c(2014,2012,2010,2008,2006,2004,2002,2000)
cargos1 <- c("Prefeito","Vereador")
cargos2 <- c("Presidente","Governador","Senador","Deputado Federal","Deputado Estadual","Deputado Distrital")
agregacoes <- c("Candidato","Partido")
regAgreg1 <- c("Estado", "Municipio")
regAgreg2 <- c("Municipio")
pibs <- list()
for(i in anos){pibs <- append(pibs,list(read.csv(sprintf("www/data/IBGE/municipios/popib%s.csv",i), stringsAsFactors = F, encoding = "UTF-8")))}
pibse <- list()
for(i in anos){pibse <- append(pibse,list(read.csv(sprintf("www/data/IBGE/estados/%s.csv",i), stringsAsFactors = F, encoding = "UTF-8")))}
colunas1 <- list("NUM_TURNO","NUMERO_CANDIDATO","QTDE_VOTOS","COD_MUN_IBGE", "UF","NUMERO_PARTIDO")
colunas2 <- list("NUM_TURNO","NUMERO_CANDIDATO","QTDE_VOTOS","NOME_UF", "UF","NUMERO_PARTIDO")
metricas <- c("total de votos", "votos/populacao", "votos/pib", "total de votos em log")

hpd1y1 <- votes(year = 2014, position = 'President', regional_aggregation = "Estado")
hpd2y1 <- candidates(year = 2014, position = 'President')
hpd3y1 <- coalitions(year = 2014, position = 'President')

hpd1y2 <- votes(year = 2010, position = "President", regional_aggregation = "Estado")
hpd2y2 <- candidates(year = 2010, position = "President")
hpd3y2 <- coalitions(year = 2010, position = "President")

hpd1y3 <- votes(year=2006, position = "President", regional_aggregation = "Estado")
hpd2y3 <- candidates(year = 2006, position = "President")
hpd3y3 <- coalitions(year = 2006, position = "President")

hpd1y4 <- votes(year=2002, position = "President", regional_aggregation = "Estado")
hpd2y4 <- candidates(year = 2002, position = "President")
hpd3y4 <- coalitions(year = 2002, position = "President")

hpd1y5 <- votes(year=1998, position = "President", regional_aggregation = "Estado")
hpd2y5 <- candidates(year = 1998, position = "President")
hpd3y5 <- coalitions(year = 1998, position = "President")



#pt2
anos1 <- c(2000, 2004, 2008, 2012, 2016)
anos2 <- c(2002, 2006, 2010, 2014)
estadosdiv <- c("AC","AL","AM","AP","BA","CE","DF","ES",
                "GO","MA","MG","MS","MT","PA","PB","PE",
                "PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")
estadosdisp <- c("all","AC","AL","AM","AP","BA","CE","DF","ES",
                 "GO","MA","MG","MS","MT","PA","PB","PE",
                 "PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")
columns <- list("QTDE_VOTOS", "NUMERO_PARTIDO", "UF")

#yk
#dados para estados
igini<-read.csv("www\\data\\indices\\ipea.csv", 
                header=TRUE,sep=";",colClasses = c("numeric","character","NULL","NULL",
                                                   "NULL","NULL","NULL","NULL","numeric","NULL","NULL",
                                                   "NULL","NULL"),dec=",")

idesemprego<-read.csv("www\\data\\indices\\ipea.csv", 
                      header=TRUE,sep=";",colClasses = c("numeric","character","NULL","NULL",
                                                         "NULL","NULL","NULL","numeric","NULL","NULL","NULL",
                                                         "NULL","NULL"),dec=",")

ipobreza<-read.csv("www\\data\\indices\\ipea.csv", 
                   header=TRUE,sep=";",colClasses = c("numeric","character","NULL","NULL",
                                                      "NULL","NULL","NULL","NULL","NULL","numeric","character",
                                                      "NULL","NULL"),dec=",")
#--------------------------
#dados nacionais
iginibr_1<-read.csv("www\\data\\indices\\gini_world_bank.csv", 
                    header=TRUE,sep=";",colClasses = c("numeric","numeric","NULL","NULL","NULL"),dec=",")

iginibr_2<-read.csv("www\\data\\indices\\gini_world_bank.csv", 
                    header=TRUE,sep=";",colClasses = c("numeric","NULL","numeric","NULL","NULL"),dec=",")

iginibr_3<-read.csv("www\\data\\indices\\gini_world_bank.csv", 
                    header=TRUE,sep=";",colClasses = c("numeric","NULL","NULL","numeric","NULL"),dec=",")

iginibr_4<-read.csv("www\\data\\indices\\gini_world_bank.csv", 
                    header=TRUE,sep=";",colClasses = c("numeric","NULL","NULL","NULL","numeric"),dec=",")

#--------------------------

peabr<-read.csv("www\\data\\indices\\pea_brasil.csv", 
                header=TRUE,sep=";",colClasses = c("numeric","numeric"),dec=",")

ppbr_1<-read.csv("www\\data\\indices\\pib.csv", 
                 header=TRUE,sep=";",colClasses = c("numeric","numeric","NULL","NULL","NULL"),dec=",")

ppbr_2<-read.csv("www\\data\\indices\\pib.csv", 
                 header=TRUE,sep=";",colClasses = c("numeric","NULL","numeric","NULL","NULL"),dec=",")

ppbr_3<-read.csv("www\\data\\indices\\pib.csv", 
                 header=TRUE,sep=";",colClasses = c("numeric","NULL","NULL","numeric","NULL"),dec=",")

ppbr_4<-read.csv("www\\data\\indices\\pib.csv", 
                 header=TRUE,sep=";",colClasses = c("numeric","numeric","numeric","numeric","numeric"),dec=",")

#dadospresidas
presidas1998<-read.csv("www\\data\\indices\\presidente1998.csv",
                       header=TRUE,sep=";",colClasses=c("character","numeric","character","numeric","character"))

eleito1998_contem_eleito<-presidas1998[,4]
pos1998<-which(eleito1998_contem_eleito==1)
eleito1998_col<-presidas1998[,2]
num_partido_1998<-eleito1998_col[pos1998]


presidas2002<-read.csv("www\\data\\indices\\presidente2002.csv",
                       header=TRUE,sep=";",colClasses=c("character","numeric","character","numeric","character"))


eleito2002_contem_eleito<-presidas2002[,4]
pos2002<-which(eleito2002_contem_eleito==1)
eleito2002_col<-presidas2002[,2]
num_partido_2002<-eleito2002_col[pos2002]

presidas2006<-read.csv("www\\data\\indices\\presidente2006.csv",
                       header=TRUE,sep=";",colClasses=c("character","numeric","character","numeric","character"))

eleito2006_contem_eleito<-presidas2006[,4]
pos2006<-which(eleito2006_contem_eleito==1)
eleito2006_col<-presidas2006[,2]
num_partido_2006<-eleito2006_col[pos2006]

presidas2010<-read.csv("www\\data\\indices\\presidente2010.csv",
                       header=TRUE,sep=";",colClasses=c("character","numeric","character","numeric","character"))


eleito2010_contem_eleito<-presidas2010[,4]
pos2010<-which(eleito2010_contem_eleito==1)
eleito2010_col<-presidas2010[,2]
num_partido_2010<-eleito2010_col[pos2010]

presidas2014<-read.csv("www\\data\\indices\\presidente2014.csv",
                       header=TRUE,sep=";",colClasses=c("character","numeric","character","numeric","character"))

eleito2014_contem_eleito<-presidas2014[,4]
pos2014<-which(eleito2014_contem_eleito==1)
eleito2014_col<-presidas2014[,2]
num_partido_2014<-eleito2014_col[pos2014]

color1=randomColor()
color2=randomColor()

#----------------------------
#vars
anos<-c(1998, 2002, 2004, 2006, 2008, 2012, 2014)
anos_pea<-c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)
anos_ginibr<-c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)
anos_pp<-c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)

estadosdiv <- c("AC","AL","AM","AP","BA","CE","DF","ES",
                "GO","MA","MG","MS","MT","PA","PB","PE",
                "PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")

indicesbr<-c("Gini Brasil","Pib per capita","PEA")
indicesestado<-c("Gini Estados","Desemprego","Pobreza")
#----------------------------------------------------------

color1=randomColor()
color2=randomColor()
#yk

#definindo a interface do usuario
ui <- dashboardPage(skin = "black",
  #header da pagina 
  dashboardHeader(title = "GV CODE"),
  
  #sidebar da pagina
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
                menuItem("Intro", tabName = "intro", icon = icon("info")),
                menuItem("Map", tabName = "map", icon = icon("map")),
                conditionalPanel(
                  #class="menu-condicional",
                  condition = "input.sidebarmenu === 'map'",
                  selectInput(inputId = "tipoConsulta", label = "Tipo da consulta",choices = c("individual","geral")),
                  selectInput(inputId = "polAgr", label = "Agregacao politica", choices = agregacoes),
                  uiOutput("partyORcandidate"),
                  selectInput(inputId = "ano", label = "Ano da eleicao", choices = anos),
                  uiOutput(outputId = "cargo"),
                  selectInput(inputId = "estado", label = "Estado", choices = estados[,1]),
                  uiOutput(outputId = "regAgr"),
                  radioButtons(inputId = "turno", label = "Turno",choices = c(1, 2)),
                  selectInput(inputId = "metrica", label = "metrica a utilizar",choices = metricas),
                  actionButton("goMap", "Gerar mapa")),
                
                menuItem("Candidatos", tabName = "candidatos", icon = icon("bar-chart"),
                         menuItem("Dados por candidato", tabName = "DadosPorCandidato"),
                         conditionalPanel(
                           class ="menu-conditional",
                           condition = "input.sidebarmenu === 'DadosPorCandidato'",
                           selectInput( inputId = "cargoPlot", label = "Cargo:", choices = c("Presidente", "Governador", "Senador", "Deputado Estadual", "Deputado Federal", "Prefeito", "Vereador")),
                           uiOutput(outputId = "anoPlot"),
                           uiOutput(outputId = "estadoPlot"),
                           uiOutput(outputId = "nomemunicipio"),
                           actionButton("go", "Gerar graficos")),
                         menuItem("Comparativo", tabName = "CompararPartidos"),
                         conditionalPanel(
                           class = "menu-conditional",
                           condition = "input.sidebarmenu === 'CompararPartidos'",
                           textInput( inputId = "partido1", label = "Digite o numero do primeiro partido", value = 13),
                           textInput(inputId = "partido2", label = "Digite o numero do segundo partido", value = 45)
                           
                         )
                ),
                conditionalPanel(
                  selectInput(inputId = "pol", label = "Selecione o cargo", choices = c("Presidente", "Governador")),
                  selectInput(inputId = "anoplot", label = "Selecione o Ano", choices = c(2014,2010,2006,2002,1998)),
                  selectInput(inputId = "local", label = "Estado", choices = estados[,1]),
                  uiOutput(outputId =  "AgregGeo"),
                  selectInput(inputId = "tipovoto", label = "Total ou porcentagem", choices = c("Total", "Porcentagem"))
                ),
                menuItem("Indices", tabName = "indices", icon = icon("database")),
                menuItem("Graficos Chart", tabName = "chartmap", icon = icon("area-chart")),
                conditionalPanel(
                  selectInput(inputId = "pol", label = "Selecione o cargo", choices = c("Presidente", "Governador")),
                  selectInput(inputId = "anoplot", label = "Selecione o Ano", choices = c(2014,2010,2006,2002,1998)),
                  selectInput(inputId = "local", label = "Estado", choices = estados[,1]),
                  uiOutput(outputId =  "AgregGeo"),
                  selectInput(inputId = "tipovoto", label = "Total ou porcentagem", choices = c("Total", "Porcentagem"))
                ),
                menuItem("Indices", tabName = "indices", icon = icon("line-chart")),
                conditionalPanel(
                  class ="menu-conditional",
                  condition = "input.sidebarmenu == 'indices'",
                  selectInput( inputId = "cargo", label = "Cargo:", choices = c("Presidente", "Governador")),
                  uiOutput(outputId = "indices"),
                  uiOutput(outputId = "estado")),
                menuItem("Source code", icon = icon("file-code-o"), href = "http://cepesp.io")
    )
  ),
  
  #corpo da pagina
  dashboardBody(
    #link ao arquivo css
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "style.css"
      )
    ),
    
    #elementos do corpo da pagina
    tabItems(
      tabItem(
        tabName = "intro",
          div(id = "intro",
            h1("Introdução:"),
            h4("Essa plataforma foi criada com o objetivo de facilitar o acesso às informações disponíveis e dados públicos sobre as eleições no Brasil. ")
            ),
          div(id= "Tadeu",class = "row",
            div(id = "mapa_tadeu",class = "col-sm-6",
                h3("mapa"),
                tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "70%")
            ),
            div(id = "tadeu", class = "col-sm-6",
                
                h1("descricao "),
                p("Na aba "Mapas" é possível a visualização de dois mapas. No primeiro, pode-se selecionar o cargo desejado, o ano da eleição, a região de agregação (federação, estado ou município) e o tipo de agregação política (partido ou candidato). A partir de cada opção selecionada, um mapa dinâmico é gerado e nele observa-se em diferentes tonalidades de azul a quantidade de votos em cada região, sendo o tom mais escuro representativo de maior quantidade de votos em absoluto e o mais claro representativo de menor quantidade. Além disso, há a opção de definir a métrica desejada (como total de votos, votos em relação à população, votos em relação ao PIB e total de votos na base logarítmica), o que permite verificar a influência da região em análise para a eleição.
O segundo mapa permite ter uma interação melhor com os dados em nível municipal. Dado que o mapa não tem muita carga visual de informação, pois o usuário clica nas agregações municipais aproximando o mapa até chegar na cidade desejada. Ressalta-se que agregações menores apenas aparecem quando se clica em uma agregação maior. Caso o usuário deseje transitar entre os dois mapas, há um menu interativo no canto superior da tela podendo selecionar a visualização desejada e também sobrepor os dois tipos de funcionalidades.
	Há também um terceiro mapa na aba "Chart Map" que permite a visualização da distribuição de votos para todos os partidos no mapa do Brasil através de um pie chart ("gráfico de pizza") que aparece para as agregações geográficas selecionadas. Em suma, aparecerá um gráfico mostrando a porcentagem de votos de cada partido em todas as unidades federativas, dessa forma, consegue-se visualizar quais partidos são mais fortes em cada localidade de maneira rápida e fácil.")
                )
                ),
        
          div(id = "Ana", class = "row",
                h1("candidatos"),
                div(id= "ana-tb-1", class= "col-sm-6",
                    h3("descricao"),
                    tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "300px"),
                    h5("Na aba "Candidatos" pode-se selecionar duas seções. Na seção "Dados por Candidato", se pode visualizar dois gráficos e uma tabela. O primeiro gráfico mostra a distribuição de gêneros dos candidatosdado o cargo selecionado pelo usuário, o ano da eleição, o Estado ou a Federação e o nome do município. O segundo gráfico engloba o percentual de votos do candidato ou partido. É possível fazer download dos gráficos passando o mouse no canto superior da tela. A tabela mostra de acordo com as variáveis selecionadas para presidente, governador e senador o nome do candidato, o partido dele, a proporção de votos e a coligação. Para os demais cargos, a tabela apresenta o partido, a coligação e o percentual de votos por partido. Na seção "Comparar Partidos", o usuário digita o número de dois partidos desejados e pode observar em um primeiro gráfico a evolução histórica do percentual de votos para os partidos selecionados. Abaixo do gráfico tem-se duas tabelas: a primeira mostra um comparativo entre os dois partidos em relação ao gasto total e médio em campanha de acordo com os cargos: Presidente, Governador, Senador, Deputado Federal e Deputado Estadual; a segunda  tabela mostra o percentual de candidatos eleitos por partido no que tange aos cargos mencionados. Para uma comparação mais abrangente, foram selecionados mais dois gráficos com as seguintes informações: o primeiro permite ter uma ideia de correlação entre o percentual de votos de dois partidos, ou seja, caso se observe uma tendência descendente no gráfico, infere-se que há uma relação antagônica entre os partidos, pois em estados nos quais há predominância de um partido, observa-se que há menos influência no outro e vice-versa. Caso não haja tendência aparente, não se observa relação direta de antagonismo ou não entre os partidos Já o segundo gráfico permite a visualização do percentual de votos para cada partido selecionado de acordo com o estado.")
                    ),
              div(id= "ana-tb-1", class= "col-sm-6",
                  h3("indices"),
                  tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "300px"),
                  h5("Na aba "Índices" foram construídos diversos gráficos no âmbito Federal e Estadual que mostram índices econômicos brasileiros e os partidos que estavam no poder durante os períodos de 1999 a 2014. No nível Federal, são mostrados os índices: Gini, PIB per Capita e PEA, já no Estadual, os índices disponíveis são: Gini, Desemprego e Pobreza para cada uma das unidades federativas escolhidas. Os gráficos estão em formatos de linha ou pontos. Cada mandato está representado em uma cor diferente mostrando também qual era o partido no período.")
                  )
                  
              )
        
  
        

        
            ),
          
      tabItem(
        tabName = "map",
        textOutput("erroMapa1"),
        leafletOutput("mapa", height = "95vh")
      ),
      tabItem(
        tabName = "DadosPorCandidato",
        fluidRow(
          uiOutput("generoSpawn", height = "95vh"),
          uiOutput("piespawn"),
          uiOutput("tabela")
        )
      ),
      
      tabItem(tabName = "CompararPartidos", #TADEU NAO SEI SE VOCE COLOCOU AQUELE MEU GRAFICO QUE TAVA DANDO PROBLEMA COLOCA ESSA PARTES SE NAO
              h1("", style = "95vh"),
              fluidRow(
                box(plotlyOutput("aa"), height = 450, width = 700),
                box(tableOutput("tabelacomp1"), title = " Comparativo - Despesa Maxima em campanha", height = 360, width = 300, align = "left", background = "light-blue"),
                box(tableOutput("tabelacomp2"), title = "Percentual de candidatos eleitos por partido selecionado",height = 300, width = 300, background = "light-blue"),
                box(plotOutput("ak"), height = 400, width = 650),
                box(plotlyOutput("gg2"), height = 450, width = 700, title = "Percentual votos nos partidos por Estado")
                )
              ),
      tabItem(
        tabName = "chartmap",
        leafletOutput("chartmapa", height = "95vh")
      ),
      tabItem(tabName = "indices",
              h1("", style = "95vh"),
              fluidRow(
                plotlyOutput("chart")
              )),
      tabItem(
        tabName = "misc"
      )
    )
  )
)



#back end do app
server <- function(input, output) {
  source("www/func/MapaIndividual.R",local = T)
  mapaIndividual()
  source("www/func/PlotCandidatos.R",local = T)
  plotCandidatos()
  source("www/func/PlotCandidatos2.R",local = T)
  plotCandidatos2()
}


shinyApp(ui = ui, server = server)
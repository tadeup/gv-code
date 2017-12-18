# Pacotes Necessarios para o programa


# install.packages("plotly")
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("leaflet")
# #if (!require("devtools")) install.packages("devtools")
# #devtools::install_github("Cepesp-Fgv/cepesp-r")
# install.packages("rgdal")
# install.packages("dplyr")
# install.packages("randomcoloR")


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

# setwd("C:\\Users\\victor-pc\\Desktop\\Cepesp\\gv-code")

#pre-codigo
estados <- data.frame(stringsAsFactors = F,
                      UF = c("BRASIL","AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"),
                      codigo = c(00,12,27,13,16,29,23,53,32,52,21,31,50,51,15,25,26,22,41,33,24,11,14,43,42,28,35,17))
anost <- c(2014,2012,2010,2008,2006,2004,2002,2000)
cargos1 <- c("Prefeito","Vereador")
cargos2 <- c("Presidente","Governador","Senador","Deputado Federal","Deputado Estadual","Deputado Distrital")
agregacoes <- c("Candidato","Partido")
regAgreg1 <- c("Estado", "Municipio")
regAgreg2 <- c("Municipio")
pibs <- list()
for(i in anost){pibs <- append(pibs,list(read.csv(sprintf("www/data/IBGE/municipios/popib%s.csv",i), stringsAsFactors = F, encoding = "UTF-8")))}
pibse <- list()
for(i in anost){pibse <- append(pibse,list(read.csv(sprintf("www/data/IBGE/estados/%s.csv",i), stringsAsFactors = F, encoding = "UTF-8")))}
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

hpd1y1 <- read.csv2("www\\datacsv-API\\hpd1y1.csv", header = T, sep = ",")[,2:13]
hpd1y2 <- read.csv2("www\\datacsv-API\\hpd1y2.csv", header = T, sep = ",")[,2:13]
hpd1y3 <- read.csv2("www\\datacsv-API\\hpd1y3.csv", header = T, sep = ",")[,2:13]
hpd1y4 <- read.csv2("www\\datacsv-API\\hpd1y4.csv", header = T, sep = ",")[,2:13]
hpd1y5 <- read.csv2("www\\datacsv-API\\hpd1y5.csv", header = T, sep = ",")[,2:13]
hpd2y1 <- read.csv2("www\\datacsv-API\\hpd2y1.csv", header = T, sep = ",")[,2:13]
hpd2y2 <- read.csv2("www\\datacsv-API\\hpd2y2.csv", header = T, sep = ",")[,2:13]
hpd2y3 <- read.csv2("www\\datacsv-API\\hpd2y3.csv", header = T, sep = ",")[,2:13]
hpd2y4 <- read.csv2("www\\datacsv-API\\hpd2y4.csv", header = T, sep = ",")[,2:13]
hpd2y5 <- read.csv2("www\\datacsv-API\\hpd2y5.csv", header = T, sep = ",")[,2:13]
hpd3y1 <- read.csv2("www\\datacsv-API\\hpd3y1.csv", header = T, sep = ",")[,2:13]
hpd3y2 <- read.csv2("www\\datacsv-API\\hpd3y2.csv", header = T, sep = ",")[,2:13]
hpd3y3 <- read.csv2("www\\datacsv-API\\hpd3y3.csv", header = T, sep = ",")[,2:13]
hpd3y4 <- read.csv2("www\\datacsv-API\\hpd3y4.csv", header = T, sep = ",")[,2:13]
hpd3y5 <- read.csv2("www\\datacsv-API\\hpd3y5.csv", header = T, sep = ",")[,2:13]
df1 <- read.csv2("www\\datacsv-API\\df1.csv", header = T, sep = ",")[,2:5]
df2 <- read.csv2("www\\datacsv-API\\df2.csv", header = T, sep = ",")[,2:5]
df3 <- read.csv2("www\\datacsv-API\\df3.csv", header = T, sep = ",")[,2:5]
df4 <- read.csv2("www\\datacsv-API\\df4.csv", header = T, sep = ",")[,2:5]
df5 <- read.csv2("www\\datacsv-API\\df5.csv", header = T, sep = ",")[,2:5]
dados_k1 <- read.csv2(file = "www\\data\\indices\\dados_k1.csv", header = T, sep = ",")[,2:6]  
dados_k2 <- read.csv2(file = "www\\data\\indices\\dados_k2.csv", header = T, sep = ",")[,2:6]  
dados_k3 <- read.csv2(file = "www\\data\\indices\\dados_k3.csv", header = T, sep = ",")[,2:6]  
dados_k4 <- read.csv2(file = "www\\data\\indices\\dados_k4.csv", header = T, sep = ",")[,2:6]  
dados_k5 <- read.csv2(file = "www\\data\\indices\\dados_k5.csv", header = T, sep = ",")[,2:6]  

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
num_partido_1998<-presidas1998[,2][pos1998]
partido_name1998<-presidas1998[,3][pos1998]


presidas2002<-read.csv("www\\data\\indices\\presidente2002.csv",
                       header=TRUE,sep=";",colClasses=c("character","numeric","character","numeric","character"))


eleito2002_contem_eleito<-presidas2002[,4]
pos2002<-which(eleito2002_contem_eleito==1)
num_partido_2002<-presidas2002[,2][pos2002]
partido_name2002<-presidas2002[,3][pos2002]


presidas2006<-read.csv("www\\data\\indices\\presidente2006.csv",
                       header=TRUE,sep=";",colClasses=c("character","numeric","character","numeric","character"))

eleito2006_contem_eleito<-presidas2006[,4]
pos2006<-which(eleito2006_contem_eleito==1)
num_partido_2006<-presidas2006[,2][pos2006]
partido_name2006<-presidas2006[,3][pos2006]

presidas2010<-read.csv("www\\data\\indices\\presidente2010.csv",
                       header=TRUE,sep=";",colClasses=c("character","numeric","character","numeric","character"))


eleito2010_contem_eleito<-presidas2010[,4]
pos2010<-which(eleito2010_contem_eleito==1)
num_partido_2010<-presidas2010[,2][pos2010]
partido_name2010<-presidas2010[,3][pos2010]

presidas2014<-read.csv("www\\data\\indices\\presidente2014.csv",
                       header=TRUE,sep=";",colClasses=c("character","numeric","character","numeric","character"))

eleito2014_contem_eleito<-presidas2014[,4]
pos2014<-which(eleito2014_contem_eleito==1)
num_partido_2014<-presidas2014[,2][pos2014]
partido_name2014<-presidas2014[,3][pos2014]

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
                
                
                #--------------------------------------FRONT VICTOR
                
                menuItem("Intro", tabName = "intro", icon = icon("info")),
                
                
                
                #------------------------------------Mapa Tadeu
                
                
                menuItem("Map", tabName = "map", icon = icon("map")),
                conditionalPanel(
                  #class="menu-condicional",
                  condition = "input.sidebarmenu === 'map'",
                  #selectInput(inputId = "tipoConsulta", label = "Tipo da consulta",choices = c("individual","geral")),
                  selectInput(inputId = "polAgr", label = "Agregacao politica", choices = agregacoes),
                  uiOutput("partyORcandidate"),
                  selectInput(inputId = "ano", label = "Ano da eleicao", choices = anost),
                  uiOutput(outputId = "cargo"),
                  selectInput(inputId = "estado", label = "Estado", choices = estados[,1]),
                  uiOutput(outputId = "regAgr"),
                  radioButtons(inputId = "turno", label = "Turno",choices = c(1, 2)),
                  selectInput(inputId = "metrica", label = "metrica a utilizar",choices = metricas),
                  actionButton("goMap", "Gerar mapa")),
                
                #------------------------------------ Ana
                
                 menuItem("Candidatos", tabName = "candidatos", icon = icon("bar-chart"),
                         menuItem("Dados por candidato", tabName = "DadosPorCandidato"),
                         conditionalPanel(
                           class ="menu-conditional",
                           condition = "input.sidebarmenu === 'DadosPorCandidato'",
                           selectInput( inputId = "cargoPlot", label = "Cargo:", choices = c("Presidente", "Governador", "Senador", "Deputado Estadual", "Deputado Federal", "Prefeito", "Vereador")),
                           uiOutput(outputId = "anoPlot"),
                           uiOutput(outputId = "estadoPlot"),
                           uiOutput(outputId = "nomemunicipio")),
                         menuItem("Comparativo", tabName = "CompararPartidos"),
                         conditionalPanel(
                           class = "menu-conditional",
                           condition = "input.sidebarmenu === 'CompararPartidos'",
                           textInput( inputId = "partido1", label = "Digite o numero do primeiro partido", value = 13),
                           textInput(inputId = "partido2", label = "Digite o numero do segundo partido", value = 45)
                           
                         )
                ),
                
                                 
              #------------------------------------KALED          
              menuItem("Indices", tabName = "indices", icon = icon("line-chart")),
              conditionalPanel(
                class ="menu-conditional",
                condition = "input.sidebarmenu == 'indices'",
                selectInput( inputId = "cargo", label = "Cargo:", choices = c("Presidente", "Governador")),
                uiOutput(outputId = "indices"),
                uiOutput(outputId = "estado_escolhido"))
             
        )
    ),
  
  
  
################------------------BODY--------------------########################  
  
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
      
      
      #-------------------VICTOR------------------#
      tabItem(
        tabName = "intro",
          div(id = "intro",
            h1("CEPESP"),
            h4("A plataforma FGV - eleicoes, foi criada com o objetivo de facilitar o acesso aos dados publicos sobre as eleicoes no Brasil. 
               De uma maneira interativa e completa, aqui pode-se visualizar mapas e graficos dinamicos de acordo com cargo politico, 
               ano e escala regional desejados e ainda comparar partidos quanto a: quantidade de votos, valor gasto em campanha e  percentual de candidatos eleitos. 
               Informacoes como a representatividade por genero e o percentual de votos por candidato na eleicao selecionada tambem sao acessiveis. 
               Se deseja uma perspectiva mais completa, por meio da aba indices, 
               pode-se visualizar indicadores economicos ao longo do tempo  de acordo com o partido no poder. "),
            h3("Veja como funciona")
            
            ),
        
          div(id= "Tadeu",class = "row",
            div(id = "mapa_tadeu",class = "col-sm-6",
                h3("Mapa Interativo"),
                tags$img(src = "https://lamfo-unb.github.io/img/geospace/chunk-5-1.png", width = "80%", height = "80%")
            ),
            div(id = "tadeu", class = "col-sm-6",
                
                h1("Como funciona:"),
                p("Em 'Mapas',tem-se a opcao de selecionar o cargo desejado, ano da eleicao, regiao de agregacao (federacao, estado ou municipio) e o tipo de agregacao politica (partido ou candidato). A partir de cada opcao selecionada gera-se um mapa, mostrando a quantidade de votos em cada regiao,de forma que: quanto mais escura for a tonalidade maior a quantidade de votos. Alem disso, pode-se tambem verificar a influencia da regiao para a eleicao ao escolher se deseja visualizar no mapa:  a quantidade de votos em relacao a populacao ou ao PIB ou ainda a quantidade de votos em escala logaritmica.
 Se deseja observar o mapa em escala municipal, em uma segunda forma de visualizacao, pode-se clicar nas agregacoes municipais ou ainda aproximar o mapa ate que se encontre a cidade desejada.
	Para visualizar a distribuicao de votos por partido no mapa, selecione a aba 'Chart Map', nela, adiciona-se ao mapa em cada unidade federativa um grafico mostrando a porcentagem de votos de cada partido.
")
                )
                ),
        
          div(id = "Ana", class = "row",
                h1("Candidatos"),
                div(id= "ana-tb-1", class= "col-sm-6",
                    h3("Dados por Candidato"),
                    tags$img(src = "https://cdn-images-1.medium.com/max/1000/1*Yu7XYHbRey0wzElg1c-f8A.png", width = "100%", height = "300px"),
                    p("Essa aba reune graficos e informacoes a respeito da distribuicao de genero dos candidatos e percentual de votos por candidato ou partido a partir da escolha do cargo, ano, Estado e nome do municipio selecionado. Pode-se fazer download dos graficos passando o mouse no canto superior da tela")
                    ),
              div(id= "ana-tb-1", class= "col-sm-6",
                  h3("Comparacao entre candidatos"),
                  tags$img(src = "https://cdn-images-1.medium.com/max/750/1*-l0E_YssK9RBWlPvNyjoqA.png", width = "100%", height = "300px"),
                  p("Na aba comparar partidos, basta digitar o numero de dois partidos desejados e, em seguida, graficos e tabelas sao gerados a respeito do percentual de votos do partido selecionado, gasto total e medio em campanha por cargo de acordo com as ultimas eleicoes e percentual de candidatos eleitos por partido. Para uma comparacao mais abrangente, mais dois graficos sao gerados: no primeiro permite-se ter uma ideia da correlacao entre o percentual de votos entre dois partidos e assim analisar se existe uma relacao antagonica entre os dois partidos, caso por exemplo haja uma tendencia decrescente. O segundo permite a visualizacao do percentual de votos para cada partido selecionado de acordo com o estado. ")
                  )
              ),
        
          div(id = "Kaled", class = "row",
              h1("Indices"),
              div(id= "ana-tb-1", class= "col-sm-6",
                  tags$img(src = "https://cdn-images-1.medium.com/max/500/1*-ISjF6tl_qpGPddTaRRo-w.png", width = "100%", height = "100%"),
                  p("Em 'Indices' pode-se visualizar graficos em escala Federal e Estadual que mostram indicadores economicos brasileiros provenientes do IPEA de acordo com os partidos que estavam no poder. No nivel estadual sao mostrados os indices: Gini,Desemprego e pobreza por Estado escolhido. ")
              ),
              div(id= "ana-tb-1", class= "col-sm-6",
                  tags$img(src = "https://cdn-images-1.medium.com/max/750/1*fo8WyMGiTtUWc5sIDMMI4g.png", width = "100%", height = "100%"),
                  p("Ja no nivel nacional, os indices disponiveis sao: Gini, PIB per Capita e Populacao economicamente ativa. ")
              )              
              
          )
      ),
      #-------------------TADEU------------------#    
      tabItem(
        tabName = "map",
        textOutput("erroMapa1"),
        leafletOutput("mapa", height = "100vh")
      ),
      
      
      
      #-------------------ANA-----------------#  
      
      tabItem(
        tabName = "DadosPorCandidato",
        fluidRow(
          uiOutput("generoSpawn", height = "95%"),
          uiOutput("piespawn"),
          uiOutput("tabela")
        )
      ),
      
      tabItem(tabName = "CompararPartidos", #TADEU NAO SEI SE VOCE COLOCOU AQUELE MEU GRAFICO QUE TAVA DANDO PROBLEMA COLOCA ESSA PARTES SE NAO
              h1("", style = "100%"),
              fluidRow(
                box(plotlyOutput("aa"), height = 450, width = 700),
                box(tableOutput("tabelacomp1"), title = ".Comparativo - Despesa Maxima em campanha", height = "50%", width = "100%", align = "center", background = "light-blue"),
                box(tableOutput("tabelacomp2"), title = ".Percentual de candidatos eleitos por partido selecionado",height = "50%", width = "100%",align = "center", background = "light-blue"),
                box(plotlyOutput("ak"), height = 400, width = 650),
                box(plotlyOutput("gg2"), height = 450, width = 700, title = "Percentual votos nos partidos por Estado")
                )
              ),
      
      
      #-------------------KALED------------------#  
      tabItem(tabName = "indices",
              h1("", style = "100%"),
              fluidRow(
                box(plotlyOutput("chart"), height = "35%", width = "250%"),
                box(tableOutput("textinho"), background = "light-blue", height = "25%", width="100%")
              ))
      
      #-------------------------------------# 
    ) #TABITEM
  ) #BODY
) #DASH


#-------------------BACK------------------#

#back end do app
server <- function(input, output) {
  source("www/func/MapaIndividual.R",local = T)
  mapaIndividual()
  source("www/func/PlotCandidatos.R",local = T)
  plotCandidatos()
  source("www/func/PlotCandidatos2.R",local = T)
  plotCandidatos2()
  source("www/func/Indices.R",local=T)
  indicesfunc()
}


shinyApp(ui = ui, server = server)

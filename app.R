

library(plotly)
library(shiny)
library(shinydashboard)
library(leaflet)
#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("Cepesp-Fgv/cepesp-r")
library(cepespR)
library(rgdal)
library(dplyr)
options(scipen = 999)

# setwd("C:\\Users\\victor-pc\\Desktop\\FINAL")

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
                  class="menu-condicional",
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
                  actionButton("goMap", "Gerar mapa")
                  
                ),
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
                menuItem("Misc", tabName = "misc", icon = icon("database")),
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
            h1("Akira Batata"),
            h4("Etiam posuere quam ac quam. Maecenas aliquet accumsan leo. Nullam dapibus fermentum ipsum. Etiam quis quam. Integer lacinia. Nulla est. 
               Nulla turpis magna, cursus sit amet, suscipit a, interdum id, felis. Integer vulputate sem a nibh rutrum consequat. Maecenas lorem. Pellentesque pretium 
               lectus id turpis. Etiam sapien elit, consequat eget, tristique non, venenatis quis, ante. Fusce wisi.Phasellus faucibus molestie nisl. Fusce eget urna. 
               Curabitur vitae diam non enim vestibulum interdum. Nulla quis diam. Ut tempus purus at lorem.")
            ),
          div(id= "Tadeu",class = "row",
            div(id = "mapa_tadeu",class = "col-sm-6",
                h3("Tarik God"),
                tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "70%")
            ),
            div(id = "tadeu", class = "col-sm-6",
                
                h1("Mapa Tadeu"),
                p("Etiam posuere quam ac quam. Maecenas aliquet accumsan leo. Nullam dapibus fermentum ipsum. Etiam quis quam. Integer lacinia. Nulla est. 
                  Nulla turpis magna, cursus sit amet, suscipit a, interdum id, felis. Integer vulputate sem a nibh rutrum consequat. Maecenas lorem. Pellentesque pretium 
                  lectus id turpis. Etiam sapien elit, consequat eget, tristique non, venenatis quis, ante. Fusce wisi.Phasellus faucibus molestie nisl. Fusce eget urna. 
                  Curabitur vitae diam non enim vestibulum interdum. Nulla quis diam. Ut tempus purus at lorem.")
                )
                ),
        
          div(id = "Ana", class = "row",
                h1("Ana"),
                div(id= "ana-tb-1", class= "col-sm-6",
                    h3("Tarik God 2"),
                    tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "300px"),
                    h5("Etiam posuere quam ac quam. Maecenas aliquet accumsan leo. Nullam dapibus fermentum ipsum. Etiam quis quam. Integer lacinia. Nulla est. 
                  Nulla turpis magna, cursus sit amet, suscipit a, interdum id, felis. Integer vulputate sem a nibh rutrum consequat. Maecenas lorem. Pellentesque pretium 
                  lectus id turpis. Etiam sapien elit, consequat eget, tristique non, venenatis quis, ante. Fusce wisi.Phasellus faucibus molestie nisl. Fusce eget urna. 
                  Curabitur vitae diam non enim vestibulum interdum. Nulla quis diam. Ut tempus purus at lorem.")
                    ),
              div(id= "ana-tb-1", class= "col-sm-6",
                  h3("Tarik God 2"),
                  tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "300px"),
                  h5("Etiam posuere quam ac quam. Maecenas aliquet accumsan leo. Nullam dapibus fermentum ipsum. Etiam quis quam. Integer lacinia. Nulla est. 
                     Nulla turpis magna, cursus sit amet, suscipit a, interdum id, felis. Integer vulputate sem a nibh rutrum consequat. Maecenas lorem. Pellentesque pretium 
                     lectus id turpis. Etiam sapien elit, consequat eget, tristique non, venenatis quis, ante. Fusce wisi.Phasellus faucibus molestie nisl. Fusce eget urna. 
                     Curabitur vitae diam non enim vestibulum interdum. Nulla quis diam. Ut tempus purus at lorem.")
                  )
                  
              ),
        
          
        
        div(class = "row",
            
            div(id = "tadeu", class = "col-sm-6",
                
                h1("Mapa Tadeu"),
                p("Etiam posuere quam ac quam. Maecenas aliquet accumsan leo. Nullam dapibus fermentum ipsum. Etiam quis quam. Integer lacinia. Nulla est. 
                  Nulla turpis magna, cursus sit amet, suscipit a, interdum id, felis. Integer vulputate sem a nibh rutrum consequat. Maecenas lorem. Pellentesque pretium 
                  lectus id turpis. Etiam sapien elit, consequat eget, tristique non, venenatis quis, ante. Fusce wisi.Phasellus faucibus molestie nisl. Fusce eget urna. 
                  Curabitur vitae diam non enim vestibulum interdum. Nulla quis diam. Ut tempus purus at lorem.")
                ),
              div(id = "mapa_tadeu",class = "col-sm-6",
                  h3("Tarik God"),
                  tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "70%")
            )
            ),
        
        div(id = "Ana", class = "row",
            h1("Kaled"),
            div(id= "ana-tb-1", class= "col-sm-3",
                h3("Tarik God 2"),
                tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "300px"),
                h5("Etiam posuere quam ac quam. Maecenas aliquet accumsan leo. Nullam dapibus fermentum ipsum. Etiam quis quam. Integer lacinia. Nulla est. 
                    Nulla turpis magna, cursus sit amet, suscipit a, interdum id, felis. Integer vulputate sem a nibh rutrum consequat. Maecenas lorem. Pellentesque pretium 
                    lectus id turpis. Etiam sapien elit, consequat eget, tristique non, venenatis quis, ante. Fusce wisi.Phasellus faucibus molestie nisl. Fusce eget urna. 
                    Curabitur vitae diam non enim vestibulum interdum. Nulla quis diam. Ut tempus purus at lorem.")
            ),
            div(id= "ana-tb-1", class= "col-sm-3",
                h3("Tarik God 2"),
                tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "300px"),
                h5("Etiam posuere quam ac quam. Maecenas aliquet accumsan leo. Nullam dapibus fermentum ipsum. Etiam quis quam. Integer lacinia. Nulla est. 
                       Nulla turpis magna, cursus sit amet, suscipit a, interdum id, felis. Integer vulputate sem a nibh rutrum consequat. Maecenas lorem. Pellentesque pretium 
                       lectus id turpis. Etiam sapien elit, consequat eget, tristique non, venenatis quis, ante. Fusce wisi.Phasellus faucibus molestie nisl. Fusce eget urna. 
                       Curabitur vitae diam non enim vestibulum interdum. Nulla quis diam. Ut tempus purus at lorem.")
            ),
            div(id= "ana-tb-1", class= "col-sm-3",
                h3("Tarik God 2"),
                tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "300px"),
                h5("Etiam posuere quam ac quam. Maecenas aliquet accumsan leo. Nullam dapibus fermentum ipsum. Etiam quis quam. Integer lacinia. Nulla est. 
                       Nulla turpis magna, cursus sit amet, suscipit a, interdum id, felis. Integer vulputate sem a nibh rutrum consequat. Maecenas lorem. Pellentesque pretium 
                       lectus id turpis. Etiam sapien elit, consequat eget, tristique non, venenatis quis, ante. Fusce wisi.Phasellus faucibus molestie nisl. Fusce eget urna. 
                       Curabitur vitae diam non enim vestibulum interdum. Nulla quis diam. Ut tempus purus at lorem.")
            ),
            div(id= "ana-tb-1", class= "col-sm-3",
                h3("Tarik God 2"),
                tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "300px"),
                h5("Etiam posuere quam ac quam. Maecenas aliquet accumsan leo. Nullam dapibus fermentum ipsum. Etiam quis quam. Integer lacinia. Nulla est. 
                       Nulla turpis magna, cursus sit amet, suscipit a, interdum id, felis. Integer vulputate sem a nibh rutrum consequat. Maecenas lorem. Pellentesque pretium 
                       lectus id turpis. Etiam sapien elit, consequat eget, tristique non, venenatis quis, ante. Fusce wisi.Phasellus faucibus molestie nisl. Fusce eget urna. 
                       Curabitur vitae diam non enim vestibulum interdum. Nulla quis diam. Ut tempus purus at lorem.")
            )
            
          ),
        
        div(id = "Tarik", class = "row",
            h1("Tarik"),
            div(id= "ana-tb-1", class= "col-sm-6",
                h3("Tarik God 2"),
                tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "300px"),
                h5("Etiam posuere quam ac quam. Maecenas aliquet accumsan leo. Nullam dapibus fermentum ipsum. Etiam quis quam. Integer lacinia. Nulla est. 
                   Nulla turpis magna, cursus sit amet, suscipit a, interdum id, felis. Integer vulputate sem a nibh rutrum consequat. Maecenas lorem. Pellentesque pretium 
                   lectus id turpis. Etiam sapien elit, consequat eget, tristique non, venenatis quis, ante. Fusce wisi.Phasellus faucibus molestie nisl. Fusce eget urna. 
                   Curabitur vitae diam non enim vestibulum interdum. Nulla quis diam. Ut tempus purus at lorem.")
                ),
            div(id= "ana-tb-1", class= "col-sm-6",
                h3("Tarik God 2"),
                tags$img(src = "https://picsum.photos/200/300", width = "100%", height = "300px"),
                h5("Etiam posuere quam ac quam. Maecenas aliquet accumsan leo. Nullam dapibus fermentum ipsum. Etiam quis quam. Integer lacinia. Nulla est. 
                   Nulla turpis magna, cursus sit amet, suscipit a, interdum id, felis. Integer vulputate sem a nibh rutrum consequat. Maecenas lorem. Pellentesque pretium 
                   lectus id turpis. Etiam sapien elit, consequat eget, tristique non, venenatis quis, ante. Fusce wisi.Phasellus faucibus molestie nisl. Fusce eget urna. 
                   Curabitur vitae diam non enim vestibulum interdum. Nulla quis diam. Ut tempus purus at lorem.")
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
      tabItem(
        tabName = "CompararPartidos",
        h1("FILLER TEXT 1", style = "margin: 0;")
      ),
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
}


shinyApp(ui = ui, server = server)
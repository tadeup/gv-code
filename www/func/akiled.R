akiled<-function(){
  
  presidas_input<-reactive({
    presidas_input<-input$presidas
  })

  output$cloud<-renderWordcloud2({
    
    if(presidas_input()=="FHC: Pronunciamento ao Congresso Nacional, 1999"){
      
      aFile = readLines("www/data/wcloud/fhc1999_limpo.txt")
      
      myCorpus = Corpus(VectorSource(aFile))
      
      myCorpus = tm_map(myCorpus, tolower)
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords, stopwords(kind="portuguese"))
      
      myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
      
      m = as.matrix(myDTM)
      
      v = sort(rowSums(m), decreasing = TRUE)
      
      v<-as.data.frame(v)
      x<-data.frame(row.names(v),v[,1])
      
    }else if(presidas_input()=="Lula: Pronunciamento ao Congresso Nacional, 2003"){
      
      aFile = readLines("www/data/wcloud/lula2003_congreso_limpo.txt")
      
      myCorpus = Corpus(VectorSource(aFile))
      
      myCorpus = tm_map(myCorpus, tolower)
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords, stopwords(kind="portuguese"))
      
      myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
      
      m = as.matrix(myDTM)
      
      v = sort(rowSums(m), decreasing = TRUE)
      
      v<-as.data.frame(v)
      x<-data.frame(row.names(v),v[,1])
      
    }else if(presidas_input()=="Lula: Pronunciamento a Nacao, 2003"){
      
      aFile = readLines("www/data/wcloud/lula2003_limpo.txt")
      
      myCorpus = Corpus(VectorSource(aFile))
      
      myCorpus = tm_map(myCorpus, tolower)
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords, stopwords(kind="portuguese"))
      
      myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
      
      m = as.matrix(myDTM)
      
      v = sort(rowSums(m), decreasing = TRUE)
      
      v<-as.data.frame(v)
      x<-data.frame(row.names(v),v[,1])
      
    }else if(presidas_input()=="Lula: Pronunciamento a Nacao, 2007"){
      
      aFile = readLines("www/data/wcloud/lula2007_limpo.txt")
      
      myCorpus = Corpus(VectorSource(aFile))
      
      myCorpus = tm_map(myCorpus, tolower)
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords, stopwords(kind="portuguese"))
      
      myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
      
      m = as.matrix(myDTM)
      
      v = sort(rowSums(m), decreasing = TRUE)
      
      v<-as.data.frame(v)
      x<-data.frame(row.names(v),v[,1])
      
    }else if(presidas_input()=="Dilma: Pronunciamento ao Congresso Nacional, 2011"){
      
      aFile = readLines("www/data/wcloud/dilma2011_congresso_limpo.txt")
      
      myCorpus = Corpus(VectorSource(aFile))
      
      myCorpus = tm_map(myCorpus, tolower)
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords, stopwords(kind="portuguese"))
      
      myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
      
      m = as.matrix(myDTM)
      
      v = sort(rowSums(m), decreasing = TRUE)
      
      v<-as.data.frame(v)
      x<-data.frame(row.names(v),v[,1])
      
    }else if(presidas_input()=="Dilma: Pronunciamento a Nacao,2011"){
      
      aFile = readLines("www/data/wcloud/dilma2011_limpo.txt")
      
      myCorpus = Corpus(VectorSource(aFile))
      
      myCorpus = tm_map(myCorpus, tolower)
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords, stopwords(kind="portuguese"))
      
      myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
      
      m = as.matrix(myDTM)
      
      v = sort(rowSums(m), decreasing = TRUE)
      
      v<-as.data.frame(v)
      x<-data.frame(row.names(v),v[,1])
      
    }else if(presidas_input()=="Dilma: Pronunciamento ao Congresso Nacional, 2015"){
    aFile = readLines("www/data/wcloud/dilma2015_congresso_limpo.txt")
    
    myCorpus = Corpus(VectorSource(aFile))
    
    myCorpus = tm_map(myCorpus, tolower)
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords, stopwords(kind="portuguese"))
    
    myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
    
    m = as.matrix(myDTM)
    
    v = sort(rowSums(m), decreasing = TRUE)
    
    v<-as.data.frame(v)
    x<-data.frame(row.names(v),v[,1])
    
    wordcloud2(data = x, size = .5, 
               shape = "circle",
               fontFamily = "CMU Sans Serif", rotateRatio = 0.5, 
               ellipticity = 0.9, color = "brown")
    }else if(presidas_input()=="Dilma: Pronunciamento a Nacao, 2015"){
      
      aFile = readLines("www/data/wcloud/dilma2015_limpo.txt")
      
      myCorpus = Corpus(VectorSource(aFile))
      
      myCorpus = tm_map(myCorpus, tolower)
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords, stopwords(kind="portuguese"))
      
      myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
      
      m = as.matrix(myDTM)
      
      v = sort(rowSums(m), decreasing = TRUE)
      
      v<-as.data.frame(v)
      x<-data.frame(row.names(v),v[,1])
      
    }else if(presidas_input()=="Temer: Pronunciamento no dia da posse, 2016"){
      aFile = readLines("www/data/wcloud/temer2016_limpo.txt")
      
      myCorpus = Corpus(VectorSource(aFile))
      
      myCorpus = tm_map(myCorpus, tolower)
      myCorpus = tm_map(myCorpus, removePunctuation)
      myCorpus = tm_map(myCorpus, removeNumbers)
      myCorpus = tm_map(myCorpus, removeWords, stopwords(kind="portuguese"))
      
      myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
      
      m = as.matrix(myDTM)
      
      v = sort(rowSums(m), decreasing = TRUE)
      
      v<-as.data.frame(v)
      x<-data.frame(row.names(v),v[,1])
      
    }
    
    wordcloud2(data = x, size = .5, 
               shape = "circle",
               fontFamily = "CMU Sans Serif", rotateRatio = 0.5, 
               ellipticity = 0.9, color = "brown")
    
  })
  output$textinhonuvem <- renderText({
    textinhonuvem <- HTML(paste(
                  h2(HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'), strong("Guia:")),
                  p(HTML('&nbsp;'),HTML('&nbsp;'), HTML('&nbsp;'), "A nuvem de palavras apresenta os vocabulos mais frequentes nos discursos presidenciais ao congresso nacional e a populacao apos a posse do candidato selecionado.")
    )
    )
  })
}
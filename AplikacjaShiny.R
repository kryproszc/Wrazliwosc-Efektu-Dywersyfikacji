#Pakiety
library(fitdistrplus)
library(numbers)
library(tinytex)
library(shiny)
library(VC2copula)
library(copula)
library(plotly)
library("shiny")
library("shinythemes")
library('plotly')
library("dplyr")
uzytkownik<-tagList(shinythemes::themeSelector(),
                    navbarPage("Wybór rodziny kopuli",
                               tabPanel("Wybór danych",
                                        sidebarPanel(
                                          
                                          fluidPage(   
                                            fileInput('target_upload', 'Wybierz plik do przesłania',
                                                      accept = c(
                                                        'text/csv',
                                                        'text/comma-separated-values',
                                                        '.csv'
                                                      )),
                                            radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=";",inline=TRUE),
                                            selectizeInput(
                                              "colm",
                                              "Wybierz linie biznesowe:",
                                              c(),
                                              multiple = TRUE
                                            ),
                                            actionButton("button", "Zatwierdź"),
                                            
                                            
                                          )),
                                        mainPanel(DT::dataTableOutput("sample_table")
                                                  
                                                  
                                        ),
                               ),
                               tabPanel("Dopasowanie rozkładów",
                                        sidebarPanel(
                                          fluidPage(
                                            selectizeInput(
                                              "kolumnaRozklady",
                                              "Wybierz kolumne:",
                                              c(),
                                              multiple = TRUE
                                            ),
                                            actionButton("buttondopasowanie", "Zatwierdź"),
                                            selectizeInput(
                                              "wybierzRozklady",
                                              "Wybierz rozklady:",
                                              c('norm', 'lnorm', 'gamma', 'weibull', 'exp','logis'),
                                              multiple = FALSE
                                            ),
                                            selectizeInput(
                                              "wybierzMetode",
                                              "Wybierz metode:",
                                              c("mle", "mme", "qme", "mge", "mse"),
                                              multiple = FALSE
                                            )
                                          )),
                                        mainPanel(tabsetPanel(
                                          tabPanel("Wykres", plotOutput("fitd"))
                                        ),
                                        )),
                               tabPanel("Archimedesa",
                                        sidebarPanel(
                                          radioButtons("dist",
                                                       label = "Wybierz kopule",
                                                       list(
                                                         "Kopula Gaussa" = '1',
                                                         "Kopula tStudenta" = '2',
                                                         "Kopula Claytona" = '3',
                                                         "Kopula Gumbela" = "4",
                                                         "Kopula Franka" = '5',
                                                         "Kopula Joe" = "6",
                                                         "Kopula BB1" = '7',
                                                         "Kopula BB6" = '8',
                                                         "Kopula BB7" = '9',
                                                         "Kopula rBB7" = '0',
                                                         "Kopula BB8" = '10'
                                                       ),
                                                       width = "1000px"),
                                          conditionalPanel(
                                            condition = "input.dist == '1'",
                                            sliderInput('n1',
                                                        label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                                                        min = -1, max = 1,step = 0.01, value = 0),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '2'",
                                            sliderInput("n2",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                                                        min = -1, max = 1,step = 0.01, value = 0),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '3'",
                                            sliderInput('n3',
                                                        label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                                                        min = 0, max = 50,step = 0.1, value = 0.1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '4'",
                                            sliderInput("n4",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                                                        min = 1, max = 50,step = 0.1, value = 1.1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '5'",
                                            sliderInput("n5",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                                                        min = -50, max = 50,step = 0.1, value = 0.1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '6'",
                                            sliderInput("n6",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                                                        min = 1, max = 50,step = 0.1, value = 1.1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '7'",
                                            sliderInput("n7",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                                                        min = 0.1, max = 7,step = 0.1, value = 0.1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '7'",
                                            sliderInput("m7",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\delta$$")),
                                                        min = 1, max = 7,step = 0.1, value = 1.1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '8'",
                                            sliderInput("n8",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                                                        min = 1, max = 6,step = 0.1, value = 1.1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '8'",
                                            sliderInput("m8",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\delta$$")),
                                                        min = 1, max = 7,step = 0.1, value = 1.1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '9'",
                                            sliderInput("n9",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                                                        min = 1, max = 6,step = 0.1, value = 1.1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '9'",
                                            sliderInput("m9",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\delta$$")),
                                                        min = 0, max = 50,step = 0.1, value = .1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '10'",
                                            sliderInput("n10",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                                                        min = 1, max = 8,step = 0.1, value = 1.1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '10'",
                                            sliderInput("m10",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\delta$$")),
                                                        min = 0, max = 1,step = 0.1, value = .1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '11'",
                                            sliderInput("n11",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\delta$$")),
                                                        min = 0, max = 360,step = 90, value = 0),
                                            
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '0'",
                                            sliderInput("n0",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                                                        min = -6, max = -1,step = 0.1, value = -1.1),
                                          ),
                                          conditionalPanel(
                                            condition = "input.dist == '0'",
                                            sliderInput("m0",
                                                        label = withMathJax(helpText("Wartość parametru: $$\\delta$$")),
                                                        min = -50, max = 0,step = 0.1, value = -.1),
                                          ),
                                          textInput("iloscsymulacji", ("Liczba obserwacji"), 
                                                    value = 500),
                                          selectInput(inputId = "lobs",
                                                      label = "Losowań:",
                                                      choices = c(2, 3),
                                                      selected = 10),
                                          checkboxInput(inputId = "drzeczywiste",
                                                        label = strong("Dane rzeczywiste"),
                                                        value = TRUE),
                                          actionButton("Proporcjonalne", "Proporcjonalne"),
                                          actionButton("Losujwagi", "Losuj Wagi"),
                                        ),
                                        mainPanel(
                                          #tabsetPanel(
                                          #tabPanel("Definicja", h4(textOutput("diagTitle")),
                                          #     withMathJax(uiOutput("formula"))))
                                          #,
                                          tabsetPanel(
                                            tabPanel("Wykres", plotlyOutput("plot")),
                                            tabPanel("Wykres", plotlyOutput("plotreal"))
                                          ),
                                          tabsetPanel(
                                            tabPanel("Wagi",tableOutput('wagiArchimedes'))),
                                          tabsetPanel(
                                            tabPanel("Wyniki", uiOutput("zaleznosc")))),
                                        
                               ),
                               tabPanel("Vine", 
                                        sidebarPanel(
                                          selectizeInput(inputId = "wybor",
                                                         label = "Wybierz wykre:",
                                                         c("Drzewa",'Dopasowania')
                                          ),
                                          radioButtons("distVine",
                                                       label = "Wybierz kopule",
                                                       list(
                                                         "Cvine" = 'C',
                                                         "Dvine" = 'D'
                                                       ),
                                                       width = "1000px"),
                                          selectizeInput(inputId = "drzewo",
                                                         label = "Wybierz drzewo:",
                                                         c()
                                          ),
                                          actionButton("ProporcjonalneVine", "Proporcjonalne"),
                                          actionButton("LosujVine", "Losuj Wagi"),
                                          
                                        ),
                                        mainPanel(
                                          conditionalPanel(
                                            condition = "input.wybor == 'Drzewa'",
                                            tabsetPanel(
                                              tabPanel("Wykres", plotOutput("plotVine"))
                                            ),
                                          ),
                                          conditionalPanel(
                                            condition = "input.wybor == 'Dopasowania'",
                                            tabsetPanel(
                                              tabPanel("Wykres 3 linie", plotlyOutput("plotDvine"))
                                            ),
                                          ),
                                          
                                          
                                          tabsetPanel(
                                            tabPanel("Wagi",tableOutput('wagaVine'))),
                                          tabsetPanel(
                                            tabPanel("Wyniki", uiOutput("zaleznoscVine"))),
                                        )
                               ),
                               tabPanel("Nieparametryczne", 
                                        sidebarPanel(
                                          radioButtons("distNon",
                                                       label = "Wybierz kopule",
                                                       list(
                                                         "Kopula szachownicy" = 'szachownicy',
                                                         "Dopasowanie Corta" = 'cortDopasowanie',
                                                         "Estymacja Corta" = 'cortEstymacja'
                                                       ),
                                                       width = "1000px"),
                                          conditionalPanel(
                                            condition = "input.distNon == 'szachownicy'",
                                            selectInput(inputId = "dzielnikiM",
                                                        label = "Wybierz m:",
                                                        choices = c()),
                                            checkboxInput(inputId = "kratownica",
                                                          label = strong("Dodaj kratownice"),
                                                          value = FALSE),
                                            
                                            
                                          ),
                                          conditionalPanel(
                                            condition = "input.distNon == 'cortDopasowanie'",
                                            textInput("Lisc", ("Minimalna liczba obserwacji w płacie:"), 
                                                      value = 1),
                                            selectInput(inputId = "liscie",
                                                        label = "Podział liści:",
                                                        choices = c(1:10),
                                                        selected = 1)
                                          ),
                                          conditionalPanel(
                                            condition = "input.distNon == 'cortEstymacja'",
                                            textInput("obserwajceWlisciuEs", ("Minimalna liczba obserwacji w płacie:"), 
                                                      value = 1),
                                            selectInput(inputId = "ktoryPodzial",
                                                        label = "Podział liści:",
                                                        choices = c(1:10),
                                                        selected = 1),
                                            textInput("liczbaObserwacjiEs", ("Liczba losowanych obserwacji:"), 
                                                      value = 1000),
                                            
                                          ),
                                          actionButton("ProporcjonalneNon", "Proporcjonalne"),
                                          actionButton("LosujwagiNon", "Losuj Wagi"),
                                        ),
                                        mainPanel(
                                          #tabsetPanel(
                                          # tabPanel("Definicja", h4(textOutput("diagTitlzCb")),
                                          #          withMathJax(uiOutput("formulaCb"))))
                                          # ,
                                          conditionalPanel(
                                            condition = "input.distNon == 'cortDopasowanie'",
                                            tabsetPanel(
                                              tabPanel("Wykres", plotOutput("cortDopasowanie"))
                                            )
                                          ),
                                          conditionalPanel(
                                            condition = "input.distNon == 'cortEstymacja'",
                                            tabsetPanel(
                                              tabPanel("Wykres", plotlyOutput("cortEstymacja")),
                                              tabPanel("Wykres 2", plotlyOutput("plotNonparapCort"))
                                            ),
                                            tabsetPanel(
                                              tabPanel("Wagi",tableOutput('wagiCort'))),
                                            tabsetPanel(
                                              tabPanel("Wyniki", uiOutput("zaleznoscCort"))),
                                            
                                          ),
                                          conditionalPanel(
                                            condition = "input.distNon == 'szachownicy'",
                                            tabsetPanel(
                                              tabPanel("Wykres", plotlyOutput("plotNon")),
                                              tabPanel("Wykres 2", plotlyOutput("plotNonparap"))
                                            ),
                                            tabsetPanel(
                                              tabPanel("Wagi",tableOutput('wagiNon'))),
                                            tabsetPanel(
                                              tabPanel("Wyniki", uiOutput("zaleznoscNon"))),
                                          ),
                                          
                                          
                                        )
                               )
                    )
)

serwer2<-shinyServer(function(input, output, session) {
  
  v <- reactiveValues(waga = NULL)
  N <- reactiveValues(wagaNon = NULL)
  V <- reactiveValues(wagaVine = NULL)
  
  
  observeEvent(c(input$Proporcjonalne,input$button), {
    v$waga <- c(rep(1,length(input$colm))/length(input$colm))
  })
  
  observeEvent(c(input$ProporcjonalneNon,input$button), {
    N$wagaNon <- c(rep(1,length(input$colm))/length(input$colm))
  })
  observeEvent(c(input$ProporcjonalneVine,input$button), {
    V$wagaVine <- c(rep(1,length(input$colm))/length(input$colm))
  })
  
  observeEvent(input$Losujwagi, {
    w<-runif(length(input$colm))
    ww<-round(w/sum(w),2)
    v$waga <- ww
  }) 
  observeEvent(input$LosujwagiNon, {
    w<-runif(length(input$colm))
    ww<-round(w/sum(w),2)
    N$wagaNon <- ww
  }) 
  observeEvent(input$LosujVine, {
    w<-runif(length(input$colm))
    ww<-round(w/sum(w),2)
    V$wagaVine <- ww
  }) 
  
  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator,dec = ',')
    return(df)
  })
  data_kopula2 <- eventReactive(input$button, {
    df<-df_products_upload()
    df<-df[, input$colm]
  })
  
  observeEvent(df_products_upload(), {
    updateSelectizeInput(session, "dadneVine", choices = names(df_products_upload()))
  })
  
  data_Vine <- eventReactive(input$button, {
    df<-df_products_upload()
    df<-df[, input$colm]
  })
  
  observeEvent(data_Vine(), {
    updateSelectizeInput(session, "drzewo", choices = c(1:(ncol(data_Vine())-1)))
  })
  
  data_fit <- eventReactive(input$buttondopasowanie, {
    df<-df_products_upload()
    df<-df[, input$kolumnaRozklady]
  })
  
  observeEvent(df_products_upload(), {
    updateSelectizeInput(session, "colm", choices = names(df_products_upload()))
  })
  
  observeEvent(df_products_upload(), {
    n <- nrow(df_products_upload())
    dzielniki <- divisors(n)
    updateSelectizeInput(session,   "dzielnikiM", choices = dzielniki)
  })
  
  observeEvent(df_products_upload(), {
    updateSelectizeInput(session, "kolumnaRozklady", choices = names(df_products_upload()))
  })
  output$table <- renderTable({
    data.react()
  })
  output$fitd <- renderPlot({
    fitDist <- fitdist(data_fit(), distr = input$wybierzRozklady, method = input$wybierzMetode, lower = c(0, 0))
    plot(fitDist)
  })
  
  output$zaleznosc<-renderText({
    rozklady <- c('norm', 'lnorm', 'gamma', 'weibull', 'exp','logis')
    o<-findDistribution(data_kopula2(),rozklady)
    margins<-o[[1]]
    paramMargins<-o[[2]] 
    #set.seed(42)
    simu<-rCopula(10000, data())
    Q<-quantileDistributions(simu,margins,paramMargins,v$waga)[1]
    Q<-as.numeric(Q)
    EX<-policzEX(margins,paramMargins)
    Qmodul<-Qmoduls(margins,paramMargins,v$waga,EX) 
    Ed<-round(1-Q/sum(Qmodul),2)
    paste('<B>Efekt dywersyfikacji:<B>',Ed)})
  
  CvineReactive <- reactive({
    dane_kopulowe<-pobs(data_Vine())
    dane_kop<-data.frame(dane_kopulowe)
    copula<-as.copuladata(dane_kop) 
    familyset=c(1,2,3,4,5,6,7,8,9,10)
    C_vine<- RVineStructureSelect(data = copula ,
                                  type = 1,
                                  familyset  = familyset, 
                                  treecrit ='tau',
                                  selectioncrit = 'logLik',
                                  rotations = TRUE, method = 'mle')
    return(C_vine)
  })
  
  DvineReactive <- reactive({
    dane_kopulowe<-pobs(data_Vine())
    dane_kop<-data.frame(dane_kopulowe)
    copula<-as.copuladata(dane_kop) 
    d = ncol(dane_kopulowe)
    M<-1-abs(TauMatrix( copula ) )
    hamilton <-TSP::insert_dummy(TSP(M) , label = 'cut' )
    sol <- solve_TSP(hamilton, 
                     method='repetitive_nn' ) 
    order <- cut_tour( sol , 'cut' )
    DVM<- D2RVine(order ,
                  family = rep(0 , d*(d-1)/2) ,
                  par = rep(0 , d*(d-1)/2)) 
    familyset=c(1,2,3,4,5,6,7,8,9,10)
    D_vine<- RVineCopSelect ( data =copula , family =familyset,
                              Matrix = DVM$Matrix , selectioncrit = 'logLik',
                              method = 'mle' , rotations = TRUE)
    return(D_vine)
  })
  
  output$zaleznoscVine<-renderText({
    rozklady <- c('norm', 'lnorm', 'gamma', 'weibull', 'exp','logis')
    o<-findDistribution(data_Vine(),rozklady)
    margins<-o[[1]]
    paramMargins<-o[[2]] 
    #set.seed(42)
    if(input$distVine=='C'){ 
      VineDop<- CvineReactive()
      
    }
    if(input$distVine=='D'){      VineDop<- DvineReactive()
    }
    family <- VineDop$family
    par<-VineDop$par
    par2<-VineDop$par2
    Matrix<-VineDop$Matrix
    RVMC = RVineMatrix(Matrix=Matrix,family=family,par=par,
                       par2=par2)
    Data_vine<-as.copuladata(RVineSim(1000,RVMC))
    Data_vine<-data.frame(Data_vine)
    Q<-quantileDistributions(Data_vine,margins,paramMargins,V$wagaVine)[1]
    Q<-as.numeric(Q)
    EX<-policzEX(margins,paramMargins)
    Qmodul<-Qmoduls(margins,paramMargins,V$wagaVine,EX) 
    Ed<-round(1-Q/sum(Qmodul),2)
    
    paste('<B>Efekt dywersyfikacji:<B>',Ed)
  })
  
  output$zaleznoscNon<-renderText({
    rozklady <- c('norm', 'lnorm', 'gamma', 'weibull', 'exp','logis')
    o<-findDistribution(data_kopula2(),rozklady)
    margins<-o[[1]]
    paramMargins<-o[[2]] 
    wagi<-N$wagaNon
    #set.seed(42)
    daneszachownica<-data_kopula2()
    pseudoSzachownica <- (apply(daneszachownica,2,rank,ties.method="max")/(nrow(daneszachownica)+1))
    cop <- cbCopulaAplication(x = pseudoSzachownica,
                              m = as.numeric(input$dzielnikiM),pseudo = TRUE)
    simuSzach <- rCopulaCb(n = 1000,copula = cop)
    Q<-quantileDistributions(simuSzach,margins,paramMargins,wagi)[[1]]
    EX<-policzEX(margins,paramMargins)
    Qmodul<-Qmoduls(margins,paramMargins,wagi,EX) 
    Ed<-round(1-Q/sum(Qmodul),2)
    paste('<B>Efekt dywersyfikacji: <B>',Ed)})
  
  output$zaleznoscCort<-renderText({
    
    rozklady <- c('norm', 'lnorm', 'gamma', 'weibull', 'exp','logis')
    o<-findDistribution(data_kopula2(),rozklady)
    margins<-o[[1]]
    paramMargins<-o[[2]] 
    wagi<-N$wagaNon
    dane <- (apply(data_kopula2(),2,rank,ties.method="max")/(nrow(data_kopula2())+1))
    listaCorta<-AplikacjaCort(dane,min_node_size=as.numeric(input$obserwajceWlisciuEs))
    yy<-rCopulaCort(as.numeric(input$liczbaObserwacjiEs),listaCorta[[as.numeric(input$ktoryPodzial)]])
    Q<-quantileDistributions(yy,margins,paramMargins,wagi)[[1]]
    EX<-policzEX(margins,paramMargins)
    Qmodul<-Qmoduls(margins,paramMargins,wagi,EX) 
    Ed<-round(1-Q/sum(Qmodul),2)
    paste('<B>Efekt dywersyfikacji: <B>',Ed)})
  
  output$wagiArchimedes <- renderTable({
    d<-data.frame(names(data_kopula2()),v$waga)
    colnames(d)<-c('Linia biznesowa','Udział lini w portfelu')
    return(d)
  })
  
  output$wagaVine <- renderTable({
    d<-data.frame(names(data_Vine()),V$wagaVine)
    colnames(d)<-c('Linia biznesowa','Udział lini w portfelu')
    return(d)
  })
  
  output$wagiNon <- renderTable({
    d<-data.frame(names(data_kopula2()),N$wagaNon)
    colnames(d)<-c('Linia biznesowa','Udział lini w portfelu')
    return(d)
  })
  
  output$wagiCort <- renderTable({
    d<-data.frame(names(data_kopula2()),N$wagaNon)
    colnames(d)<-c('Linia biznesowa','Udział lini w portfelu')
    return(d)
  })
  
  output$sample_table<- DT::renderDataTable({
    df <- df_products_upload()
    DT::datatable(df)
  })
  
  data <- reactive({
    dist <- switch(input$dist,
                   '1' = normalCopula,
                   '2' = tCopula,
                   '3' = claytonCopula,
                   '4' = gumbelCopula,
                   '5' = frankCopula,
                   '6' = joeCopula,
                   '7' = BB1Copula,
                   '8' = BB6Copula,
                   '9' = BB7Copula,
                   '0' = r90BB7Copula,
                   '10' = BB8Copula,
                   '11' = BiCop
    )
    if(input$dist %in% list('3') ){ dist(input$n3,length(input$colm))}
    else  if(input$dist %in% list('1') ){ dist(input$n1,length(input$colm))}
    else  if(input$dist %in% list('0') ){ dist(c(input$n0,input$m0),length(input$colm))}
    else  if(input$dist %in% list('2') ){ dist(input$n2,length(input$colm))}
    else  if(input$dist %in% list('4') ){ dist(input$n4,length(input$colm))}
    else  if(input$dist %in% list('5') ){ dist(input$n5,length(input$colm))}
    else  if(input$dist %in% list('6') ){ dist(input$n6,length(input$colm))}
    else if(input$dist %in% list('7')){ dist(c(input$n7,input$m7),length(input$colm))}
    else if(input$dist %in% list('8')){ dist(c(input$n8,input$m8),length(input$colm))}
    else if(input$dist %in% list('9')){  dist(c(input$n9,input$m9),length(input$colm))}
    else if(input$dist %in% list('10')){ dist(c(input$n10,input$m10),length(input$colm))}
    else if(input$dist %in% list('11')){ if(input$n11==90){ BiCop(13,input$lobs)}
    }
  })
  
  nazwyObserwacji = c("Random<br>observations","Real<br>data")
  
  output$plotVine <- renderPlot({
    if(input$distVine=='C'){
      plot(CvineReactive(), tree=as.numeric(input$drzewo),edge.labels = "family-tau",type=1)}
    if(input$distVine=='D'){
      plot(DvineReactive(), tree=as.numeric(input$drzewo),edge.labels = "family-tau",type=1)}
    
  })
  
  output$plotDvine <- renderPlotly({
    rozklady <- c('norm', 'lnorm', 'gamma', 'weibull', 'exp','logis')
    o<-findDistribution(data_Vine(),rozklady)
    margins<-o[[1]]
    paramMargins<-o[[2]] 
    #set.seed(42)
    if(input$distVine=='C'){ 
      VineDop<- CvineReactive()
      
    }
    if(input$distVine=='D'){      VineDop<- DvineReactive()
    }
    family <- VineDop$family
    par<-VineDop$par
    par2<-VineDop$par2
    Matrix<-VineDop$Matrix
    RVMC = RVineMatrix(Matrix=Matrix,family=family,par=par,
                       par2=par2)
    Data_vine<-as.copuladata(RVineSim(1000,RVMC))
    Data_vine<-data.frame(Data_vine)
    
    Q<-quantileDistributions(Data_vine,margins,paramMargins,wagi)[[2]]
    Q<-data.frame(Q)
    fig <- plot_ly() 
    fig <- fig %>%
      add_trace(
        x = ~Q[,1],
        y = ~Q[,2],
        z = ~Q[,3],
        name = nazwyObserwacji[1],
        marker = list(
          size = 3
        ))
    fig <- fig %>% layout(scene = list(xaxis = list(title = 'u1'),
                                       yaxis = list(title = 'u2'),
                                       zaxis = list(title = 'u3')))
    
    fig <- fig %>%
      add_trace(
        x = data_Vine()[,1],
        y = data_Vine()[,2],
        z = data_Vine()[,3],
        name = nazwyObserwacji[2],
        marker = list(
          size = 5,
          color = 'red'
        )
      )
    fig <- fig %>% layout(scene = list(xaxis = list(title = colnames(data_Vine())[1]),
                                       yaxis = list(title = colnames(data_Vine())[2]),
                                       zaxis = list(title = colnames(data_Vine())[3])))
  })
  
  output$plotNonparap <- renderPlotly({
    daneszachownica<-data_kopula2()
    pseudoSzachownica <- (apply(daneszachownica,2,rank,ties.method="max")/(nrow(daneszachownica)+1))
    rozklady <- c('norm', 'lnorm', 'gamma', 'weibull', 'exp','logis')
    o<-findDistribution(daneszachownica,rozklady)
    margins<-o[[1]]
    paramMargins<-o[[2]] 
    #set.seed(42)
    if(input$distNon=='szachownicy'){ 
      cop <- cbCopulaAplication(x = pseudoSzachownica,
                                m = as.numeric(input$dzielnikiM),pseudo = TRUE)
      simuSzach <- rCopulaCb(n = 1000,copula = cop)}
    else if(input$distNon=='cortEstymacja'){ 
      listaCorta<-AplikacjaCort(pseudoSzachownica,min_node_size=as.numeric(input$obserwajceWlisciuEs))
      simuSzach<-rCopulaCort(as.numeric(input$liczbaObserwacjiEs),listaCorta[[as.numeric(input$ktoryPodzial)]])
    }
    
    Q<-quantileDistributions(simuSzach,margins,paramMargins,wagi)[[2]]
    Q<-data.frame(Q)
    fig <- plot_ly() 
    fig <- fig %>%
      add_trace(
        x = ~Q[,1],
        y = ~Q[,2],
        z = ~Q[,3],
        name = nazwyObserwacji[1],
        marker = list(
          size = 3
        ))
    fig <- fig %>% layout(scene = list(xaxis = list(title = 'u1'),
                                       yaxis = list(title = 'u2'),
                                       zaxis = list(title = 'u3')))
    
    fig <- fig %>%
      add_trace(
        x = data_kopula2()[,1],
        y = data_kopula2()[,2],
        z = data_kopula2()[,3],
        name = nazwyObserwacji[2],
        marker = list(
          size = 5,
          color = 'red'
        )
      )
    fig <- fig %>% layout(scene = list(xaxis = list(title = colnames(data_kopula2())[1]),
                                       yaxis = list(title = colnames(data_kopula2())[2]),
                                       zaxis = list(title = colnames(data_kopula2())[3])))
  })
  
  output$plotNonparapCort <- renderPlotly({
    daneszachownica<-data_kopula2()
    pseudoSzachownica <- (apply(daneszachownica,2,rank,ties.method="max")/(nrow(daneszachownica)+1))
    rozklady <- c('norm', 'lnorm', 'gamma', 'weibull', 'exp','logis')
    o<-findDistribution(daneszachownica,rozklady)
    margins<-o[[1]]
    paramMargins<-o[[2]] 
    #set.seed(42)
    
    
    listaCorta<-AplikacjaCort(pseudoSzachownica,min_node_size=as.numeric(input$obserwajceWlisciuEs))
    simuSzach<-rCopulaCort(as.numeric(input$liczbaObserwacjiEs),listaCorta[[as.numeric(input$ktoryPodzial)]])
    
    
    Q<-quantileDistributions(simuSzach,margins,paramMargins,wagi)[[2]]
    Q<-data.frame(Q)
    fig <- plot_ly() 
    fig <- fig %>%
      add_trace(
        x = ~Q[,1],
        y = ~Q[,2],
        z = ~Q[,3],
        name = nazwyObserwacji[1],
        marker = list(
          size = 3
        ))
    fig <- fig %>% layout(scene = list(xaxis = list(title = 'u1'),
                                       yaxis = list(title = 'u2'),
                                       zaxis = list(title = 'u3')))
    
    fig <- fig %>%
      add_trace(
        x = data_kopula2()[,1],
        y = data_kopula2()[,2],
        z = data_kopula2()[,3],
        name = nazwyObserwacji[2],
        marker = list(
          size = 5,
          color = 'red'
        )
      )
    fig <- fig %>% layout(scene = list(xaxis = list(title = colnames(data_kopula2())[1]),
                                       yaxis = list(title = colnames(data_kopula2())[2]),
                                       zaxis = list(title = colnames(data_kopula2())[3])))
  })
  
  output$plotNon <- renderPlotly({
    
    daneszachownica<-data_kopula2()
    pseudoSzachownica <- (apply(daneszachownica,2,rank,ties.method="max")/(nrow(daneszachownica)+1))
    
    if(length(input$colm)==2){
      
      cop <- cbCopulaAplication(x = pseudoSzachownica,
                                m = as.numeric(input$dzielnikiM),pseudo = TRUE)
      simuSzach <- rCopulaCb(n = 1000,copula = cop)
      fig <- plot_ly() 
      fig <- fig %>%
        add_trace(
          x = ~simuSzach[,1],
          y = ~simuSzach[,2],
          name = nazwyObserwacji[1],
          marker = list(
            size = 3
          ))
      fig <- fig %>%
        add_trace(
          x = pobs(pseudoSzachownica[,1]),
          y = pobs(pseudoSzachownica[,2]),
          name = nazwyObserwacji[2],
          marker = list(
            size = 5,
            color = 'red'
          ))
      fig <- fig %>% 
        layout(xaxis = list(title = colnames(data_kopula2())[1]), 
               yaxis = list(title=colnames(data_kopula2())[2]))
      
      if(input$kratownica==TRUE){
        podzial<-zwracaPodzial(as.numeric(input$dzielnikiM))
        for (i in podzial){
          fig <- add_lines(fig, 
                           x = i, 
                           y = 0:1,
                           inherit = FALSE,
                           line = list(color = 'rgb(0, 0, 1)'),
                           showlegend = F,
                           cex=0.1
          )
        }
        for (i in podzial){
          fig <- add_lines(fig, 
                           x = 0:1, 
                           y = i,
                           inherit = FALSE,
                           line = list(color = 'rgb(0, 0, 1)'),
                           showlegend = F,
                           cex=0.1
          )
        }
        fig
      }
      else{fig}
    }
    else if(length(input$colm)==3){
      cop <- cbCopulaAplication(x = pseudoSzachownica,
                                m = as.numeric(input$dzielnikiM),pseudo = TRUE)
      simuSzach <- rCopulaCb(n = 1000,copula = cop)
      
      fig <- plot_ly() 
      fig <- fig %>%
        add_trace(
          x = ~simuSzach[,1],
          y = ~simuSzach[,2],
          z = ~simuSzach[,3],
          name = nazwyObserwacji[1],
          marker = list(
            size = 3
          ))
      
      fig <- fig %>%
        add_trace(
          x = pobs(pseudoSzachownica[,1]),
          y = pobs(pseudoSzachownica[,2]),
          z = pobs(pseudoSzachownica[,3]),
          name = nazwyObserwacji[2],
          marker = list(
            size = 5,
            color = 'red'
          ))
      fig <- fig %>% layout(scene = list(xaxis = list(title = colnames(data_kopula2())[1]),
                                         yaxis = list(title = colnames(data_kopula2())[2]),
                                         zaxis = list(title = colnames(data_kopula2())[3])))
      if(input$kratownica==TRUE){
        podzial<-zwracaPodzial(as.numeric(input$dzielnikiM))
        for (i in podzial){
          fig <- add_lines(fig, 
                           x = i, 
                           y = 0:1,
                           z = 0:1,
                           inherit = FALSE,
                           line = list(color = 'rgb(0, 0, 1)',color = 'rgb(0, 0, 1)'),
                           showlegend = F,
                           cex=0.1
          )
        }
        
        fig
      }
      else{fig}
      
      
    }
  })
  
  output$cortDopasowanie <- renderPlot({
    listaCorta<-AplikacjaCort(data_kopula2(),min_node_size=as.numeric(input$Lisc))
    plot.Cort(listaCorta[[as.numeric(input$liscie)]])
    
  })
  
  output$cortEstymacja <- renderPlotly({
    daneszachownica<-data_kopula2()
    dane <- (apply(daneszachownica,2,rank,ties.method="max")/(nrow(daneszachownica)+1))
    if(input$distNon=='cortEstymacja' & length(input$colm)==2){
      listaCorta<-AplikacjaCort(dane,min_node_size=as.numeric(input$obserwajceWlisciuEs))
      yy<-rCopulaCort(as.numeric(input$liczbaObserwacjiEs),listaCorta[[as.numeric(input$ktoryPodzial)]])
      fig <- plot_ly() 
      fig <- fig %>%
        add_trace(
          x = ~yy[,1],
          y = ~yy[,2],
          name = nazwyObserwacji[1],
          marker = list(
            size = 3
          ))
      fig <- fig %>%
        add_trace(
          x = pobs(dane[,1]),
          y = pobs(dane[,2]),
          name = nazwyObserwacji[2],
          marker = list(
            size = 5,
            color = 'red'
          ))
      fig <- fig %>% 
        layout(xaxis = list(title = colnames(dane)[1]), yaxis = list(title=colnames(dane)[2]))
      fig
    }
    else if(input$distNon=='cortEstymacja' & length(input$colm)==3){
      listaCorta<-AplikacjaCort(dane,min_node_size=as.numeric(input$obserwajceWlisciuEs))
      yy<-rCopulaCort(as.numeric(input$liczbaObserwacjiEs),listaCorta[[as.numeric(input$ktoryPodzial)]])
      fig <- plot_ly() 
      fig <- fig %>%
        add_trace(
          x = ~yy[,1],
          y = ~yy[,2],
          z = ~yy[,3],
          name = nazwyObserwacji[1],
          marker = list(
            size = 3
          ))
      fig <- fig %>%
        add_trace(
          x = pobs(dane[,1]),
          y = pobs(dane[,2]),
          z = pobs(dane[,3]),
          name = nazwyObserwacji[2],
          marker = list(
            size = 5,
            color = 'red'
          ))
      fig <- fig %>% layout(scene = list(xaxis = list(title = colnames(dane)[1]),
                                         yaxis = list(title = colnames(dane)[2]),
                                         zaxis = list(title = colnames(dane)[3])))
      fig
    }
    
  })
  
  output$plotreal<-renderPlotly({
    rozklady <- c('norm', 'lnorm', 'gamma', 'weibull', 'exp','logis')
    o<-findDistribution(data_kopula2(),rozklady)
    margins<-o[[1]]
    paramMargins<-o[[2]] 
    wagi<-v$waga
    #set.seed(42)
    simu<-rCopula(1000, data())
    Q<-quantileDistributions(simu,margins,paramMargins,wagi)[[2]]
    Q<-data.frame(Q)
    if(length(input$colm)==3){
      fig <- plot_ly() 
      fig <- fig %>%
        add_trace(
          x = ~Q[,1],
          y = ~Q[,2],
          z = ~Q[,3],
          name = nazwyObserwacji[1],
          marker = list(
            size = 3
          ))
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'u1'),
                                         yaxis = list(title = 'u2'),
                                         zaxis = list(title = 'u3')))
      
      if(input$drzeczywiste==TRUE){
        fig <- fig %>%
          add_trace(
            x = data_kopula2()[,1],
            y = data_kopula2()[,2],
            z = data_kopula2()[,3],
            name = nazwyObserwacji[2],
            marker = list(
              size = 5,
              color = 'red'
            )
          )
        fig <- fig %>% layout(scene = list(xaxis = list(title = colnames(data_kopula2())[1]),
                                           yaxis = list(title = colnames(data_kopula2())[2]),
                                           zaxis = list(title = colnames(data_kopula2())[3])))
      }
      else {fig}
    }
    else if(length(input$colm)==2){
      fig <- plot_ly() 
      fig <- fig %>%
        add_trace(
          x = ~Q[,1],
          y = ~Q[,2],
          name = nazwyObserwacji[1],
          marker = list(
            size = 3
          ))
      fig <- fig %>% 
        layout(xaxis = list(title = 'u1'), yaxis = list(title="u2"))
      if(input$drzeczywiste==TRUE){
        fig <- fig %>%
          add_trace(
            x = data_kopula2()[,1],
            y = data_kopula2()[,2],
            name = nazwyObserwacji[2],
            marker = list(
              size = 5,
              color = 'red'
            )
          )
        fig <- fig %>% 
          layout(xaxis = list(title = colnames(data_kopula2())[1]), yaxis = list(title=colnames(data_kopula2())[2]))
      }
      else {fig}
    }
    
  })
  
  output$plot <- renderPlotly({
    simu<-rCopula(as.numeric(as.numeric(input$iloscsymulacji)), data())
    if(length(input$colm)==3){
      fig <- plot_ly() 
      fig <- fig %>%
        add_trace(
          x = ~simu[,1],
          y = ~simu[,2],
          z = ~simu[,3],
          name = nazwyObserwacji[1],
          marker = list(
            size = 3
          ))
      fig <- fig %>% layout(scene = list(xaxis = list(title = 'u1'),
                                         yaxis = list(title = 'u2'),
                                         zaxis = list(title = 'u3')))
      
      if(input$drzeczywiste==TRUE){
        fig <- fig %>%
          add_trace(
            x = pobs(data_kopula2()[,1]),
            y = pobs(data_kopula2()[,2]),
            z = pobs(data_kopula2()[,3]),
            name = nazwyObserwacji[2],
            marker = list(
              size = 5,
              color = 'red'
            )
          )
        fig <- fig %>% layout(scene = list(xaxis = list(title = colnames(data_kopula2())[1]),
                                           yaxis = list(title = colnames(data_kopula2())[2]),
                                           zaxis = list(title = colnames(data_kopula2())[3])))
      }
      else {fig}
    }
    else if(length(input$colm)==2){
      fig <- plot_ly() 
      fig <- fig %>%
        add_trace(
          x = ~simu[,1],
          y = ~simu[,2],
          name = nazwyObserwacji[1],
          marker = list(
            size = 3
          ))
      fig <- fig %>% 
        layout(xaxis = list(title = 'u1'), yaxis = list(title="u2"))
      if(input$drzeczywiste==TRUE){
        fig <- fig %>%
          add_trace(
            x = pobs(data_kopula2()[,1]),
            y = pobs(data_kopula2()[,2]),
            name = nazwyObserwacji[2],
            marker = list(
              size = 5,
              color = 'red'
            )
          )
        fig <- fig %>% 
          layout(xaxis = list(title = colnames(data_kopula2())[1]), yaxis = list(title=colnames(data_kopula2())[2]))
      }
      else {fig}
    }
  })
  
  output$formulaVine <- renderUI({
    kopulaVine <- switch(input$distVine,
                         'C' = 'Cvine',
                         'D' = 'Dvine'
    )
    
    if(input$distVine=="C" && input$macierzKan==FALSE){
      withMathJax(
        helpText('Czterowymiarową kopulę ',kopulaVine, 'definiujemy w następujący sposób: ','          
       $$
\\begin{aligned}
f\\left(x_{1}, x_{2}, x_{3}, x_{4}\\right)=& f_{1}\\left(x_{1}\\right) f_{2}\\left(x_{2}\\right) f_{3}\\left(x_{3}\\right) f_{4}\\left(x_{4}\\right) \\\\
& \\cdot c_{12}\\left(F_{1}\\left(x_{1}\\right), F_{2}\\left(x_{2}\\right)\\right) c_{13}\\left(F_{1}\\left(x_{1}\\right), F_{3}\\left(x_{3}\\right)\\right) c_{14}\\left(F_{1}\\left(x_{1}\\right), F_{4}\\left(x_{4}\\right)\\right) \\\\
& \\cdot c_{23 ; 1}\\left(F_{2 \\mid 1}\\left(x_{2} \\mid x_{1}\\right), F_{3 \\mid 1}\\left(x_{3} \\mid x_{1}\\right),\\right) \\cdot c_{24 ; 1}\\left(F_{2 \\mid 1}\\left(x_{2} \\mid x_{1}\\right), F_{4 \\mid 1}\\left(x_{4} \\mid x_{1}\\right),\\right) \\\\
& \\cdot c_{34 ; 12}\\left(F_{3 \\mid 12}\\left(x_{3} \\mid x_{1}, x_{2}\\right), F_{4 \\mid 12}\\left(x_{4} \\mid x_{1}, x_{2}\\right)\\right)
\\end{aligned}
$$
                 '))
    }
    else if(input$distVine=="C" && input$macierzKan==TRUE){
      withMathJax(
        helpText('$$
\\left(\\begin{array}{ccc}
1 & & \\left|\\tau_{1, d}\\right| \\\\
\\left|\\tau_{2,1}\\right| & \\cdots & \\left|\\tau_{2, d}\\right| \\\\
\\vdots & \\ddots & \\vdots \\\\
\\left|\\tau_{d, 1}\\right| & \\cdots & 1
\\end{array}\\right)
$$         
       
                 '))
    }
    else if(input$distVine=="D"){
      withMathJax(
        helpText('Czterowymiarową kopulę ',kopulaVine, 'definiujemy w następujący sposób: ','
               $$
              \\begin{aligned}
f\\left(x_{1}, x_{2}, x_{3}, x_{4}\\right)=&
f_{1}\\left(x_{1}\\right) f_{2}\\left(x_{2}\\right) f_{3}\\left(x_{3}\\right) f_{4}\\left(x_{4}\\right) \\\\
& \\cdot c_{12}\\left(F_{1}\\left(x_{1}\\right), F_{2}\\left(x_{2}\\right)\\right) c_{23}\\left(F_{2}\\left(x_{2}\\right), F_{3}\\left(x_{3}\\right)\\right) c_{34}\\left(F_{3}\\left(x_{3}\\right), F_{4}\\left(x_{4}\\right)\\right) \\\\
& \\cdot c_{13 ; 2}\\left(F_{1 \\mid 2}\\left(x_{1} \\mid x_{2}\\right), F_{3 \\mid 2}\\left(x_{3} \\mid x_{2}\\right),\\right) \\cdot c_{24 ; 3}\\left(F_{2 \\mid 3}\\left(x_{2} \\mid x_{3}\\right), F_{4 \\mid 3}\\left(x_{4} \\mid x_{3}\\right),\\right) \\\\
& \\cdot c_{14 ; 23}\\left(F_{1 \\mid 23}\\left(x_{1} \\mid x_{2}, x_{3}\\right), F_{4 \\mid 23}\\left(x_{4} \\mid x_{2}, x_{3}\\right)\\right)
\\end{aligned}
$$
               
            
                 '))
    }
  })
  
  output$formulaCb <- renderUI({
    kopulaParametryczna <- switch(input$distNon,
                                  'szachownicy' = 'Kopula szachownicy',
                                  'D' = 'Dvine'
    )
    if(input$distNon=="szachownicy"){
      withMathJax(
        helpText('Kopulę szachownicy definiujemy w następujący sposób:',
                 '$$C(x)=\\sum_{\\mathbf{i} \\in I} m^{d} \\mu\\left(I_{\\mathbf{i}, m}\\right) \\lambda\\left([0, \\mathbf{x}] \\cap I_{\\mathbf{i}, m}\\right)$$',
                 '$$\\text { Gdzie zbiór indeksów } \\mathcal{J}=\\left\\{\\mathbf{i}=\\left(i_{1}, \\ldots, i_{d}\\right) \\subset\\{1, \\ldots, m\\}^{d}\\right\\}$$',
                 '$$\\text {indeksujących kostki } I_{i, m}=\\prod_{j=1}^{d}\\left(\\frac{i_{j}-1}{m}, \\frac{i_{j}}{m}\\right]$$',
                 '$$\\text{natomiast } \\left[0, x_{1}, \\ldots, x_{d}\\right]=\\prod_{i=1}^{d}\\left[0, x_{i}\\right]$$',
                 
                 
        ))
    }
    else if(input$distNon=="cortDopasowanie"){
      withMathJax(
        helpText('Odcinkowo liniową kopule definiujemy następująco:',
                 '$$C_{\\boldsymbol{p}, \\mathcal{L}}(\\mathbf{x})=\\sum_{i=1}^{k} p_{i} \\lambda_{\\ell_{i}}(\\boldsymbol{x})$$',
                 '$$\\text {gdzie } \\lambda_{\\ell_{i}}(\\mathbf{x})=\\lambda\\left(\\ell_{i}\\right)^{-1} \\lambda\\left([0, \\mathbf{x}] \\cap \\ell_{i}\\right) \\text {jest miarą Lebesgue-a }$$',
                 '$$\\text {natomiast } \\left[0, x_{1}, \\ldots, x_{d}\\right]=\\prod_{i=1}^{d}\\left[0, x_{i}\\right]$$',
                 '$$\\text{Jeśli } p_{\\ell} = \\lambda\\left(\\ell\\right)\\text{wówczas } C_{\\boldsymbol{p}, \\mathcal{L}}(\\mathbf{x}) \\text{ jest kopulą} $$',
                 
                 
        ))
    }
    
  })
  
  output$formula <- renderUI({
    nazwa_kopuli <- switch(input$dist,
                           '1' = 'Gaussa',
                           '2' = 'tStudenta',
                           '3' = 'Claytona',
                           '4' = 'Gumbela',
                           '5' = 'Franka',
                           '6' = 'Joe' ,
                           '7' = 'BB1',
                           '8' = 'BB6',
                           '9' = 'BB7',
                           '10' = 'BB8',
                           '0' = 'rBB7'
    )
    if(input$dist=="3"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ',nazwa_kopuli, 'definiujemy w następujący sposób: ','          
        $$C_{\\theta}^{Clayton}(u_{1}, u_{2})=\\left(u_{1}^{-\\theta}+
               u_{2}^{-\\theta}-1\\right)^{-\\frac{1}{\\theta}}$$
               dla $$\\theta >0.$$
                 '))
    }
    else if(input$dist=="1"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ',nazwa_kopuli, 'definiujemy w następujący sposób: ','
               $$C_{\\theta}^{G a}(u_{1}, u_{2})=\\int_{-\\infty}^{\\Phi^{-1}(u_{1})} \\int_{-\\infty}^{\\Phi^{-1}(u_{2})} 
               \\frac{1}{2 \\pi \\sqrt{1-\\theta^{2}}}exp\\left(-\\frac{s^{2}-2 \\theta s t+t^{2}}
               {2\\left(1-\\theta^{2}\\right)}\\right) d s d t$$
               dla $$ -1 \\leq \\theta \\leq 1.$$
                 '))
    }
    else if(input$dist=="2"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ',nazwa_kopuli, 'definiujemy w następujący sposób: ','
               $$C_{\\theta, v}^{S t}\\left(u_{1}, u_{2}\\right)=\\int_{-\\infty}^{t_{\\nu}^{-1}(u_{1})}
               \\int_{-\\infty}^{t_{\\nu}^{-1}(u_{2})} \\frac{1}{2 \\pi \\sqrt{1-\\theta^{2}}}
               \\left(1+\\frac{s^{2}-2 \\theta s t+t^{2}}{\\nu\\left(1-\\theta^{2}\\right)}\\right)^{-\\frac{(\\nu+2)}{2}} 
               \\mathrm{~d} s \\mathrm{~d} t$$
               dla $$ -1 \\leq \\theta \\leq 1.$$
                 '))
    }
    else if(input$dist=="4"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ',nazwa_kopuli, 'definiujemy w następujący sposób: ','
               $$C_{\\theta}^{Gumbel}(u_{1}, u_{2})=\\exp \\left[-\\left\\{\\left(-\\ln u_{1}\\right)^{\\theta}+
               \\left(-\\ln u_{2}\\right)^{\\theta}\\right\\}^{\\frac{1}{\\theta}}\\right]$$
               dla $$\\theta \\geq 1.$$
                 '))
    }
    else if(input$dist=="5"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ',nazwa_kopuli, 'definiujemy w następujący sposób: ','
               $$C_{\\theta}^{Frank}(u_{1}, u_{2})=-\\frac{1}{\\theta} \\ln 
               \\left(\\frac{1}{1-e^{-\\theta}}\\left[\\left(1-e^{-\\theta}\\right)
               -\\left(1-e^{-\\theta u_{1}}\\right)
               \\left(1-e^{-\\theta u_{2}}\\right)\\right]\\right)$$
               dla $$\\theta >0.$$
                 '))
    }
    else if(input$dist=="6"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ',nazwa_kopuli, 'definiujemy w następujący sposób: ','
               $$C_{\\theta}^{Joe}(u_{1}, u_{2})=1-\\left(\\left(1-u_{1}\\right)^{\\theta}+
               \\left(1-u_{2}\\right)^{\\theta}-
               \\left(1-u_{1}\\right)^{\\theta}\\left(1-
               u_{2}\\right)^{\\theta}\\right)^{\\frac{1}{\\theta}}$$
               dla $$\\theta >1.$$
                 '))
    }
    else if(input$dist=="7"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ','Claytona-Gumbela', 'definiujemy w następujący sposób: ','
               $$C_{\\theta, \\delta}^{B B 1}(u_{1}, u_{2})=
               \\left\\{1+\\left[\\left(u_{1}^{-\\theta}-1\\right)^{\\delta}+
               \\left(u_{2}^{-\\theta}-1\\right)^{\\delta}\\right]^{\\frac{1}{\\delta}}
               \\right\\}^{-\\frac{1}{\\theta}}$$
               dla $$\\theta > 0,$$
               oraz $$\\delta \\geq 1.$$
                 '))
    }
    else if(input$dist=="8"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ','Joe-Gumbela', 'definiujemy w następujący sposób: ','
               $$\\left.\\left.C_{\\theta, \\delta}^{B B 6}\\left(u_{1}, u_{2}\\right)=
               1-\\left(1-\\exp -\\left[\\left(-\\log \\left(1-u_{1}\\right)^{\\theta}
               \\right)\\right)\\right)^{\\delta}+\\left(-\\log \\left(1-
               \\left(1-u_{2}\\right)^{\\theta}\\right)\\right)^{\\delta}\\right]^{\\frac{1}{\\delta}}\\right)^{\\frac{1}{\\theta}}$$
               dla $$\\theta \\geq 1,$$
               oraz $$\\delta > 0.$$
                 '))
    }
    else if(input$dist=="9"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ','Joe-Claytona', 'definiujemy w następujący sposób: ','
               $$C_{\\theta, \\delta}^{B B 7}\\left(u_{1}, u_{2}\\right)=
               1-\\left[1-\\left(\\left(1-u_{1}^{\\theta}\\right)^{-\\delta}+
               \\left(1-u_{2}\\right)^{-\\delta}-1\\right)^{-\\frac{1}{\\delta}}\\right]^{\\frac{1}{\\theta}}$$
               dla $$\\theta \\geq 1,$$
               oraz $$\\delta > 0.$$
                 '))
    }
    else if(input$dist=="10"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ','Joe-Franka', 'definiujemy w następujący sposób: ','
               $$\\left.C_{\\theta, \\delta}^{B B 8}\\left(u_{1}, u_{2}\\right)=
               \\frac{1}{\\theta}\\left(1-\\left[1-\\frac{1}{1-(1-\\delta)^{\\theta}}
               \\left(1-\\left(1-\\delta u_{1}\\right)^{\\theta}\\right)
               \\left(1-\\delta u_{2}\\right)^{\\theta}\\right)\\right]^{\\frac{1}{\\delta}}\\right)$$
               dla $$\\theta \\geq 1,$$
               oraz $$ 0< \\delta < 1.$$
                 '))
    }
    else if(input$dist=="0"){
      withMathJax(
        helpText('Kopule obrócone: ','
               $$\\begin{array}{l}
                C_{r o t 90}\\left(u_{1}, u_{2}\\right)=u_{2}-C\\left(1-u_{1}, u_{2}\\right) \\\\
                C_{r o t 180}\\left(u_{1}, u_{2}\\right)=u_{1}+u_{2}-1+C\\left(1-u_{1}, 1-u_{2}\\right) \\\\
                C_{r o t 270}\\left(u_{1}, u_{2}\\right)=u_{1}-C\\left(u_{1}, 1-u_{2}\\right)
                \\end{array}$$
                 '))}
    
  })
  
})



shinyApp(ui = uzytkownik, server = serwer2)

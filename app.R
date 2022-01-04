#    http://shiny.rstudio.com/
#

pacman::p_load(shiny,readr,rsconnect,dplyr,highcharter,shinythemes,
               shinydashboard,quantmod,fPortfolio,
               data.table,shinyjs, shinyWidgets)

L_Acciones = list(NASDAQ = list("AAPL", "TSLA","NVDA","PYPL", "AMD", "MSFT",
                                "MU", "AMZN", "FB", "NFLX"),
                  NYSE = list("NIO", "SQ","JPM", "V", 
                              "TWTR","KO","NKE","IBM"))


Arreglos <- function(Accion){
  Accion = Accion[,6]
  Accion = as.data.frame(Accion)
  setDT(Accion,keep.rownames = T)
}

SP <- getSymbols("^GSPC",src="yahoo",periodicity="daily",
                 auto.assign=F, from = "2017-01-01")

GSPC <- Arreglos(SP)

SP500 <- GSPC%>%
  mutate(variacion = ROC(GSPC.Adjusted, type = "discrete") * 100)

Adj_SP <- adjustOHLC(SP)
Adj_SP.SMA.20 <- SMA(Cl(Adj_SP), n = 20)
Adj_SP.SMA.200 <- SMA(Cl(Adj_SP), n = 200)
Adj_SP.RSI.14 <- RSI(Cl(Adj_SP))
Adj_SP.RSI.SellLevel <- xts(rep(70, NROW(Adj_SP)), index(Adj_SP))
Adj_SP.RSI.BuyLevel <- xts(rep(30, NROW(Adj_SP)), index(Adj_SP))


## CAJAS

Negativos <- SP500%>%
  dplyr::filter(variacion <= 0.00000)

Positivos <- SP500%>%
  dplyr::filter(variacion > 0.00000)

Primero <- head(SP500,1)
Ultimo <- tail(SP500,1)

### PRIMERA LINEA

PP_box <-  valueBox(
  value = paste0(round(nrow(Positivos)/nrow(SP500)*100,0),"%"),
  subtitle = "Porcentaje de dias positivos",
  icon = icon("arrow-up"),
  color = "green")

NP_box <-  valueBox(
  value = paste0(round(nrow(Negativos)/nrow(SP500)*100,0),"%"),
  subtitle = "Porcentaje de dias Negativos",
  icon = icon("arrow-down"),
  color = "red")

DP_box <-  valueBox(
  value = paste0(round(((Ultimo[1,2] - Primero[1,2]) /Primero[1,2])*100,2),"%"),
  subtitle = "Variacion porcentual del precio desde 2017/01/01",
  icon = icon("triangle-top", lib = "glyphicon"),
  color = "blue")


### SEGUNDA LINEA


PN_box <-  valueBox(
  value = nrow(Positivos),
  subtitle = "Numero total de dias positivos",
  icon = icon("plus"),
  color = "green")

NN_box <-  valueBox(
  value = nrow(Negativos),
  subtitle = "Numero total de dias Negativos",
  icon = icon("minus"),
  color = "red")

DN_box <-  valueBox(
  value = round(((Ultimo[1,2]-Primero[1,2])),2),
  subtitle = "Diferencia del precio desde 2017/01/01",
  icon = icon("dollar-sign"),
  color = "blue")



### TERCERA LINEA


PD_box <-  valueBox(
  value = paste0(round(max(Positivos[,3]),2),"%"),
  subtitle = "Mayor alza en un dia",
  color = "green")

ND_box <-  valueBox(
  value = paste0(round(min(Negativos[,3]),2),"%"),
  subtitle = "Mayor baja en un dia",
  color = "red")

PAD_box <-  valueBox(
  value = round(max(Positivos[,2]),2),
  subtitle = "Precio mas alto",
  color = "green")

PBD_box <-  valueBox(
  value = round(min(Negativos[,2]),2),
  subtitle = "Precio mas bajo",
  color = "red")




### PORTAFOLIO OPTIMO

getSymbols("DTP30F40",src="FRED",periodicity="daily")

TES <- as.data.frame(DTP30F40)
setDT(TES,keep.rownames = T)
TES$rn <- as.Date(TES$rn, format= "%Y-%m-%d") 
TES <- subset(TES, rn > "2018-12-31")
TES$rn <- as.character(TES$rn) 



#source("./global.R",encoding = "utf-8")

## ICONOS
### https://getbootstrap.com/docs/3.4/components/#glyphicons
### https://fontawesome.com/v5.15/icons?d=gallery&p=3&m=free

ui <- dashboardPage(
  dashboardHeader(title = span("Dashboard Financiero",
                               style = "font-size: 16px"), 
                  titleWidth = 200 ),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "Menu",
      style = "position: relative; overflow: visible;",
      menuItem("Indice S&P 500", tabName = "Resultados", icon = icon("stats", lib = "glyphicon")),
      
      menuItem("Acciones", tabName = 'Stocks', icon = icon("chart-line", lib = "font-awesome")),
      menuItem("Portafolio",tabName = 'P_Optimo', icon = icon("chart-pie", lib = "font-awesome"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "Resultados",
              h1("Dashboard rendimiento SP500"),
              br(), br(),
              fluidRow(
                valueBoxOutput("Caja_PP"),
                valueBoxOutput("Caja_NP"),
                valueBoxOutput("Caja_DP")),
              fluidRow(
                valueBoxOutput("Caja_PN"),
                valueBoxOutput("Caja_NN"),
                valueBoxOutput("Caja_DN")),
              fluidRow(
                valueBoxOutput("Caja_PD", width =3),
                valueBoxOutput("Caja_ND", width =3),
                valueBoxOutput("Caja_PAD", width =3),
                valueBoxOutput("Caja_PBD", width =3)),
              br(),
              br(),
              h2("GRAFICO INTERACTIVO COMPORTAMIENTO S&P 500"),
              awesomeCheckbox(
                inputId = "RSI",
                label = "Desea agregar el indice de fuerza relativa?", 
                value = FALSE),
              awesomeCheckbox(
                inputId = "MA",
                label = "Desea agregar las medias moviles de 20 y 200?", 
                value = FALSE),
              highchartOutput("Graf_SP500", height = 800),
              br(),
              h2("Se Acabo :v")),
      
      
      tabItem(tabName = "Stocks",
              h1("Seguimiento de acciones"),
              br(),
              
              fluidRow(
                box(width=3,selectizeInput("S_Acciones", 
                                           "Seleccione las acciones que quiere analizar.",
                                           choices = L_Acciones,
                                           selected =NULL, width = "200px", 
                                           multiple = F),
                    br(),
                    dateRangeInput("F_Acciones", "Seleccione el rango de fechas",
                                   start = NULL,end = NULL,
                                   min = "2017-01-01", format = "yyyy-mm-dd",
                                   width = "200px"),
                    actionButton("go", "Hacer grafico")),
                
                tabBox(width = 9,title = "Analisis Accion",
                       id = "tabset1",
                       tabPanel("Grafico Accion", plotOutput("Grafico")),
                       tabPanel("Informacion ultimo dia", verbatimTextOutput("ult_dia")))
                
              ),
              
              br(),br(),
              
              fluidRow(
                box(width=3,selectizeInput("S_Acciones1", 
                                           "Seleccione las acciones que quiere analizar.",
                                           choices = L_Acciones,
                                           selected =NULL, width = "200px", 
                                           multiple = F),
                    br(),
                    dateRangeInput("F_Acciones1", "Seleccione el rango de fechas",
                                   start = NULL,end = NULL,
                                   min = "2017-01-01", format = "yyyy-mm-dd",
                                   width = "200px"),
                    actionButton("go1", "Hacer grafico")),
                
                
                tabBox(width = 8,title = "Analisis Accion",
                       id = "tabset2",
                       tabPanel("Grafico Accion", plotOutput("Grafico1")),
                       tabPanel("Informacion ultimo dia", verbatimTextOutput("ult_dia1")))
              ),
              
              br(),br(),
              
              fluidRow(
                box(width=3,selectizeInput("S_Acciones2", 
                                           "Seleccione las acciones que quiere analizar.",
                                           choices = L_Acciones,
                                           selected =NULL, width = "200px", 
                                           multiple = F),
                    br(),
                    dateRangeInput("F_Acciones2", "Seleccione el rango de fechas",
                                   start = NULL,end = NULL,
                                   min = "2017-01-01", format = "yyyy-mm-dd",
                                   width = "200px"),
                    actionButton("go2", "Hacer grafico")),
                
                
                tabBox(width = 8,title = "Analisis Accion",
                       id = "tabset1",
                       tabPanel("Grafico Accion", plotOutput("Grafico2")),
                       tabPanel("Informacion ultimo dia", verbatimTextOutput("ult_dia2")))
              )),
      
      tabItem(tabName = "P_Optimo",
              h1("CREACION DE PORTAFOLIO OPTIMO"),
              br(),
              fluidRow(column(2,selectInput("PO_A1",
                                            "Primera accion",
                                            choices = L_Acciones,
                                            selected = "NFLX",width = "200px",
                                            multiple = F)),
                       column(2,selectInput("PO_A2",
                                            "Segunda accion",
                                            choices = L_Acciones,
                                            selected = "SQ",width = "200px",
                                            multiple = F)),
                       column(2,selectInput("PO_A3",
                                            "Tercera accion",
                                            choices = L_Acciones,
                                            selected = "KO",width = "200px",
                                            multiple = F)),
                       column(2,selectInput("PO_A4",
                                            "Cuarta accion",
                                            choices = L_Acciones,
                                            selected = "TWTR",width = "200px",
                                            multiple = F)),
                       column(2,selectInput("PO_A5",
                                            "Quinta accion",
                                            choices = L_Acciones,
                                            selected = "MSFT",width = "200px",
                                            multiple = F)),
                       column(2,selectInput("PO_A6",
                                            "Sexta accion",
                                            choices = L_Acciones,
                                            selected = "FB",width = "200px",
                                            multiple = F))),
              br(),
              dataTableOutput("Tablaa"),
              br(),
              plotOutput("Linea_Frontera"),
              br(),
              verbatimTextOutput("eficiente"))
      
      
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## PRIMERA LINEA DE CAJAS
  
  output$Caja_PP <- renderValueBox({
    PP_box
  })
  
  output$Caja_NP <- renderValueBox({
    NP_box
  })
  
  output$Caja_DP <- renderValueBox({
    DP_box
  })
  
  ## SEGUNDA LINEA DE CAJAS
  
  output$Caja_PN <- renderValueBox({
    PN_box
  })
  
  output$Caja_NN <- renderValueBox({
    NN_box
  })
  
  output$Caja_DN <- renderValueBox({
    DN_box
  })
  
  
  ## TERCERA LINEA DE CAJAS
  
  output$Caja_PD <- renderValueBox({
    PD_box
  })
  
  output$Caja_ND <- renderValueBox({
    ND_box
  })
  
  output$Caja_PAD <- renderValueBox({
    PAD_box
  })
  
  output$Caja_PBD <- renderValueBox({
    PBD_box
  })
  
  ## GRAFICO INTERACTIVO SP500
  
  output$Graf_SP500 <- renderHighchart({
    
    if(input$RSI == TRUE && input$MA == FALSE){
      
      highchart(type = "stock") %>% 
        hc_yAxis_multiples(create_yaxis(2, height = c(2,1), turnopposite = T)) %>% 
        hc_add_series(Adj_SP, yAxis = 0, name = "Adj_SP") %>% 
        hc_add_series(Adj_SP.RSI.14, yAxis = 1, name = "Osciallator",
                      color = hex_to_rgba("green", 0.7)) %>%
        hc_add_series(Adj_SP.RSI.SellLevel, color = hex_to_rgba("red", 0.7),
                      yAxis = 1, name = "Zona de venta") %>% 
        hc_add_series(Adj_SP.RSI.BuyLevel, color = hex_to_rgba("blue", 0.7),
                      yAxis = 1, name = "Zona de compra")
      
    }else if (input$RSI == TRUE && input$MA == TRUE){
      highchart(type = "stock") %>% 
        hc_yAxis_multiples(create_yaxis(2, height = c(2,1), turnopposite = T)) %>% 
        hc_add_series(Adj_SP, yAxis = 0, name = "Adj_SP") %>% 
        hc_add_series(Adj_SP.SMA.20, yAxis = 0, name = "Media de 20") %>% 
        hc_add_series(Adj_SP.SMA.200, yAxis = 0, name = "Media de 200") %>% 
        hc_add_series(Adj_SP.RSI.14, yAxis = 1, name = "Osciallator",
                      color = hex_to_rgba("green", 0.7)) %>%
        hc_add_series(Adj_SP.RSI.SellLevel, color = hex_to_rgba("red", 0.7),
                      yAxis = 1, name = "Zona de venta") %>% 
        hc_add_series(Adj_SP.RSI.BuyLevel, color = hex_to_rgba("blue", 0.7),
                      yAxis = 1, name = "Zona de compra")
    }else if(input$RSI == FALSE && input$MA == FALSE){
      highchart(type = "stock") %>% 
        hc_add_series(Adj_SP, yAxis = 0, name = "Adj_SP") 
    }else if(input$RSI == FALSE && input$MA == TRUE){
      highchart(type = "stock") %>% 
        hc_add_series(Adj_SP, yAxis = 0, name = "Adj_SP") %>% 
        hc_add_series(Adj_SP.SMA.20, yAxis = 0, name = "Media de 20") %>% 
        hc_add_series(Adj_SP.SMA.200, yAxis = 0, name = "Media de 200")
    } 
      #        hc_title(text = "GRAFICO INTERACTIVO COMPORTAMIENTO S&P 500",
      #                 align = "center", style = list(color = "black", fontWeight = "bold", fontSize = "22px"))
      
      
    
  })
  
  
  
  ## ACTUALIZACION GRAFICOS SEGUNDO MENU
  
  
  ### PRIMER GRAFICO
  
  Descarga_A <- reactive({
    getSymbols(input$S_Acciones, src = "yahoo",
               from = input$F_Acciones[1],
               to = input$F_Acciones[2],
               auto.assign = FALSE)
  })
  
  
  Boton <- eventReactive(input$go, {
    Descarga_A()
  })
  
  output$Grafico <- renderPlot({
    chartSeries(Boton())
  })
  
  output$ult_dia <- renderPrint({
    tail(Boton(),1)
  })
  
  
  ### SEGUNDO GRAFICO
  
  Descarga_A1 <- reactive({
    getSymbols(input$S_Acciones1, src = "yahoo",
               from = input$F_Acciones1[1],
               to = input$F_Acciones1[2],
               auto.assign = FALSE)
  })
  
  
  Boton1 <- eventReactive(input$go1, {
    Descarga_A1()
  })
  
  output$Grafico1 <- renderPlot({
    chartSeries(Boton1(),theme="white",TA="addVo();addBBands()")
  })
  
  output$ult_dia1 <- renderPrint({
    tail(Boton1(),1)
  })
  
  ### TERCER GRAFICO
  
  Descarga_A2 <- reactive({
    getSymbols(input$S_Acciones2, src = "yahoo",
               from = input$F_Acciones2[1],
               to = input$F_Acciones2[2],
               auto.assign = FALSE)
  })
  
  
  Boton2 <- eventReactive(input$go2, {
    Descarga_A2()
  })
  
  output$Grafico2 <- renderPlot({
    chartSeries(Boton2(),TA="addVo();addBBands()")
  })
  
  output$ult_dia2 <- renderPrint({
    tail(Boton2(),1)
  })
  
  
  ## CREACION DEL PORTAFOLIO OPTIMO
  
  ### PRIMERA
  
  Funcion_P1 <- reactive({
    Accion1 <- Arreglos(getSymbols(input$PO_A1, src = "yahoo",
                                   from = "2019-01-01", periodicity="daily",
                                   auto.assign = FALSE))
    
  })
  
  Funcion_P2 <- reactive({
    Accion2 <- Arreglos(getSymbols(input$PO_A2, src = "yahoo",
                                   from = "2019-01-01", periodicity="daily",
                                   auto.assign = FALSE))
    
    
  })
  
  Funcion_P3 <- reactive({
    Accion3 <- Arreglos(getSymbols(input$PO_A3, src = "yahoo",
                                   from = "2019-01-01", periodicity="daily",
                                   auto.assign = FALSE))
    
  })
  
  
  Funcion_P4 <- reactive({
    Accion4 <- Arreglos(getSymbols(input$PO_A4, src = "yahoo",
                                   from = "2019-01-01", periodicity="daily",
                                   auto.assign = FALSE))
    
  })
  
  
  Funcion_P5 <- reactive({
    Accion5 <- Arreglos(getSymbols(input$PO_A5, src = "yahoo",
                                   from = "2019-01-01", periodicity="daily",
                                   auto.assign = FALSE))
    
  })
  
  
  Funcion_P6 <- reactive({
    Accion6 <- Arreglos(getSymbols(input$PO_A6, src = "yahoo",
                                   from = "2019-01-01", periodicity="daily",
                                   auto.assign = FALSE))
  })
  
  output$Tablaa <- renderDataTable({
    
    #https://stackoverflow.com/questions/14096814/merging-a-lot-of-data-frames    
    CAPM<- Reduce(function(x, y) merge(x, y, all=TRUE, by = "rn"), 
                  list(Funcion_P1(),Funcion_P2(),Funcion_P3(),Funcion_P4(),Funcion_P5(),
                       Funcion_P6(), TES,GSPC))
    
    colnames(CAPM)=c("FECHAS", "ACCION1", "ACCION2","ACCION3", "ACCION4",
                     "ACCION5", "ACCION6", "BONO","INDICE")
    CAPM <- na.omit(CAPM)
    
    attach(CAPM)
    ###Definir como series de tiempo
    ACC1<-ts(ACCION1, frequency = 252, start=c(2019,01,02))
    ACC2<-ts(ACCION2, frequency = 252, start=c(2019,01,02))
    ACC3<-ts(ACCION3, frequency = 252, start=c(2019,01,02))
    ACC4<-ts(ACCION4, frequency = 252, start=c(2019,01,02))
    ACC5<-ts(ACCION5, frequency = 252, start=c(2019,01,02))
    ACC6<-ts(ACCION6, frequency = 252, start=c(2019,01,02))
    I_SP<-ts(INDICE, frequency = 252, start=c(2019,01,02))
    
    ##Retornos
    R_acc1<- diff(log(ACC1))
    R_acc2<- diff(log(ACC2))
    R_acc3<- diff(log(ACC3))
    R_acc4<- diff(log(ACC4))
    R_acc5<- diff(log(ACC5))
    R_acc6<- diff(log(ACC6))
    Rm<-diff(log(I_SP))
    Rf<-(1+BONO/100)^(1/252)-1
    Rf<-Rf[!is.na(Rf)]
    
    retornos<-cbind(R_acc1,R_acc2,R_acc3,R_acc4,R_acc5,R_acc6)
    
    
    #Rendimiento y volatilidad
    
    rendimiento<-c("Rendimiento",round(mean(R_acc1)*100,3), round(mean(R_acc2)*100,3),
                   round(mean(R_acc3)*100,3),round(mean(R_acc4)*100,3), 
                   round(mean(R_acc5)*100,3),round(mean(R_acc6)*100,3),
                   round(mean(Rm)*100,3), round(mean(Rf)*100,3))
    volatilidad<-c("Volatilidad",round(sd(R_acc1)*100,3), round(sd(R_acc2)*100,3),
                   round(sd(R_acc3)*100,3),round(sd(R_acc4)*100,3), 
                   round(sd(R_acc5)*100,3),round(sd(R_acc6)*100,3),
                   round(sd(Rm)*100,3), round(sd(Rf)*100,3))
    
    tabla1<-data.frame(rbind(rendimiento, volatilidad))
    colnames(tabla1)<-c("  ",input$PO_A1,input$PO_A2,input$PO_A3,input$PO_A4,
                        input$PO_A5,input$PO_A6, "SP 500", "TES")
    tabla1
    
    
  })
  
  output$Linea_Frontera <- renderPlot({
    
    CAPM<- Reduce(function(x, y) merge(x, y, all=TRUE, by = "rn"), 
                  list(Funcion_P1(),Funcion_P2(),Funcion_P3(),Funcion_P4(),Funcion_P5(),
                       Funcion_P6(), TES,GSPC))
    
    colnames(CAPM)=c("FECHAS", "ACCION1", "ACCION2","ACCION3", "ACCION4",
                     "ACCION5", "ACCION6", "BONO","INDICE")
    CAPM <- na.omit(CAPM)
    
    attach(CAPM)
    ###Definir como series de tiempo
    ACC1<-ts(ACCION1, frequency = 252, start=c(2019,01,02))
    ACC2<-ts(ACCION2, frequency = 252, start=c(2019,01,02))
    ACC3<-ts(ACCION3, frequency = 252, start=c(2019,01,02))
    ACC4<-ts(ACCION4, frequency = 252, start=c(2019,01,02))
    ACC5<-ts(ACCION5, frequency = 252, start=c(2019,01,02))
    ACC6<-ts(ACCION6, frequency = 252, start=c(2019,01,02))
    I_SP<-ts(INDICE, frequency = 252, start=c(2019,01,02))
    
    ##Retornos
    R_acc1<- diff(log(ACC1))
    R_acc2<- diff(log(ACC2))
    R_acc3<- diff(log(ACC3))
    R_acc4<- diff(log(ACC4))
    R_acc5<- diff(log(ACC5))
    R_acc6<- diff(log(ACC6))
    Rm<-diff(log(I_SP))
    Rf<-(1+BONO/100)^(1/252)-1
    Rf<-Rf[!is.na(Rf)]
    
    retornos<-cbind(R_acc1,R_acc2,R_acc3,R_acc4,R_acc5,R_acc6)
    
    espcartera<-portfolioSpec()
    
    setRiskFreeRate(espcartera)<- 0.0003 ##Rendimiento Activo Libre de Riesgo
    setNFrontierPoints(espcartera) <- 20
    
    Frontera <- portfolioFrontier(as.timeSeries(retornos),spec=espcartera )
    
    ##Graficar frontera eficiente
    frontierPlot(Frontera)
    grid()
    tangencyPoints(Frontera, pch = 19, col = "red", cex=2)
    tangencyLines(Frontera, col="darkgreen", pch=19, cex=2)
    minvariancePoints(Frontera, col="blue", pch=19, cex=2)
    monteCarloPoints(Frontera, col="green", cex=0.001)
    
    
  })
  
  output$eficiente <- renderPrint({
    CAPM<- Reduce(function(x, y) merge(x, y, all=TRUE, by = "rn"), 
                  list(Funcion_P1(),Funcion_P2(),Funcion_P3(),Funcion_P4(),Funcion_P5(),
                       Funcion_P6(), TES,GSPC))
    
    colnames(CAPM)=c("FECHAS", "ACCION1", "ACCION2","ACCION3", "ACCION4",
                     "ACCION5", "ACCION6", "BONO","INDICE")
    CAPM <- na.omit(CAPM)
    
    attach(CAPM)
    ###Definir como series de tiempo
    ACC1<-ts(ACCION1, frequency = 252, start=c(2019,01,02))
    ACC2<-ts(ACCION2, frequency = 252, start=c(2019,01,02))
    ACC3<-ts(ACCION3, frequency = 252, start=c(2019,01,02))
    ACC4<-ts(ACCION4, frequency = 252, start=c(2019,01,02))
    ACC5<-ts(ACCION5, frequency = 252, start=c(2019,01,02))
    ACC6<-ts(ACCION6, frequency = 252, start=c(2019,01,02))
    I_SP<-ts(INDICE, frequency = 252, start=c(2019,01,02))
    
    ##Retornos
    R_acc1<- diff(log(ACC1))
    R_acc2<- diff(log(ACC2))
    R_acc3<- diff(log(ACC3))
    R_acc4<- diff(log(ACC4))
    R_acc5<- diff(log(ACC5))
    R_acc6<- diff(log(ACC6))
    Rm<-diff(log(I_SP))
    Rf<-(1+BONO/100)^(1/252)-1
    Rf<-Rf[!is.na(Rf)]
    
    retornos<-cbind(R_acc1,R_acc2,R_acc3,R_acc4,R_acc5,R_acc6)
    
    espcartera<-portfolioSpec()
    
    setRiskFreeRate(espcartera)<- 0.0000227 ##Rendimiento Activo Libre de Riesgo
    setNFrontierPoints(espcartera) <- 20
    
    ##Portafolios
    efPortfolio <- efficientPortfolio(as.timeSeries(retornos),espcartera)
    efPortfolio  
  })
  
  
  
  
}


shinyApp(ui = ui, server = server)

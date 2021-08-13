#    http://shiny.rstudio.com/
#

pacman::p_load(shiny,readr,rsconnect,dplyr,highcharter,shinythemes,shinydashboard,quantmod,
               data.table,shinyjs)

getSymbols(c("AAPL","TSLA","PYPL","NVDA",
             "JPM","SQ","PLTR","NIO","^GSPC"),src="yahoo",periodicity="daily",
           auto.assign=T, from = "2015-01-01")

L_Acciones = list(`NASDAQ` = list("AAPL", "TSLA","NVDA","PYPL"),
                  `NYSE` = list("PLTR", "NIO", "SQ","JPM"))


Arreglos <- function(Accion){
  Accion = Accion[,6]
  Accion = as.data.frame(Accion)
  setDT(Accion,keep.rownames = T)
}

GSPC <- Arreglos(GSPC)

SP500 <- GSPC%>%
  mutate(variacion = (GSPC.Adjusted/lag(GSPC.Adjusted) - 1) * 100)
head(SP500)

Negativos <- SP500%>%
  filter(variacion <= 0.00000)

Positivos <- SP500%>%
  filter(variacion > 0.00000)

P_box <-  valueBox(
  value = paste0(round(nrow(Positivos)/nrow(SP500)*100,0),"%"),
  subtitle = "Porcentaje de dias positivos",
  icon = icon("arrow-up"),
  width = 2,
  color = "green")

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
      
      menuItem("Acciones", tabName = 'Stocks', icon = icon("chart-line", lib = "font-awesome"),
               selectizeInput("S_Acciones",
                              "Seleccione las acciones que quiere analizar.",
                              choices = L_Acciones,
                              selected = NULL, width = "200px", multiple =T, 
                              options = list(maxItems = 3)),
               dateRangeInput("F_Acciones", "Seleccione el rango de fechas",start = NULL,end = NULL,
                              min = "2015-01-01", format = "yyyy-mm-dd"),br(),
               actionButton('B_Acciones',
                            paste0('Construir graficas'),
                            icon = icon('wrench'))),
      menuItem("Portafolio", tabName = 'P_Optimo', icon = icon("chart-pie", lib = "font-awesome"),
               fileInput(inputId='archivo',
                         label="Importa tu archivo:"),
               badgeColor = "red")
    )
  ),
  dashboardBody(
    tabItem(tabName = "Resultados",
            h1("Dashboard rendimiento SP500")),
    fluidRow(
      valueBoxOutput("P_box")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$P_box <- renderValueBox({
    P_box
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)

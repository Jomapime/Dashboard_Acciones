#    http://shiny.rstudio.com/
#

library(pacman)
pacman::p_load(shiny,readr,rsconnect,dplyr,highcharter,shinythemes,shinydashboard)

#source("./global.R",encoding = "utf-8")



# Define UI for application that draws a histogram


ui <- dashboardPage(
  dashboardHeader(title="Dashboard Example"),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "Menu",
      style = "position: relative; overflow: visible;",
      menuItem("Encuesta", tabName = "Resultados", icon = icon("stats", lib = "glyphicon"),
               badgeColor = "green"),
      menuItem("Market Intelligence", tabName = 'country_intel', icon = icon("align-top", lib = "glyphicon"))
    )
  ),
  dashboardBody()
)

# Define server logic required to draw a histogram
server <- function(input, output) {}


# Run the application 
shinyApp(ui = ui, server = server)

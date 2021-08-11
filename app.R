#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(pacman)

pacman::p_load(shiny,readr,rsconnect,dplyr,highcharter,shinythemes,shinydashboard)


sidebar <- 
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "Menu",
      style = "position: relative; overflow: visible;",
      menuItem("Encuesta", tabName = "Resultados", icon = )
    )
  )



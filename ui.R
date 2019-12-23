library(shinydashboard)
library(shiny)
library(ggplot2)
library(data.table)
library(googlesheets)
library(lubridate)
library(readr)
library(RColorBrewer)
library(shinycssloaders)


#### HTML - CSS part for formatting ####

multicollabs <- list(tags$head(tags$style(
  HTML(
    "
    .shiny-options-group {
    height: 190px;
    -webkit-column-count: 2; /* Chrome, Safari, Opera */
    -moz-column-count: 2;    /* Firefox */
    column-count: 2;
    -webkit-column-fill: balance;
    -moz-column-fill: balance;
    column-fill: balance;
    margin-top: 0px;
    }
    
    .control-label {
    padding-bottom: 0px;
    }
    
    div.radio {
    margin-top: 0px;
    margin-bottom: 0px;
    padding-bottom: 3px;
    }
    "
  )
  )))

#### Google Calendar imbedding code ####

naptar_beagyazas = '<iframe
src="https://calendar.google.com/calendar/embed?
showPrint=0&amp;
showCalendars=0&amp;
mode=WEEK&amp;
height=600&amp;wkst=2&amp;
bgcolor=%23ffffff&amp;
src=6tmcjn18vpr1n67ugl27hcv3ro%40group.calendar.google.com&amp;
color=%23875509&amp;
ctz=Europe%2FBudapest"
style="border-width:0"
width="98%"
height="560px"
frameborder="0"
scrolling="no"></iframe>'
  


#### ####
#### DasboardPage ####
dashboardPage(
  
  #### Menu structure and style ####
  skin = 'blue',
  header = dashboardHeader(title = "Kézilabda Naptár"),
  sidebar = dashboardSidebar(sidebarMenu(
    
    actionButton(
      "btn_runUpdate",
      "  Naptár Frissítés",
      icon = icon("futbol", class = NULL, lib = "font-awesome"),
      style = "margin: 10%; align: center"
    ),
    
    dateInput("inp_dt",
              value = today(),
              min = today() - 5,
              max = today() + 10,
              label = "Válassz dátumot hogy melyik naptól fusson a frissítés",
              format = "yyyy.mm.dd.",
              weekstart = 1,
              language = "hu",
              startview = "month",
              width = '100%'),
    
    radioButtons("inp_chan",
                 label = "Válassz csatornát",
                  choices = c("SPORT1", "M4_SPORT", "SPORT2", "DUNA", 
                              "DUNAWORLD", "SPILER_TV", "SPILER2_TV")
                 ),
    
    box(id = 'friss',
      withSpinner(
      htmlOutput("RUNTIME_TEXT"),
      
      size = 0.4,
      proxy.height = 0.1,
      color = "white"
    ),
    width = 12,
    height = '60%'),
    
    box(id = "futasi_uzenet",
      htmlOutput("URL_FOR_CHECK"),
    width = 12,
    height = '60%'
    )
  )),
  
  #### Page structure ####
  body = dashboardBody(#
    includeCSS("styles.css"),
    
    #### Áttekintő oldal ####
    fluidRow(
      box(
        h3("A következő meccs: "),
        withSpinner(
          htmlOutput("nextMatch"),
          size = 0.4,
          proxy.height = 0.1
        ),
        width = 12,
        height = '10%'
      )
    ),
    fluidRow(box(
      HTML(naptar_beagyazas),
      width = 12, 
      height = 'auto'
    )))
)

library(shiny)
library(shinymanager)
library(shinyWidgets)
library(shinyBS)
# library(shinyhelper)
library(shinybusy)
library(DT)
library(data.table)
library(dplyr)
library(shinydashboard)
library(openxlsx)
library(stringr)
library(shinyalert)
library(rintrojs)
library(bsplus)
library(reactable)
library(reactablefmtr)
library(tippy)


source('global.R')

# Define server logic required to draw a histogram
credentials <- data.frame(
  user = c("nicholas"), # mandatory
  password = c("komarov"), # mandatory
  stringsAsFactors = FALSE
)



server <- shinyServer(function(session, input, output) {
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  source('home.R',local=T)$value # Home
  source('ra.R',local=T)$value # Rheumatoid Arthritis
  source('ms.R',local=T)$value # Relapsing Remitting Multiple Sclerosis
  source('HP.R' ,local=T)$value # HemoPhilia A: Prophylaxis
  source('HO.R', local=T)$value #Hemophilia A: On-demand
  source('SA.R', local=T)$value #Severe Asthma
  source('PP.R', local=T)$value #Plaque Psoriasis
  

  
  
  
})




library(shiny)
library(shinymanager)
# Define UI for application that draws a histogram
ui <- secure_app(
  shinyUI(dashboardPage(
    
    
    
    
    dashboardHeader(title = "Specialty MEDS",
                    tags$li(class = "dropdown", 
                            div(class = "dropdown2",
                                actionButton("home", label = "Home", icon = icon("home")))
                            ),
                    tags$li(class = "dropdown", 
                            dropdownButton(
                              tags$li(class = "subdropdown", actionButton("ra2", label = "Rheumatoid Arthritis")),
                              tags$li(class = "subdropdown", actionButton("ms2", label = "Multiple Sclerosis")),
                              tags$li(class = "subdropdown", actionButton("hp2", label = "Hemophilia A: Phrophylaxis")),
                              tags$li(class = "subdropdown", actionButton("ho2", label = "Hemophilia A: On-demand")),
                              tags$li(class = "subdropdown", actionButton("sa2", label = "Severe Asthma")),
                              tags$li(class = "subdropdown", actionButton("pp2", label = "Plauqe Psoriasis")),
                              circle = FALSE,
                              status = "default",
                              size = "lg",
                              label = " Navigate",
                              tooltip = FALSE,
                              right = TRUE,
                              up = FALSE,
                              width = NULL,
                              margin = "10px",
                              inline = TRUE,
                              inputId = "conditionselection"
                            )
                    ),
                    tags$li(class = "dropdown", 
                              dropdownButton(
                              tags$li(class = "subdropdown", actionButton("faq", label = "Frequently Asked Questions")),
                              tags$li(class = "subdropdown", actionButton("userguide", label = "User Help Guide")),
                              tags$li(class = "subdropdown", actionButton("userspecs", label = "Algorithm Specifications")),
                              circle = FALSE,
                              status = "default",
                              size = "lg",
                              label = "More Info",
                              tooltip = FALSE,
                              right = TRUE,
                              up = FALSE,
                              width = NULL,
                              margin = "10px",
                              inline = TRUE,
                              inputId = "userhelp"
                            )
                    ),
                    titleWidth = 350),
         
              
    
    
    
    dashboardSidebar(
      bcbsLogo(),
      hr(),
      sidebarMenu(
        uiOutput("customFilters_RA"),
        uiOutput("customFilters_MS"),
        uiOutput("customFilters_HP"),
        uiOutput("customFilters_HO"),
        uiOutput("customFilters_SA"),
        uiOutput("customFilters_PP"),
        hr(),
        div(column(12, 
                   column(6, bcbsButton("reset", label = "Reset Values", icon = icon("refresh"))),
                   column(6, bcbsButton("update", label = "Run Analysis", icon = icon("refresh")))
        ), class = "button-row")
        
      ),
      width = 350,
      collapsed = TRUE
    ),  
    
    ##Shiny dashboard page UI. Opening page. Empty. Populates following observeEvent of dossierOpen####
    dashboardBody(
      tags$style(type = 'text/css', '.modal-dialog { width: fit-content !important; }'),
      includeCSS(paste0(getwd(), "/www/styles.css")),
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = "shinyjs.init = function() { var x = document.getElementsByClassName('menu-introjs-has-children')[0]; $('.'+x).attr('id','menu');}", functions = c()),
      shinybusy::add_busy_spinner(spin = "fading-circle",
                                  color = "#0079C2",
                                  timeout = 100,
                                  # position = "full-page",
                                  position = "bottom-right",
                                  onstart = FALSE,
                                  margins = c(400, 600),
                                  height = "150px",
                                  width = "150px"
      ),
      
      bsModal(id = "faq_window",
              title = "Frequently Asked Questions",
              trigger = "faq",
              uiOutput("FAQ")
      ),
      
      # uiOutput("homePage"),
      # Buttons
      div(class = "nav-buttons", id = "homepage",
          fluidRow(
            h1("Welcome to the BCBS Specialty MEDS App!"),
            img(src='logo.svg', align = "right", width = "200px")
          ),
          fluidRow(
            h2("Please select a condition below for data analysis and report generation.")
          ),
          br(),
          br(),
          fluidRow(
            column(12,
                   column(4, actionButton("ra","Rheumatoid Arthritis")),
                   column(4, actionButton("ms","Multiple Sclerosis")),
                   column(4, actionButton("hp","Hemophilia A: Prophylaxis"))
            )
          ),
          br(),
          fluidRow(
            column(12,
                   column(4, actionButton("ho", "Hemophilia A: On-demand")),
                   column(4, actionButton("sa", "Severe Asthma")),
                   column(4, actionButton("pp","Plaque Psoriasis"))
            )
          ),
          fluidRow(
            column(12,
                   div(class = "disclaimer", 
                       h5("*Disclaimer: MEDS Model Analyses and Special Reports are scientific opinions, provided solely for informational purposes. 
                 MEDS Model Analyses and Special Reports should not be construed to suggest that Blue Cross Blue Shield Association or the Office of Clinical Affairs recommends, advocates, requires, encourages, or discourages any particular treatment, procedure, or service; 
                    any particular course of treatment, procedure, or service; or the payment or nonpayment of the technology or technologies evaluated.")))
          )
      ),
      
      hr(),
      
      # Results Tables
      uiOutput('bcbsaResults_RA'),
      uiOutput('bcbsaResults_MS'),
      uiOutput('bcbsaResults_HP'),
      uiOutput('bcbsaResults_HO'),
      uiOutput('bcbsaResults_SA'),
      uiOutput('bcbsaResults_PP')
      
      
    )
  ))
)


shinyApp(ui,server)
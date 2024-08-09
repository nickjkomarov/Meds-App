shinyUI(dashboardPage(
  
  
  dashboardHeader(title = "Specialty MEDS",
                  tags$li(class = "dropdown", actionBttn("home", label = "Home", style = "stretch", color = "primary", icon = icon("home"))),
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
      # uiOutput("customFilters_PP"),
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
        )
        
    ),
    
    hr(),
    
    # Results Tables
    uiOutput('bcbsaResults_RA'),
    uiOutput('bcbsaResults_MS') ,
    uiOutput('bcbsaResults_HP'),
    uiOutput('bcbsaResults_HO'),
    uiOutput('bcbsaResults_SA'),
    # uiOutput('bcbsaResults_PP')
    
    
  )
))


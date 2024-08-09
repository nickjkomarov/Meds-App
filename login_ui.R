shinyUI(dashboardPage(
  
  
  dashboardHeader(title = "Specialty MEDS",
                  tags$li(class = "dropdown", actionBttn("home", label = "Home", style = "stretch", color = "primary", icon = icon("home"))),
                  titleWidth = 350),
  
  dashboardSidebar(
    # bcbsLogo(),
    # hr(),
    # sidebarMenu(
    #   uiOutput("customFilters_RA"),
    #   uiOutput("customFilters_MS"),
    #   uiOutput("customFilters_HP"),
    #   uiOutput("customFilters_HO"),
    #   # uiOutput("customFilters_SA"),
    #   # uiOutput("customFilters_PP"),
    #   hr(),
    #   div(column(12, 
    #              column(6, bcbsButton("reset", label = "Reset Values", icon = icon("refresh"))),
    #              column(6, bcbsButton("update", label = "Run Analysis", icon = icon("refresh")))
    #   ), class = "button-row")
    #   
    # ),
    # width = 350,
    # collapsed = TRUE
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
    
    textInput("username", "Username", value = ""),
    textInput("password", "Password", value = "")
   
))
)


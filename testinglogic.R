library(shiny)
library(shinymanager)
library(shinyWidgets)
library(shinyBS)
library(shinybusy)
library(DT)
library(data.table)
library(dplyr)
library(shinydashboard)
library(openxlsx)
library(stringr)
library(shinyalert)
library(bsplus)

source('global.R')



server <- shinyServer(function(session, input, output) {
  
  
  
  
  ##########################################
  
  # addPopover(session, 
  #            id = 'content01', 
  #            title = "Tutorial Step 1", 
  #            content = "Test Tutorial Content", 
  #            placement = "bottom",
  #            trigger = "step01", 
  #            options = NULL)

  # observeEvent(input$step01, {
  #   
  #   addPopover(session, 
  #              id = 'content01', 
  #              title = "Tutorial Step 1", 
  #              content = "Test Tutorial Content", 
  #              placement = "bottom",
  #              trigger = "step01", 
  #              options = NULL)
  #   
  #   
  #   #   # bsModal(id = "step01_tutorial",
  #   #   #         title = "step 1",
  #   #   #         trigger = "step01",
  #   #   #         uiOutput("Tutorial_Step1")
  #   #   # ),
  #   
  #   # output$Tutorial_Step1 <- renderUI({
  #   #   
  #   # })
  #   
  # }, ignoreInit = TRUE)
  
  ##########################################
  
  observeEvent(input$step02, {
    
    removePopover(session, id = "step01_tutorial")
    
    # output$Tutorial_Step2 <- renderUI({
    #   
    # })
    
  }, ignoreInit = TRUE)
  
  ##########################################
  
  observeEvent(input$step03, {
    
    # output$Tutorial_Step3 <- renderUI({
    #   
    # })
    
  }, ignoreInit = TRUE)
  
  ##########################################
  
  observeEvent(input$step04, {
    
    # output$Tutorial_Step4 <- renderUI({
    #   
    # })
    
  }, ignoreInit = TRUE)
  
  ##########################################
  
  defaultData <- reactive({
    data <- mtcars %>%
      filter(mpg > 20) %>%
      filter(cyl != 4)
    data
  })
  
  output$datatable01 <- renderDataTable({
    data <- defaultData() 
    datatable(data) 
  })
  
  
  output$datatable02 <- renderDataTable({
    data <- mtcars
    datatable(data) 
  })
  
})




ui <- shinyUI(dashboardPage(
    
  
    dashboardHeader(title = "Specialty MEDS"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Test Tutorial", tabName = "tutorial", icon = icon("dashboard"), badgeLabel = NULL, badgeColor = "green"),
        menuItem("Test Caorusel", tabName = "carousel", icon = icon("th"), badgeLabel = NULL, badgeColor = "green")      
        )
    ),  
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "tutorial",
                fluidRow(
                  column(3, actionButton("step01", "STEP 1")),
                  column(3, actionButton("step02", "STEP 2")),
                  column(3, actionButton("step03", "STEP 3")),
                  column(3, actionButton("step04", "STEP 4"))
                ),
                hr(),
                fluidRow(
                  column(3, 
                         # div(id = "content01", 
                         box(title = "Content 1", width = 12, selectInput("select01", "Test input", choices = NULL)) #)
                  ),
                  column(3, 
                         # div(id = "content02", 
                         box(title = "Content 2", width = 12, selectInput("select02", "Test input", choices = NULL)) #)
                  ),
                  column(3, 
                         # div(id = "content03", 
                         box(title = "Content 3", width = 12, selectInput("select03", "Test input", choices = NULL)) #)
                  ),
                  column(3, 
                         # div(id = "content01", 
                         box(title = "Content 1", width = 12, selectInput("select04", "Test input", choices = NULL)) #)
                  )
                )
                
        ),
        
        # Second tab content
        tabItem(tabName = "carousel",
                   fluidRow(
                  box(title = "Custom Table",
                      width = 12,
                      dataTableOutput('datatable01')
                  )
                ),
                fluidRow(
                  box(title = "Default Table",
                      width = 12,
                      dataTableOutput('datatable02')
                  )
                )
        )
      )
    )
  )
)


shinyApp(ui,server)
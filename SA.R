# Severe Asthma server file-------------------------------------


observeEvent(c(input$sa, input$sa2), {
  condition$selected <- "Severe Asthma"
}, ignoreInit = TRUE)


observeEvent(c(input$sa, input$sa2), {
  shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")
  shinyjs::show('customFilters_SA')
  shinyjs::show('bcbsaResults_SA')
  shinyjs::show('update')
  
  shinyjs::hide('homepage')
  
  shinyjs::hide('customFilters_RA')
  shinyjs::hide('bcbsaResults_RA')
  
  shinyjs::hide('customFilters_MS')
  shinyjs::hide('bcbsaResults_MS')
  
  shinyjs::hide('customFilters_HO')
  shinyjs::hide('bcbsaResults_HO')
  
  shinyjs::hide('customFilters_HP')
  shinyjs::hide('bcbsaResults_HP')
  
  shinyjs::hide('customFilters_PP')
  shinyjs::hide('bcbsaResults_PP')
}, ignoreInit = TRUE)

output$customFilters_SA <- renderUI({
  div(id = "customizeFilters",
      fluidRow(
        div(style = "margin-left: 15px; margin-top: -10px;",
            pickerInput("medBoxs_SA", label = "Select Medications",
                        choices = SA_Data$Arm,
                        selected = SA_Data$Arm,
                        multiple = TRUE,
                        width = 300,
                        options = list(`live-search` = TRUE,
                                       `selected-text-format` = "count > 3",
                                       `actions-box` = TRUE))
        ),
        div(id="inline",
            hr(),
            tags$h5("Benefits Outcomes"),
            sliderInput('Ex_of_As',"Acute exacerbation of asthma", 0,100,30),
            sliderInput('visit_rate',"Hospitalization/ED visit rate", 0,100,70),
            uiOutput('benefitAlert_SA'),
            hr(),
            tags$h5("Harms Outcomes"),
            sliderInput('Discont_SA',"Discontinuation",0,100,70),
            sliderInput('Adverse_Eff',"Severe Adverse Effect",0,100,30),
            uiOutput('harmAlert_SA')
        )
      )
  )
})




# Reset filters to default values
observe({
  req(input$reset)
  
  if (condition$selected == "Severe Asthma") {
    updatePickerInput(session, "medBoxs_SA", "Select Medications", SA_Data$Arm, SA_Data$Arm)
    updateSliderInput(session, 'Ex_of_As', "Acute exacerbation of asthma", 30, 0, 100)
    updateSliderInput(session, 'visit_rate', "Hospitalization/ED visit rate", 70, 0, 100)
    updateSliderInput(session,'Discont_SA',"Discontinuation",70,0,100)
    updateSliderInput(session, 'Adverse_Eff',"Severe Adverse Effect",30,0,100)
  }
})


# User input validation -- Benefits Weights

observe({
  req(input$Ex_of_As)
  
  max.value <- 100 - input$Ex_of_As
  if(max.value >= 0){
    output$benefitAlert_SA <- NULL
    updateSliderInput(session, "visit_rate", value = max.value, min = 0, max = 100, step = 1)
    shinyjs::enable('update')
  }else{
    output$benefitAlert_SA <- renderUI({div(style='width:300; color: red !important;',
                                            h6('Total weight cannot be more than 100!'))})
    shinyjs::disable('update')
  }
})

observe({
  req(input$visit_rate)
  max.value <- 100 - input$visit_rate
  if(max.value >= 0){
    output$benefitAlert_SA <- NULL
    updateSliderInput(session, "Ex_of_As", value = max.value, min = 0, max = 100, step = 1)
    shinyjs::enable('update')
  }else{
    output$benefitAlert_SA  <- renderUI({div(style='width:300; color: red !important;',
                                             h6('Total weight cannot be more than 100!'))})
    shinyjs::disable('update')
  }
})


# User input validation -- Harms Weights

observe({
  req(input$Discont_SA)
  max.value <- 100 - input$Discont_SA
  if(max.value >= 0){
    output$harmAlert_SA <- NULL
    updateSliderInput(session, "Adverse_Eff", value = max.value, min = 0, max = 100, step = 1)
    shinyjs::enable('update')
  }else{
    output$harmAlert_SA  <- renderUI({div(style='width:300; color: red !important;',
                                          h6('Total weight cannot be more than 100!'))})
    shinyjs::disable('update')
  }
})

observe({
  req(input$Adverse_Eff)
  max.value <- 100 - input$Adverse_Eff
  if(max.value >= 0){
    output$benefitAlert_SA <- NULL
    updateSliderInput(session, "Discont_SA", value = max.value, min = 0, max = 100, step = 1)
    shinyjs::enable('update')
  }else{
    output$benefitAlert_SA  <- renderUI({div(style='width:300; color: red !important;',
                                             h6('Total weight cannot be more than 100!'))})
    shinyjs::disable('update')
  }
})

output$boxTitle_SA <- renderUI({
  req(input$update || input$sa || input$sa2)
  
  if (isolate(input$Ex_of_As) == 30 &&
      isolate(input$visit_rate) == 70 &&
      isolate(input$Discont_SA) == 70 &&
      isolate(input$Adverse_Eff) == 30 &&
      
      length(isolate(input$medBoxs_SA)) == length(SA_Data$Arm)) {
    
    h3('Results from BCBSA using default weights')
    
  } else {
    
    h3('Results from BCBSA using user-defined weights')
  }
  
})




observe({
  req(input$sa_tab_box)
  
  if (input$sa_tab_box == "Preview Snapshot") {
    shinyjs::hide("more_info_MEDS_SA")
    shinyjs::show("downloadSnapshot_SA")
  } else {
    shinyjs::show("more_info_MEDS_SA")
    shinyjs::hide("downloadSnapshot_SA")
  }
})



# Update Data Tables and Snapshot

observeEvent(c(input$sa, input$update, input$sa2), {
  if(isolate(condition$selected) == "Severe Asthma") {
    
    meds_values$SA_MEDS_Sel <- isolate(input$medBoxs_SA)
    meds_values$Ex_of_As_SA <- isolate(input$Ex_of_As)
    meds_values$Visit_rate_SA <- isolate(input$visit_rate)
    meds_values$Discont_SA_SA<-  isolate(input$Discont_SA)
    meds_values$Adverse_Eff_SA<- isolate(input$Adverse_Eff)
    print(meds_values$SA_MEDS_Sel)
    print(meds_values$Ex_of_As_SA)
    print(meds_values$Visit_rate_SA)
    print(meds_values$Discont_SA_SA)
    print(meds_values$Adverse_Eff_SA)
    
    
    SA_Data_user <- openxlsx::read.xlsx('Data/Asthma/Asthma.xlsx', 'Sheet1',startRow = 1,
                                        colNames = TRUE,
                                        rowNames = FALSE,
                                        detectDates = FALSE,
                                        skipEmptyRows = TRUE,
                                        skipEmptyCols = TRUE,
                                        rows = NULL,
                                        cols = NULL,
                                        check.names = FALSE,
                                        sep.names = ".",
                                        namedRegion = NULL,
                                        na.strings = "NA",
                                        fillMergedCells = FALSE)
    
    SA_Data_user <- SA_Data_user[SA_Data_user$Arm %in% isolate(meds_values$SA_MEDS_Sel),] 
    
    # BENEFITS WEIGHTS -------------------------------------------------------------
    
    WT_EX <- isolate(meds_values$Ex_of_As_SA)/100
    WT_HED <- isolate(meds_values$Visit_rate_SA)/100 
    COL_EX <- SA_Data_user$RD.Acute.Exacerbation
    COL_HED <- SA_Data_user$RD.Acute.Exacerbation.HospED
    
    for(i in 1:nrow(SA_Data_user)){
      if(!is.na(COL_EX[i]) & !is.na(COL_HED[i])){
        SA_Data_user$cNNT[i] <- 1/(WT_EX * COL_EX[i] + WT_HED * COL_HED[i])
        
      }else if(!is.na(COL_EX[i]) & is.na(COL_HED[i])){
        SA_Data_user$cNNT[i] <- 1/(WT_EX*COL_EX[i] + WT_HED*COL_EX[i])
        
      }else if(is.na(COL_EX[i]) & !is.na(COL_HED[i])){
        SA_Data_user$cNNT[i] <- 1/(WT_EX*COL_HED[i] + WT_HED*COL_HED[i])
      } 
    }
    
    # HARMS WEIGHTS -------------------------------------------------------------
    
    WT_DISC <- isolate(meds_values$Discont_SA_SA)/100
    WT_SAE <- isolate(meds_values$Adverse_Eff_SA)/100
    COL_DISC <- SA_Data_user$RD.Discon
    COL_SAE <- SA_Data_user$RD.SAE
    
    for(i in 1:nrow(SA_Data_user)){
      if(!is.na(COL_DISC[i]) & !is.na(COL_SAE[i])){
        SA_Data_user$cNNH[i] <- 1/(WT_DISC * COL_DISC[i] + WT_SAE * COL_SAE[i])
        
      }else if(!is.na(COL_DISC[i]) & is.na(COL_SAE[i])){
        
        if(!is.na(COL_SAE[i])){
          SA_Data_user$cNNH[i]<-1/(WT_DISC*COL_DISC[i]+WT_SAE*COL_SAE[i])
        }else{
          SA_Data_user$cNNH[i]<-1/(WT_DISC*COL_DISC[i]+WT_SAE*0.5*COL_DISC[i])
        }
      }else if(is.na(COL_DISC[i]) & !is.na(COL_SAE[i])){
        SA_Data_user$cNNH[i] <- 1/(WT_DISC*0.5*COL_SAE[i]+WT_SAE*COL_SAE[i])
      } 
    }
    
    # CLINICAL EFFECTIVENESS RANKING -----------------------------------------------
    
    for(i in 1:nrow(SA_Data_user)){
      SA_Data_user$cRDBen[i]<-1/SA_Data_user$cNNT[i]
      SA_Data_user$cRDHarm[i]<-1/SA_Data_user$cNNH[i]
    }
    
    for(i in 1:nrow(SA_Data_user)){
      if(SA_Data_user$cRDBen[i]>=quantile(SA_Data_user$cRDBen,0.7)){
        SA_Data_user$NNTCat[i]<-'A+'
      }else if(SA_Data_user$cRDBen[i]>=quantile(SA_Data_user$cRDBen,0.5)){
        SA_Data_user$NNTCat[i]<-'A'
      }else if(SA_Data_user$cRDBen[i]>=quantile(SA_Data_user$cRDBen,0.3)){
        SA_Data_user$NNTCat[i]<-'B+'
      }else{
        SA_Data_user$NNTCat[i]<-'B'
      }
    }
    
    for(i in 1:nrow(SA_Data_user)){
      if(SA_Data_user$cRDHarm[i]>=quantile(SA_Data_user$cRDHarm,0.7)){
        SA_Data_user$NNHCat[i]<-'B'
      }else if(SA_Data_user$cRDHarm[i]>=quantile(SA_Data_user$cRDHarm,0.5)){
        SA_Data_user$NNHCat[i]<-'B+'
      }else if(SA_Data_user$cRDHarm[i]>=quantile(SA_Data_user$cRDHarm,0.3)){
        SA_Data_user$NNHCat[i]<-'A'
      }else{
        SA_Data_user$NNHCat[i]<-'A+'
      }
    }
    
    for(i in 1:nrow(SA_Data_user)){
      if(SA_Data_user$NNTCat[i]=='A+' & SA_Data_user$NNHCat[i]=='A+'){
        SA_Data_user$ceRank[i]<-'A'
      }else if(SA_Data_user$NNTCat[i]=='A+' & SA_Data_user$NNHCat[i]=='A'){
        SA_Data_user$ceRank[i]<-'A'
      }else if(SA_Data_user$NNTCat[i]=='A+' & SA_Data_user$NNHCat[i]=='B+'){
        SA_Data_user$ceRank[i]<-'B'
      }else if(SA_Data_user$NNTCat[i]=='A+' & SA_Data_user$NNHCat[i]=='B'){
        SA_Data_user$ceRank[i]<-'B'
      }else if(SA_Data_user$NNTCat[i]=='A' & SA_Data_user$NNHCat[i]=='A+'){
        SA_Data_user$ceRank[i]<-'B'
      }else if(SA_Data_user$NNTCat[i]=='A' & SA_Data_user$NNHCat[i]=='A'){
        SA_Data_user$ceRank[i]<-'B'
      }else if(SA_Data_user$NNTCat[i]=='A' & SA_Data_user$NNHCat[i]=='B+'){
        SA_Data_user$ceRank[i]<-'C'
      }else if(SA_Data_user$NNTCat[i]=='A' & SA_Data_user$NNHCat[i]=='B'){
        SA_Data_user$ceRank[i]<-'C'
      }else if(SA_Data_user$NNTCat[i]=='B+' & SA_Data_user$NNHCat[i]=='A+'){
        SA_Data_user$ceRank[i]<-'C'
      }else if(SA_Data_user$NNTCat[i]=='B+' & SA_Data_user$NNHCat[i]=='A'){
        SA_Data_user$ceRank[i]<-'D'
      }else if(SA_Data_user$NNTCat[i]=='B+' & SA_Data_user$NNHCat[i]=='B+'){
        SA_Data_user$ceRank[i]<-'D'
      }else if(SA_Data_user$NNTCat[i]=='B+' & SA_Data_user$NNHCat[i]=='B'){
        SA_Data_user$ceRank[i]<-'D'
      }else{
        SA_Data_user$ceRank[i]<-'F'
      }
    }
    
    SA_Data_user_save<<-SA_Data_user
    
    for(i in 1:nrow(SA_Data_user_save)){
      print(i)
      SA_Data_user_save$LL_Ben[i]<-(1-(0.5-1000/((1/SA_Data_user_save$cRDBen[i])+1)/(1000-1000/((1/SA_Data_user_save$cRDBen[i])+1))))*100
      SA_Data_user_save$UL_Ben[i]<-(1-(0.5-1000/((1/SA_Data_user_save$cRDBen[i])-1)/(1000-1000/((1/SA_Data_user_save$cRDBen[i])-1))))*100
      if(SA_Data_user_save$LL_Ben[i]>100){
        SA_Data_user_save$LL_Ben[i]<-100
      }else if(SA_Data_user_save$LL_Ben[i]<0){
        SA_Data_user_save$LL_Ben[i]<-0
      }else{
        SA_Data_user_save$LL_Ben[i]<-SA_Data_user_save$LL_Ben[i]
      }
      if(SA_Data_user_save$UL_Ben[i]>100){
        SA_Data_user_save$UL_Ben[i]<-100
      }else if(SA_Data_user_save$UL_Ben[i]<0){
        SA_Data_user_save$UL_Ben[i]<-0
      }else{
        SA_Data_user_save$UL_Ben[i]<-SA_Data_user_save$UL_Ben[i]
      }
      SA_Data_user_save$LL_Harm[i]<-100-(1-(0.5-1000/((1/SA_Data_user_save$cRDHarm[i])-1)/(1000-1000/((1/SA_Data_user_save$cRDHarm[i])-1))))*100
      SA_Data_user_save$UL_Harm[i]<-100-(1-(0.5-1000/((1/SA_Data_user_save$cRDHarm[i])+1)/(1000-1000/((1/SA_Data_user_save$cRDHarm[i])+1))))*100
      if(SA_Data_user_save$LL_Harm[i]>100){
        SA_Data_user_save$LL_Harm[i]<-100
      }else if(SA_Data_user_save$LL_Harm[i]<0){
        SA_Data_user_save$LL_Harm[i]<-0
      }else{
        SA_Data_user_save$LL_Harm[i]<-SA_Data_user_save$LL_Harm[i]
      }
      if(SA_Data_user_save$UL_Harm[i]>100){
        SA_Data_user_save$UL_Harm[i]<-100
      }else if(SA_Data_user_save$UL_Harm[i]<0){
        SA_Data_user_save$UL_Harm[i]<-0
      }else{
        SA_Data_user_save$UL_Harm[i]<-SA_Data_user_save$UL_Harm[i]
      }
      SA_Data_user_save$MeanLL[i]<-(SA_Data_user_save$LL_Harm[i]+SA_Data_user_save$LL_Ben[i])/2
      SA_Data_user_save$MeanUL[i]<-(SA_Data_user_save$UL_Harm[i]+SA_Data_user_save$UL_Ben[i])/2
      SA_Data_user_save$MeanLL[i]<-format(round(as.numeric(SA_Data_user_save$MeanLL[i]), 2), nsmall = 2)
      SA_Data_user_save$MeanUL[i]<-format(round(as.numeric(SA_Data_user_save$MeanUL[i]), 2), nsmall = 2)
    }
    
    
    SA_Data_user_save<<-SA_Data_user_save
    SA_Data_user<-SA_Data_user_save
    SA_Data_user1<-SA_Data_user[, c("Arm", "cNNT","cNNH","ceRank","MeanLL","MeanUL")]
    SA_Data_user1 <- round_df(SA_Data_user1, digits = 2)
    SA_Data_user1$AVScore<-paste(SA_Data_user1$MeanLL,'-',SA_Data_user1$MeanUL)
    assign("SA_Data_user1", SA_Data_user1, .GlobalEnv)
    
    meds_values$SA_Data_user1 <- SA_Data_user1
    
    selected.meds <- str_extract(input$medBoxs_SA, "^\\D+") %>% unique()
    
    # GENERATE MARKDOWN
    rmarkdown::render(input = "snapshot_sa.Rmd",
                      output_file = "snapshot_preview.html",
                      params = list(condition =  condition$selected,
                                    selected_meds = selected.meds,
                                    EOS_val = input$Ex_of_As,
                                    VR_val=input$visit_rate,
                                    Dis_SA_val=input$Discont_SA,
                                    Ad_Ef_val=input$Adverse_Eff,
                                    sa_data = SA_Data_user1))
    
    xml2::write_html(rvest::html_node(xml2::read_html("snapshot_preview.html"), "body"), file = "snapshot_preview_SA.html")
    
    # print('ac1')
    data <- SA_Data_user1[,c("Arm", "cNNT","cNNH","ceRank","AVScore")]
    # # data <- SA_Data_user
    # print('ac2')
    # 
    
    
    # DATA TABLE
    output$userDefRes_SA <- renderReactable({
      reactable(
        data,
        filterable = TRUE, 
        outlined = TRUE,
        showPageSizeOptions = TRUE,
        defaultColDef = colDef(
          align = "center",
          vAlign = "center"
        ),
        columns = list(
          Arm = colDef(header = "Medication"),
          cNNT = colDef(header = "Composite NNT"),
          cNNH = colDef(header = "Composite NNH"),
          ceRank = colDef(header = "Clinical Effectiveness Ranking",
          # Arm = colDef(header = with_tooltip("Medication", "Medications for the disease/condition included in the analysis.")),
          # cNNT = colDef(header = with_tooltip("Composite NNT", "Composite Numbers Needed to Treat: Weighted average of numbers needed to treat to achieve benefits outcomes.")),
          # cNNH = colDef(header = with_tooltip("Composite NNH", "Composite Numbers Needed to Harm: Weighted average of numbers needed to treat to achieve harms outcomes.")),
          # ceRank = colDef(header = with_tooltip("Clinical Effectiveness Ranking", "Clinical Effectiveness Ranking based on each drug's distance of composite NNT/NNH from the median composite NNT/NNH. Click on 'More Info' for the calculation methodology and examples."),
                          align = "center",
                          vAlign = "center",
                          style = function(value) {
                            if (value == "F") { 
                              background <- "#ff8086" 
                              color = "#47292b"
                            }
                            else if (value == "D") { 
                              background <- "#faf29d" 
                              color = "#73705a"
                            }
                            else if (value == "C") { 
                              background <- "#e6e6e6" 
                              color = "#4a4a4a"
                            }
                            else if (value == "B") { 
                              background <- "#71bceb" 
                              color = "#344957"
                            }
                            else if (value == "A") { 
                              background <- "#c8f5b3" 
                              color = "#2b3d23"
                            }
                            else { 
                              background <- "#FF4E4E" 
                              color = "#47292b"
                            }
                            list(background = background, color = color)
                          }),
        AVScore = colDef(header = "Adjusted Value Score")
        
          # AVScore = colDef(header = with_tooltip("Adjusted Value Score", "Adjusted value score is the estimated percentage of full value after discounting the difference in benefitted vs not benefitted ratio calculated using optimum NNT and observed NNT for the drug. Adjusted value score above 100 is assigned as 100 and adjusted value score below 0 is assigned 0."))
        )
      )
    })
    
    # output$userDefRes_SA<-DT::renderDataTable(datatable(data,
    #                                                     colnames = c('Medication', 'Composite NNT', 'Composite NNH', 'Clinical Effectiveness Ranking', 'Adjusted Value Score'),
    #                                                     escape = F,
    #                                                     rownames = F,
    #                                                     selection = 'single',
    #                                                     filter = 'top',
    #                                                     options = list(
    #                                                       scrollX = TRUE,
    #                                                       scrollY = '40vh',
    #                                                       scrollCollapse = TRUE,
    #                                                       autoWidth = F)) %>% 
    #                                             formatStyle(c("Arm", "cNNT","cNNH","ceRank","AVScore"),
    #                                                         backgroundColor = styleEqual(c("F",NA,""),
    #                                                                                      c("#FF4E4E","#FF4E4E", "#FF4E4E"))) %>%
    #                                             formatStyle('ceRank',
    #                                                         backgroundColor = styleEqual(
    #                                                           c("D", "C", "B", "A"),
    #                                                           c('#FDF59F', '#E3E3E3', '#0074BB', '#A3E982'))
    #                                             )
    # )
    
    output$previewSnapshot_SA <- renderUI({
      includeHTML("snapshot_preview_SA.html")
    })
    
    output$bcbsaResults_SA <- renderUI({
      
      fluidRow(
        h2("Severe Asthma"),
        h4("Use the filters on the left sidebar to select medications and efficacy outcome weight"),
        h4("Click the 'Run Analysis' button to update the data in the results table and snapshot. 
           Navigate to the 'Preview Snapshot' tab to preview and/or download the BCBS Snapshot report"),
        br(),
        
        bcbsTabBox(id = "sa_tab_box",
                   title = p(actionButton("more_info_MEDS_SA", label = NULL, icon = icon("info")),
                             shinyjs::hidden(downloadButton("downloadSnapshot_SA", "Download Snapshot"))),
                   width = 12,
                   tabPanel("Results Overview",
                            uiOutput("boxTitle_SA"),
                            reactableOutput('userDefRes_SA'),
                            tags$head(tags$style("#meds_info .modal-footer button {font-size: 20px; line-height: 25px; font-weight: bold; text-transform: uppercase;}")),
                            tags$head(tags$style("#meds_info .modal-header {padding-bottom: 0px; padding-top: 0px;}")),
                            tags$head(tags$style("#meds_info .modal-header button {display: none;}")) ,
                            
                            bsModal('meds_info_SA',
                                    title = h2('MEDS Information', style = 'color: #005876; font-weight: bold; line-height: 25px;'),
                                    size = 'large',
                                    trigger = 'more_info_MEDS_SA',
                                    uiOutput('info_details_SA')
                            )
                            
                   ),
                   tabPanel("Preview Snapshot",
                            uiOutput("previewSnapshot_SA") #,
                            # downloadButton("downloadSnapshot_SA", "Download Snapshot")
                   )
        )
      )
      
    })
    
  }
  
  
  
},ignoreInit = TRUE)




observeEvent(input$more_info_MEDS_SA,{
  output$info_details_SA<-renderUI({
    
    
    div(shiny::HTML(
      '<h4>Inclusion/Exclusion Criteria</h4>
                         <p>The following criteria for evidence inclusion for the MEDS Model Analysis was determined through the guidance of the BCBS Plans’ executive and clinical pharmacists.</p><br>
                         <h4>Inclusion Criteria</h4>
                         <br>
                         
                         <h4>Patient Population</h4>
                         <ul>
                            <li>Individuals ages 12 years or older with moderate to severe allergic asthma or eosinophilic asthma </li>
                            <li>Randomized Controlled Trials: At least 50 patients </li>
                            
                          </ul><br>
                          
                          <h4>Intervention</h4>
                         <ul>
                            <li>Omalizumab (Xolair)</li>
                            <li>Dupilumab (Dupixent)</li>
                            <li>Mepolizumab (Nucala)</li>
                            <li>Reslizumab (Cinqair)</li>
                            <li>Benralizumab (Fasenra)</li>
                           
                          </ul><br>
                          
                          <h4>Comparator</h4>
                         <ul>
                            <li>Head to Head</li>
                            <li>Standard of Care treatment with inhaled corticosteroids (ICS) and at least one additional controller agent</li>
                          </ul><br>
                          
                          <h4>Efficacy Outcomes:</h4>
                         <ul>
                            <li>Acute exacerbation of asthma (30% weight)</li>
                            <li>Rate of Hospitalization/Emergency Department Visit (70% weight)</li>
                          </ul><br>
                          
                          <h4>Safety Outcomes</h4>
                         <ul>
                            <li>Discontinuation (70% weight)</li>
                            <li>Severe Adverse Events (30% weight)</li>
                           
                          </ul><br>
                          
                          <h4>Timing</h4>
                         <ul>
                            <li>12 weeks – 1 year </li>
                          </ul><br>
                          
                          <h4>Setting</h4>
                         <ul>
                            <li>Outpatient</li>
                          </ul><br>
                          
                           <h4>Exclusion Criteria</h4>
                           <ul>
                           <li>Studies involving patients under 12</li>
                           <li>Single-arm studies</li>
                           <li>Open label long term extension studies (unless the study reported the blinded phase before crossover)</li>
                           <li>Randomized controlled trials with sample size of less than 50 patients</li>
                           </ul>'),
      div(bcbsButton('ref_list_SA','References'))
      
    )
  })
})

observeEvent(input$more_info_MEDS_SA,{
  output$info_details_SA<-renderUI({
    
    
    div(shiny::HTML(
      '<h4>Inclusion/Exclusion Criteria</h4>
                         <p>The following criteria for evidence inclusion for the MEDS Model Analysis was determined through the guidance of the BCBS Plans’ executive and clinical pharmacists.</p><br>
                         <h4>Inclusion Criteria</h4>
                         <br>
                         
                         <h4>Patient Population</h4>
                         <ul>
                            <li>Individuals ages 12 years or older with moderate to severe allergic asthma or eosinophilic asthma </li>
                            <li>Randomized Controlled Trials: At least 50 patients </li>
                            
                          </ul><br>
                          
                          <h4>Intervention</h4>
                         <ul>
                            <li>Omalizumab (Xolair)</li>
                            <li>Dupilumab (Dupixent)</li>
                            <li>Mepolizumab (Nucala)</li>
                            <li>Reslizumab (Cinqair)</li>
                            <li>Benralizumab (Fasenra)</li>
                           
                          </ul><br>
                          
                          <h4>Comparator</h4>
                         <ul>
                            <li>Head to Head</li>
                            <li>Standard of Care treatment with inhaled corticosteroids (ICS) and at least one additional controller agent</li>
                          </ul><br>
                          
                          <h4>Efficacy Outcomes:</h4>
                         <ul>
                            <li>Acute exacerbation of asthma (30% weight)</li>
                            <li>Rate of Hospitalization/Emergency Department Visit (70% weight)</li>
                          </ul><br>
                          
                          <h4>Safety Outcomes</h4>
                         <ul>
                            <li>Discontinuation (70% weight)</li>
                            <li>Severe Adverse Events (30% weight)</li>
                           
                          </ul><br>
                          
                          <h4>Timing</h4>
                         <ul>
                            <li>12 weeks – 1 year </li>
                          </ul><br>
                          
                          <h4>Setting</h4>
                         <ul>
                            <li>Outpatient</li>
                          </ul><br>
                          
                           <h4>Exclusion Criteria</h4>
                           <ul>
                           <li>Studies involving patients under 12</li>
                           <li>Single-arm studies</li>
                           <li>Open label long term extension studies (unless the study reported the blinded phase before crossover)</li>
                           <li>Randomized controlled trials with sample size of less than 50 patients</li>
                           </ul>'),
      div(bcbsButton('ref_list_SA','References'))
      
    )
  })
})



observeEvent(input$ref_list_SA,{
  output$info_details_SA <- renderUI({
    
    references <- readRDS("SA_references.RDS")
    
    html_string <- c("<h3>References</h3> <ul>",
                     paste0("<li>", references, "</li>"),
                     "</ul>")
    
    div(shiny::HTML(paste(html_string, sep = "")),
        div(bcbsButton('info_methods_SA','Methods'))
    )
  })
})

observeEvent(input$info_methods_SA,{
  output$info_details_SA<-renderUI({
    div(shiny::HTML(
      '<h4>Inclusion/Exclusion Criteria</h4>
                         <p>The following criteria for evidence inclusion for the MEDS Model Analysis was determined through the guidance of the BCBS Plans’ executive and clinical pharmacists.</p><br>
                         <h4>Inclusion Criteria</h4>
                         <br>
                         
                         <h4>Patient Population</h4>
                         <ul>
                            <li>Individuals ages 12 years or older with moderate to severe allergic asthma or eosinophilic asthma </li>
                            <li>Randomized Controlled Trials: At least 50 patients </li>
                            
                          </ul><br>
                          
                          <h4>Intervention</h4>
                         <ul>
                            <li>Omalizumab (Xolair)</li>
                            <li>Dupilumab (Dupixent)</li>
                            <li>Mepolizumab (Nucala)</li>
                            <li>Reslizumab (Cinqair)</li>
                            <li>Benralizumab (Fasenra)</li>
                           
                          </ul><br>
                          
                          <h4>Comparator</h4>
                         <ul>
                            <li>Head to Head</li>
                            <li>Standard of Care treatment with inhaled corticosteroids (ICS) and at least one additional controller agent</li>
                          </ul><br>
                          
                          <h4>Efficacy Outcomes:</h4>
                         <ul>
                            <li>Acute exacerbation of asthma (30% weight)</li>
                            <li>Rate of Hospitalization/Emergency Department Visit (70% weight)</li>
                          </ul><br>
                          
                          <h4>Safety Outcomes</h4>
                         <ul>
                            <li>Discontinuation (70% weight)</li>
                            <li>Severe Adverse Events (30% weight)</li>
                           
                          </ul><br>
                          
                          <h4>Timing</h4>
                         <ul>
                            <li>12 weeks – 1 year </li>
                          </ul><br>
                          
                          <h4>Setting</h4>
                         <ul>
                            <li>Outpatient</li>
                          </ul><br>
                          
                           <h4>Exclusion Criteria</h4>
                           <ul>
                           <li>Studies involving patients under 12</li>
                           <li>Single-arm studies</li>
                           <li>Open label long term extension studies (unless the study reported the blinded phase before crossover)</li>
                           <li>Randomized controlled trials with sample size of less than 50 patients</li>
                           </ul>'),
      div(bcbsButton('ref_list_SA','References'))
    )
  })
})


output$downloadSnapshot_SA <- downloadHandler(
  filename = function() {
    paste0('MEDS_Snapshot_Severe_Asthma.html')
  },
  
  content = function(file) {
    src <- normalizePath('snapshot_sa.Rmd')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'snapshot_sa.Rmd', overwrite = TRUE)
    
    library(rmarkdown)
    
    selected.meds <- str_extract(input$medBoxs_SA, "^\\D+") %>% unique()
    
    out <- rmarkdown::render(input = "snapshot_sa.Rmd",
                             output_file = "snapshot_preview.html",
                             params = list(condition =  condition$selected, 
                                           selected_meds = selected.meds,
                                           EOS_val = input$Ex_of_As,
                                           VR_val=input$visit_rate,
                                           Dis_SA_val=input$Discont_SA,
                                           Ad_Ef_val=input$Adverse_Eff,
                                           sa_data = SA_Data_user1))
    
    file.rename(out, file)
  }
)

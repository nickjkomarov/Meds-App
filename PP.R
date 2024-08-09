# Plaque Psoriasis server file-------------------------------------

observeEvent(c(input$pp, input$pp2), {
  condition$selected <- "Plaque Psoriasis"
}, ignoreInit = TRUE)



observeEvent(c(input$pp, input$pp2), {
  shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")
  shinyjs::show('customFilters_PP')
  shinyjs::show('bcbsaResults_PP')
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
  
  shinyjs::hide('customFilter_SA')
  shinyjs::hide('bcbsaResults_SA')
}, ignoreInit = TRUE)

output$customFilters_PP <- renderUI({
  div(id = "customizeFilters",
      fluidRow(
        div(style = "margin-left: 15px; margin-top: -10px;",
            pickerInput("medBoxs_PP", label = "Select Medications",
                        choices = PP_Data$Arm,
                        selected = PP_Data$Arm,
                        multiple = TRUE,
                        width = 300,
                        options = list(`live-search` = TRUE,
                                       `selected-text-format` = "count > 3",
                                       `actions-box` = TRUE))
        ),
        div(id="inline",
            hr(),
            tags$h5("Benefits Outcomes"),
            sliderInput('P_90',"PASI90", 0,100,30),
            sliderInput('IGA',"IGA 0/1", 0,100,70),
            # uiOutput('benefitAlert_PP'),
            hr(),
            tags$h5("Harms Outcomes"),
            sliderInput('Discont_PP',"Discontinuation",0,100,70),
            sliderInput('Adverse_Eff_PP',"Severe Adverse Effect",0,100,30)
            # uiOutput('harmAlert_PP')
        )
      )
  )
})


# Reset filters to default values

observe({
  req(input$reset)
  
  if (condition$selected == "Plaque Psoriasis") {
    updatePickerInput(session, "medBoxs_PP", "Select Medications", PP_Data$Arm, PP_Data$Arm)
    updateSliderInput(session, 'P_90', "PASI90", 30, 0, 100)
    updateSliderInput(session, 'IGA', "IGA 0/1", 70, 0, 100)
    updateSliderInput(session,'Discont_PP',"Discontinuation",70,0,100)
    updateSliderInput(session, 'Adverse_Eff_PP',"Severe Adverse Effect",30,0,100)
  }
})


# User input validation -- Benefits Weights

observe({
  req(input$P_90)
  
  max.value <- 100 - input$P_90
  if(max.value >= 0){
    output$benefitAlert_PP <- NULL
    updateSliderInput(session, "IGA", value = max.value, min = 0, max = 100, step = 1)
  }
})

observe({
  req(input$IGA)
  max.value <- 100 - input$IGA
  if(max.value >= 0){
    output$benefitAlert_PP <- NULL
    updateSliderInput(session, "P_90", value = max.value, min = 0, max = 100, step = 1)
  } 
})

# User input validation -- Harms Weights

observe({
  req(input$Discont_PP)
  max.value <- 100 - input$Discont_PP
  if(max.value >= 0){
    output$harmAlert_PP <- NULL
    updateSliderInput(session, "Adverse_Eff_PP", value = max.value, min = 0, max = 100, step = 1)
  }
})

observe({
  req(input$Adverse_Eff_PP)
  max.value <- 100 - input$Adverse_Eff_PP
  if(max.value >= 0){
    output$benefitAlert_PP <- NULL
    updateSliderInput(session, "Discont_PP", value = max.value, min = 0, max = 100, step = 1)
  }
})


output$boxTitle_PP <- renderUI({
  req(input$update || input$pp || input$pp2)
  
  if (isolate(input$P_90) == 30 &&
      isolate(input$IGA) == 70 &&
      isolate(input$Discont_PP) == 70 &&
      isolate(input$Adverse_Eff_PP) == 30 &&
      
      length(isolate(input$medBoxs_PP)) == length(PP_Data$Arm)) {
    
    h3('Results from BCBSA using default weights')
    
  } else {
    
    h3('Results from BCBSA using user-defined weights')
  }
  
})

observe({
  req(input$pp_tab_box)
  
  if (input$pp_tab_box == "Preview Snapshot") {
    shinyjs::hide("more_info_MEDS_PP")
    shinyjs::show("downloadSnapshot_PP")
  } else {
    shinyjs::show("more_info_MEDS_PP")
    shinyjs::hide("downloadSnapshot_PP")
  }
})


# Update Data Tables and Snapshot

observeEvent(c(input$pp, input$update, input$pp2), {
  
  if(isolate(condition$selected) == "Plaque Psoriasis") {
    
    meds_values$PP_MEDS_Sel <- isolate(input$medBoxs_PP)
    meds_values$P_90_PP <- isolate(input$P_90)
    meds_values$IGA_PP <- isolate(input$IGA)
    meds_values$Discont_PP_PP<-  isolate(input$Discont_PP)
    meds_values$Adverse_Eff_PP_PP<- isolate(input$Adverse_Eff_PP)
    print(meds_values$PP_MEDS_Sel)
    print(meds_values$P_90_PP)
    print(meds_values$IGA_PP)
    print(meds_values$Discont_PP_PP)
    print(meds_values$Adverse_Eff_PP_PP)
    
    
    PP_Data_user <- openxlsx::read.xlsx('Data/Psoriasis/Psoriasis.xlsx', 'Sheet1',startRow = 1,
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
    
    PP_Data_user <- PP_Data_user %>%
      filter(Arm != "GP2017 Adalimumab biosimilar")
    
    PP_Data_user <- PP_Data_user[PP_Data_user$Arm %in% isolate(meds_values$PP_MEDS_Sel),] 
    
    # BENEFITS WEIGHTS -------------------------------------------------------------
    
    WT_P_90 <- isolate(meds_values$P_90_PP)/100
    WT_IGA <- isolate(meds_values$IGA_PP)/100 
    COL_P_90 <- PP_Data_user$RD.PASI90
    COL_IGA <- PP_Data_user$`RD.IGA.0/1`
    
    for(i in 1:nrow(PP_Data_user)){
      if(!is.na(COL_P_90[i]) & !is.na(COL_IGA[i])){
        PP_Data_user$cNNT[i] <- 1/(WT_P_90 * COL_P_90[i] + WT_IGA * COL_IGA[i])
        
      }else if(!is.na(COL_P_90[i]) & is.na(COL_IGA[i])){
        PP_Data_user$cNNT[i] <- 1/(WT_P_90*COL_P_90[i] + WT_IGA*COL_P_90[i])
        
      }else if(is.na(COL_P_90[i]) & !is.na(COL_IGA[i])){
        PP_Data_user$cNNT[i] <- 1/(WT_P_90*COL_IGA[i] + WT_IGA*COL_IGA[i])
      } 
    }
    
    # HARMS WEIGHTS -------------------------------------------------------------
    
    WT_DISCON <- isolate(meds_values$Discont_PP_PP)/100
    WT_SAE <- isolate(meds_values$Adverse_Eff_PP_PP)/100
    COL_DISCON <- PP_Data_user$RD.Discon
    COL_SAE <- PP_Data_user$RD.SAE
    
    for(i in 1:nrow(PP_Data_user)){
      if(!is.na(COL_DISCON[i]) & !is.na(COL_SAE[i])){
        PP_Data_user$cNNH[i] <- 1/(WT_DISCON * COL_DISCON[i] + WT_SAE * COL_SAE[i])
        
      }else if(!is.na(COL_DISCON[i]) & is.na(COL_SAE[i])){
        
        if(!is.na(COL_SAE[i])){
          PP_Data_user$cNNH[i]<-1/(WT_DISCON*COL_DISCON[i]+WT_SAE*COL_SAE[i])
        }else{
          PP_Data_user$cNNH[i]<-1/(WT_DISCON*COL_DISCON[i]+WT_SAE*0.5*COL_DISCON[i])
        }
      }else if(is.na(COL_DISCON[i]) & !is.na(COL_SAE[i])){
        PP_Data_user$cNNH[i] <- 1/(WT_DISCON*0.5*COL_SAE[i]+WT_SAE*COL_SAE[i])
      } 
    }
    
    # CLINICAL EFFECTIVENESS RANKING -----------------------------------------------
    
    for(i in 1:nrow(PP_Data_user)){
      PP_Data_user$cRDBen[i]<-1/PP_Data_user$cNNT[i]
      PP_Data_user$cRDHarm[i]<-1/PP_Data_user$cNNH[i]
    }
    
    for(i in 1:nrow(PP_Data_user)){
      if(PP_Data_user$cRDBen[i]>=quantile(PP_Data_user$cRDBen,0.7)){
        PP_Data_user$NNTCat[i]<-'A+'
      }else if(PP_Data_user$cRDBen[i]>=quantile(PP_Data_user$cRDBen,0.5)){
        PP_Data_user$NNTCat[i]<-'A'
      }else if(PP_Data_user$cRDBen[i]>=quantile(PP_Data_user$cRDBen,0.3)){
        PP_Data_user$NNTCat[i]<-'B+'
      }else{
        PP_Data_user$NNTCat[i]<-'B'
      }
    }
    
    for(i in 1:nrow(PP_Data_user)){
      if(PP_Data_user$cRDHarm[i]>=quantile(PP_Data_user$cRDHarm,0.7)){
        PP_Data_user$NNHCat[i]<-'B'
      }else if(PP_Data_user$cRDHarm[i]>=quantile(PP_Data_user$cRDHarm,0.5)){
        PP_Data_user$NNHCat[i]<-'B+'
      }else if(PP_Data_user$cRDHarm[i]>=quantile(PP_Data_user$cRDHarm,0.3)){
        PP_Data_user$NNHCat[i]<-'A'
      }else{
        PP_Data_user$NNHCat[i]<-'A+'
      }
    }
    
    for(i in 1:nrow(PP_Data_user)){
      if(PP_Data_user$NNTCat[i]=='A+' & PP_Data_user$NNHCat[i]=='A+'){
        PP_Data_user$ceRank[i]<-'A'
      }else if(PP_Data_user$NNTCat[i]=='A+' & PP_Data_user$NNHCat[i]=='A'){
        PP_Data_user$ceRank[i]<-'A'
      }else if(PP_Data_user$NNTCat[i]=='A+' & PP_Data_user$NNHCat[i]=='B+'){
        PP_Data_user$ceRank[i]<-'B'
      }else if(PP_Data_user$NNTCat[i]=='A+' & PP_Data_user$NNHCat[i]=='B'){
        PP_Data_user$ceRank[i]<-'B'
      }else if(PP_Data_user$NNTCat[i]=='A' & PP_Data_user$NNHCat[i]=='A+'){
        PP_Data_user$ceRank[i]<-'B'
      }else if(PP_Data_user$NNTCat[i]=='A' & PP_Data_user$NNHCat[i]=='A'){
        PP_Data_user$ceRank[i]<-'B'
      }else if(PP_Data_user$NNTCat[i]=='A' & PP_Data_user$NNHCat[i]=='B+'){
        PP_Data_user$ceRank[i]<-'C'
      }else if(PP_Data_user$NNTCat[i]=='A' & PP_Data_user$NNHCat[i]=='B'){
        PP_Data_user$ceRank[i]<-'C'
      }else if(PP_Data_user$NNTCat[i]=='B+' & PP_Data_user$NNHCat[i]=='A+'){
        PP_Data_user$ceRank[i]<-'C'
      }else if(PP_Data_user$NNTCat[i]=='B+' & PP_Data_user$NNHCat[i]=='A'){
        PP_Data_user$ceRank[i]<-'D'
      }else if(PP_Data_user$NNTCat[i]=='B+' & PP_Data_user$NNHCat[i]=='B+'){
        PP_Data_user$ceRank[i]<-'D'
      }else if(PP_Data_user$NNTCat[i]=='B+' & PP_Data_user$NNHCat[i]=='B'){
        PP_Data_user$ceRank[i]<-'D'
      }else{
        PP_Data_user$ceRank[i]<-'F'
      }
    }
    
    PP_Data_user_save<<-PP_Data_user
    
    for(i in 1:nrow(PP_Data_user_save)){
      print(i)
      PP_Data_user_save$LL_Ben[i]<-ifelse((PP_Data_user_save$cNNT[i]+1)>=1 & (PP_Data_user_save$cNNT[i]+1)<=3,100,(1-(0.5-1000/((1/PP_Data_user_save$cRDBen[i])+1)/(1000-1000/((1/PP_Data_user_save$cRDBen[i])+1))))*100)
      PP_Data_user_save$UL_Ben[i]<-ifelse((PP_Data_user_save$cNNT[i]-1)>=0 & (PP_Data_user_save$cNNT[i]-1)<=3,100,(1-(0.5-1000/((1/PP_Data_user_save$cRDBen[i])-1)/(1000-1000/((1/PP_Data_user_save$cRDBen[i])-1))))*100)
      if(PP_Data_user_save$LL_Ben[i]>100){
        PP_Data_user_save$LL_Ben[i]<-100
      }else if(PP_Data_user_save$LL_Ben[i]<0){
        PP_Data_user_save$LL_Ben[i]<-0
      }else{
        PP_Data_user_save$LL_Ben[i]<-PP_Data_user_save$LL_Ben[i]
      }
      if(PP_Data_user_save$UL_Ben[i]>100){
        PP_Data_user_save$UL_Ben[i]<-100
      }else if(PP_Data_user_save$UL_Ben[i]<0){
        PP_Data_user_save$UL_Ben[i]<-0
      }else{
        PP_Data_user_save$UL_Ben[i]<-PP_Data_user_save$UL_Ben[i]
      }
      
      PP_Data_user_save$UL_Harm[i]<-ifelse((PP_Data_user_save$cNNH[i]+1)>=1 & (PP_Data_user_save$cNNH[i]+1)<=3,100,(1-(0.5-1000/((1/PP_Data_user_save$cRDHarm[i])-1)/(1000-1000/((1/PP_Data_user_save$cRDHarm[i])-1))))*100)
      PP_Data_user_save$LL_Harm[i]<-ifelse((PP_Data_user_save$cNNH[i]-1)>=0 & (PP_Data_user_save$cNNH[i]-1)<=3,100,(1-(0.5-1000/((1/PP_Data_user_save$cRDHarm[i])+1)/(1000-1000/((1/PP_Data_user_save$cRDHarm[i])+1))))*100)
      
      if(PP_Data_user_save$LL_Harm[i]>100){
        PP_Data_user_save$LL_Harm[i]<-100
      }else if(PP_Data_user_save$LL_Harm[i]<0){
        PP_Data_user_save$LL_Harm[i]<-0
      }else{
        PP_Data_user_save$LL_Harm[i]<-PP_Data_user_save$LL_Harm[i]
      }
      if(PP_Data_user_save$UL_Harm[i]>100){
        PP_Data_user_save$UL_Harm[i]<-100
      }else if(PP_Data_user_save$UL_Harm[i]<0){
        PP_Data_user_save$UL_Harm[i]<-0
      }else{
        PP_Data_user_save$UL_Harm[i]<-PP_Data_user_save$UL_Harm[i]
      }
      PP_Data_user_save$MeanLL[i]<-(PP_Data_user_save$LL_Harm[i]+PP_Data_user_save$LL_Ben[i])/2
      PP_Data_user_save$MeanUL[i]<-(PP_Data_user_save$UL_Harm[i]+PP_Data_user_save$UL_Ben[i])/2
      PP_Data_user_save$MeanLL[i]<-format(round(as.numeric(PP_Data_user_save$MeanLL[i]), 2), nsmall = 2)
      PP_Data_user_save$MeanUL[i]<-format(round(as.numeric(PP_Data_user_save$MeanUL[i]), 2), nsmall = 2)
    }
    
    
    PP_Data_user_save<<-PP_Data_user_save
    PP_Data_user<-PP_Data_user_save
    PP_Data_user1<-PP_Data_user[, c("Arm", "cNNT","cNNH","ceRank","MeanLL","MeanUL")]
    PP_Data_user1 <- round_df(PP_Data_user1, digits = 2)
    PP_Data_user1$AVScore<-paste(PP_Data_user1$MeanLL,'-',PP_Data_user1$MeanUL)
    
    
    
    assign("PP_Data_user1", PP_Data_user1, .GlobalEnv)
    
    meds_values$PP_Data_user1 <- PP_Data_user1
    
    selected.meds <- str_extract(input$medBoxs_PP, "^\\D+") %>% unique()

    # GENERATE MARKDOWN
     rmarkdown::render(input = "snapshot_pp.Rmd",
                       output_file = "snapshot_preview.html",
                      params = list(condition =  condition$selected,
                                     selected_meds = selected.meds,
                                    P90_val = input$P_90,
                                    IGA_val=input$IGA,
                                    Dis_PP_val=input$Discont_PP,
                                    Ad_PP_val=input$Adverse_Eff_PP,
                                    pp_data = PP_Data_user1))
    
     xml2::write_html(rvest::html_node(xml2::read_html("snapshot_preview.html"), "body"), file = "snapshot_preview_PP.html")
    
    # print('ac1')
    data <- PP_Data_user1[,c("Arm", "cNNT","cNNH","ceRank","AVScore")]
    # # data <- PP_Data_user
    # print('ac2')
    #
    
    # data[data$Arm=='GP2017 Adalimumab biosimilar', ]$cNNT <- " "
    # data[data$Arm=='GP2017 Adalimumab biosimilar', ]$cNNH <- " "
    # data[data$Arm=='GP2017 Adalimumab biosimilar', ]$ceRank <- " "
    # data[data$Arm=='GP2017 Adalimumab biosimilar', ]$MeanLL <- " "
    # data[data$Arm=='GP2017 Adalimumab biosimilar', ]$MeanUL <- " "
    # data[data$Arm=='GP2017 Adalimumab biosimilar', ]$AVScore <- " "

    # DATA TABLE
    output$userDefRes_PP <- renderReactable({
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
        )
      )
    })
    
    # output$userDefRes_PP<-DT::renderDataTable(datatable(data,
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
    
    output$previewSnapshot_PP <- renderUI({
      includeHTML("snapshot_preview_PP.html")
    })

    output$bcbsaResults_PP <- renderUI({
      
      fluidRow(
        h2("Plaque Psoriasis"),
        h4("Use the filters on the left sidebar to select medications and efficacy outcome weight"),
        h4("Click the 'Run Analysis' button to update the data in the results table and snapshot. 
           Navigate to the 'Preview Snapshot' tab to preview and/or download the BCBS Snapshot report"),
        br(),
        
        bcbsTabBox(id = "pp_tab_box",
                   title = p(actionButton("more_info_MEDS_PP", label = NULL, icon = icon("info")),
                             shinyjs::hidden(downloadButton("downloadSnapshot_PP", "Download Snapshot"))),
                   width = 12,
                   tabPanel("Results Overview",
                            uiOutput("boxTitle_PP"),
                            reactableOutput('userDefRes_PP'),
                            tags$head(tags$style("#meds_info .modal-footer button {font-size: 20px; line-height: 25px; font-weight: bold; text-transform: uppercase;}")),
                            tags$head(tags$style("#meds_info .modal-header {padding-bottom: 0px; padding-top: 0px;}")),
                            tags$head(tags$style("#meds_info .modal-header button {display: none;}")) ,
                            
                            bsModal('meds_info_PP',
                                    title = h2('MEDS Information', style = 'color: #005876; font-weight: bold; line-height: 25px;'),
                                    size = 'large',
                                    trigger = 'more_info_MEDS_PP',
                                    uiOutput('info_details_PP')
                            )
                            
                   ),
                   tabPanel("Preview Snapshot",
                            uiOutput("previewSnapshot_PP") #,
                   )
        )
      )
      
    })
    
  }
  
  
  
},ignoreInit = TRUE)




observeEvent(input$more_info_MEDS_PP,{
  output$info_details_PP<-renderUI({
    
    
    div(shiny::HTML(
      '<h4>Inclusion/Exclusion Criteria</h4>
                         <p>The following criteria for evidence inclusion for the MEDS Model Analysis was determined through the guidance of the BCBS Plans’ executive and clinical pharmacists.</p><br>
                         <h4>Inclusion Criteria</h4>
                         <br>
                         
                         <h4>Patient Population</h4>
                         <ul>
                            <li>Individuals ages 18 years and older with moderate-to-severe chronic plaque psoriasis  </li>
                            <li>Randomized Controlled Trials: At least 50 patients </li>
                            
                          </ul><br>
                          
                          <h4>Intervention</h4>
                         <ul>
                            <li>Secukinumab (Cosentyx)</li>
                            <li>Etanercept (Enbrel)</li>
                            <li>Adalimumab (Humira)</li>
                            <li>Infliximab (Remicade)</li>
                            <li>Brodalumab (Siliq)</li>
                            <li>Ustekinumab (Stelara)</li>
                            <li>Ixekizumab (Taltz)</li>
                            <li>Guselkumab (Tremfya)</li>
                            <li>Certolizumab (Cimzia)</li>
                            <li>Tildrakizumab (Ilumya)</li>
                            <li>Risankizumab (Skyrizi)</li>
                            <li>Apremilast (Otezla)</li>
                           
                          </ul><br>
                          
                          <h4>Comparator</h4>
                         <ul>
                            <li>Head to Head</li>
                            <li>Placebo</li>
                          </ul><br>
                          
                          <h4>Efficacy Outcomes:</h4>
                         <ul>
                            <li>Psoriasis Area and Severity Index (PASI) 90 (30% weight)</li>
                            <li>Physician Global Assessment (PGA) or Investigator’s Global Assessment (IGA) (‘clear’/’almost clear’) (70% weight)</li>
                          </ul><br>
                          
                          <h4>Safety Outcomes</h4>
                         <ul>
                            <li>Discontinuation (70% weight)</li>
                            <li>Severe Adverse Events (30% weight)</li>
                           
                          </ul><br>
                          
                          <h4>Timing</h4>
                         <ul>
                            <li>10 – 24 weeks  </li>
                          </ul><br>
                          
                          <h4>Setting</h4>
                         <ul>
                            <li>Outpatient</li>
                          </ul><br>
                          
                           <h4>Exclusion Criteria</h4>
                           <ul>
                           <li>Studies involving patients under 18</li>
                           <li>Single-arm studies</li>
                           <li>Open label long term extension studies (unless the study reported the blinded phase before crossover)</li>
                           <li>Randomized controlled trials with sample size of less than 50 patients</li>
                           </ul>'),
      div(bcbsButton('ref_list_PP','References'))
      
    )
  })
})

observeEvent(input$more_info_MEDS_PP,{
  output$info_details_PP<-renderUI({
    
    
    div(shiny::HTML(
      '<h4>Inclusion/Exclusion Criteria</h4>
                         <p>The following criteria for evidence inclusion for the MEDS Model Analysis was determined through the guidance of the BCBS Plans’ executive and clinical pharmacists.</p><br>
                         <h4>Inclusion Criteria</h4>
                         <br>
                         
                         <h4>Patient Population</h4>
                         <ul>
                            <li>Individuals ages 18 years and older with moderate-to-severe chronic plaque psoriasis  </li>
                            <li>Randomized Controlled Trials: At least 50 patients </li>
                            
                          </ul><br>
                          
                          <h4>Intervention</h4>
                         <ul>
                            <li>Secukinumab (Cosentyx)</li>
                            <li>Etanercept (Enbrel)</li>
                            <li>Adalimumab (Humira)</li>
                            <li>Infliximab (Remicade)</li>
                            <li>Brodalumab (Siliq)</li>
                            <li>Ustekinumab (Stelara)</li>
                            <li>Ixekizumab (Taltz)</li>
                            <li>Guselkumab (Tremfya)</li>
                            <li>Certolizumab (Cimzia)</li>
                            <li>Tildrakizumab (Ilumya)</li>
                            <li>Risankizumab (Skyrizi)</li>
                            <li>Apremilast (Otezla)</li>
                           
                          </ul><br>
                          
                          <h4>Comparator</h4>
                         <ul>
                            <li>Head to Head</li>
                            <li>Placebo</li>
                          </ul><br>
                          
                          <h4>Efficacy Outcomes:</h4>
                         <ul>
                            <li>Psoriasis Area and Severity Index (PASI) 90 (30% weight)</li>
                            <li>Physician Global Assessment (PGA) or Investigator’s Global Assessment (IGA) (‘clear’/’almost clear’) (70% weight)</li>
                          </ul><br>
                          
                          <h4>Safety Outcomes</h4>
                         <ul>
                            <li>Discontinuation (70% weight)</li>
                            <li>Severe Adverse Events (30% weight)</li>
                           
                          </ul><br>
                          
                          <h4>Timing</h4>
                         <ul>
                            <li>10 – 24 weeks  </li>
                          </ul><br>
                          
                          <h4>Setting</h4>
                         <ul>
                            <li>Outpatient</li>
                          </ul><br>
                          
                           <h4>Exclusion Criteria</h4>
                           <ul>
                           <li>Studies involving patients under 18</li>
                           <li>Single-arm studies</li>
                           <li>Open label long term extension studies (unless the study reported the blinded phase before crossover)</li>
                           <li>Randomized controlled trials with sample size of less than 50 patients</li>
                           </ul>'),
      div(bcbsButton('ref_list_PP','References'))
      
    )
  })
})



observeEvent(input$ref_list_PP,{
  output$info_details_PP <- renderUI({
    
    references <- readRDS("PP_references.RDS")
    
    html_string <- c("<h3>References</h3> <ul>",
                     paste0("<li>", references, "</li>"),
                     "</ul>")
    
    div(shiny::HTML(paste(html_string, sep = "")),
        div(bcbsButton('info_methods_PP','Methods'))
    )
  })
})

observeEvent(input$info_methods_PP,{
  output$info_details_PP<-renderUI({
    div(shiny::HTML(
      '<h4>Inclusion/Exclusion Criteria</h4>
                         <p>The following criteria for evidence inclusion for the MEDS Model Analysis was determined through the guidance of the BCBS Plans’ executive and clinical pharmacists.</p><br>
                         <h4>Inclusion Criteria</h4>
                         <br>
                         
                         <h4>Patient Population</h4>
                         <ul>
                            <li>Individuals ages 18 years and older with moderate-to-severe chronic plaque psoriasis  </li>
                            <li>Randomized Controlled Trials: At least 50 patients </li>
                            
                          </ul><br>
                          
                          <h4>Intervention</h4>
                         <ul>
                            <li>Secukinumab (Cosentyx)</li>
                            <li>Etanercept (Enbrel)</li>
                            <li>Adalimumab (Humira)</li>
                            <li>Infliximab (Remicade)</li>
                            <li>Brodalumab (Siliq)</li>
                            <li>Ustekinumab (Stelara)</li>
                            <li>Ixekizumab (Taltz)</li>
                            <li>Guselkumab (Tremfya)</li>
                            <li>Certolizumab (Cimzia)</li>
                            <li>Tildrakizumab (Ilumya)</li>
                            <li>Risankizumab (Skyrizi)</li>
                            <li>Apremilast (Otezla)</li>
                           
                          </ul><br>
                          
                          <h4>Comparator</h4>
                         <ul>
                            <li>Head to Head</li>
                            <li>Placebo</li>
                          </ul><br>
                          
                          <h4>Efficacy Outcomes:</h4>
                         <ul>
                            <li>Psoriasis Area and Severity Index (PASI) 90 (30% weight)</li>
                            <li>Physician Global Assessment (PGA) or Investigator’s Global Assessment (IGA) (‘clear’/’almost clear’) (70% weight)</li>
                          </ul><br>
                          
                          <h4>Safety Outcomes</h4>
                         <ul>
                            <li>Discontinuation (70% weight)</li>
                            <li>Severe Adverse Events (30% weight)</li>
                           
                          </ul><br>
                          
                          <h4>Timing</h4>
                         <ul>
                            <li>10 – 24 weeks  </li>
                          </ul><br>
                          
                          <h4>Setting</h4>
                         <ul>
                            <li>Outpatient</li>
                          </ul><br>
                          
                           <h4>Exclusion Criteria</h4>
                           <ul>
                           <li>Studies involving patients under 18</li>
                           <li>Single-arm studies</li>
                           <li>Open label long term extension studies (unless the study reported the blinded phase before crossover)</li>
                           <li>Randomized controlled trials with sample size of less than 50 patients</li>
                           </ul>'),
      div(bcbsButton('ref_list_PP','References'))
    )
  })
})



output$downloadSnapshot_PP <- downloadHandler(
  filename = function() {
    paste0('MEDS_Snapshot_Plaque_Psoriasis.html')
  },
  
  content = function(file) {
    src <- normalizePath('snapshot_pp.Rmd')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'snapshot_pp.Rmd', overwrite = TRUE)
    
    library(rmarkdown)
    
    selected.meds <- str_extract(input$medBoxs_PP, "^\\D+") %>% unique()
    
    out <- rmarkdown::render(input = "snapshot_pp.Rmd",
                             output_file = "snapshot_preview.html",
                             params = list(condition =  condition$selected, 
                                           selected_meds = selected.meds,
                                           P90_val = input$P_90,
                                           IGA_val=input$IGA,
                                           Dis_PP_val=input$Discont_PP,
                                           Ad_PP_val=input$Adverse_Eff_PP,
                                           pp_data = PP_Data_user1))
    
    file.rename(out, file)
  }
)

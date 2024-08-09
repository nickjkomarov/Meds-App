# Multiple Sclerosis -- Server file
shinyjs::hide("customFilters_MS")
shinyjs::hide('flu_MS')
shinyjs::hide('DR_MS')


observeEvent(c(input$ms, input$ms2), {
  condition$selected <- "Multiple Sclerosis"
}, ignoreInit = TRUE)


# Reset filters to default values
observe({
  req(input$reset)
  
  if (condition$selected == "Multiple Sclerosis") {
    updatePickerInput(session, "medBoxs_MS", "Select Medications", MS_Data$Arm, MS_Data$Arm)
    updateSliderInput(session, 'rel_free_MS', "Relapse Free", 40, 0, 100)
    updateSliderInput(session, 'dis_prog_MS', "Disability Progression", 60, 0, 100)
    updateSliderInput(session, 'discont_MS',"Discontinuation", 90, 0, 100)
    updateSliderInput(session, 'heptox_flu_MS',"Hepatoxicity/Influenza", 7, 0, 100)
    updateSliderInput(session, 'IJR_DR_MS', "Injection Reaction/Diarrhea", 3, 0, 100)
  }
})

output$customFilters_MS <- renderUI({
  div(id = "customizeFilters",
      fluidRow(
        div(style = "margin-left: 15px; margin-top: -10px;",
            pickerInput("medBoxs_MS", label = "Select Medications",
                        choices = MS_Data$Arm,
                        selected = MS_Data$Arm,
                        multiple = TRUE,
                        width = 300,
                        options = list(`live-search` = TRUE,
                                       `selected-text-format` = "count > 3",
                                       `actions-box` = TRUE))
        ),
        div(id="inline",
            hr(),
            tags$h5("Benefits Outcomes"),
            sliderInput('rel_free_MS',"Relapse Free", 0,100,40),
            sliderInput('dis_prog_MS',"Disability Progression", 0,100,60),
            uiOutput('benefitAlert_MS'),
            hr(),
            tags$h5("Harms Outcomes"),
            sliderInput('discont_MS',"Discontinuation", 0,100,90),
            sliderInput('heptox_flu_MS',"Hepatoxicity/Influenza", 0,100,7),
            sliderInput('IJR_DR_MS',"Injection Reaction/Diarrhea", 0,100,3),
            uiOutput('harmAlert_MS')
        )
      )
  )
})

observeEvent(c(input$ms, input$ms2), {
  shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")
  shinyjs::show('customFilters_MS')
  shinyjs::show('bcbsaResults_MS')
  shinyjs::show('update')
  
  # shinyjs::hide('ra')
  # shinyjs::hide('ms')
  # shinyjs::hide('hap')
  # shinyjs::hide('hao')
  # shinyjs::hide('sa')
  # shinyjs::hide('pp')
  shinyjs::hide('homepage')
  
  shinyjs::hide('customFilters_RA')
  shinyjs::hide('bcbsaResults_RA')
  shinyjs::hide('customFilters_HP')
  shinyjs::hide('bcbsaResults_HP')
  shinyjs::hide('customFilters_HO')
  shinyjs::hide('bcbsaResults_HO')
  shinyjs::hide('customFilters_SA')
  shinyjs::hide('bcbsaResults_SA')
  shinyjs::hide('customFilters_PP')
  shinyjs::hide('bcbsaResults_PP')
}, ignoreInit = TRUE)

# User input validation -- Benefits Weights
observe({
  req(input$rel_free_MS)
  max.value <- 100 - input$rel_free_MS
  if(max.value >= 0){
    output$benefitAlert_MS <- NULL
    updateSliderInput(session, "dis_prog_MS", value = max.value, min = 0, max = 100, step = 1)
    shinyjs::enable('update')
  }else{
    output$benefitAlert_MS <- renderUI({
      div(style='width:300;', h6('Total weight cannot be more than 100!'))
    })
    shinyjs::disable('update')
  }
})

output$boxTitle_MS <- renderUI({
  req(input$update || input$ms || input$ms2)
  
  if (isolate(input$rel_free_MS) == 40 &&
      isolate(input$dis_prog_MS) == 60 &&
      isolate(input$discont_MS) == 90 &&
      isolate(input$heptox_flu_MS) == 7 &&
      isolate(input$IJR_DR_MS) == 3 &&
      
      length(isolate(input$medBoxs_MS)) == length(MS_Data$Arm)) {
    
    h3('Results from BCBSA using default weights')
    
  } else {
    
    h3('Results from BCBSA using user-defined weights')
  }
  
})


observe({
  req(input$dis_prog_MS)
  max.value <- 100 - input$dis_prog_MS
  if(max.value >= 0){
    output$benefitAlert_MS <- NULL
    updateSliderInput(session, "rel_free_MS", value = max.value, min = 0, max = 100, step = 1)
    shinyjs::enable('update')
  }else{
    output$benefitAlert_MS <- renderUI({
      div(style='width:300;', h6('Total weight cannot be more than 100!'))
    })
    shinyjs::disable('update')
  }
})

# User input validation -- Harms Weights
observe({
  req(input$discont_MS, input$heptox_flu_MS)
  max.value <- 100 - (input$discont_MS + input$heptox_flu_MS)
  if(max.value >= 0){
    output$harmAlert_MS <- NULL
    updateSliderInput(session, "IJR_DR_MS", value = max.value, min = 0, max = max.value, step = 1)
    shinyjs::enable('update')
  } else {
    output$harmAlert_MS <- renderUI({
      div(style='width:300;',h6('Total weight cannot be more than 100!'))
    })
    shinyjs::disable('update')
  }
})

observe({
  req(input$ms_tab_box)
  
  if (input$ms_tab_box == "Preview Snapshot") {
    shinyjs::hide("more_info_MEDS_MS")
    shinyjs::show("downloadSnapshot_MS")
  } else {
    shinyjs::show("more_info_MEDS_MS")
    shinyjs::hide("downloadSnapshot_MS")
  }
})

# Update Data Tables and Snapshot
observeEvent(c(input$ms, input$update, input$ms2), {
  
  if(isolate(condition$selected) == "Multiple Sclerosis") {
    
    meds_values$MS_MEDS_Sel <- isolate(input$medBoxs_MS)
    meds_values$RF_MS <- isolate(input$rel_free_MS)
    meds_values$DisProg_MS <- isolate(input$dis_prog_MS)
    meds_values$Disc_MS <- isolate(input$discont_MS)
    meds_values$Heptox_Flu_MS <- isolate(input$heptox_flu_MS)
    meds_values$IJR_DR_MS <- isolate(input$IJR_DR_MS)
    print(meds_values$MS_MEDS_Sel)
    print(meds_values$RF_MS)
    print(meds_values$DisProg_MS)
    print(meds_values$Disc_MS)
    print(meds_values$Heptox_Flu_MS)
    print(meds_values$IJR_DR_MS)
    
    MS_Data_user<-openxlsx::read.xlsx('Data/MS/MS_Data.xlsx', 'Sheet1',startRow = 1,
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
    cols_remove <- c("cNNT","cNNH","CERank")
    MS_Data_user<-MS_Data_user[, !(colnames(MS_Data_user) %in% cols_remove)]
    MS_Data_user<-MS_Data_user[MS_Data_user$Arm %in% isolate(meds_values$MS_MEDS_Sel),] 
    
    wtRF_MS<-isolate(meds_values$RF_MS)/100
    wtDisProg_MS<-isolate(meds_values$DisProg_MS)/100 
    
    print('a')
    
    # Benefits Weights
    for(i in 1:nrow(MS_Data_user)){
      if(!is.na(MS_Data_user$Relapse.Free.RD[i]) & !is.na(MS_Data_user$Disability.progression.RD[i])){
        MS_Data_user$cNNT[i] <- 1/(wtRF_MS*MS_Data_user$Relapse.Free.RD[i]+wtDisProg_MS*MS_Data_user$Disability.progression.RD[i])
        
      }else if(!is.na(MS_Data_user$Relapse.Free.RD[i]) & is.na(MS_Data_user$Disability.progression.RD[i])){
        if(!is.na(MS_Data_user$Disability.progression.RD.CI[i])){
          MS_Data_user$cNNT[i]<-1/(wtRF_MS*MS_Data_user$Relapse.Free.RD[i]+wtDisProg_MS*MS_Data_user$Disability.progression.RD.CI[i])
        }else{
          MS_Data_user$cNNT[i]<-1/(wtRF_MS*MS_Data_user$Relapse.Free.RD[i]+wtDisProg_MS*0.5*MS_Data_user$Relapse.Free.RD[i])
        }
      }else if(is.na(MS_Data_user$Relapse.Free.RD[i]) & !is.na(MS_Data_user$Disability.progression.RD[i])){
        MS_Data_user$cNNT[i] <- 1/(wtRF_MS*0.5*MS_Data_user$Disability.progression.RD[i]+wtDisProg_MS*MS_Data_user$Disability.progression.RD[i])
      } 
    }
    print(MS_Data_user$cNNT)
    
    
    # Harms Weights
    wtDisc_MS<-isolate(meds_values$Disc_MS)/100 
    wtHeptox_Flu_MS<-isolate(meds_values$Heptox_Flu_MS)/100 
    wtIJR_DR_MS<-isolate(meds_values$IJR_DR_MS)/100 
    
    data <- MS_Data_user
    
    med_group01 <- "Alemtuzumab" 
    med_group02 <- c("Cladribine", "Siponimod", "Natalizumab") 
    med_group03 <- c("Dimethyl fumerate", "Fingolimod", "Glatiramer Acetate", "Teriflunomide 14 mg", "Teriflunomide 7 mg")
    med_group04 <- c("Interferon beta 1a 22 mcg SC", "Interferon beta 1a 30 mg IM", 
                     "Interferon beta 1a 44 mcg SC", "Interferon beta 1b 250 mcg sc ", 
                     "Ocrelizumab", "Ozanimod", "Peginterferon beta 1 a", "Rituximab")
    med_group05 <- "Ofatumumab"
    
    for(i in 1:nrow(data)){
      if(data$Arm[i] %in% med_group01) {
        data$cNNH[i]<- 1/(wtDisc_MS * data$Discontinuation.RD[i] + wtHeptox_Flu_MS * data$Increased.liver.enzyme.RD[i] + wtIJR_DR_MS * data$Injection.reaction.RD[i])
      } else if(data$Arm[i] %in% med_group02) {
        data$cNNH[i]<- 1/(wtDisc_MS * data$Discontinuation.RD[i] + wtHeptox_Flu_MS * data$Increased.liver.enzyme.RD[i] + (wtIJR_DR_MS/2) * data$Discontinuation.RD[i] + (wtIJR_DR_MS/2) * data$Increased.liver.enzyme.RD[i])
      } else if(data$Arm[i] %in% med_group03) {
        data$cNNH[i]<- 1/(wtDisc_MS * data$Discontinuation.RD[i] + wtHeptox_Flu_MS * data$Increased.liver.enzyme.RD[i] + wtIJR_DR_MS * data$Diarrhea.RD[i])
      } else if(data$Arm[i] %in% med_group04) {
        data$cNNH[i]<- 1/(wtDisc_MS * data$Discontinuation.RD[i] + wtHeptox_Flu_MS * data$`Influenza-like.illness.RD`[i] +wtIJR_DR_MS * data$Injection.reaction.RD[i])
      } else if(data$Arm[i] %in% med_group05) { 
        data$cNNH[i]<- 1/(wtDisc_MS * data$Discontinuation.RD[i] + wtIJR_DR_MS * data$Injection.reaction.RD[i] + (wtHeptox_Flu_MS/2) * data$Discontinuation.RD[i] + (wtHeptox_Flu_MS/2) * data$Injection.reaction.RD[i])
      }
    }
    MS_Data_user <- data
    print(MS_Data_user$cNNH)
    
    # for(i in 1:nrow(MS_Data_user)){
    #   
    #   # If Discontinuation is not NA
    #   if(!is.na(MS_Data_user$Discontinuation.RD[i])) {
    #     
    #     # Hepatoxicity is not NA
    #     # IJR is NA
    #     if(!is.na(MS_Data_user$Increased.liver.enzyme.RD[i]) & is.na(MS_Data_user$Injection.reaction.RD[i])){
    #       if (!is.na(MS_Data_user$Diarrhea.RD[i])) { # If Diarrhea is not NA
    #         MS_Data_user$cNNH[i]<-1/(wtDisc_MS*MS_Data_user$Discontinuation.RD[i]+wtHeptox_Flu_MS*MS_Data_user$Increased.liver.enzyme.RD[i]+wtIJR_DR_MS*MS_Data_user$Diarrhea.RD[i])
    #       } else { # If IJR and Diarrhea is NA  
    #         MS_Data_user$cNNH[i]<-1/(wtDisc_MS*MS_Data_user$Discontinuation.RD[i]+wtHeptox_Flu_MS*MS_Data_user$Increased.liver.enzyme.RD[i]+wtIJR_DR_MS*0.5*MS_Data_user$Discontinuation.RD[i]+wtIJR_DR_MS*0.5*MS_Data_user$Increased.liver.enzyme.RD[i])
    #       }
    #       
    #       # Hepatoxicity is not NA
    #       # IJR is not NA  
    #     } else if(!is.na(MS_Data_user$Increased.liver.enzyme.RD[i]) & !is.na(MS_Data_user$Injection.reaction.RD[i])){
    #       MS_Data_user$cNNH[i]<-1/(wtDisc_MS*MS_Data_user$Discontinuation.RD[i]+wtHeptox_Flu_MS*MS_Data_user$Increased.liver.enzyme.RD[i]+wtIJR_DR_MS*MS_Data_user$Injection.reaction.RD[i])
    #       
    #       # IJR is not NA
    #       # Hepatoxicity is NA  
    #     } else if(is.na(MS_Data_user$Increased.liver.enzyme.RD[i]) & !is.na(MS_Data_user$Injection.reaction.RD[i])){
    #       if (!is.na(MS_Data_user$`Influenza-like.illness.RD`[i])) { # If Influenza is not NA
    #         MS_Data_user$cNNH[i]<-1/(wtDisc_MS*MS_Data_user$Discontinuation.RD[i]+wtHeptox_Flu_MS*MS_Data_user$`Influenza-like.illness.RD`[i]+wtIJR_DR_MS*MS_Data_user$Injection.reaction.RD[i])
    #       } else { # If Hepatoxicity and Influenza is NA  
    #         MS_Data_user$cNNH[i]<-1/(wtDisc_MS*MS_Data_user$Discontinuation.RD[i]+wtIJR_DR_MS*MS_Data_user$Injection.reaction.RD[i]+wtHeptox_Flu_MS*0.5*MS_Data_user$Discontinuation.RD[i]+wtHeptox_Flu_MS*0.5*MS_Data_user$Injection.reaction.RD[i])
    #       }
    #     }
    #     
    #   }
    # }
    print(MS_Data_user$cNNH)
    
    for(i in 1:nrow(MS_Data_user)){
      MS_Data_user$cRDBen[i]<-1/MS_Data_user$cNNT[i]
      MS_Data_user$cRDHarm[i]<-1/MS_Data_user$cNNH[i]
    }
    
    for(i in 1:nrow(MS_Data_user)){
      if(MS_Data_user$cRDBen[i]>=quantile(MS_Data_user$cRDBen,0.7)){
        MS_Data_user$NNTCat[i]<-'A+'
      }else if(MS_Data_user$cRDBen[i]>=quantile(MS_Data_user$cRDBen,0.5)){
        MS_Data_user$NNTCat[i]<-'A'
      }else if(MS_Data_user$cRDBen[i]>=quantile(MS_Data_user$cRDBen,0.3)){
        MS_Data_user$NNTCat[i]<-'B+'
      }else{
        MS_Data_user$NNTCat[i]<-'B'
      }
    }
    
    for(i in 1:nrow(MS_Data_user)){
      if(MS_Data_user$cRDHarm[i]>=quantile(MS_Data_user$cRDHarm,0.7)){
        MS_Data_user$NNHCat[i]<-'B'
      }else if(MS_Data_user$cRDHarm[i]>=quantile(MS_Data_user$cRDHarm,0.5)){
        MS_Data_user$NNHCat[i]<-'B+'
      }else if(MS_Data_user$cRDHarm[i]>=quantile(MS_Data_user$cRDHarm,0.3)){
        MS_Data_user$NNHCat[i]<-'A'
      }else{
        MS_Data_user$NNHCat[i]<-'A+'
      }
    }
    
    for(i in 1:nrow(MS_Data_user)){
      if(MS_Data_user$NNTCat[i]=='A+' & MS_Data_user$NNHCat[i]=='A+'){
        MS_Data_user$ceRank[i]<-'A'
      }else if(MS_Data_user$NNTCat[i]=='A+' & MS_Data_user$NNHCat[i]=='A'){
        MS_Data_user$ceRank[i]<-'A'
      }else if(MS_Data_user$NNTCat[i]=='A+' & MS_Data_user$NNHCat[i]=='B+'){
        MS_Data_user$ceRank[i]<-'B'
      }else if(MS_Data_user$NNTCat[i]=='A+' & MS_Data_user$NNHCat[i]=='B'){
        MS_Data_user$ceRank[i]<-'B'
      }else if(MS_Data_user$NNTCat[i]=='A' & MS_Data_user$NNHCat[i]=='A+'){
        MS_Data_user$ceRank[i]<-'B'
      }else if(MS_Data_user$NNTCat[i]=='A' & MS_Data_user$NNHCat[i]=='A'){
        MS_Data_user$ceRank[i]<-'B'
      }else if(MS_Data_user$NNTCat[i]=='A' & MS_Data_user$NNHCat[i]=='B+'){
        MS_Data_user$ceRank[i]<-'C'
      }else if(MS_Data_user$NNTCat[i]=='A' & MS_Data_user$NNHCat[i]=='B'){
        MS_Data_user$ceRank[i]<-'C'
      }else if(MS_Data_user$NNTCat[i]=='B+' & MS_Data_user$NNHCat[i]=='A+'){
        MS_Data_user$ceRank[i]<-'C'
      }else if(MS_Data_user$NNTCat[i]=='B+' & MS_Data_user$NNHCat[i]=='A'){
        MS_Data_user$ceRank[i]<-'D'
      }else if(MS_Data_user$NNTCat[i]=='B+' & MS_Data_user$NNHCat[i]=='B+'){
        MS_Data_user$ceRank[i]<-'D'
      }else if(MS_Data_user$NNTCat[i]=='B+' & MS_Data_user$NNHCat[i]=='B'){
        MS_Data_user$ceRank[i]<-'D'
      }else{
        MS_Data_user$ceRank[i]<-'F'
      }
    }
    
    MS_Data_user_save<<-MS_Data_user
    
    for(i in 1:nrow(MS_Data_user_save)){
      print(i)
      MS_Data_user_save$LL_Ben[i]<-(1-(0.5-1000/((1/MS_Data_user_save$cRDBen[i])+1)/(1000-1000/((1/MS_Data_user_save$cRDBen[i])+1))))*100
      MS_Data_user_save$UL_Ben[i]<-(1-(0.5-1000/((1/MS_Data_user_save$cRDBen[i])-1)/(1000-1000/((1/MS_Data_user_save$cRDBen[i])-1))))*100
      if(MS_Data_user_save$LL_Ben[i]>100){
        MS_Data_user_save$LL_Ben[i]<-100
      }else if(MS_Data_user_save$LL_Ben[i]<0){
        MS_Data_user_save$LL_Ben[i]<-0
      }else{
        MS_Data_user_save$LL_Ben[i]<-MS_Data_user_save$LL_Ben[i]
      }
      if(MS_Data_user_save$UL_Ben[i]>100){
        MS_Data_user_save$UL_Ben[i]<-100
      }else if(MS_Data_user_save$UL_Ben[i]<0){
        MS_Data_user_save$UL_Ben[i]<-0
      }else{
        MS_Data_user_save$UL_Ben[i]<-MS_Data_user_save$UL_Ben[i]
      }
      MS_Data_user_save$LL_Harm[i]<-100-(1-(0.5-1000/((1/MS_Data_user_save$cRDHarm[i])-1)/(1000-1000/((1/MS_Data_user_save$cRDHarm[i])-1))))*100
      MS_Data_user_save$UL_Harm[i]<-100-(1-(0.5-1000/((1/MS_Data_user_save$cRDHarm[i])+1)/(1000-1000/((1/MS_Data_user_save$cRDHarm[i])+1))))*100
      if(MS_Data_user_save$LL_Harm[i]>100){
        MS_Data_user_save$LL_Harm[i]<-100
      }else if(MS_Data_user_save$LL_Harm[i]<0){
        MS_Data_user_save$LL_Harm[i]<-0
      }else{
        MS_Data_user_save$LL_Harm[i]<-MS_Data_user_save$LL_Harm[i]
      }
      if(MS_Data_user_save$UL_Harm[i]>100){
        MS_Data_user_save$UL_Harm[i]<-100
      }else if(MS_Data_user_save$UL_Harm[i]<0){
        MS_Data_user_save$UL_Harm[i]<-0
      }else{
        MS_Data_user_save$UL_Harm[i]<-MS_Data_user_save$UL_Harm[i]
      }
      MS_Data_user_save$MeanLL[i]<-(MS_Data_user_save$LL_Harm[i]+MS_Data_user_save$LL_Ben[i])/2
      MS_Data_user_save$MeanUL[i]<-(MS_Data_user_save$UL_Harm[i]+MS_Data_user_save$UL_Ben[i])/2
      MS_Data_user_save$MeanLL[i]<-format(round(as.numeric(MS_Data_user_save$MeanLL[i]), 2), nsmall = 2)
      MS_Data_user_save$MeanUL[i]<-format(round(as.numeric(MS_Data_user_save$MeanUL[i]), 2), nsmall = 2)
    }
    
    
    MS_Data_user_save<<-MS_Data_user_save
    MS_Data_user<-MS_Data_user_save
    MS_Data_user1<-MS_Data_user[, c("Arm", "cNNT","cNNH","ceRank","MeanLL","MeanUL")]
    MS_Data_user1 <- round_df(MS_Data_user1, digits = 2)
    MS_Data_user1$AVScore<-paste(MS_Data_user1$MeanLL,'-',MS_Data_user1$MeanUL)
    assign("MS_Data_user1", MS_Data_user1, .GlobalEnv)
    print('done!')
    
    meds_values$MS_Data_user1<-MS_Data_user1
    
    selected.meds <- str_extract(input$medBoxs_MS, "^\\D+") %>% unique()
    
    
    rmarkdown::render(input = "snapshot_ms.Rmd",
                      output_file = "snapshot_preview.html",
                      params = list(condition =  condition$selected, 
                                    selected_meds = selected.meds,
                                    disprog_val = input$dis_prog_MS,
                                    rf_val = input$rel_free_MS,
                                    disc_val = input$discont_MS,
                                    hepatox_flu_val = input$heptox_flu_MS,
                                    ijr_dr_val = input$IJR_DR_MS,
                                    ms_data = MS_Data_user1))
    
    xml2::write_html(rvest::html_node(xml2::read_html("snapshot_preview.html"), "body"), file = "snapshot_preview_MS.html")
    
    print('ac1')
    data <- MS_Data_user1[,c("Arm", "cNNT","cNNH","ceRank","AVScore")]
    print('ac2')
    
    output$userDefRes_MS <- renderReactable({
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
          Arm = colDef(header = with_tooltip("Medication", "Medications for the disease/condition included in the analysis.")),
          cNNT = colDef(header = with_tooltip("Composite NNT", "Composite Numbers Needed to Treat: Weighted average of numbers needed to treat to achieve benefits outcomes.")),
          cNNH = colDef(header = with_tooltip("Composite NNH", "Composite Numbers Needed to Harm: Weighted average of numbers needed to treat to achieve harms outcomes.")),
          ceRank = colDef(header = with_tooltip("Clinical Effectiveness Ranking", "Clinical Effectiveness Ranking based on each drug's distance of composite NNT/NNH from the median composite NNT/NNH. Click on 'More Info' for the calculation methodology and examples."),
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
          
          AVScore = colDef(header = with_tooltip("Adjusted Value Score", "Adjusted value score is the estimated percentage of full value after discounting the difference in benefitted vs not benefitted ratio calculated using optimum NNT and observed NNT for the drug. Adjusted value score above 100 is assigned as 100 and adjusted value score below 0 is assigned 0."))
        )
      )
    })
    
    # output$userDefRes_MS<-DT::renderDataTable(datatable(data,
    #                                                     colnames = c('Medication', 'Composite NNT', 'Composite NNH', 'Clinical Effectiveness Ranking', 'Adjusted Value Score'),
    #                                                     escape = F,
    #                                                     rownames = F,
    #                                                     selection = 'single',
    #                                                     filter = 'top',
    #                                                     options = list(
    #                                                       scrollX = TRUE,
    #                                                       scrollY = '40vh',
    #                                                       scrollCollapse = TRUE,
    #                                                       autoWidth = F)) %>% formatStyle(c("Arm", "cNNT","cNNH","ceRank","AVScore"),
    #                                                                                       backgroundColor = styleEqual(c("F",NA,""),
    #                                                                                                                    c("#FF4E4E","#FF4E4E", "#FF4E4E"))) %>%
    #                                             formatStyle('ceRank',
    #                                                         backgroundColor = styleEqual(
    #                                                           c("D", "C", "B", "A"),
    #                                                           c('#FDF59F', '#E3E3E3', '#0074BB', '#A3E982'))
    #                                             )
    # )
    
    output$previewSnapshot_MS <- renderUI({
      includeHTML("snapshot_preview_MS.html")
    })
    
    output$bcbsaResults_MS <-renderUI({
      
      fluidRow(
        h2("Multiple Sclerosis"),
        h4("Use the filters on the left sidebar to select medications, harms weights, and benefit weights"),
        h4("Click the 'Run Analysis' button to update the data in the results table and snapshot. 
           Navigate to the 'Preview Snapshot' tab to preview and/or download the BCBS Snapshot report"),
        br(),
        
        bcbsTabBox(id = "ms_tab_box",
                   title = p(actionButton("more_info_MEDS_MS", label = NULL, icon = icon("info")),
                             shinyjs::hidden(downloadButton("downloadSnapshot_MS", "Download Snapshot"))),
                   width = 12,
                   tabPanel("Results Overview",
                            uiOutput("boxTitle_MS"),
                            reactableOutput('userDefRes_MS'),
                            # dataTableOutput('userDefRes_MS'),
                            
                            # bcbsButton("more_info_MEDS_MS", label = NULL, icon = icon("info")),
                            
                            tags$head(tags$style("#meds_info .modal-footer button {font-size: 20px; line-height: 25px; font-weight: bold; text-transform: uppercase;}")),
                            tags$head(tags$style("#meds_info .modal-header {padding-bottom: 0px; padding-top: 0px;}")),
                            tags$head(tags$style("#meds_info .modal-header button {display: none;}")),
                            
                            bsModal('meds_info_MS', 
                                    title = h2('MEDS Information', style = 'color: #005876; font-weight: bold; line-height: 25px;'),
                                    size = 'large',
                                    trigger = 'more_info_MEDS_MS', 
                                    uiOutput('info_details_MS') 
                            )
                            
                   ),
                   tabPanel("Preview Snapshot",
                            uiOutput("previewSnapshot_MS")#, 
                            # downloadButton("downloadSnapshot_MS", "Download Snapshot")
                   )
        )
      )
      
    })
    
  }
}, ignoreInit = TRUE)


observeEvent(input$more_info_MEDS_MS,{
  output$info_details_MS<-renderUI({
    
    
    div(shiny::HTML(
      '<h4>MS Inclusion/Exclusion Criteria</h4>
                         <p>The following criteria for evidence inclusion for the MEDS Model Analysis was determined through the guidance of the BCBS Plans’ executive and clinical pharmacists.</p><br>
                         <h4>Inclusion Criteria</h4>
                         <br>
                         
                         <h4>Patient Population</h4>
                         <ul>
                            <li>Adults (18 years or older) with RRMS</li>
                            <li>Randomized Controlled Trials: At least 100 patients</li>
                            <li> Observational: At least 1000</li>
                          </ul><br>
                          
                          <h4>Intervention</h4>
                         <ul>
                            <li>Avonex (interferon B-1a)</li>
                            <li>Betaseron (interferon B-1b)</li>
                            <li>Extavia (interferon B-1b)</li>
                            <li>Rebif (interferon B-1a)</li>
                            <li>Plegridy (peginterferon B-1a)</li>
                            <li>Copaxone (glatiramer acetate)</li>
                            <li>Glatopa (glatiramer acetate)</li>
                            <li>Gilenya (fingolimod)</li>
                            <li>Aubagio (teriflunomide)</li>
                            <li>Tecfidera (dimethyl fumarate)</li>
                            <li>Tysabri (natalizumab)</li>
                            <li>Lemtrada (alemtuzumab)</li>
                            <li>Ocrevus (ocrelizumab)</li>
                            <li>Rituxan (rituximab)</li>
                            <li>Mayzent (siponimod)</li>
                          </ul><br>
                          
                          <h4>Comparator</h4>
                         <ul>
                            <li>Head to Head</li>
                            <li>Placebo</li>
                          </ul><br>
                          
                          <h4>Efficacy Outcomes:</h4>
                         <ul>
                            <li>Kurtzke Extended Disability Status Scale (EDSS)</li>
                            <li>% who are relapse free</li>
                          </ul><br>
                          
                          <h4>Safety Outcomes</h4>
                         <ul>
                            <li>Discontinuation due to adverse event</li>
                            <li>Liver toxicity</li>
                            <li>Flu-like reactions</li>
                            <li>Injection site reactions (injectables or infusions)</li>
                            <li>Diarrhea (oral agents)</li>
                          </ul><br>
                          
                          <h4>Timing</h4>
                         <ul>
                            <li>6 months – 3 years</li>
                          </ul><br>
                          
                          <h4>Setting</h4>
                         <ul>
                            <li>Multiple included (inpatient, primary care, outpatient, home care)</li>
                          </ul><br>
                          
                           <h4>Exclusion Criteria</h4>
                           <ul>
                           <li>Studies involving patients under 18</li>
                           <li>Single-arm studies</li>
                           <li>Open label long term extension studies (unless the study reported the blinded phase before crossover)</li>
                           <li>Randomized controlled trials with sample size of less than 100 patients</li>
                           </ul>'),
      div(bcbsButton('ref_list_MS','References'))
      
    )
    
    
    
    # div(shiny::HTML('<h4>RA Specialty Drugs and Outcome Measures Selection</h4>
    #                      <p>The RA drug set and outcome measures were objectively selected based on a series of webinar discussions and surveys with BCBS Plans.
    #                      The audience were both executive and clinical pharmacists from each BCBS Plan with appropriate experience and knowledge in rheumatoid arthritis treatments.
    #                      Since the MEDS Model utilizes Number Needed to Treat and Number Needed to Harm, the executive and clinical pharmacists were aware that the outcome measures
    #                      needed to have dichotomous characteristics.</p><br>
    #                      <h4>Selected RA Specialty Drugs</h4>
    #                      <ul>
    #                         <li>Orencia (abatacept)</li>
    #                         <li>Humira (adalimumab)</li>
    #                         <li>Amjevita (adaliumumab)</li>
    #                         <li>Cimzia (certolizumab pegol)</li>
    #                         <li>Enbrel (etanercept)</li>
    #                         <li>Simponi (golimumab)</li>
    #                         <li>Remicade (infliximab)</li>
    #                         <li>Inflectra (infliximab)</li>
    #                         <li>Renflexis (infliximab)</li>
    #                         <li>Rituxan (rituximab)</li>
    #                         <li>Actmera (tocilizumab)</li>
    #                         <li>Xeljanz (tofacitinib)</li>
    #                         <li>Upadacitinib</li>
    #                       </ul><br>
    #                     <h4>Selected RA Outcome Measures</h4>
    #                     <h5>Efficacy</h5>
    #                     <ul>
    #                     <li>American College of Rheumatology (ACR) 50 Criteria</li>
    #                     <li>Disease Activity Score (DAS) - 28</li>
    #                     <li>Health Assessment Questionnaire – Disability Index (HAQ-DI)</li>
    #                     </ul>
    #                     <h5>Safety</h5>
    #                     <ul>
    #                     <li>Discontinuation due to adverse event</li>
    #                     <li>Serious Infection (pneumonia or upper respiratory infection, infection requiring IV antibodies or any hospitalization due to infection)</li>
    #                     <li>Injection Site Reaction (Injectable Drug) or gastrointestinal side effects (Oral Drug)</li>
    #                     </ul>'),
    #     div(bcbsButton('ref_list_MS','References'))
    # )
  })
})

observeEvent(input$info_methods_MS,{
  output$info_details_MS<-renderUI({
    div(shiny::HTML(
      '<h4>MS Inclusion/Exclusion Criteria</h4>
                         <p>The following criteria for evidence inclusion for the MEDS Model Analysis was determined through the guidance of the BCBS Plans’ executive and clinical pharmacists.</p><br>
                         <h4>Inclusion Criteria</h4>
                         <br>
                         
                         <h4>Patient Population</h4>
                         <ul>
                            <li>Adults (18 years or older) with RRMS</li>
                            <li>Randomized Controlled Trials: At least 100 patients</li>
                            <li> Observational: At least 1000</li>
                          </ul><br>
                          
                          <h4>Intervention</h4>
                         <ul>
                            <li>Avonex (interferon B-1a)</li>
                            <li>Betaseron (interferon B-1b)</li>
                            <li>Extavia (interferon B-1b)</li>
                            <li>Rebif (interferon B-1a)</li>
                            <li>Plegridy (peginterferon B-1a)</li>
                            <li>Copaxone (glatiramer acetate)</li>
                            <li>Glatopa (glatiramer acetate)</li>
                            <li>Gilenya (fingolimod)</li>
                            <li>Aubagio (teriflunomide)</li>
                            <li>Tecfidera (dimethyl fumarate)</li>
                            <li>Tysabri (natalizumab)</li>
                            <li>Lemtrada (alemtuzumab)</li>
                            <li>Ocrevus (ocrelizumab)</li>
                            <li>Rituxan (rituximab)</li>
                            <li>Mayzent (siponimod)</li>
                          </ul><br>
                          
                          <h4>Comparator</h4>
                         <ul>
                            <li>Head to Head</li>
                            <li>Placebo</li>
                          </ul><br>
                          
                          <h4>Efficacy Outcomes:</h4>
                         <ul>
                            <li>Kurtzke Extended Disability Status Scale (EDSS)</li>
                            <li>% who are relapse free</li>
                          </ul><br>
                          
                          <h4>Safety Outcomes</h4>
                         <ul>
                            <li>Discontinuation due to adverse event</li>
                            <li>Liver toxicity</li>
                            <li>Flu-like reactions</li>
                            <li>Injection site reactions (injectables or infusions)</li>
                            <li>Diarrhea (oral agents)</li>
                          </ul><br>
                          
                          <h4>Timing</h4>
                         <ul>
                            <li>6 months – 3 years</li>
                          </ul><br>
                          
                          <h4>Setting</h4>
                         <ul>
                            <li>Multiple included (inpatient, primary care, outpatient, home care)</li>
                          </ul><br>
                          
                           <h4>Exclusion Criteria</h4>
                           <ul>
                           <li>Studies involving patients under 18</li>
                           <li>Single-arm studies</li>
                           <li>Open label long term extension studies (unless the study reported the blinded phase before crossover)</li>
                           <li>Randomized controlled trials with sample size of less than 100 patients</li>
                           </ul>'),
      div(bcbsButton('ref_list_MS','References'))
    )
  })
})

observeEvent(input$ref_list_MS,{
  output$info_details_MS <- renderUI({
    div(shiny::HTML("<h3>References</h3>
                            <ul>
                            <li>Kappos L, Bar-Or A, Cree BAC, Fox RJ, Giovannoni G, Gold R, Vermersch P, Arnold DL, Arnould S, Scherz T, Wolf C, Wallström E, Dahlke F; EXPAND Clinical Investigators. Siponimod versus placebo in secondary progressive multiple sclerosis (EXPAND): a double-blind, randomised, phase 3 study. Lancet. 2018 Mar 31;391(10127):1263-1273. doi: 10.1016/S0140-6736(18)30475-6. Epub 2018 Mar 23. Erratum in: Lancet. 2018 Nov 17;392(10160):2170. PubMed PMID: 29576505.</li>
                            <li>Kapoor R, Ho PR, Campbell N, Chang I, Deykin A, Forrestal F, Lucas N, Yu B, Arnold DL, Freedman MS, Goldman MD, Hartung HP, Havrdová EK, Jeffery D, Miller A, Sellebjerg F, Cadavid D, Mikol D, Steiner D; ASCEND investigators. Effect of natalizumab on disease progression in secondary progressive multiple sclerosis (ASCEND): a phase 3, randomised, double-blind, placebo-controlled trial with an open-label extension. Lancet Neurol. 2018 May;17(5):405-415. doi: 10.1016/S1474-4422(18)30069-3. Epub 2018 Mar 12. PubMed PMID: 29545067.</li>
                            <li>Comi G, Patti F, Rocca MA, Mattioli FC, Amato MP, Gallo P, Centonze D, Pozzilli C, Saccà F, Bergh FT, Bartezaghi M, Turrini R, Filippi M; Golden Study Group. Efficacy of fingolimod and interferon beta-1b on cognitive, MRI, and clinical outcomes in relapsing-remitting multiple sclerosis: an 18-month, open-label, rater-blinded, randomised, multicentre study (the GOLDEN study). J Neurol. 2017 Dec;264(12):2436-2449. doi: 10.1007/s00415-017-8642-5. Epub 2017 Oct 23. PubMed PMID: 29063244; PubMed Central PMCID: PMC5688215.</li>
                            <li>Montalban X, Hauser SL, Kappos L, Arnold DL, Bar-Or A, Comi G, de Seze J, Giovannoni G, Hartung HP, Hemmer B, Lublin F, Rammohan KW, Selmaj K, Traboulsee A, Sauter A, Masterman D, Fontoura P, Belachew S, Garren H, Mairon N, Chin P, Wolinsky JS; ORATORIO Clinical Investigators. Ocrelizumab versus Placebo in Primary Progressive Multiple Sclerosis. N Engl J Med. 2017 Jan 19;376(3):209-220. doi: 10.1056/NEJMoa1606468. Epub 2016 Dec 21. PubMed PMID: 28002688.</li>
                            <li>Hauser SL, Bar-Or A, Comi G, Giovannoni G, Hartung HP, Hemmer B, Lublin F, Montalban X, Rammohan KW, Selmaj K, Traboulsee A, Wolinsky JS, Arnold DL, Klingelschmitt G, Masterman D, Fontoura P, Belachew S, Chin P, Mairon N, Garren H, Kappos L; OPERA I and OPERA II Clinical Investigators. Ocrelizumab versus Interferon Beta-1a in Relapsing Multiple Sclerosis. N Engl J Med. 2017 Jan 19;376(3):221-234. doi: 10.1056/NEJMoa1601277. Epub 2016 Dec 21. PubMed PMID: 28002679.</li>
                            <li>Lublin F, Miller DH, Freedman MS, Cree BAC, Wolinsky JS, Weiner H, Lubetzki C, Hartung HP, Montalban X, Uitdehaag BMJ, Merschhemke M, Li B, Putzki N, Liu FC, Häring DA, Kappos L; INFORMS study investigators. Oral fingolimod in primary progressive multiple sclerosis (INFORMS): a phase 3, randomised, double-blind, placebo-controlled trial. Lancet. 2016 Mar 12;387(10023):1075-1084. doi: 10.1016/S0140-6736(15)01314-8. Epub 2016 Jan 28. Erratum in: Lancet. 2017 Jan 21;389(10066):254. PubMed PMID: 26827074.</li>
                            <li>Cohen J, Belova A, Selmaj K, Wolf C, Sormani MP, Oberyé J, van den Tweel E, Mulder R, Koper N, Voortman G, Barkhof F; Glatiramer Acetate Clinical Trial to Assess Equivalence With Copaxone (GATE) Study Group. Equivalence of Generic Glatiramer Acetate in Multiple Sclerosis: A Randomized Clinical Trial. JAMA Neurol. 2015 Dec;72(12):1433-41. doi: 10.1001/jamaneurol.2015.2154. PubMed PMID: 26458034.</li>
                            <li>Calabresi PA, Kieseier BC, Arnold DL, Balcer LJ, Boyko A, Pelletier J, Liu S, Zhu Y, Seddighzadeh A, Hung S, Deykin A; ADVANCE Study Investigators. Pegylated interferon β-1a for relapsing-remitting multiple sclerosis (ADVANCE): a randomised, phase 3, double-blind study. Lancet Neurol. 2014 Jul;13(7):657-65. doi: 10.1016/S1474-4422(14)70068-7. Epub 2014 Apr 30. PubMed PMID: 24794721.</li>
                            <li>Calabresi PA, Radue EW, Goodin D, Jeffery D, Rammohan KW, Reder AT, Vollmer T, Agius MA, Kappos L, Stites T, Li B, Cappiello L, von Rosenstiel P, Lublin FD. Safety and efficacy of fingolimod in patients with relapsing-remitting multiple sclerosis (FREEDOMS II): a double-blind, randomised, placebo-controlled, phase 3 trial. Lancet Neurol. 2014 Jun;13(6):545-56. doi: 10.1016/S1474-4422(14)70049-3. Epub 2014 Mar 28. Erratum in: Lancet Neurol. 2013 Jun;13(6):536. PubMed PMID: 24685276.</li>
                            <li>Vollmer TL, Sorensen PS, Selmaj K, Zipp F, Havrdova E, Cohen JA, Sasson N, Gilgun-Sherki Y, Arnold DL; BRAVO Study Group. A randomized placebo-controlled phase III trial of oral laquinimod for multiple sclerosis. J Neurol. 2014 Apr;261(4):773-83. doi: 10.1007/s00415-014-7264-4. Epub 2014 Feb 18. PubMed PMID: 24535134.</li>
                            <li>Confavreux C, O'Connor P, Comi G, Freedman MS, Miller AE, Olsson TP, Wolinsky JS, Bagulho T, Delhay JL, Dukovic D, Truffinet P, Kappos L; TOWER Trial Group. Oral teriflunomide for patients with relapsing multiple sclerosis (TOWER): a randomised, double-blind, placebo-controlled, phase 3 trial. Lancet Neurol. 2014 Mar;13(3):247-56. doi: 10.1016/S1474-4422(13)70308-9. Epub 2014 Jan 23. PubMed PMID: 24461574.</li>
                            <li>Vermersch P, Czlonkowska A, Grimaldi LM, Confavreux C, Comi G, Kappos L, Olsson TP, Benamor M, Bauer D, Truffinet P, Church M, Miller AE, Wolinsky JS, Freedman MS, O'Connor P; TENERE Trial Group. Teriflunomide versus subcutaneous interferon beta-1a in patients with relapsing multiple sclerosis: a randomised, controlled phase 3 trial. Mult Scler. 2014 May;20(6):705-16. doi: 10.1177/1352458513507821. Epub 2013 Oct 14. PubMed PMID: 24126064.</li>
                            <li>Lublin FD, Cofield SS, Cutter GR, Conwit R, Narayana PA, Nelson F, Salter AR, Gustafson T, Wolinsky JS; CombiRx Investigators. Randomized study combining interferon and glatiramer acetate in multiple sclerosis. Ann Neurol. 2013 Mar;73(3):327-40. doi: 10.1002/ana.23863. Epub 2013 Mar 11. PubMed PMID: 23424159; PubMed Central PMCID: PMC3631288.</li>
                            <li>Cohen JA, Coles AJ, Arnold DL, Confavreux C, Fox EJ, Hartung HP, Havrdova E, Selmaj KW, Weiner HL, Fisher E, Brinar VV, Giovannoni G, Stojanovic M, Ertik BI, Lake SL, Margolin DH, Panzara MA, Compston DA; CARE-MS I investigators. Alemtuzumab versus interferon beta 1a as first-line treatment for patients with relapsing-remitting multiple sclerosis: a randomised controlled phase 3 trial. Lancet. 2012 Nov 24;380(9856):1819-28. doi: 10.1016/S0140-6736(12)61769-3. Epub 2012 Nov 1. PubMed PMID: 23122652.</li>
                            <li>Coles AJ, Twyman CL, Arnold DL, Cohen JA, Confavreux C, Fox EJ, Hartung HP, Havrdova E, Selmaj KW, Weiner HL, Miller T, Fisher E, Sandbrink R, Lake SL, Margolin DH, Oyuela P, Panzara MA, Compston DA; CARE-MS II investigators. Alemtuzumab for patients with relapsing multiple sclerosis after disease-modifying therapy: a randomised controlled phase 3 trial. Lancet. 2012 Nov 24;380(9856):1829-39. doi: 10.1016/S0140-6736(12)61768-1. Epub 2012 Nov 1. PubMed PMID: 23122650.</li>
                            <li>Gold R, Kappos L, Arnold DL, Bar-Or A, Giovannoni G, Selmaj K, Tornatore C, Sweetser MT, Yang M, Sheikh SI, Dawson KT; DEFINE Study Investigators. Placebo-controlled phase 3 study of oral BG-12 for relapsing multiple sclerosis.  N Engl J Med. 2012 Sep 20;367(12):1098-107. Erratum in: N Engl J Med. 2012 Dec 13;367(24):2362. PubMed PMID: 22992073.</li>
                            <li>Fox RJ, Miller DH, Phillips JT, Hutchinson M, Havrdova E, Kita M, Yang M, Raghupathi K, Novas M, Sweetser MT, Viglietta V, Dawson KT; CONFIRM Study Investigators. Placebo-controlled phase 3 study of oral BG-12 or glatiramer in multiple sclerosis. N Engl J Med. 2012 Sep 20;367(12):1087-97. Erratum in: N Engl J Med. 2012 Oct 25;367(17):1673. PubMed PMID: 22992072.</li>
                            <li>Saida T, Kikuchi S, Itoyama Y, Hao Q, Kurosawa T, Nagato K, Tang D, Zhang-Auberson L, Kira J. A randomized, controlled trial of fingolimod (FTY720) in Japanese patients with multiple sclerosis. Mult Scler. 2012 Sep;18(9):1269-77. doi: 10.1177/1352458511435984. Epub 2012 Feb 21. PubMed PMID: 22354739.</li>
                            <li>Kappos L, Li D, Calabresi PA, O'Connor P, Bar-Or A, Barkhof F, Yin M, Leppert D, Glanzman R, Tinbergen J, Hauser SL. Ocrelizumab in relapsing-remitting multiple sclerosis: a phase 2, randomised, placebo-controlled, multicentre trial. Lancet. 2011 Nov 19;378(9805):1779-87. doi: 10.1016/S0140-6736(11)61649-8. Epub 2011 Oct 31. PubMed PMID: 22047971.</li>
                            <li>O'Connor P, Wolinsky JS, Confavreux C, Comi G, Kappos L, Olsson TP, Benzerdjeb H, Truffinet P, Wang L, Miller A, Freedman MS; TEMSO Trial Group. Randomized trial of oral teriflunomide for relapsing multiple sclerosis. N Engl J Med. 2011 Oct 6;365(14):1293-303. doi: 10.1056/NEJMoa1014656. PubMed PMID: 21991951.</li>
                            <li>Cohen JA, Barkhof F, Comi G, Hartung HP, Khatri BO, Montalban X, Pelletier J, Capra R, Gallo P, Izquierdo G, Tiel-Wilck K, de Vera A, Jin J, Stites T, Wu S, Aradhye S, Kappos L; TRANSFORMS Study Group. Oral fingolimod or intramuscular interferon for relapsing multiple sclerosis. N Engl J Med. 2010 Feb 4;362(5):402-15. doi: 10.1056/NEJMoa0907839. Epub 2010 Jan 20. PubMed PMID: 20089954.</li>
                            <li>Kappos L, Radue EW, O'Connor P, Polman C, Hohlfeld R, Calabresi P, Selmaj K, Agoropoulou C, Leyk M, Zhang-Auberson L, Burtin P; FREEDOMS Study Group. A placebo-controlled trial of oral fingolimod in relapsing multiple sclerosis. N Engl J Med. 2010 Feb 4;362(5):387-401. doi: 10.1056/NEJMoa0909494. Epub 2010 Jan 20. PubMed PMID: 20089952.</li>
                            <li>Hawker K, O'Connor P, Freedman MS, Calabresi PA, Antel J, Simon J, Hauser S, Waubant E, Vollmer T, Panitch H, Zhang J, Chin P, Smith CH; OLYMPUS trial group. Rituximab in patients with primary progressive multiple sclerosis: results of a randomized double-blind placebo-controlled multicenter trial. Ann Neurol. 2009 Oct;66(4):460-71. doi: 10.1002/ana.21867. PubMed PMID: 19847908.</li>
                            <li>O'Connor P, Filippi M, Arnason B, Comi G, Cook S, Goodin D, Hartung HP, Jeffery D, Kappos L, Boateng F, Filippov V, Groth M, Knappertz V, Kraus C, Sandbrink R, Pohl C, Bogumil T; BEYOND Study Group, O'Connor P, Filippi M, Arnason B, Cook S, Goodin D, Hartung HP, Kappos L, Jeffery D, Comi G. 250 microg or 500 microg interferon beta-1b versus 20 mg glatiramer acetate in relapsing-remitting multiple sclerosis: a prospective, randomised, multicenter study. Lancet Neurol. 2009 Oct;8(10):889-97. doi: 10.1016/S1474-4422(09)70226-1. Epub 2009 Sep 2. Erratum in: Lancet Neurol. 2009 Nov;8(11):981. Lancet Neurol. 2011 Feb;10(2):115. Lancet Neurol. 2012 Jan;11(1):27. Cree, B [added]; Harung, H-P [corrected to Hartung, H-P]. PubMed PMID: 19729344.</li>
                            <li>CAMMS223 Trial Investigators, Coles AJ, Compston DA, Selmaj KW, Lake SL, Moran S, Margolin DH, Norris K, Tandon PK. Alemtuzumab vs. interferon beta-1a in early multiple sclerosis. N Engl J Med. 2008 Oct 23;359(17):1786-801. doi: 10.1056/NEJMoa0802670. PubMed PMID: 18946064.</li>
                            <li>Mikol DD, Barkhof F, Chang P, Coyle PK, Jeffery DR, Schwid SR, Stubinski B, Uitdehaag B; REGARD study group. Comparison of subcutaneous interferon beta-1a with glatiramer acetate in patients with relapsing multiple sclerosis (the REbif vs Glatiramer Acetate in Relapsing MS Disease [REGARD] study): a multicentre, randomised, parallel, open-label trial. Lancet Neurol. 2008 Oct;7(10):903-14. doi: 10.1016/S1474-4422(08)70200-X. Epub 2008 Sep 11. PubMed PMID: 18789766.</li>
                            <li>Hauser SL, Waubant E, Arnold DL, Vollmer T, Antel J, Fox RJ, Bar-Or A, Panzara M, Sarkar N, Agarwal S, Langer-Gould A, Smith CH; HERMES Trial Group. B-cell depletion with rituximab in relapsing-remitting multiple sclerosis. N Engl J Med. 2008 Feb 14;358(7):676-88. doi: 10.1056/NEJMoa0706383. PubMed PMID: 18272891.</li>
                            <li>Wolinsky JS, Narayana PA, O'Connor P, Coyle PK, Ford C, Johnson K, Miller A, Pardo L, Kadosh S, Ladkani D; PROMiSe Trial Study Group. Glatiramer acetate in primary progressive multiple sclerosis: results of a multinational, multicenter, double-blind, placebo-controlled trial. Ann Neurol. 2007 Jan;61(1):14-24. PubMed PMID: 17262850.</li>
                            <li>Etemadifar M, Janghorbani M, Shaygannejad V. Comparison of Betaferon, Avonex, and Rebif in treatment of relapsing-remitting multiple sclerosis. Acta Neurol Scand. 2006 May;113(5):283-7. PubMed PMID: 16629762.</li>
                            <li>O'Connor PW, Li D, Freedman MS, Bar-Or A, Rice GP, Confavreux C, Paty DW, Stewart JA, Scheyer R; Teriflunomide Multiple Sclerosis Trial Group; University of British Columbia MS/MRI Research Group. A Phase II study of the safety and efficacy of teriflunomide in multiple sclerosis with relapses. Neurology. 2006 Mar 28;66(6):894-900. PubMed PMID: 16567708.</li>
                            <li>Polman CH, O'Connor PW, Havrdova E, Hutchinson M, Kappos L, Miller DH, Phillips JT, Lublin FD, Giovannoni G, Wajgt A, Toal M, Lynn F, Panzara MA, Sandrock AW; AFFIRM Investigators. A randomized, placebo-controlled trial of natalizumab for relapsing multiple sclerosis. N Engl J Med. 2006 Mar 2;354(9):899-910. PubMed PMID: 16510744.</li>
                            <li>Panitch H, Miller A, Paty D, Weinshenker B; North American Study Group on Interferon beta-1b in Secondary Progressive MS. Interferon beta-1b in secondary progressive MS: results from a 3-year controlled study. Neurology. 2004 Nov 23;63(10):1788-95. PubMed PMID: 15557491.</li>
                            <li>Andersen O, Elovaara I, Färkkilä M, Hansen HJ, Mellgren SI, Myhr KM, Sandberg-Wollheim M, Soelberg Sørensen P. Multicentre, randomised, double blind, placebo controlled, phase III study of weekly, low dose, subcutaneous interferon beta-1a in secondary progressive multiple sclerosis. J Neurol Neurosurg Psychiatry. 2004 May;75(5):706-10. PubMed PMID: 15090564; PubMed Central PMCID: PMC1763573.</li>
                            <li>Miller DH, Khan OA, Sheremata WA, Blumhardt LD, Rice GP, Libonati MA, Willmer-Hulme AJ, Dalton CM, Miszkiel KA, O'Connor PW; International Natalizumab Multiple Sclerosis Trial Group. A controlled trial of natalizumab for relapsing multiple sclerosis. N Engl J Med. 2003 Jan 2;348(1):15-23. PubMed PMID: 12510038.</li>
                            <li>Panitch H, Goodin DS, Francis G, Chang P, Coyle PK, O'Connor P, Monaghan E, Li D, Weinshenker B; EVIDENCE Study Group. EVidence of Interferon Dose-response: Europian North American Compartative Efficacy; University of British Columbia MS/MRI Research Group. Randomized, comparative study of interferon beta-1a treatment regimens in MS: The EVIDENCE Trial. Neurology. 2002 Nov 26;59(10):1496-506. PubMed PMID: 12451188.</li>
                            <li>Durelli L, Verdun E, Barbero P, Bergui M, Versino E, Ghezzi A, Montanari E, Zaffaroni M; Independent Comparison of Interferon (INCOMIN) Trial Study Group. Every-other-day interferon beta-1b versus once-weekly interferon beta-1a for multiple sclerosis: results of a 2-year prospective randomised multicentre study (INCOMIN). Lancet. 2002 Apr 27;359(9316):1453-60. PubMed PMID: 11988242.</li>
                            <li>Randomised double-blind placebo-controlled study of interferon beta-1a in relapsing/remitting multiple sclerosis. PRISMS (Prevention of Relapses and Disability by Interferon beta-1a Subcutaneously in Multiple Sclerosis) Study Group. Lancet. 1998 Nov 7;352(9139):1498-504. Erratum in: Lancet 1999 Feb 20;353(9153):678. PubMed PMID: 9820297.</li>
                            <li>Johnson KP, Brooks BR, Cohen JA, Ford CC, Goldstein J, Lisak RP, Myers LW, Panitch HS, Rose JW, Schiffer RB. Copolymer 1 reduces relapse rate and improves disability in relapsing-remitting multiple sclerosis: results of a phase III multicenter, double-blind placebo-controlled trial. The Copolymer 1 Multiple Sclerosis Study Group. Neurology. 1995 Jul;45(7):1268-76. PubMed PMID: 7617181.</li>
                            <li>Interferon beta-1b is effective in relapsing-remitting multiple sclerosis. I. Clinical results of a multicenter, randomized, double-blind, placebo-controlled trial. The IFNB Multiple Sclerosis Study Group. Neurology. 1993 Apr;43(4):655-61. PubMed PMID: 8469318.</li>
                    </ul>"),
        div(bcbsButton('info_methods_MS','Methods'))
    )
  })
})

output$downloadSnapshot_MS <- downloadHandler(
  filename = function() {
    paste0('MEDS_Snapshot_Multiple_Sclerosis.html')
  },
  
  content = function(file) {
    src <- normalizePath('snapshot_ms.Rmd')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'snapshot_ms.Rmd', overwrite = TRUE)
    
    library(rmarkdown)
    
    selected.meds <- str_extract(input$medBoxs_MS, "^\\D+") %>% unique()
    
    out <- rmarkdown::render(input = "snapshot_ms.Rmd",
                             output_file = "snapshot_preview.html",
                             params = list(condition =  condition$selected, 
                                           selected_meds = selected.meds,
                                           disprog_val = input$dis_prog_MS,
                                           rf_val = input$rel_free_MS,
                                           disc_val = input$discont_MS,
                                           hepatox_flu_val = input$heptox_flu_MS,
                                           ijr_dr_val = input$IJR_DR_MS,
                                           ms_data = MS_Data_user1))
    
    file.rename(out, file)
  }
)
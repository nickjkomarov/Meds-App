shinyjs::hide("customFilters_RA")
shinyjs::hide("update")


observeEvent(c(input$ra, input$ra2), {
  condition$selected <- "Rheumatoid Arthritis"
}, ignoreInit = TRUE)

# Reset filters to default values
observe({
  req(input$reset)
  
  if (condition$selected == "Rheumatoid Arthritis") {
    
    updatePickerInput(session, "medBoxs", "Select Medications", RA_Data$Arm, RA_Data$Arm)
    updateSliderInput(session, 'ACR50', "ACR 50", 60, 0, 100)
    updateSliderInput(session, 'DAS28', "DAS 28", 30, 0, 100)
    updateSliderInput(session, 'HAQ', "HAQ", 10, 0, 100)
    updateSliderInput(session, 'severeAE', "Severe AE", 7, 0, 100)
    updateSliderInput(session, 'IJR_diarrhea', "IJR/Diarrhea", 3, 0, 100)
    updateSliderInput(session, 'discont', "Discontinuation", 90, 0, 100)
  }
})

output$customFilters_RA <- renderUI({
  div(id = "customizeFilters",
      fluidRow(
        div(style = "margin-left: 15px; margin-top: -10px;",
            
              pickerInput("medBoxs", label = "Select Medications",
                          choices = RA_Data$Arm,
                          selected = RA_Data$Arm,
                          multiple = TRUE,
                          width = 300,
                          options = list(`live-search` = TRUE,
                                         `selected-text-format` = "count > 3",
                                         `actions-box` = TRUE))
        ),
        div(id="inline",  
            hr(),
            tags$h5("Benefits Outcomes"),
            sliderInput('ACR50',"ACR 50", 0,100,60),
           
            sliderInput('DAS28',"DAS 28", 0,100,30),
           
            sliderInput('HAQ',"HAQ", 0,100,10),
            uiOutput('benefitAlert'),
            hr(),
            tags$h5("Harms Outcomes"),
            sliderInput('severeAE',"Severe AE", 0,100,7),
            sliderInput('IJR_diarrhea',"IJR/Diarrhea", 0,100,3),
            sliderInput('discont',"Discontinuation", 0,100,90),
            uiOutput('harmAlert')
        )
      )
  )
})


observeEvent(c(input$ra, input$ra2), {
  shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")
  # shinyjs::hide('ra')
  # shinyjs::hide('ms')
  # shinyjs::hide('hap')
  # shinyjs::hide('hao')
  # shinyjs::hide('sa')
  # shinyjs::hide('pp')
  shinyjs::hide('homepage')
  shinyjs::show('customFilters_RA')
  shinyjs::show('update')
  shinyjs::show('bcbsaResults_RA')
  
  shinyjs::hide('customFilters_MS')
  shinyjs::hide('bcbsaResults_MS')
  
  shinyjs::hide('customFilters_HP')
  shinyjs::hide('bcbsaResults_HP')
  
  shinyjs::hide('customFilters_HO')
  shinyjs::hide('bcbsaResults_HO')
  
  shinyjs::hide('customFilters_SA')
  shinyjs::hide('bcbsaResults_SA')
  
  shinyjs::hide('customFilters_PP')
  shinyjs::hide('bcbsaResults_PP')
}, ignoreInit = TRUE)

# Validation: Sum of sliders must equal 100
observe({
  
  if (sum(input$ACR50, input$DAS28, input$HAQ) != 100) {
    shinyjs::disable('nexttoHarms')
    shinyjs::disable('nexttoHarms1')
    # shinyjs::show('nexttoHarms_warning')
  } else {
    shinyjs::enable('nexttoHarms')
    shinyjs::enable('nexttoHarms1')
    # shinyjs::hide('nexttoHarms_warning')
  }
  
  if (sum(input$severeAE, input$IJR_diarrhea, input$discont) != 100) {
    shinyjs::disable('nexttoMedList')
    shinyjs::disable('nexttoMedList1')
    # shinyjs::show('nexttoMedList_warning')
  } else {
    shinyjs::enable('nexttoMedList')
    shinyjs::enable('nexttoMedList1')
    # shinyjs::hide('nexttoMedList_warning')
  }
  
  if (is.null(input$medBoxs)) {
    shinyjs::disable('runModel')
    shinyjs::show('runModel_warning')
  } else {
    shinyjs::enable('runModel')
    shinyjs::hide('runModel_warning')
  }
})

observe({
  req(input$ACR50, input$DAS28)
  max.value <- 100 - (input$ACR50 + input$DAS28)
  if(max.value>=0){
    output$benefitAlert<-NULL
    updateSliderInput(session, "HAQ", value = max.value, min = 0, max = max.value, step = 1)
    shinyjs::enable('update')
  }else{
    output$benefitAlert<-renderUI({div(style='width:300; color: red !important;',
                                       h6('Total weight cannot be more than 100!'))})
    # shinyalert("Invalid!", "Sum weight of all benefit outcomes cannot be more than 100. Update the weights using slider to run the analysis.", type = "error")
    shinyjs::disable('update')
  }
})

observe({
  req(input$ra_tab_box)
  
  if (input$ra_tab_box == "Preview Snapshot") {
    shinyjs::hide("more_info_MEDS")
    shinyjs::show("downloadSnapshot_RA")
  } else {
    shinyjs::show("more_info_MEDS")
    shinyjs::hide("downloadSnapshot_RA")
  }
})

observe({
  req(input$severeAE, input$IJR_diarrhea)
  max.value <- 100 - (input$severeAE + input$IJR_diarrhea)
  if(max.value>=0){
    output$harmAlert<-NULL
    updateSliderInput(session, "discont", value = max.value, min = 0, max = max.value, step = 1)
    shinyjs::enable('update')}else{
      output$harmAlert<-renderUI({div(style='width:300; color: red !important;',
                                      h6('Total weight cannot be more than 100!'))})
      
      # shinyalert("Invalid!", "Sum weight of all harms outcomes cannot be more than 100. Update the weights using slider to run the analysis.", type = "error")
      shinyjs::disable('update')
    }
})

output$boxTitle_RA <- renderUI({
  req(input$update || input$ra || input$ra2)

  if (isolate(input$ACR50) == 60 &&
      isolate(input$DAS28) == 30 &&
      isolate(input$HAQ) == 10 &&
      isolate(input$severeAE) == 7 &&
      isolate(input$IJR_diarrhea) == 3 &&
      isolate(input$discont) == 90 &&
      length(isolate(input$medBoxs)) == length(RA_Data$Arm)) {

    h3('Results from BCBSA using default weights')

  } else {

    h3('Results from BCBSA using user-defined weights')
  }

})


observeEvent(c(input$ra, input$ra2, input$update), {
  
  if(isolate(condition$selected)=='Rheumatoid Arthritis'){
    meds_values$RA_MEDS_Sel<-isolate(input$medBoxs)
    meds_values$ACR50<-isolate(input$ACR50)
    meds_values$DAS28<-isolate(input$DAS28)
    meds_values$HAQ<-isolate(input$HAQ)
    meds_values$SAE<-isolate(input$severeAE)
    meds_values$IJRDia<-isolate(input$IJR_diarrhea)
    meds_values$Discont<-isolate(input$discont)
    print(meds_values$ACR50)
    print(meds_values$DAS28)
    print(meds_values$HAQ)
    print(meds_values$SAE)
    print(meds_values$IJRDia)
    print(meds_values$Discont)
    
    RA_Data_user<-openxlsx::read.xlsx('Data/RA/RA_Data.xlsx', 'Sheet1',startRow = 1,
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
    RA_Data_user<-RA_Data_user[, !(colnames(RA_Data_user) %in% cols_remove)]
    RA_Data_user<-RA_Data_user[RA_Data_user$Arm %in% isolate(meds_values$RA_MEDS_Sel),]
    output$benefitOutcomes<-NULL
    output$harmsOutcomes<-NULL
    output$selectMEDS<-NULL
    wtACR<-isolate(meds_values$ACR50)/100
    wtDAS<-isolate(meds_values$DAS28)/100
    wtHAQ<-isolate(meds_values$HAQ)/100
    
    print('a')
    for(i in 1:nrow(RA_Data_user)){
      if(!is.na(RA_Data_user$ACR50.RD[i]) & !is.na(RA_Data_user$DAS28ESR.RD[i]) & !is.na(RA_Data_user$HAQ.MCID.RD[i])){
        RA_Data_user$cNNT[i]<-1/(wtACR*RA_Data_user$ACR50.RD[i]+wtDAS*RA_Data_user$DAS28ESR.RD[i]+wtHAQ*RA_Data_user$HAQ.MCID.RD[i])
      }else if(!is.na(RA_Data_user$ACR50.RD[i]) & is.na(RA_Data_user$DAS28ESR.RD[i]) & !is.na(RA_Data_user$HAQ.MCID.RD[i])){
        if(!is.na(RA_Data_user$DAS28CRP.RD[i])){
          RA_Data_user$cNNT[i]<-1/(wtACR*RA_Data_user$ACR50.RD[i]+wtDAS*RA_Data_user$DAS28CRP.RD[i]+wtHAQ*RA_Data_user$HAQ.MCID.RD[i])
        }else{
          RA_Data_user$cNNT[i]<-1/(wtACR*RA_Data_user$ACR50.RD[i]+wtDAS*0.5*RA_Data_user$ACR50.RD[i]+wtDAS*0.5*RA_Data_user$HAQ.MCID.RD[i]+wtHAQ*RA_Data_user$HAQ.MCID.RD[i])
        }
      }else if(is.na(RA_Data_user$ACR50.RD[i]) & !is.na(RA_Data_user$DAS28ESR.RD[i]) & !is.na(RA_Data_user$HAQ.MCID.RD[i])){
        RA_Data_user$cNNT[i]<-1/(wtACR*0.5*RA_Data_user$DAS28ESR.RD[i]+wtACR*0.5*RA_Data_user$HAQ.MCID.RD[i]+wtDAS*RA_Data_user$DAS28ESR.RD[i]+wtHAQ*RA_Data_user$HAQ.MCID.RD[i])
      }else if(is.na(RA_Data_user$ACR50.RD[i]) & is.na(RA_Data_user$DAS28ESR.RD[i]) & !is.na(RA_Data_user$DAS28CRP.RD[i]) & !is.na(RA_Data_user$HAQ.MCID.RD[i])){
        RA_Data_user$cNNT[i]<-1/(wtACR*0.5*RA_Data_user$DAS28CRP.RD[i]+wtACR*0.5*RA_Data_user$HAQ.MCID.RD[i]+wtDAS*RA_Data_user$DAS28CRP.RD[i]+wtHAQ*RA_Data_user$HAQ.MCID.RD[i])
      }else if(!is.na(RA_Data_user$ACR50.RD[i]) & !is.na(RA_Data_user$DAS28ESR.RD[i]) & is.na(RA_Data_user$HAQ.MCID.RD[i])){
        RA_Data_user$cNNT[i]<-1/(wtACR*RA_Data_user$ACR50.RD[i]+wtDAS*RA_Data_user$DAS28ESR.RD[i]+wtHAQ*0.5*RA_Data_user$ACR50.RD[i]+wtHAQ*0.5*RA_Data_user$DAS28ESR.RD[i])
      }else if(!is.na(RA_Data_user$ACR50.RD[i]) & is.na(RA_Data_user$DAS28ESR.RD[i]) & !is.na(RA_Data_user$DAS28CRP.RD[i]) & is.na(RA_Data_user$HAQ.MCID.RD[i])){
        RA_Data_user$cNNT[i]<-1/(wtACR*RA_Data_user$ACR50.RD[i]+wtDAS*RA_Data_user$DAS28CRP.RD[i]+wtHAQ*0.5*RA_Data_user$ACR50.RD[i]+wtHAQ*0.5*RA_Data_user$DAS28CRP.RD[i])
      }
    }
    print(RA_Data_user$cNNT)
    
    wtInf<-isolate(meds_values$SAE)/100
    wtINJGI<-isolate(meds_values$IJRDia)/100
    wtDiscon<-isolate(meds_values$Discont)/100
    for(i in 1:nrow(RA_Data_user)){
      if(!is.na(RA_Data_user$Infection.RD[i]) & !is.na(RA_Data_user$Reaction.RD[i]) & !is.na(RA_Data_user$Discontinuation.RD[i])){
        RA_Data_user$cNNH[i]<-1/(wtInf*RA_Data_user$Infection.RD[i]+wtINJGI*RA_Data_user$Reaction.RD[i]+wtDiscon*RA_Data_user$Discontinuation.RD[i])
      }else if(!is.na(RA_Data_user$Infection.RD[i]) & is.na(RA_Data_user$Reaction.RD[i]) & !is.na(RA_Data_user$Discontinuation.RD[i])){
        if(!is.na(RA_Data_user$GI.SeE.NNT[i])){
          RA_Data_user$cNNH[i]<-1/(wtInf*RA_Data_user$Infection.RD[i]+wtINJGI*RA_Data_user$GI.SeE.NNT[i]+wtDiscon*RA_Data_user$Discontinuation.RD[i])
        }else{
          RA_Data_user$cNNH[i]<-1/(wtInf*RA_Data_user$Infection.RD[i]+wtINJGI*0.5*RA_Data_user$Infection.RD[i]+wtINJGI*0.5*RA_Data_user$Discontinuation.RD[i]+wtDiscon*RA_Data_user$Discontinuation.RD[i])
        }
      }else if(is.na(RA_Data_user$Infection.RD[i]) & !is.na(RA_Data_user$Reaction.RD[i]) & !is.na(RA_Data_user$Discontinuation.RD[i])){
        RA_Data_user$cNNH[i]<-1/(wtInf*0.5*RA_Data_user$Reaction.RD[i]+wtInf*0.5*RA_Data_user$Discontinuation.RD[i]+wtINJGI*RA_Data_user$Reaction.RD[i]+wtDiscon*RA_Data_user$Discontinuation.RD[i])
      }else if(is.na(RA_Data_user$Infection.RD[i]) & is.na(RA_Data_user$Reaction.RD[i]) & !is.na(RA_Data_user$GI.SeE.NNT[i]) & !is.na(RA_Data_user$Discontinuation.RD[i])){
        RA_Data_user$cNNH[i]<-1/(wtInf*0.5*RA_Data_user$GI.SeE.NNT[i]+wtInf*0.5*RA_Data_user$Discontinuation.RD[i]+wtINJGI*RA_Data_user$GI.SeE.NNT[i]+wtDiscon*RA_Data_user$Discontinuation.RD[i])
      }else if(!is.na(RA_Data_user$Infection.RD[i]) & !is.na(RA_Data_user$Reaction.RD[i]) & is.na(RA_Data_user$Discontinuation.RD[i])){
        RA_Data_user$cNNH[i]<-1/(wtInf*RA_Data_user$Infection.RD[i]+wtINJGI*RA_Data_user$Reaction.RD[i]+wtDiscon*0.5*RA_Data_user$Infection.RD[i]+wtDiscon*0.5*RA_Data_user$Reaction.RD[i])
      }else if(!is.na(RA_Data_user$Infection.RD[i]) & is.na(RA_Data_user$Reaction.RD[i]) & !is.na(RA_Data_user$GI.SeE.NNT[i]) & is.na(RA_Data_user$Discontinuation.RD[i])){
        RA_Data_user$cNNH[i]<-1/(wtInf*RA_Data_user$Infection.RD[i]+wtINJGI*RA_Data_user$GI.SeE.NNT[i]+wtDiscon*0.5*RA_Data_user$GI.SeE.NNT[i]+wtDiscon*0.5*RA_Data_user$Reaction.RD[i])
      }
    }
    print(RA_Data_user$cNNH)
    for(i in 1:nrow(RA_Data_user)){
      RA_Data_user$cRDBen[i]<-1/RA_Data_user$cNNT[i]
      RA_Data_user$cRDHarm[i]<-1/RA_Data_user$cNNH[i]}
    for(i in 1:nrow(RA_Data_user)){
      if(RA_Data_user$cRDBen[i]>=quantile(RA_Data_user$cRDBen,0.7)){
        RA_Data_user$NNTCat[i]<-'A+'
      }else if(RA_Data_user$cRDBen[i]>=quantile(RA_Data_user$cRDBen,0.5)){
        RA_Data_user$NNTCat[i]<-'A'
      }else if(RA_Data_user$cRDBen[i]>=quantile(RA_Data_user$cRDBen,0.3)){
        RA_Data_user$NNTCat[i]<-'B+'
      }else{
        RA_Data_user$NNTCat[i]<-'B'
      }
    }
    for(i in 1:nrow(RA_Data_user)){
      if(RA_Data_user$cRDHarm[i]>=quantile(RA_Data_user$cRDHarm,0.7)){
        RA_Data_user$NNHCat[i]<-'B'
      }else if(RA_Data_user$cRDHarm[i]>=quantile(RA_Data_user$cRDHarm,0.5)){
        RA_Data_user$NNHCat[i]<-'B+'
      }else if(RA_Data_user$cRDHarm[i]>=quantile(RA_Data_user$cRDHarm,0.3)){
        RA_Data_user$NNHCat[i]<-'A'
      }else{
        RA_Data_user$NNHCat[i]<-'A+'
      }
    }
    for(i in 1:nrow(RA_Data_user)){
      if(RA_Data_user$NNTCat[i]=='A+' & RA_Data_user$NNHCat[i]=='A+'){
        RA_Data_user$ceRank[i]<-'A'
      }else if(RA_Data_user$NNTCat[i]=='A+' & RA_Data_user$NNHCat[i]=='A'){
        RA_Data_user$ceRank[i]<-'A'
      }else if(RA_Data_user$NNTCat[i]=='A+' & RA_Data_user$NNHCat[i]=='B+'){
        RA_Data_user$ceRank[i]<-'B'
      }else if(RA_Data_user$NNTCat[i]=='A+' & RA_Data_user$NNHCat[i]=='B'){
        RA_Data_user$ceRank[i]<-'B'
      }else if(RA_Data_user$NNTCat[i]=='A' & RA_Data_user$NNHCat[i]=='A+'){
        RA_Data_user$ceRank[i]<-'B'
      }else if(RA_Data_user$NNTCat[i]=='A' & RA_Data_user$NNHCat[i]=='A'){
        RA_Data_user$ceRank[i]<-'B'
      }else if(RA_Data_user$NNTCat[i]=='A' & RA_Data_user$NNHCat[i]=='B+'){
        RA_Data_user$ceRank[i]<-'C'
      }else if(RA_Data_user$NNTCat[i]=='A' & RA_Data_user$NNHCat[i]=='B'){
        RA_Data_user$ceRank[i]<-'C'
      }else if(RA_Data_user$NNTCat[i]=='B+' & RA_Data_user$NNHCat[i]=='A+'){
        RA_Data_user$ceRank[i]<-'C'
      }else if(RA_Data_user$NNTCat[i]=='B+' & RA_Data_user$NNHCat[i]=='A'){
        RA_Data_user$ceRank[i]<-'D'
      }else if(RA_Data_user$NNTCat[i]=='B+' & RA_Data_user$NNHCat[i]=='B+'){
        RA_Data_user$ceRank[i]<-'D'
      }else if(RA_Data_user$NNTCat[i]=='B+' & RA_Data_user$NNHCat[i]=='B'){
        RA_Data_user$ceRank[i]<-'D'
      }else{
        RA_Data_user$ceRank[i]<-'F'
      }
    }
    RA_Data_user_save<<-RA_Data_user
    for(i in 1:nrow(RA_Data_user_save)){
      print(i)
      RA_Data_user_save$LL_Ben[i]<-(1-(0.5-1000/((1/RA_Data_user_save$cRDBen[i])+1)/(1000-1000/((1/RA_Data_user_save$cRDBen[i])+1))))*100
      RA_Data_user_save$UL_Ben[i]<-(1-(0.5-1000/((1/RA_Data_user_save$cRDBen[i])-1)/(1000-1000/((1/RA_Data_user_save$cRDBen[i])-1))))*100
      if(RA_Data_user_save$LL_Ben[i]>100){
        RA_Data_user_save$LL_Ben[i]<-100
      }else if(RA_Data_user_save$LL_Ben[i]<0){
        RA_Data_user_save$LL_Ben[i]<-0
      }else{
        RA_Data_user_save$LL_Ben[i]<-RA_Data_user_save$LL_Ben[i]
      }
      if(RA_Data_user_save$UL_Ben[i]>100){
        RA_Data_user_save$UL_Ben[i]<-100
      }else if(RA_Data_user_save$UL_Ben[i]<0){
        RA_Data_user_save$UL_Ben[i]<-0
      }else{
        RA_Data_user_save$UL_Ben[i]<-RA_Data_user_save$UL_Ben[i]
      }
      RA_Data_user_save$LL_Harm[i]<-100-(1-(0.5-1000/((1/RA_Data_user_save$cRDHarm[i])-1)/(1000-1000/((1/RA_Data_user_save$cRDHarm[i])-1))))*100
      RA_Data_user_save$UL_Harm[i]<-100-(1-(0.5-1000/((1/RA_Data_user_save$cRDHarm[i])+1)/(1000-1000/((1/RA_Data_user_save$cRDHarm[i])+1))))*100
      if(RA_Data_user_save$LL_Harm[i]>100){
        RA_Data_user_save$LL_Harm[i]<-100
      }else if(RA_Data_user_save$LL_Harm[i]<0){
        RA_Data_user_save$LL_Harm[i]<-0
      }else{
        RA_Data_user_save$LL_Harm[i]<-RA_Data_user_save$LL_Harm[i]
      }
      if(RA_Data_user_save$UL_Harm[i]>100){
        RA_Data_user_save$UL_Harm[i]<-100
      }else if(RA_Data_user_save$UL_Harm[i]<0){
        RA_Data_user_save$UL_Harm[i]<-0
      }else{
        RA_Data_user_save$UL_Harm[i]<-RA_Data_user_save$UL_Harm[i]
      }
      RA_Data_user_save$MeanLL[i]<-(RA_Data_user_save$LL_Harm[i]+RA_Data_user_save$LL_Ben[i])/2
      RA_Data_user_save$MeanUL[i]<-(RA_Data_user_save$UL_Harm[i]+RA_Data_user_save$UL_Ben[i])/2
      RA_Data_user_save$MeanLL[i]<-format(round(as.numeric(RA_Data_user_save$MeanLL[i]), 2), nsmall = 2)
      RA_Data_user_save$MeanUL[i]<-format(round(as.numeric(RA_Data_user_save$MeanUL[i]), 2), nsmall = 2)
    }
    RA_Data_user_save<<-RA_Data_user_save
    RA_Data_user<-RA_Data_user_save
    RA_Data_user1<-RA_Data_user[, c("Arm", "cNNT","cNNH","ceRank","MeanLL","MeanUL")]
    RA_Data_user1 <- round_df(RA_Data_user1, digits = 2)
    RA_Data_user1$AVScore<-paste(RA_Data_user1$MeanLL,'-',RA_Data_user1$MeanUL)
    assign("RA_Data_user1", RA_Data_user1, .GlobalEnv)
    # RA_Data_user1<-RA_Data_user1[,c("Arm", "cNNT","cNNH","ceRank","AVScore")]
    print('done!')
    
    meds_values$RA_Data_user1<-RA_Data_user1
    RA_Data_User1<-meds_values$RA_Data_User1
    
    selected.meds <- str_extract(input$medBoxs, "^\\D+") %>% unique()
    
    rmarkdown::render(input = "snapshot_ra.Rmd",
                      output_file = "snapshot_preview.html",
                      params = list(condition =  condition$selected, # input$condition,
                                    selected_meds = selected.meds,
                                    acr50_val = input$ACR50,
                                    das28_val = input$DAS28,
                                    haq_val = input$HAQ,
                                    disc_val = input$discont,
                                    si_val = input$severeAE,
                                    ijr_val = input$IJR_diarrhea,
                                    ra_data = RA_Data_user1))
    xml2::write_html(rvest::html_node(xml2::read_html("snapshot_preview.html"), "body"), file = "snapshot_preview_RA.html")
    
    
    # BCBS Results with User-defined weights
    data <- RA_Data_user1[,c("Arm", "cNNT","cNNH","ceRank","AVScore")]
    output$userDefRes_RA <- renderReactable({
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
    
    # output$userDefRes_RA<-DT::renderDataTable(datatable(data,
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
    
    
    print('ac3') 
    
    output$previewSnapshot_RA <- renderUI({
      includeHTML("snapshot_preview_RA.html")
    })
    
    output$bcbsaResults_RA <-renderUI({
      
      fluidRow(
        h2("Rhuematoid Arthritis"),
        h4("Use the filters on the left sidebar to select medications, harms weights, and benefit weights"),
        h4("Click the 'Run Analysis' button to update the data in the results table and snapshot. 
           Navigate to the 'Preview Snapshot' tab to preview and/or download the BCBS Snapshot report"),
        br(),
        bcbsTabBox(id = "ra_tab_box",
                   title = p(actionButton("more_info_MEDS", label = NULL, icon = icon("info")),
                             shinyjs::hidden(downloadButton("downloadSnapshot_RA", "Download Snapshot"))),
                   width = 12,
                   tabPanel("Results Overview",
                            uiOutput("boxTitle_RA"),
                            reactableOutput('userDefRes_RA'),
                            tags$head(tags$style("#meds_info .modal-footer button {font-size: 20px; line-height: 25px; font-weight: bold; text-transform: uppercase;}")),
                            tags$head(tags$style("#meds_info .modal-header {padding-bottom: 0px; padding-top: 0px;}")),
                            tags$head(tags$style("#meds_info .modal-header button {display: none;}")),
                            
                            bsModal('meds_info',
                                    title = h2('MEDS Information',style = 'color: #005876; font-weight: bold; line-height: 25px;'),
                                    size = 'large',
                                    trigger = 'more_info_MEDS',
                                    uiOutput('info_details')
                            )
                            
                   ),
                   
                   tabPanel("Preview Snapshot",
                            uiOutput("previewSnapshot_RA") #,
                            # downloadButton("downloadSnapshot_RA", "Download Snapshot")
                   )
        )
      )
      
      
    })
    print('ac4')
  }
}, ignoreInit = TRUE)

observeEvent(input$more_info_MEDS,{
  output$info_details<-renderUI({
    div(shiny::HTML('<h4>RA Specialty Drugs and Outcome Measures Selection</h4>
                         <p>The RA drug set and outcome measures were objectively selected based on a series of webinar discussions and surveys with BCBS Plans. 
                         The audience were both executive and clinical pharmacists from each BCBS Plan with appropriate experience and knowledge in rheumatoid arthritis treatments. 
                         Since the MEDS Model utilizes Number Needed to Treat and Number Needed to Harm, the executive and clinical pharmacists were aware that the outcome measures 
                         needed to have dichotomous characteristics.</p><br>
                         <h4>Selected RA Specialty Drugs</h4>
                         <ul>
                            <li>Orencia (abatacept)</li>
                            <li>Humira (adalimumab)</li>
                            <li>Amjevita (adaliumumab)</li>
                            <li>Cimzia (certolizumab pegol)</li>
                            <li>Enbrel (etanercept)</li>
                            <li>Simponi (golimumab)</li>
                            <li>Remicade (infliximab)</li>
                            <li>Inflectra (infliximab)</li>
                            <li>Renflexis (infliximab)</li>
                            <li>Rituxan (rituximab)</li>
                            <li>Actmera (tocilizumab)</li>
                            <li>Xeljanz (tofacitinib)</li>
                            <li>Upadacitinib</li>
                          </ul><br>
                        <h4>Selected RA Outcome Measures</h4>
                        <h5>Efficacy</h5>
                        <ul>
                        <li>American College of Rheumatology (ACR) 50 Criteria</li>
                        <li>Disease Activity Score (DAS) - 28</li>
                        <li>Health Assessment Questionnaire – Disability Index (HAQ-DI)</li>
                        </ul>
                        <h5>Safety</h5>
                        <ul>
                        <li>Discontinuation due to adverse event</li>
                        <li>Serious Infection (pneumonia or upper respiratory infection, infection requiring IV antibodies or any hospitalization due to infection)</li>
                        <li>Injection Site Reaction (Injectable Drug) or gastrointestinal side effects (Oral Drug)</li>
                        </ul>'),
        div(bcbsButton('ref_list','References'))
    )
  })
})

observeEvent(input$info_methods,{
  output$info_details<-renderUI({
    div(shiny::HTML('<h4>RA Specialty Drugs and Outcome Measures Selection</h4>
                         <p>The RA drug set and outcome measures were objectively selected based on a series of webinar discussions and surveys with BCBS Plans. 
                         The audience were both executive and clinical pharmacists from each BCBS Plan with appropriate experience and knowledge in rheumatoid arthritis treatments. 
                         Since the MEDS Model utilizes Number Needed to Treat and Number Needed to Harm, the executive and clinical pharmacists were aware that the outcome measures 
                         needed to have dichotomous characteristics.</p><br>
                         <h4>Selected RA Specialty Drugs</h4>
                         <ul>
                            <li>Orencia (abatacept)</li>
                            <li>Humira (adalimumab)</li>
                            <li>Amjevita (adaliumumab)</li>
                            <li>Cimzia (certolizumab pegol)</li>
                            <li>Enbrel (etanercept)</li>
                            <li>Simponi (golimumab)</li>
                            <li>Remicade (infliximab)</li>
                            <li>Inflectra (infliximab)</li>
                            <li>Renflexis (infliximab)</li>
                            <li>Rituxan (rituximab)</li>
                            <li>Actmera (tocilizumab)</li>
                            <li>Xeljanz (tofacitinib)</li>
                            <li>Upadacitinib</li>
                          </ul><br>
                        <h4>Selected RA Outcome Measures</h4>
                        <h5>Efficacy</h5>
                        <ul>
                        <li>American College of Rheumatology (ACR) 50 Criteria</li>
                        <li>Disease Activity Score (DAS) - 28</li>
                        <li>Health Assessment Questionnaire – Disability Index (HAQ-DI)</li>
                        </ul>
                        <h5>Safety</h5>
                        <ul>
                        <li>Discontinuation due to adverse event</li>
                        <li>Serious Infection (pneumonia or upper respiratory infection, infection requiring IV antibodies or any hospitalization due to infection)</li>
                        <li>Injection Site Reaction (Injectable Drug) or gastrointestinal side effects (Oral Drug)</li>
                        </ul>'),
        div(bcbsButton('ref_list','References'))
    )
  })
})

observeEvent(input$ref_list,{
  output$info_details<-renderUI({
    div(shiny::HTML("<h3>References</h3>
                            <ul>
                            <li>Burmester, G. R., Blanco, R., Charles-Schoeman, C., Wollenhaupt, J., Zerbini, C., Benda, B., . . . Mebus, C. (2013). Tofacitinib (CP-690,550) in combination with methotrexate in patients with active rheumatoid arthritis with an inadequate response to tumour necrosis factor inhibitors: a randomised phase 3 trial. Lancet, 381(9865), 451-460. doi:10.1016/s0140-6736(12)61424-x</li>
                            <li>Burmester, G. R., Kremer, J. M., Van den Bosch, F., Kivitz, A., Bessette, L., Li, Y., . . . Camp, H. S. (2018). Safety and efficacy of upadacitinib in patients with rheumatoid arthritis and inadequate response to conventional synthetic disease-modifying anti-rheumatic drugs (SELECT-NEXT): a randomised, double-blind, placebo-controlled phase 3 trial. Lancet, 391(10139), 2503-2512. doi:10.1016/s0140-6736(18)31115-2</li>
                            <li>Choe, J. Y., Prodanovic, N., Niebrzydowski, J., Staykov, I., Dokoupilova, E., Baranauskaite, A., . . . Smolen, J. S. (2017). A randomised, double-blind, phase III study comparing SB2, an infliximab biosimilar, to the infliximab reference product Remicade in patients with moderate to severe rheumatoid arthritis despite methotrexate therapy. Ann Rheum Dis, 76(1), 58-64. doi:10.1136/annrheumdis-2015-207764</li>
                            <li>Choy, E., McKenna, F., Vencovsky, J., Valente, R., Goel, N., Vanlunen, B., . . . Alten, R. (2012). Certolizumab pegol plus MTX administered every 4 weeks is effective in patients with RA who are partial responders to MTX. Rheumatology (Oxford), 51(7), 1226-1234. doi:10.1093/rheumatology/ker519</li>
                            <li>Cohen, S., Genovese, M. C., Choy, E., Perez-Ruiz, F., Matsumoto, A., Pavelka, K., . . . Kaur, P. (2017). Efficacy and safety of the biosimilar ABP 501 compared with adalimumab in patients with moderate to severe rheumatoid arthritis: a randomised, double-blind, phase III equivalence study. Ann Rheum Dis, 76(10), 1679-1687. doi:10.1136/annrheumdis-2016-210459</li>
                            <li>Cohen, S. B., Emery, P., Greenwald, M. W., Dougados, M., Furie, R. A., Genovese, M. C., . . . Totoritis, M. C. (2006). Rituximab for rheumatoid arthritis refractory to anti-tumor necrosis factor therapy: Results of a multicenter, randomized, double-blind, placebo-controlled, phase III trial evaluating primary efficacy and safety at twenty-four weeks. Arthritis Rheum, 54(9), 2793-2806. doi:10.1002/art.22025</li>
                            <li>Combe, B., Codreanu, C., Fiocco, U., Gaubitz, M., Geusens, P. P., Kvien, T. K., . . . Fatenejad, S. (2006). Etanercept and sulfasalazine, alone and combined, in patients with active rheumatoid arthritis despite receiving sulfasalazine: a double-blind comparison. Ann Rheum Dis, 65(10), 1357-1362. doi:10.1136/ard.2005.049650</li>
                            <li>Edwards, J. C., Szczepanski, L., Szechinski, J., Filipowicz-Sosnowska, A., Emery, P., Close, D. R., . . . Shaw, T. (2004). Efficacy of B-cell-targeted therapy with rituximab in patients with rheumatoid arthritis. N Engl J Med, 350(25), 2572-2581. doi:10.1056/NEJMoa032534</li>
                            <li>Emery, P., Deodhar, A., Rigby, W. F., Isaacs, J. D., Combe, B., Racewicz, A. J., . . . Tyrrell, H. (2010). Efficacy and safety of different doses and retreatment of rituximab: a randomised, placebo-controlled trial in patients who are biological naive with active rheumatoid arthritis and an inadequate response to methotrexate (Study Evaluating Rituximab's Efficacy in MTX iNadequate rEsponders (SERENE)). Ann Rheum Dis, 69(9), 1629-1635. doi:10.1136/ard.2009.119933</li>
                            <li>Emery, P., Fleischmann, R., Filipowicz-Sosnowska, A., Schechtman, J., Szczepanski, L., Kavanaugh, A., . . . Shaw, T. M. (2006). The efficacy and safety of rituximab in patients with active rheumatoid arthritis despite methotrexate treatment: results of a phase IIB randomized, double-blind, placebo-controlled, dose-ranging trial. Arthritis Rheum, 54(5), 1390-1400. doi:10.1002/art.21778</li>
                            <li>Emery, P., Keystone, E., Tony, H. P., Cantagrel, A., van Vollenhoven, R., Sanchez, A., . . . Kremer, J. (2008). IL-6 receptor inhibition with tocilizumab improves treatment outcomes in patients with rheumatoid arthritis refractory to anti-tumour necrosis factor biologicals: results from a 24-week multicentre randomised placebo-controlled trial. Ann Rheum Dis, 67(11), 1516-1523. doi:10.1136/ard.2008.092932</li>
                            <li>Emery, P., Vencovsky, J., Sylwestrzak, A., Leszczynski, P., Porawska, W., Baranauskaite, A., . . . Ghil, J. (2017). A phase III randomised, double-blind, parallel-group study comparing SB4 with etanercept reference product in patients with active rheumatoid arthritis despite methotrexate therapy. Ann Rheum Dis, 76(1), 51-57. doi:10.1136/annrheumdis-2015-207588</li>
                            <li>Fleischmann, R., Cutolo, M., Genovese, M. C., Lee, E. B., Kanik, K. S., Sadis, S., . . . Zwillich, S. H. (2012). Phase IIb dose-ranging study of the oral JAK inhibitor tofacitinib (CP-690,550) or adalimumab monotherapy versus placebo in patients with active rheumatoid arthritis with an inadequate response to disease-modifying antirheumatic drugs. Arthritis Rheum, 64(3), 617-629. doi:10.1002/art.33383</li>
                            <li>Fleischmann, R., Mysler, E., Hall, S., Kivitz, A. J., Moots, R. J., Luo, Z., . . . Smolen, J. S. (2017). Efficacy and safety of tofacitinib monotherapy, tofacitinib with methotrexate, and adalimumab with methotrexate in patients with rheumatoid arthritis (ORAL Strategy): a phase 3b/4, double-blind, head-to-head, randomised controlled trial. Lancet, 390(10093), 457-468. doi:10.1016/s0140-6736(17)31618-5</li>
                            <li>Furst, D. E., Schiff, M. H., Fleischmann, R. M., Strand, V., Birbara, C. A., Compagnone, D., . . . Chartash, E. K. (2003). Adalimumab, a fully human anti tumor necrosis factor-alpha monoclonal antibody, and concomitant standard antirheumatic therapy for the treatment of rheumatoid arthritis: results of STAR (Safety Trial of Adalimumab in Rheumatoid Arthritis). J Rheumatol, 30(12), 2563-2571.</li>
                            <li>Gabay, C., Emery, P., van Vollenhoven, R., Dikranian, A., Alten, R., Pavelka, K., . . . Kavanaugh, A. (2013). Tocilizumab monotherapy versus adalimumab monotherapy for treatment of rheumatoid arthritis (ADACTA): a randomised, double-blind, controlled phase 4 trial. Lancet, 381(9877), 1541-1550. doi:10.1016/s0140-6736(13)60250-0</li>
                            <li>Genovese, M. C., Becker, J. C., Schiff, M., Luggen, M., Sherrer, Y., Kremer, J., . . . Dougados, M. (2005). Abatacept for rheumatoid arthritis refractory to tumor necrosis factor alpha inhibition. N Engl J Med, 353(11), 1114-1123. doi:10.1056/NEJMoa050524</li>
                            <li>Genovese, M. C., Fleischmann, R., Combe, B., Hall, S., Rubbert-Roth, A., Zhang, Y., . . . Pangan, A. L. (2018). Safety and efficacy of upadacitinib in patients with active rheumatoid arthritis refractory to biologic disease-modifying anti-rheumatic drugs (SELECT-BEYOND): a double-blind, randomised controlled phase 3 trial. Lancet, 391(10139), 2513-2524. doi:10.1016/s0140-6736(18)31116-4</li>
                            <li>Genovese, M. C., McKay, J. D., Nasonov, E. L., Mysler, E. F., da Silva, N. A., Alecock, E., . . . Gomez-Reino, J. J. (2008). Interleukin-6 receptor inhibition with tocilizumab reduces disease activity in rheumatoid arthritis with inadequate response to disease-modifying antirheumatic drugs: the tocilizumab in combination with traditional disease-modifying antirheumatic drug therapy study. Arthritis Rheum, 58(10), 2968-2980. doi:10.1002/art.23940</li>
                            <li>Jones, G., Sebba, A., Gu, J., Lowenstein, M. B., Calvo, A., Gomez-Reino, J. J., . . . Genovese, M. C. (2010). Comparison of tocilizumab monotherapy versus methotrexate monotherapy in patients with moderate to severe rheumatoid arthritis: the AMBITION study. Ann Rheum Dis, 69(1), 88-96. doi:10.1136/ard.2008.105197</li>
                            <li>Kennedy, W. P., Simon, J. A., Offutt, C., Horn, P., Herman, A., Townsend, M. J., . . . Davis, J. C. (2014). Efficacy and safety of pateclizumab (anti-lymphotoxin-alpha) compared to adalimumab in rheumatoid arthritis: a head-to-head phase 2 randomized controlled study (The ALTARA Study). Arthritis Res Ther, 16(5), 467. doi:10.1186/s13075-014-0467-3</li>
                            <li>Keystone, E., Heijde, D., Mason, D., Jr., Landewe, R., Vollenhoven, R. V., Combe, B., . . . Pavelka, K. (2008). Certolizumab pegol plus methotrexate is significantly more effective than placebo plus methotrexate in active rheumatoid arthritis: findings of a fifty-two-week, phase III, multicenter, randomized, double-blind, placebo-controlled, parallel-group study. Arthritis Rheum, 58(11), 3319-3329. doi:10.1002/art.23964</li>
                            <li>Keystone, E. C., Genovese, M. C., Klareskog, L., Hsia, E. C., Hall, S. T., Miranda, P. C., . . . Rahman, M. U. (2009). Golimumab, a human antibody to tumour necrosis factor {alpha} given by monthly subcutaneous injections, in active rheumatoid arthritis despite methotrexate therapy: the GO-FORWARD Study. Ann Rheum Dis, 68(6), 789-796. doi:10.1136/ard.2008.099010</li>
                            <li>Keystone, E. C., Kavanaugh, A. F., Sharp, J. T., Tannenbaum, H., Hua, Y., Teoh, L. S., . . . Chartash, E. K. (2004). Radiographic, clinical, and functional outcomes of treatment with adalimumab (a human anti-tumor necrosis factor monoclonal antibody) in patients with active rheumatoid arthritis receiving concomitant methotrexate therapy: a randomized, placebo-controlled, 52-week trial. Arthritis Rheum, 50(5), 1400-1411. doi:10.1002/art.20217</li>
                            <li>Kim, H. Y., Hsu, P. N., Barba, M., Sulaiman, W., Robertson, D., Vlahos, B., . . . Koenig, A. (2012). Randomized comparison of etanercept with usual therapy in an Asian population with active rheumatoid arthritis: the APPEAL trial. Int J Rheum Dis, 15(2), 188-196. doi:10.1111/j.1756-185X.2011.01680.x</li>
                            <li>Kivitz, A., Olech, E., Borofsky, M., Zazueta, B. M., Navarro-Sarabia, F., Radominski, S. C., . . . Pope, J. E. (2014). Subcutaneous tocilizumab versus placebo in combination with disease-modifying antirheumatic drugs in patients with rheumatoid arthritis. Arthritis Care Res (Hoboken), 66(11), 1653-1661. doi:10.1002/acr.22384</li>
                            <li>Klareskog, L., van der Heijde, D., de Jager, J. P., Gough, A., Kalden, J., Malaise, M., . . . Sanda, M. (2004). Therapeutic effect of the combination of etanercept and methotrexate compared with each treatment alone in patients with rheumatoid arthritis: double-blind randomised controlled trial. Lancet, 363(9410), 675-681. doi:10.1016/s0140-6736(04)15640-7</li>
                            <li>Kremer, J., Li, Z. G., Hall, S., Fleischmann, R., Genovese, M., Martin-Mola, E., . . . Bradley, J. (2013). Tofacitinib in combination with nonbiologic disease-modifying antirheumatic drugs in patients with active rheumatoid arthritis: a randomized trial. Ann Intern Med, 159(4), 253-261. doi:10.7326/0003-4819-159-4-201308200-00006</li>
                            <li>Kremer, J. M., Blanco, R., Brzosko, M., Burgos-Vargas, R., Halland, A. M., Vernon, E., . . . Fleischmann, R. (2011). Tocilizumab inhibits structural joint damage in rheumatoid arthritis patients with inadequate responses to methotrexate: results from the double-blind treatment phase of a randomized placebo-controlled trial of tocilizumab safety and prevention of structural joint damage at one year. Arthritis Rheum, 63(3), 609-621. doi:10.1002/art.30158</li>
                            <li>Kremer, J. M., Cohen, S., Wilkinson, B. E., Connell, C. A., French, J. L., Gomez-Reino, J., . . . Zwillich, S. H. (2012). A phase IIb dose-ranging study of the oral JAK inhibitor tofacitinib (CP-690,550) versus placebo in combination with background methotrexate in patients with active rheumatoid arthritis and an inadequate response to methotrexate alone. Arthritis Rheum, 64(4), 970-981. doi:10.1002/art.33419</li>
                            <li>Kremer, J. M., Genant, H. K., Moreland, L. W., Russell, A. S., Emery, P., Abud-Mendoza, C., . . . Westhovens, R. (2006). Effects of abatacept in patients with methotrexate-resistant active rheumatoid arthritis: a randomized trial. Ann Intern Med, 144(12), 865-876.</li>
                            <li>Kremer, J. M., Westhovens, R., Leon, M., Di Giorgio, E., Alten, R., Steinfeld, S., . . . Moreland, L. W. (2003). Treatment of rheumatoid arthritis by selective inhibition of T-cell activation with fusion protein CTLA4Ig. N Engl J Med, 349(20), 1907-1915. doi:10.1056/NEJMoa035075</li>
                            <li>Li, Z., Zhang, F., Kay, J., Fei, K., Han, C., Zhuang, Y., . . . Hsia, E. C. (2016). Efficacy and safety results from a Phase 3, randomized, placebo-controlled trial of subcutaneous golimumab in Chinese patients with active rheumatoid arthritis despite methotrexate therapy. Int J Rheum Dis, 19(11), 1143-1156. doi:10.1111/1756-185x.12723</li>
                            <li>Machado, D. A., Guzman, R. M., Xavier, R. M., Simon, J. A., Mele, L., Pedersen, R., . . . Vlahos, B. (2014). Open-label observation of addition of etanercept versus a conventional disease-modifying antirheumatic drug in subjects with active rheumatoid arthritis despite methotrexate therapy in the Latin American region. J Clin Rheumatol, 20(1), 25-33. doi:10.1097/rhu.0000000000000055</li>
                            <li>Maini, R., St Clair, E. W., Breedveld, F., Furst, D., Kalden, J., Weisman, M., . . . Lipsky, P. (1999). Infliximab (chimeric anti-tumour necrosis factor alpha monoclonal antibody) versus placebo in rheumatoid arthritis patients receiving concomitant methotrexate: a randomised phase III trial. ATTRACT Study Group. Lancet, 354(9194), 1932-1939. </li>
                            <li>Maini, R. N., Taylor, P. C., Szechinski, J., Pavelka, K., Broll, J., Balint, G., . . . Kishimoto, T. (2006). Double-blind randomized controlled clinical trial of the interleukin-6 receptor antagonist, tocilizumab, in European patients with rheumatoid arthritis who had an incomplete response to methotrexate. Arthritis Rheum, 54(9), 2817-2829. doi:10.1002/art.22033</li>
                            <li>Nishimoto, N., Hashimoto, J., Miyasaka, N., Yamamoto, K., Kawai, S., Takeuchi, T., . . . Kishimoto, T. (2007). Study of active controlled monotherapy used for rheumatoid arthritis, an IL-6 inhibitor (SAMURAI): evidence of clinical and radiographic benefit from an x ray reader-blinded randomised controlled trial of tocilizumab. Ann Rheum Dis, 66(9), 1162-1167. doi:10.1136/ard.2006.068064</li><li>Nishimoto, N., Miyasaka, N., Yamamoto, K., Kawai, S., Takeuchi, T., Azuma, J., & Kishimoto, T. (2009). Study of active controlled tocilizumab monotherapy for rheumatoid arthritis patients with an inadequate response to methotrexate (SATORI): significant reduction in disease activity and serum vascular endothelial growth factor by IL-6 receptor inhibition therapy. Mod Rheumatol, 19(1), 12-19. doi:10.1007/s10165-008-0125-1</li>
                            <li>O'Dell, J. R., Mikuls, T. R., Taylor, T. H., Ahluwalia, V., Brophy, M., Warren, S. R., . . . Keystone, E. (2013). Therapies for active rheumatoid arthritis after methotrexate failure. N Engl J Med, 369(4), 307-318. doi:10.1056/NEJMoa1303006</li>
                            <li>Peterfy, C., Emery, P., Tak, P. P., Ostergaard, M., DiCarlo, J., Otsa, K., . . . Gabriele, A. (2016). MRI assessment of suppression of structural damage in patients with rheumatoid arthritis receiving rituximab: results from the randomised, placebo-controlled, double-blind RA-SCORE study. Ann Rheum Dis, 75(1), 170-177. doi:10.1136/annrheumdis-2014-206015</li>
                            <li>Schiff, M., Keiserman, M., Codding, C., Songcharoen, S., Berman, A., Nayiager, S., . . . Dougados, M. (2008). Efficacy and safety of abatacept or infliximab vs placebo in ATTEST: a phase III, multi-centre, randomised, double-blind, placebo-controlled study in patients with rheumatoid arthritis and an inadequate response to methotrexate. Ann Rheum Dis, 67(8), 1096-1103. doi:10.1136/ard.2007.080002</li>
                            <li>Smolen, J., Landewe, R. B., Mease, P., Brzezicki, J., Mason, D., Luijtens, K., . . . van der Heijde, D. (2009). Efficacy and safety of certolizumab pegol plus methotrexate in active rheumatoid arthritis: the RAPID 2 study. A randomised controlled trial. Ann Rheum Dis, 68(6), 797-804. doi:10.1136/ard.2008.101659</li>
                            <li>Smolen, J. S., Beaulieu, A., Rubbert-Roth, A., Ramos-Remus, C., Rovensky, J., Alecock, E., . . . Alten, R. (2008). Effect of interleukin-6 receptor inhibition with tocilizumab in patients with rheumatoid arthritis (OPTION study): a double-blind, placebo-controlled, randomised trial. Lancet, 371(9617), 987-997. doi:10.1016/s0140-6736(08)60453-5</li>
                            <li>Smolen, J. S., Emery, P., Ferraccioli, G. F., Samborski, W., Berenbaum, F., Davies, O. R., . . . Burkhardt, H. (2015). Certolizumab pegol in rheumatoid arthritis patients with low to moderate activity: the CERTAIN double-blind, randomised, placebo-controlled trial. Ann Rheum Dis, 74(5), 843-850. doi:10.1136/annrheumdis-2013-204632</li>
                            <li>Takeuchi, T., Matsubara, T., Nitobe, T., Suematsu, E., Ohta, S., Honjo, S., . . . Miyasaka, N. (2013). Phase II dose-response study of abatacept in Japanese patients with active rheumatoid arthritis with an inadequate response to methotrexate. Mod Rheumatol, 23(2), 226-235. doi:10.1007/s10165-012-0668-z</li>
                            <li>Takeuchi, T., Miyasaka, N., Zang, C., Alvarez, D., Fletcher, T., Wajdula, J., . . . Vlahos, B. (2013). A phase 3 randomized, double-blind, multicenter comparative study evaluating the effect of etanercept versus methotrexate on radiographic outcomes, disease activity, and safety in Japanese subjects with active rheumatoid arthritis. Mod Rheumatol, 23(4), 623-633. doi:10.1007/s10165-012-0742-6</li>
                            <li>Tanaka, Y., Harigai, M., Takeuchi, T., Yamanaka, H., Ishiguro, N., Yamamoto, K., . . . Baker, D. (2012). Golimumab in combination with methotrexate in Japanese patients with active rheumatoid arthritis: results of the GO-FORTH study. Ann Rheum Dis, 71(6), 817-824. doi:10.1136/ard.2011.200317</li>
                            <li>Taylor, P. C., Keystone, E. C., van der Heijde, D., Weinblatt, M. E., Del Carmen Morales, L., Reyes Gonzaga, J., . . . Tanaka, Y. (2017). Baricitinib versus Placebo or Adalimumab in Rheumatoid Arthritis. N Engl J Med, 376(7), 652-662. doi:10.1056/NEJMoa1608345</li>
                            <li>van der Heijde, D., Tanaka, Y., Fleischmann, R., Keystone, E., Kremer, J., Zerbini, C., . . . Connell, C. A. (2013). Tofacitinib (CP-690,550) in patients with rheumatoid arthritis receiving methotrexate: twelve-month data from a twenty-four-month phase III randomized radiographic study. Arthritis Rheum, 65(3), 559-570. doi:10.1002/art.37816</li>
                            <li>van Vollenhoven, R. F., Fleischmann, R., Cohen, S., Lee, E. B., Garcia Meijide, J. A., Wagner, S., . . . Wilkinson, B. (2012). Tofacitinib or adalimumab versus placebo in rheumatoid arthritis. N Engl J Med, 367(6), 508-519. doi:10.1056/NEJMoa1112072</li>
                            <li>Weinblatt, M. E., Fleischmann, R., Huizinga, T. W., Emery, P., Pope, J., Massarotti, E. M., . . . Dougados, M. (2012). Efficacy and safety of certolizumab pegol in a broad population of patients with active rheumatoid arthritis: results from the REALISTIC phase IIIb study. Rheumatology (Oxford), 51(12), 2204-2214. doi:10.1093/rheumatology/kes150</li>
                            <li>Weinblatt, M. E., Keystone, E. C., Furst, D. E., Moreland, L. W., Weisman, M. H., Birbara, C. A., . . . Chartash, E. K. (2003). Adalimumab, a fully human anti-tumor necrosis factor alpha monoclonal antibody, for the treatment of rheumatoid arthritis in patients taking concomitant methotrexate: the ARMADA trial. Arthritis Rheum, 48(1), 35-45. doi:10.1002/art.10697</li>
                            <li>Weinblatt, M. E., Schiff, M., Valente, R., van der Heijde, D., Citera, G., Zhao, C., . . . Fleischmann, R. (2013). Head-to-head comparison of subcutaneous abatacept versus adalimumab for rheumatoid arthritis: findings of a phase IIIb, multinational, prospective, randomized study. Arthritis Rheum, 65(1), 28-38. doi:10.1002/art.37711</li>
                            <li>Yamamoto, K., Takeuchi, T., Yamanaka, H., Ishiguro, N., Tanaka, Y., Eguchi, K., . . . Koike, T. (2014). Efficacy and safety of certolizumab pegol plus methotrexate in Japanese rheumatoid arthritis patients with an inadequate response to methotrexate: the J-RAPID randomized, placebo-controlled trial. Mod Rheumatol, 24(5), 715-724. doi:10.3109/14397595.2013.864224</li>
                            <li>Yazici, Y., Curtis, J. R., Ince, A., Baraf, H., Malamet, R. L., Teng, L. L., & Kavanaugh, A. (2012). Efficacy of tocilizumab in patients with moderate to severe active rheumatoid arthritis and a previous inadequate response to disease-modifying antirheumatic drugs: the ROSE study. Ann Rheum Dis, 71(2), 198-205. doi:10.1136/ard.2010.148700</li>
                            </ul>"),
        div(bcbsButton('info_methods','Methods'))
    )
  })
})

output$downloadSnapshot_RA <- downloadHandler(
  filename = function() {
    paste0('MEDS_Snapshot_Rheumatoid_Arthritis.html')
  },
  
  content = function(file) {
    src <- normalizePath('snapshot_ra.Rmd')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'snapshot_ra.Rmd', overwrite = TRUE)
    
    library(rmarkdown)
    
    selected.meds <- str_extract(input$medBoxs, "^\\D+") %>% unique()
    
    out <- rmarkdown::render(input = "snapshot_ra.Rmd",
                             params = list(condition =  condition$selected, # input$condition,
                                           selected_meds = selected.meds,
                                           acr50_val = input$ACR50,
                                           das28_val = input$DAS28,
                                           haq_val = input$HAQ,
                                           disc_val = input$discont,
                                           si_val = input$severeAE,
                                           ijr_val = input$IJR_diarrhea,
                                           ra_data = RA_Data_user1))
    
    file.rename(out, file)
  }
)
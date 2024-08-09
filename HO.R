# Hemophilia A: On-demand server file-----------------------------------



observeEvent(c(input$ho, input$ho2), {
  condition$selected <- "Hemophilia A: On-demand"
}, ignoreInit = TRUE)

# Reset filters to default values
observe({
  req(input$reset)
  
  if (condition$selected == "Hemophilia A: On-demand") {
    updatePickerInput(session, "medBoxs_HO", "Select Medications", HO_Data$Name, HO_Data$Name)
  }
})


observeEvent(c(input$ho, input$ho2), {
  shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")
  shinyjs::show('customFilters_HO')
  shinyjs::show('bcbsaResults_HO')
  shinyjs::show('update')
  
  
  shinyjs::hide('homepage')
  
  shinyjs::hide('customFilters_RA')
  shinyjs::hide('bcbsaResults_RA')
  shinyjs::hide('customFilters_MS')
  shinyjs::hide('bcbsaResults_MS')
  shinyjs::hide('customFilters_HP')
  shinyjs::hide('bcbsaResults_HP')
  shinyjs::hide('customFilters_SA')
  shinyjs::hide('bcbsaResults_SA')
  shinyjs::hide('customFilters_PP')
  shinyjs::hide('bcbsaResults_PP')
  
}, ignoreInit = TRUE)


output$customFilters_HO<-renderUI({
  div(id = "customizeFilters",
      fluidRow(
        div(style = "margin-left: 15px; margin-top: -10px;",
            pickerInput("medBoxs_HO", label = "Select Medications",
                        choices = HO_Data$Name,
                        selected = HO_Data$Name,
                        multiple = TRUE,
                        width = 300,
                        options = list(`live-search` = TRUE,
                                       `selected-text-format` = "count > 3",
                                       `actions-box` = TRUE))
        ),
        div(id="inline",
            hr(),
            tags$h5("Efficacy Outcomes"),
            shinyjs::disabled(sliderInput('Bleeding_Control_HO',"Bleeding Control", 0,100,100)),
            hr()
        )
      )
  )
})

observe({
  req(input$ho_tab_box)
  
  if (input$ho_tab_box == "Preview Snapshot") {
    shinyjs::hide("more_info_MEDS_HO") #
    shinyjs::show("downloadSnapshot_HO")
  } else {
    shinyjs::show("more_info_MEDS_HO")
    shinyjs::hide("downloadSnapshot_HO")
  }
})


# Update Data Tables and Snapshot
observeEvent(c(input$ho, input$update, input$ho2), {
  
  if(isolate(condition$selected) == "Hemophilia A: On-demand") {
    
    
    meds_values$HO_MEDS_Sel <- isolate(input$medBoxs_HO)
    meds_values$BC_HO <- isolate(input$Bleeding_Control_HO)
    
    HO_Data_user<-openxlsx::read.xlsx('Data/HemoDemand/HemoDemand.xlsx', 'Sheet1',startRow = 1,
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
    HO_Data_user<-HO_Data_user[, !(colnames(HO_Data_user) %in% cols_remove)]
    HO_Data_user<-HO_Data_user[HO_Data_user$Name %in% isolate(meds_values$HO_MEDS_Sel),] 
    
    wtBC_HO <- isolate(meds_values$BC_HO)/100
    
    
    for(i in 1:nrow(HO_Data_user)){
      HO_Data_user$cNNT[i] <- wtBC_HO * HO_Data_user$NNT[i]
      HO_Data_user$LL[i] <- wtBC_HO * HO_Data_user$LL[i]
      HO_Data_user$UL[i] <- wtBC_HO * HO_Data_user$UL[i]
    }
    
    HO_Data_user <- HO_Data_user %>%
      rename("ceRank" = "CE.Ranking")
    
    HO_Data_user1<-HO_Data_user[, c("Name", "cNNT","ceRank","LL","UL")]
    HO_Data_user1 <- round_df(HO_Data_user1, digits = 2)
    HO_Data_user1$AVScore<-paste(HO_Data_user1$LL,'-',HO_Data_user1$UL)
    assign("HO_Data_user1", HO_Data_user1, .GlobalEnv)
    print('done!')
    
    meds_values$HO_Data_user1<-HO_Data_user1
    
    selected.meds <- str_extract(input$medBoxs_HO, "^\\D+") %>% unique()
    
    
    output$boxTitle_HO <- renderUI({
      req(input$update || input$ho || input$ho2)
      
      if (isolate(input$Bleeding_Control_HO) == 100&&
          
          length(isolate(input$medBoxs_HO)) == length(HO_Data$Name)) {
        
        h3('Results from BCBSA using default weights')
        
      } else {
        
        h3('Results from BCBSA using user-defined weights')
      }
      
    })
    
    # GENERATE MARKDOWN
    rmarkdown::render(input = "snapshot_ho.Rmd",
                      output_file = "snapshot_preview.html",
                      params = list(condition =  condition$selected,
                                    selected_meds = selected.meds,
                                    bc_val = input$Bleeding_Control_HO,
                                    ho_data = HO_Data_user1))
    
    xml2::write_html(rvest::html_node(xml2::read_html("snapshot_preview.html"), "body"), file = "snapshot_preview_HO.html")
    
    print('ac1')
    data <- HO_Data_user1[,c("Name", "cNNT","ceRank","AVScore")]
    # data <- HO_Data_user
    print('ac2')
    
    # DATA TABLE
    output$userDefRes_HO <- renderReactable({
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

    # output$userDefRes_HO<-DT::renderDataTable(datatable(data,
    #                                                     colnames = c('Medication', 'Composite NNT', 'Clinical Effectiveness Ranking', 'Adjusted Value Score'),
    #                                                     escape = F,
    #                                                     rownames = F,
    #                                                     selection = 'single',
    #                                                     filter = 'top',
    #                                                     options = list(
    #                                                       scrollX = TRUE,
    #                                                       scrollY = '40vh',
    #                                                       scrollCollapse = TRUE,
    #                                                       autoWidth = F)) %>% 
    #                                             formatStyle(c("Name", "cNNT","ceRank","AVScore"),
    #                                                         backgroundColor = styleEqual(c("F",NA,""),
    #                                                                                      c("#FF4E4E","#FF4E4E", "#FF4E4E"))) %>%
    #                                             formatStyle('ceRank',
    #                                                         backgroundColor = styleEqual(
    #                                                           c("D", "C", "B", "A"),
    #                                                           c('#FDF59F', '#E3E3E3', '#0074BB', '#A3E982'))
    #                                             )
    # )
    
    output$previewSnapshot_HO <- renderUI({
      includeHTML("snapshot_preview_HO.html")
    })
    
    output$bcbsaResults_HO <-renderUI({
      
      fluidRow(
        h2("Hemophilia A: On-demand"),
        h4("Use the filters on the left sidebar to select medications and efficacy outcome weight"),
        h4("Click the 'Run Analysis' button to update the data in the results table and snapshot. 
           Navigate to the 'Preview Snapshot' tab to preview and/or download the BCBS Snapshot report"),
        br(),
        
        bcbsTabBox(id = "ho_tab_box",
                   title = p(actionButton("more_info_MEDS_HO", label = NULL, icon = icon("info")),
                             shinyjs::hidden(downloadButton("downloadSnapshot_HO", "Download Snapshot"))),
                   width = 12,
                   tabPanel("Results Overview",
                            uiOutput("boxTitle_HO"),
                            reactableOutput('userDefRes_HO'),
                            tags$head(tags$style("#meds_info .modal-footer button {font-size: 20px; line-height: 25px; font-weight: bold; text-transform: uppercase;}")),
                            tags$head(tags$style("#meds_info .modal-header {padding-bottom: 0px; padding-top: 0px;}")),
                            tags$head(tags$style("#meds_info .modal-header button {display: none;}")),
                            
                            bsModal('meds_info_HO', 
                                    title = h2('MEDS Information', style = 'color: #005876; font-weight: bold; line-height: 25px;'),
                                    size = 'large',
                                    trigger = 'more_info_MEDS_HO', 
                                    uiOutput('info_details_HO') 
                            )
                            
                   ),
                   tabPanel("Preview Snapshot",
                            uiOutput("previewSnapshot_HO")#, 
                            # downloadButton("downloadSnapshot_HO", "Download Snapshot")
                   )
        )
      )
      
    })
    
  }
}, ignoreInit = TRUE)




observeEvent(input$more_info_MEDS_HO,{
  output$info_details_HO<-renderUI({
    
    
    div(shiny::HTML(
      '<h4>Methods</h4>
                         <p></p><br>
                        
                         <br>
                         
                         <h4>Study eligibility criteria</h4>
                         <ul>
                            <li>Patients with severe hemophilia A (Factor VIII levels <1% or <2%) who could be previously treated or untreated </li>
                            <li>Minimum of 24 weeks of follow up</li>
                            <li>Included at least one of the efficacy outcomes </li>
                            <li>Excluded</li>
                            <ul>
                            <li>Observational Studies</li>
                            <li>Patients with acquired hemophilia</li>
                            <li>Follow-up period shorter than 24 weeks</li>
                            <li>Study population included mild and moderate patients (>2% Factor VIII levels) </li>
                            <li>Reported only surgical prophylaxis </li>
                            <li>Study population includes only patients with inhibitors </li>
                            </ul>
                          
                            <li>Published before January 1,2020</li>
                            <li>FDA approved dosage regimens of the eligible medications, as recommended to be included by the BCBS plans</li>
                          </ul><br>
                          
                          <h4>Eligible Medications</h4>
                         <ul>
                          <h5>Advate (Recombinant factor VIII)</h5>
                          <h5>Adynovate (Recombinant, PEGylated factor VIII)</h5>
                          <h5>Afstyla (Recombinant, single chain, factor VIII)</h5>
                          <h5>Eloctate (Recombinant, Fc Fusion Protein, factor VIII)</h5>
                          <h5>Kogenate FS (Recombinant, factor VIII)</h5>
                          <h5>Kovaltry (Recombinant, factor VIII)</h5>
                          <h5>NovoEight (Recombinant, B-domain truncated, factor VIII)</h5>
                          <h5>Nuwiq (Recombinant, B-domain deleted, factor VIII)</h5>
                          <h5>Recombinate (Recombinant, factor VIII)</h5>
                          <h5>Xyntha (Recombinant, B-domain deleted, factor VIII)</h5>
                          <h5>Hemofil M (Human Plasma-derived immunoaffinity purified, factor VIII)</h5>
                          <h5>Koate-DVI (antihemophilic factor, human, factor VIII)</h5>
                          <h5>Jivi (recombinant, PEGylated-aucl, factor VIII)</h5>
                          <h5>Esperoct (recombinant, glycopegylated-exei, factor VIII)</h5>
                          <h5>Hemlibra (emicizumab-kxwh)</h5>
                          </ul><br>
                          
                        
                          
                          <h4>Efficacy Outcomes</h4>
                         <ul>
                           <h5>Proportion of bleeding episodes that is controlled using no more than 2 infusions </h5>
                          </ul><br>
                          
                          <h4>Safety Outcomes</h4>
                         <ul>
                          <h5>The incidence of development of inhibitors was too small to detect a difference, with most studies reporting zero, therefore, was not included.</h5>
                          </ul><br>
                         
                         
                            
                            <h4>Study comparator and Meta-analysis</h4>
                            <ul>
                            <li>Hemophilia A studies generally compare prophylaxis with on-demand treatment using the same medication or are single-arm studies.</li>
                            <li>We matched prophylaxis arms from different studies to simulate head-to-head trials based on covariates (age, follow-up period and other study population characteristics (% severe hemophilia, % previously treated, % with inhibitors) in order to conduct a network meta-analysis. We did the same between on-demand arms.</li>
                            <li>Kogenate FS was chosen as the reference comparator based on it’s market share as well as it being the oldest drug with the relevant data available (Reference: IPD Analytics) </li>
                            
                          
                          
                                 </ul>'),                           
      div(bcbsButton('ref_list_HO','References'))
      
    )
    
    
    
    
  })
})

observeEvent(input$info_methods_HO,{
  output$info_details_HO<-renderUI({
    div(shiny::HTML(
      '<h4>Methods</h4>
                         <p></p><br>
                        
                         <br>
                         
                         <h4>Study eligibility criteria</h4>
                         <ul>
                            <li>Patients with severe hemophilia A (Factor VIII levels <1% or <2%) who could be previously treated or untreated </li>
                            <li>Minimum of 24 weeks of follow up</li>
                            <li>Included at least one of the efficacy outcomes </li>
                            <li>Excluded</li>
                            <ul>
                            <li>Observational Studies</li>
                            <li>Patients with acquired hemophilia</li>
                            <li>Follow-up period shorter than 24 weeks</li>
                            <li>Study population included mild and moderate patients (>2% Factor VIII levels) </li>
                            <li>Reported only surgical prophylaxis </li>
                            <li>Study population includes only patients with inhibitors </li>
                            </ul>
                          
                            <li>Published before January 1,2020</li>
                            <li>FDA approved dosage regimens of the eligible medications, as recommended to be included by the BCBS plans</li>
                          </ul><br>
                          
                          <h4>Eligible Medications</h4>
                         <ul>
                          <h5>Advate (Recombinant factor VIII)</h5>
                          <h5>Adynovate (Recombinant, PEGylated factor VIII)</h5>
                          <h5>Afstyla (Recombinant, single chain, factor VIII)</h5>
                          <h5>Eloctate (Recombinant, Fc Fusion Protein, factor VIII)</h5>
                          <h5>Kogenate FS (Recombinant, factor VIII)</h5>
                          <h5>Kovaltry (Recombinant, factor VIII)</h5>
                          <h5>NovoEight (Recombinant, B-domain truncated, factor VIII)</h5>
                          <h5>Nuwiq (Recombinant, B-domain deleted, factor VIII)</h5>
                          <h5>Recombinate (Recombinant, factor VIII)</h5>
                          <h5>Xyntha (Recombinant, B-domain deleted, factor VIII)</h5>
                          <h5>Hemofil M (Human Plasma-derived immunoaffinity purified, factor VIII)</h5>
                          <h5>Koate-DVI (antihemophilic factor, human, factor VIII)</h5>
                          <h5>Jivi (recombinant, PEGylated-aucl, factor VIII)</h5>
                          <h5>Esperoct (recombinant, glycopegylated-exei, factor VIII)</h5>
                          <h5>Hemlibra (emicizumab-kxwh)</h5>
                          </ul><br>
                          
                        
                          
                          <h4>Efficacy Outcomes</h4>
                         <ul>
                           <h5>Proportion of bleeding episodes that is controlled using no more than 2 infusions </h5>
                          </ul><br>
                          
                          <h4>Safety Outcomes</h4>
                         <ul>
                          <h5>The incidence of development of inhibitors was too small to detect a difference, with most studies reporting zero, therefore, was not included.</h5>
                          </ul><br>
                         
                         
                            
                            <h4>Study comparator and Meta-analysis</h4>
                            <ul>
                            <li>Hemophilia A studies generally compare prophylaxis with on-demand treatment using the same medication or are single-arm studies.</li>
                            <li>We matched prophylaxis arms from different studies to simulate head-to-head trials based on covariates (age, follow-up period and other study population characteristics (% severe hemophilia, % previously treated, % with inhibitors) in order to conduct a network meta-analysis. We did the same between on-demand arms.</li>
                            <li>Kogenate FS was chosen as the reference comparator based on it’s market share as well as it being the oldest drug with the relevant data available (Reference: IPD Analytics) </li>
                            
                          
                          
                                 </ul>'),   
      
      
      
      div(bcbsButton('ref_list_HO','References'))
    )
  })
})

observeEvent(input$ref_list_HO,{
  output$info_details_HO <- renderUI({
    div(shiny::HTML("<h3>References</h3>
                            <ul>
                            <li>Blanchette, V. S., Shapiro, A. D., Liesner, R. J., Hernandez Navarro, F., Warrier, I., Schroth, P. C., . . . Ewenstein, B. M. (2008). Plasma and albumin-free recombinant factor VIII: pharmacokinetics, efficacy and safety in previously treated pediatric patients. J Thromb Haemost, 6(8), 1319-1326. doi:10.1111/j.1538-7836.2008.03032.x</li>
                            <li>Courter, S. G., & Bedrosian, C. L. (2001). Clinical evaluation of B-domain deleted recombinant factor VIII in previously untreated patients. Semin Hematol, 38(2 Suppl 4), 52-59. doi:10.1016/s0037-1963(01)90109-x</li>
                            <li>Giangrande, P., Andreeva, T., Chowdary, P., Ehrenforth, S., Hanabusa, H., Leebeek, F. W., . . . Oldenburg, J. (2017). Clinical evaluation of glycoPEGylated recombinant FVIII: Efficacy and safety in severe haemophilia A. Thromb Haemost, 117(2), 252-261. doi:10.1160/th16-06-0444</li>
                            <li>Giangrande, P. L. (2002). Safety and efficacy of KOGENATE Bayer in previously untreated patients (PUPs) and minimally treated patients (MTPs). Haemophilia, 8 Suppl 2, 19-22. doi:10.1046/j.1351-8216.2001.00133.x</li>
                            <li>Kavakli, K., Yang, R., Rusen, L., Beckmann, H., Tseneklidou-Stoeter, D., & Maas Enriquez, M. (2015). Prophylaxis vs. on-demand treatment with BAY 81-8973, a full-length plasma protein-free recombinant factor VIII product: results from a randomized trial (LEOPOLD II). J Thromb Haemost, 13(3), 360-369. doi:10.1111/jth.12828</li>
                            <li>Klukowska, A., Szczepanski, T., Vdovin, V., Knaub, S., Bichler, J., Jansen, M., . . . Liesner, R. J. (2018). Long-term tolerability, immunogenicity and efficacy of Nuwiq((R)) (human-cl rhFVIII) in children with severe haemophilia A. Haemophilia, 24(4), 595-603. doi:10.1111/hae.13460</li>
                            <li>Klukowska, A., Szczepanski, T., Vdovin, V., Knaub, S., Jansen, M., & Liesner, R. (2016). Novel, human cell line-derived recombinant factor VIII (Human-cl rhFVIII, Nuwiq((R)) ) in children with severe haemophilia A: efficacy, safety and pharmacokinetics. Haemophilia, 22(2), 232-239. doi:10.1111/hae.12797</li>
                            <li>Konkle, B. A., Stasyshyn, O., Chowdary, P., Bevan, D. H., Mant, T., Shima, M., . . . Abbuehl, B. (2015). Pegylated, full-length, recombinant factor VIII for prophylactic and on-demand treatment of severe hemophilia A. Blood, 126(9), 1078-1085. doi:10.1182/blood-2015-03-630897</li>
                            <li>Lalezari, S., Reding, M. T., Pabinger, I., Holme, P. A., Negrier, C., Chalasani, P., . . . Maas Enriquez, M. (2019). BAY 94-9027 prophylaxis is efficacious and well tolerated for up to >5 years with extended dosing intervals: PROTECT VIII extension interim results. Haemophilia, 25(6), 1011-1019. doi:10.1111/hae.13853</li>
                            <li>Lentz, S. R., Janic, D., Kavakli, K., Miljic, P., Oldenburg, J., M, C. O., . . . Tiede, A. (2018). Long-term safety and efficacy of turoctocog alfa in prophylaxis and treatment of bleeding episodes in severe haemophilia A: Final results from the guardian 2 extension trial. Haemophilia, 24(6), e391-e394. doi:10.1111/hae.13617</li>
                            <li>Lentz, S. R., Misgav, M., Ozelo, M., Salek, S. Z., Veljkovic, D., Recht, M., . . . Martinowitz, U. (2013). Results from a large multinational clinical trial (guardian1) using prophylactic treatment with turoctocog alfa in adolescent and adult patients with severe haemophilia A: safety and efficacy. Haemophilia, 19(5), 691-697. doi:10.1111/hae.12159</li>
                            <li>Liesner, R. J., Abashidze, M., Aleinikova, O., Altisent, C., Belletrutti, M. J., Borel-Derlon, A., . . . Neufeld, E. J. (2018). Immunogenicity, efficacy and safety of Nuwiq((R)) (human-cl rhFVIII) in previously untreated patients with severe haemophilia A-Interim results from the NuProtect Study. Haemophilia, 24(2), 211-220. doi:10.1111/hae.13320</li>
                            <li>Lissitchkov, T., Rusen, L., Georgiev, P., Windyga, J., Klamroth, R., Gercheva, L., . . . Pasi, K. J. (2017). PK-guided personalized prophylaxis with Nuwiq((R)) (human-cl rhFVIII) in adults with severe haemophilia A. Haemophilia, 23(5), 697-704. doi:10.1111/hae.13251</li>
                            <li>Ljung, R., Kenet, G., Mancuso, M. E., Kaleva, V., Rusen, L., Tseneklidou-Stoeter, D., . . . Maas Enriquez, M. (2016). BAY 81-8973 safety and efficacy for prophylaxis and treatment of bleeds in previously treated children with severe haemophilia A: results of the LEOPOLD Kids Trial. Haemophilia, 22(3), 354-360. doi:10.1111/hae.12866</li>
                            <li>Lusher, J., Abildgaard, C., Arkin, S., Mannucci, P. M., Zimmermann, R., Schwartz, L., & Hurst, D. (2004). Human recombinant DNA-derived antihemophilic factor in the treatment of previously untreated patients with hemophilia A: final report on a hallmark clinical investigation. J Thromb Haemost, 2(4), 574-583. doi:10.1111/j.1538-7933.2004.00646.x</li>
                            <li>Mahlangu, J., Kuliczkowski, K., Karim, F. A., Stasyshyn, O., Kosinova, M. V., Lepatan, L. M., . . . Pabinger, I. (2016). Efficacy and safety of rVIII-SingleChain: results of a phase 1/3 multicenter clinical trial in severe hemophilia A. Blood, 128(5), 630-637. doi:10.1182/blood-2016-01-687434</li>
                            <li>Mahlangu, J., Oldenburg, J., Paz-Priel, I., Negrier, C., Niggli, M., Mancuso, M. E., . . . Kruse-Jarres, R. (2018). Emicizumab Prophylaxis in Patients Who Have Hemophilia A without Inhibitors. N Engl J Med, 379(9), 811-822. doi:10.1056/NEJMoa1803550</li>
                            <li>Mahlangu, J., Powell, J. S., Ragni, M. V., Chowdary, P., Josephson, N. C., Pabinger, I., . . . Pierce, G. F. (2014). Phase 3 study of recombinant factor VIII Fc fusion protein in severe hemophilia A. Blood, 123(3), 317-325. doi:10.1182/blood-2013-10-529974</li>
                            <li>Manco-Johnson, M. J., Kempton, C. L., Reding, M. T., Lissitchkov, T., Goranov, S., Gercheva, L., . . . Hong, W. (2013). Randomized, controlled, parallel-group trial of routine prophylaxis vs. on-demand treatment with sucrose-formulated recombinant factor VIII in adults with severe hemophilia A (SPINART). J Thromb Haemost, 11(6), 1119-1127. doi:10.1111/jth.12202</li>
                            <li>Meunier, S., Alamelu, J., Ehrenforth, S., Hanabusa, H., Abdul Karim, F., Kavakli, K., . . . Rageliene, L. (2017). Safety and efficacy of a glycoPEGylated rFVIII (turoctocog alpha pegol, N8-GP) in paediatric patients with severe haemophilia A. Thromb Haemost, 117(9), 1705-1713. doi:10.1160/th17-03-0166</li>
                            <li>Mullins, E. S., Stasyshyn, O., Alvarez-Roman, M. T., Osman, D., Liesner, R., Engl, W., . . . Abbuehl, B. E. (2017). Extended half-life pegylated, full-length recombinant factor VIII for prophylaxis in children with severe haemophilia A. Haemophilia, 23(2), 238-246. doi:10.1111/hae.13119</li>
                            <li>Pipe, S. W., Shima, M., Lehle, M., Shapiro, A., Chebon, S., Fukutake, K., . . . Jimenez-Yuste, V. (2019). Efficacy, safety, and pharmacokinetics of emicizumab prophylaxis given every 4 weeks in people with haemophilia A (HAVEN 4): a multicentre, open-label, non-randomised phase 3 study. Lancet Haematol, 6(6), e295-e305. doi:10.1016/s2352-3026(19)30054-7</li>
                            <li>Powell, J., Martinowitz, U., Windyga, J., Di Minno, G., Hellmann, A., Pabinger, I., . . . Ingerslev, J. (2012). Efficacy and safety of prophylaxis with once-weekly BAY 79-4980 compared with thrice-weekly rFVIII-FS in haemophilia A patients. A randomised, active-controlled, double-blind study. Thromb Haemost, 108(5), 913-922. doi:10.1160/th12-03-0188</li>
                            <li>Reding, M. T., Ng, H. J., Poulsen, L. H., Eyster, M. E., Pabinger, I., Shin, H. J., . . . Michaels, L. A. (2017). Safety and efficacy of BAY 94-9027, a prolonged-half-life factor VIII. J Thromb Haemost, 15(3), 411-419. doi:10.1111/jth.13597</li>
                            <li>Rothschild, C., Scharrer, I., Brackmann, H. H., Stieltjes, N., Vicariot, M., Torchet, M. F., & Effenberger, W. (2002). European data of a clinical trial with a sucrose formulated recombinant factor VIII in previously treated haemophilia A patients. Haemophilia, 8 Suppl 2, 10-14. doi:10.1046/j.1351-8216.2001.00131.x</li>
                            <li>Rusen, L., Kavakli, K., Korth-Bradley, J., Huard, F., Rendo, P., Fuiman, J., . . . Rupon, J. (2018). Clinical experience with moroctocog alfa (AF-CC) in younger paediatric patients with severe haemophilia A: Two open-label studies. Haemophilia, 24(4), 604-610. doi:10.1111/hae.13466</li>
                            <li>Saxena, K., Lalezari, S., Oldenburg, J., Tseneklidou-Stoeter, D., Beckmann, H., Yoon, M., & Maas Enriquez, M. (2016). Efficacy and safety of BAY 81-8973, a full-length recombinant factor VIII: results from the LEOPOLD I trial. Haemophilia, 22(5), 706-712. doi:10.1111/hae.12952</li>
                            <li>Schwartz, R. S., Abildgaard, C. F., Aledort, L. M., Arkin, S., Bloom, A. L., Brackmann, H. H., . . . et al. (1990). Human recombinant DNA-derived antihemophilic factor (factor VIII) in the treatment of hemophilia A. recombinant Factor VIII Study Group. N Engl J Med, 323(26), 1800-1805. doi:10.1056/nejm199012273232604</li>
                            <li>Seremetis, S., Lusher, J. M., Abildgaard, C. F., Kasper, C. K., Allred, R., & Hurst, D. (1999). Human recombinant DNA-derived antihaemophilic factor (factor VIII) in the treatment of haemophilia A: conclusions of a 5-year study of home therapy. The KOGENATE Study Group. Haemophilia, 5(1), 9-16. doi:10.1046/j.1365-2516.1999.00191.x</li>
                            <li>Shapiro, A., Gruppo, R., Pabinger, I., Collins, P. W., Hay, C. R., Schroth, P., . . . Ewenstein, B. M. (2009). Integrated analysis of safety and efficacy of a plasma- and albumin-free recombinant factor VIII (rAHF-PFM) from six clinical studies in patients with hemophilia A. Expert Opin Biol Ther, 9(3), 273-283. doi:10.1517/14712590902729392</li>
                            <li>Stasyshyn, O., Djambas Khayat, C., Iosava, G., Ong, J., Abdul Karim, F., Fischer, K., . . . Pabinger, I. (2017). Safety, efficacy and pharmacokinetics of rVIII-SingleChain in children with severe hemophilia A: results of a multicenter clinical trial. J Thromb Haemost, 15(4), 636-644. doi:10.1111/jth.13647</li>
                            <li>Tagliaferri, A., Feola, G., Molinari, A. C., Santoro, C., Rivolta, G. F., Cultrera, D. B., . . . Coppola, A. (2015). Benefits of prophylaxis versus on-demand treatment in adolescents and adults with severe haemophilia A: the POTTER study. Thromb Haemost, 114(1), 35-45. doi:10.1160/th14-05-0407</li>
                            <li>Tarantino, M. D., Collins, P. W., Hay, C. R., Shapiro, A. D., Gruppo, R. A., Berntorp, E., . . . Ewenstein, B. M. (2004). Clinical evaluation of an advanced category antihaemophilic factor prepared using a plasma/albumin-free method: pharmacokinetics, efficacy, and safety in previously treated patients with haemophilia A. Haemophilia, 10(5), 428-437. doi:10.1111/j.1365-2516.2004.00932.x</li>
                            <li>Tiede, A., Oldenburg, J., Lissitchkov, T., Knaub, S., Bichler, J., & Manco-Johnson, M. J. (2016). Prophylaxis vs. on-demand treatment with Nuwiq((R)) (Human-cl rhFVIII) in adults with severe haemophilia A. Haemophilia, 22(3), 374-380. doi:10.1111/hae.12859</li>
                            <li>Valentino, L. A., Mamonov, V., Hellmann, A., Quon, D. V., Chybicka, A., Schroth, P., . . . Wong, W. Y. (2012). A randomized comparison of two prophylaxis regimens and a paired comparison of on-demand and prophylaxis treatments in hemophilia A management. J Thromb Haemost, 10(3), 359-367. doi:10.1111/j.1538-7836.2011.04611.x</li>
                    </ul>"),
        div(bcbsButton('info_methods_HO','Methods'))
    )
  })
})





output$downloadSnapshot_HO <- downloadHandler(
  filename = function() {
    paste0('MEDS_Snapshot_Hemophilia_On_Demand.html')
  },
  
  content = function(file) {
    src <- normalizePath('snapshot_ho.Rmd')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'snapshot_ho.Rmd', overwrite = TRUE)
    
    library(rmarkdown)
    
    selected.meds <- str_extract(input$medBoxs_HO, "^\\D+") %>% unique()
    
    out <- rmarkdown::render(input = "snapshot_ho.Rmd",
                             output_file = "snapshot_preview.html",
                             params = list(condition =  condition$selected,
                                           selected_meds = selected.meds,
                                           bc_val = input$Bleeding_Control_HO,
                                           ho_data = meds_values$HO_Data_user1))
    
    file.rename(out, file)
  }
)


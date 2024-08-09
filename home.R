# home.R 

# shinyjs::show('homePage')
shinyjs::hide('customFilters_RA')
shinyjs::hide('customFilters_MS')
shinyjs::hide('customFilters_HO')
shinyjs::hide('customFilters_HP')
shinyjs::hide('customFilters_SA')
shinyjs::hide('customFilters_PP')
shinyjs::hide('update')
shinyjs::hide('bcbsaResults_RA')
shinyjs::hide('bcbsaResults_MS')
shinyjs::hide('bcbsaResults_HO')
shinyjs::hide('bcbsaResults_HP')
shinyjs::hide('bcbsaResults_SA')
shinyjs::hide('bcbsaResults_PP')





observeEvent(input$home, {
  
  # shinyjs::show('ra')
  # shinyjs::show('ms')
  # shinyjs::show('hap')
  # shinyjs::show('hao')
  # shinyjs::show('sa')
  # shinyjs::show('pp')
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
  shinyjs::show('homepage')
  
  
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  shinyjs::hide('customFilters_RA')
  shinyjs::hide('customFilters_MS')
  shinyjs::hide('customFilters_HO')
  shinyjs::hide('customFilters_HP')
  shinyjs::hide('customFilters_SA')
  shinyjs::hide('customFilters_PP')
  
  shinyjs::hide('bcbsaResults_RA')
  shinyjs::hide('bcbsaResults_MS')
  shinyjs::hide('bcbsaResults_HO')
  shinyjs::hide('bcbsaResults_HP')
  shinyjs::hide('bcbsaResults_SA')
  shinyjs::hide('bcbsaResults_PP')
  
  # shinyjs::hide('customFilters_HAP')
  
  # shinyjs::hide('customFilters_SA')
  # shinyjs::hide('customFilters_PP')
  
  # shinyjs::hide('update')
  # shinyjs::hide('bcbsaResults_RA')
  # shinyjs::hide('bcbsaResults_MS')
  # shinyjs::hide('customFilters_HAP')
  # shinyjs::hide('customFilters_HAO')
  # shinyjs::hide('customFilters_SA')
  # shinyjs::hide('customFilters_PP')
}, ignoreInit = TRUE)

# output$homePage <- renderUI({
#   
#   # Buttons
#   fluidRow(
#     div(class = "nav-buttons",
#         fluidRow(
#           actionButton("ra","Rheumatoid Arthritis"),
#           actionButton("ms","Multiple Sclerosis"),
#           actionButton("hap","Hemophilia A: Prophylaxis"),
#           actionButton("hao", "Hemophilia A: On-demand"),
#           actionButton("sa", "Severe Asthma"),
#           actionButton("pp","Plaque Psoriasis")
#         )
#     )
#   )
#   
# })

observeEvent(input$faq, {
  
  output$FAQ <- renderUI({
    fluidRow(
      column(width = 12,
             bcbsBox(title = "What is MEDS?",
                     solidHeader = TRUE, 
                     collapsible = TRUE, 
                     collapsed = TRUE, 
                     width = 12, 
                     status = "primary",
                     h4("MEDS stands for Specialty Medication Efficacy Determination System. This is a model developed by BCBSA to conduct comparative clinical effectiveness analysis of specialty pharmacy drugs.")
             ),
             bcbsBox(title = "Why did BCBSA start MEDS initiative?",
                     solidHeader = TRUE, 
                     collapsible = TRUE, 
                     collapsed = TRUE, 
                     width = 12, 
                     status = "primary",
                     h4("Specialty pharmacy being one of the most difficult challenges for health plans to manage for their members, with the support and guidance of key stakeholders BCBSA started the MEDS model initiative as a platform that could be a resource for Plans to evaluate specialty pharmacy drugs and to help minimize the wide variation in the evaluation process between different Plans.")
             ),
             bcbsBox(title = "Who decides which medication class will be reviewed under MEDS?",
                     solidHeader = TRUE, 
                     collapsible = TRUE, 
                     collapsed = TRUE, 
                     width = 12, 
                     status = "primary",
                     h4("A group of key stakeholders provide critical feedback and recommendations on the specialty drugs/disease state for the MEDS analysis via surveys/group discussions that includes BCBS Plan Executives and Clinical Pharmacists (at least one person per plan) ")
             ),
             bcbsBox(title = "Where can I access the methodology used for a MEDS model?",
                     solidHeader = TRUE, 
                     collapsible = TRUE, 
                     collapsed = TRUE, 
                     width = 12, 
                     status = "primary",
                     div(h4("You can find the methods used for a MEDS model by clicking the information button."), img(src="info-button.png", width = "60px")),
                     h4("You can also find the list of references there.")
             ),
             bcbsBox(title = "How the methodology for a MEDS model is developed?",
                     solidHeader = TRUE, 
                     collapsible = TRUE, 
                     collapsed = TRUE, 
                     width = 12, 
                     status = "primary",
                     h4("Using NNT, NNH and Persistence as the foundational clinical effectiveness measures, methodology for a MEDS model is developed based on discussion/consensus/voting with the key stakeholders.
                        The following key stakeholders provide critical feedback and recommendations on the specialty drugs/disease state for the MEDS analysis via surveys/group discussions:"),
                     h4("⦁	Blue Cross and Blue Shield Plan Executive and Clinical Pharmacists"),
                     h4("⦁	Blue Cross and Blue Shield Association’s Specialty Pharmacy Medical Benefit Solutions Team (SPMBS)")
                     ),
                     
             bcbsBox(title = "Why use NNT/NNH?",
                             solidHeader = TRUE, 
                             collapsible = TRUE, 
                             collapsed = TRUE, 
                             width = 12, 
                             status = "primary",
                             h4("NNT (Number Needed to Treat) and NNH (Number Needed to Harm) were adopted by the key stakeholders as foundational effectiveness measures in MEDS models to address the variability in assessment measures between Plans."), 
h4("These measures were supported and adopted by the National Council of Physician and Pharmacist Executives (NCPE), comprised by the Chief Medical Officers and Chief Pharmacy Officers of each BCBS Plan, in 2017."),
h4("In addition to the NCPE Members, the Specialty MEDS Model was developed with the support and guidance of external subject matter experts from Prime Therapeutics, Memorial Sloan Kettering and the University of Maryland.")
),

       bcbsBox(title = "What type of studies are included in MEDS model?",
        solidHeader = TRUE, 
        collapsible = TRUE, 
        collapsed = TRUE, 
        width = 12, 
        status = "primary",
        h4("The process of the systematic review of clinical literature is based on established, industry-accepted standards, 
        including ICER, the Agency for Healthcare Research and Quality (AHRQ) and the National Institute for Health Research.
"
        )
        ),
        
        bcbsBox(title = "How frequently MEDS model results are updated?
",
                solidHeader = TRUE, 
                collapsible = TRUE, 
                collapsed = TRUE, 
                width = 12, 
                status = "primary",
                h4("
"
                )
                ),
         bcbsBox(title = "Can I customize MEDS model based on my plan's need?",
                        solidHeader = TRUE, 
                        collapsible = TRUE, 
                        collapsed = TRUE, 
                        width = 12, 
                        status = "primary",
                        h4("Yes. From the listed drugs for a condition/disease state you can select which ones you want to include in your model 
                        and you can also change the weights assigned to the benefit and harms outcomes in the model."),
                        h4("Clinical effectiveness ranking and adjusted value scores will be updated based on your selection. 
                           You can also create a snapshot of the customized results and download that as an html file.")
                 ),
           bcbsBox(title = "How can I request MEDS review of a drug class?",
                         solidHeader = TRUE, 
                         collapsible = TRUE, 
                         collapsed = TRUE, 
                         width = 12, 
                         status = "primary",
                         h4("")
           )
      )
    )

  })
  
}, ignoreInit = TRUE)


observeEvent(input$tutorial, {
  
  sendSweetAlert(session = getDefaultReactiveDomain(),
                 title = "Specialty Meds App Interactive Tutorial",
                 text = tags$div(
                   br(),
                   h4("This interactive tutorial will provide a step-by-step demo to guide you through the use of the Speciality Meds app."),
                   br(),
                   h4("Click the 'Start Tutorial' button to begin the demo."),
                   br(),
                   h4("If you would like to skip the tutorial, click the 'close' button found on the top right corner of the window to return to the home page"),
                   br(),
                   hr(),
                   br(),
                   bcbsButton("start_tutorial", "Begin Tutorial!")
                 ),
                 type = "info",
                 btn_labels = NULL,
                 btn_colors = NULL,
                 html = FALSE,
                 closeOnClickOutside = FALSE,
                 showCloseButton = TRUE,
                 width = "700px")
    
  
}, ignoreInit = TRUE)


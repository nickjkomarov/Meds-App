##Initiate the packages####
# library(shiny)
# library(shinycssloaders)
# library(shinyjs)
# library(shinyWidgets)
# library(shinyBS)
# # library(shinyhelper)
# library(shinybusy)
# library(DT)
# library(data.table)
# library(dplyr)
# library(shinydashboard)
# library(openxlsx)
# library(stringr)
# library(shinyalert)

source('./functions/helper_functions.R')
source('./Ref_func.R')

RA_Data <- openxlsx::read.xlsx('Data/RA/RA_Data.xlsx', 'Sheet1',startRow = 1,
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

MS_Data<-openxlsx::read.xlsx('Data/MS/MS_Data.xlsx', 'Sheet1',startRow = 1,
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

HP_Data <- openxlsx::read.xlsx('Data/HemoProph/HemoProph.xlsx', 'Sheet1',startRow = 1,
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

HO_Data <- openxlsx::read.xlsx('Data/HemoDemand/HemoDemand.xlsx', 'Sheet1',startRow = 1,
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

SA_Data <- openxlsx::read.xlsx('Data/Asthma/Asthma.xlsx', 'Sheet1',startRow = 1,
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

PP_Data <- openxlsx::read.xlsx('Data/Psoriasis/Psoriasis.xlsx','Sheet1',startRow=1,
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
                               fillMergedCells = FALSE ) %>%
  filter(Arm != "GP2017 Adalimumab biosimilar")


meds_values <- reactiveValues()

BCBS <- reactiveValues(Data = NULL)

Snapshot <- reactiveValues(Preview = NULL)

Tab <- reactiveValues(Title = NULL)

condition <- reactiveValues(selected = NULL)




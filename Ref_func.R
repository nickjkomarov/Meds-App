library(easyPubMed)
library(qdapRegex)
library(stringi)

RefSource2 <<- function(search_PMID){
  myIdList <- get_pubmed_ids(search_PMID)
  if(as.numeric(myIdList$Count) > 0){
    toprecords <- fetch_pubmed_data(myIdList, retmax = 20,encoding = "UTF-8")
    toprecords_list = articles_to_list(toprecords)
    x1_1 <- ifelse(lapply(unlist(toprecords_list),
                          function(x) sum(custom_grep(x,"LastName","char")!=0) >3), 
                   TRUE, FALSE)
    
    
    xfirstauthor = NULL #character(length(toprecords_list))
    xyear = NULL #character(length(toprecords_list))
    xmonth = NULL #character(length(toprecords_list))
    xday = NULL #character(length(toprecords_list))
    xref = NULL #character(length(toprecords_list))
    xabs = NULL #character(length(toprecords_list))
    xpmid = NULL #character(length(toprecords_list))
    xpubtype = NULL #character(length(toprecords_list))
    xpublidate = NULL #character(length(toprecords_list))
    
    
    for (i in 1:length(toprecords_list)){
      
      if(x1_1[i] == TRUE){
        x1 <- paste(custom_grep(unlist(toprecords_list)[i],"LastName","char")[1:3],
                    custom_grep(unlist(toprecords_list)[i],"Initials","char")[1:3],
                    collapse = ", ")
        x1 <- paste(x1, "et al")
      }else{
        x1 <- paste(custom_grep(unlist(toprecords_list)[i],"LastName","char"),
                    custom_grep(unlist(toprecords_list)[i],"Initials","char"),
                    collapse = ", ")
      }
      x1 = gsub(", NA, NANA", "", x1)
      x2_1 = custom_grep(unlist(toprecords_list)[i],"PubDate","char")
      x2_1 = qdap::bracketX(x2_1,"angle")
      x2_1 = gsub("[[:punct:]]", " ",x2_1)
      x2_1 = paste0(strsplit(x2_1[[1]],split = " ")[[1]][1:2],collapse = " ")
      x2_1 = suppressWarnings(paste0(c(strsplit(x2_1,split = " ")[[1]][1],
                                       ifelse(!is.na(as.numeric(strsplit(x2_1,split = " ")[[1]][2])),
                                              month.abb[as.numeric(strsplit(x2_1,split = " ")[[1]][2])],
                                              strsplit(x2_1,split = " ")[[1]][2])),collapse = " "))
      x2_1 = gsub("NA","",x2_1)
      x2 = trimws(x2_1)
      
      x3_1 = custom_grep(unlist(toprecords_list)[i],"Volume","char")
      x3_1 = ifelse(is.null(x3_1),NA,x3_1)
      
      x3_2 = custom_grep(unlist(toprecords_list)[i],"MedlinePgn","char")
      x3_2 = ifelse(is.null(x3_2),NA,x3_2)
      
      x3 <- ifelse(is.na(x3_1), x3_2, x3_1)
      
      x4_1 = custom_grep(unlist(toprecords_list)[i],"Issue","char")
      x4_1 = ifelse(is.null(x4_1),NA,x4_1)
      
      x4 = ifelse(is.na(x4_1), paste0(":", x3_2), paste0("(", x4_1, ")"))
      
      x5 <- paste0(x2,
                   ifelse(is.na(x3), "", 
                          ifelse(is.na(x4), 
                                 paste0(";", x3), 
                                 paste0(";", x3, x4))))
      
      x6 = custom_grep(unlist(toprecords_list)[i],"Abstract","char")
      x6 = qdap::bracketX(x6,"angle")
      
      x_title = custom_grep(unlist(toprecords_list)[i],"ArticleTitle","char")[1]
      x_journal = custom_grep(unlist(toprecords_list)[i],"ISOAbbreviation","char")[1]
      x_pmid_1 = custom_grep(unlist(toprecords_list)[i],"PMID","char")[1]
      if(any(c("Meta-Analysis","Systematic Review","Review") %in% custom_grep(unlist(toprecords_list)[i],"PublicationType","char")=="TRUE")){
        x_pubtype_1 = "Systematic Review/Meta-Analysis"
      }else if(any(c("Randomized Controlled Trial","Clinical Trial") %in% custom_grep(unlist(toprecords_list)[i],"PublicationType","char")=="TRUE")){
        x_pubtype_1 = "Randomized Controlled Trial/Clinical Trial"
      }else if(any(c("Comparative Study","Observational Study") %in% custom_grep(unlist(toprecords_list)[i],"PublicationType","char")=="TRUE")){
        x_pubtype_1 = "Observational Comparative Study"
      }else if(any(c("Case Report","Case Series") %in% custom_grep(unlist(toprecords_list)[i],"PublicationType","char")=="TRUE")){
        x_pubtype_1 = "Case Series/Case Study"
      }else{
        x_pubtype_1 = custom_grep(unlist(toprecords_list)[i],"PublicationType","char")[1]
      }
      x2_pubdate = custom_grep(unlist(toprecords_list), "PubDate","char")
      x2_year <- trimws(ex_between(x2_pubdate, '<Year>', '</Year>')[[1]])
      if(is.na(x2_year)){x2_year<-regmatches(x2_pubdate, gregexpr("\\d{4}", x2_pubdate))}else{x2_year<-x2_year}
      x2_month <- trimws(ex_between(x2_pubdate, '<Month>', '</Month>')[[1]])
      x2_month <- suppressWarnings(ifelse(!is.na(as.numeric(x2_month)),month.abb[as.numeric(x2_month)],x2_month))
      if(is.na(x2_month)){x2_month<-"Dec"}else{x2_month<-x2_month}
      x2_day <- trimws(ex_between(x2_pubdate, '<Day>', '</Day>')[[1]])
      if(is.na(x2_day)){x2_day<-"1"}else{x2_day<-x2_day}
      xpublidate[i]<-paste0(x2_year, " ", x2_month," ", x2_day)
      x1_firstauthor <- paste(custom_grep(unlist(toprecords_list)[i],"LastName","char")[1:1],
                              custom_grep(unlist(toprecords_list)[i],"Initials","char")[1:1])
      xfirstauthor[i] <- paste(x1_firstauthor, "et al")
      xyear[i] = as.character(x2_year)
      xmonth[i] = as.character(x2_month)
      xday[i] = as.character(x2_day)
      xref[i] = gsub("\\.\\.","\\.",as.character(paste(x1,paste(x_title,x_journal,sep = ". "), x5, paste0("PMID ", x_pmid_1),sep = ". ")))
      xabs[i] = ifelse(identical(x6,character(0)),NA,x6)
      xpmid[i] = x_pmid_1
      xpubtype[i] = x_pubtype_1
    }
    
    new_PM_df = data.frame(Source = 'PubMed',
                           # Type = search_type,
                           Author = as.character(xfirstauthor),
                           Year = as.character(xyear),
                           Month = as.character(xmonth),
                           Day = as.character(xday),
                           Reference = stri_trans_general(as.character(xref), "latin-ascii"),
                           Link = as.character(paste0('https://www.ncbi.nlm.nih.gov/pubmed/', as.character(xpmid))),
                           PMID = as.character(xpmid),
                           Abstract = as.character(xabs),
                           Pubtype = as.character(xpubtype),
                           Publidate = as.character(xpublidate)
    )
    
  }else{
    new_PM_df = data.frame(Source = '', 
                           # Type = '', 
                           Author = '', Year = '',Month ='',Day = '',Reference = '', Link = '', PMID = '', Abstract = '', Pubtype = '', Publidate = '')
  }
  return(new_PM_df)
  
}
asthmaPMIDS<-openxlsx::read.xlsx('Data/Asthma/PMIDS.xlsx', 'Sheet1',startRow = 1,
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

psoriasisPMIDS<-openxlsx::read.xlsx('Data/Psoriasis/PMIDS.xlsx', 'Sheet1',startRow = 1,
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
# 
# 
# reftosearch<-psoriasisPMIDS$PMID
# reflist<-list()
# for(i in 1:length(reftosearch)){
#   refasthma<-RefSource2(reftosearch[i])
#   reflist[[i]]<-paste(refasthma$Reference,refasthma$Link)
# }
# 
# saveRDS(reflist, "PP_references.RDS")

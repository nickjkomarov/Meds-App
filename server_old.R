shinyServer(function(input,output,session)
{
  source('home.R',local=T)$value # Home
  source('ra.R',local=T)$value # Rheumatoid Arthritis
  source('ms.R',local=T)$value # Relapsing Remitting Multiple Sclerosis
  source('HP.R' ,local=T)$value # HemoPhilia A: Prophylaxis
  source('HO.R', local=T)$value #Hemophilia A: On-demand
})
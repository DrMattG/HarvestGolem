#' addnewdata
#' @description function to convert new data to correct format
#' @param path = path to the xlsx file
#' @export

#

#path="data-raw/Gaupedata_20210525.xlsx"

addnewdata<-function(path){
 library(tidyverse)
Gaupedata <- read_excel(path)
col_Names<-c("Aar", "Region" ,"FG"  ,  "Antall.belastet.kvoten",
             "V.Hunner.belastet.kvoten", "V.Hunner.Avgang","kommentar")
Gaupedata %>% janitor::remove_empty(which = c("cols"))
names(Gaupedata)<-col_Names 

if(is.numeric(Gaupedata$FG)==TRUE){
  Lynx_monitoring_data<-Gaupedata
  saveRDS(Lynx_monitoring_data, "data-raw/Lynx_monitoring_data.rds")
}else{
  Lynx_monitoring_data<-Gaupedata %>% 
    mutate(FG=gsub(",",".",FG)) %>% 
    mutate(FG=as.numeric(FG))
  saveRDS(Lynx_monitoring_data, "data-raw/Lynx_monitoring_data.rds")
}
}
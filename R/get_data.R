#' get www.ssb.no data and Norway shapes
#'
#' @return datasets to Global Environment
#' @noRd

get_data<-function(){
  #packages
  library(HydeNet)
  #install_github("rich-iannone/DiagrammeR") need development version
  #install_version("DiagrammeR", version = "1.0.1", repos = "http://cran.us.r-project.org")
  library(DiagrammeR)
  library(data.table)
  library(rvest)
  library(rgdal)
  library(leaflet)
  library(fuzzyjoin)
  library(readxl)
  library(tidyverse)
  library(plotly)
  library(shinydashboard)
  library(shinythemes)
  library(shiny)
  library(leaflet.extras)
  library(dashboardthemes)
  library(tidyverse)
  library(httr)
  library(rjstat)
  library(htmlwidgets)
  
  #Scrape List of Norway Municipalities
  url <- "https://en.wikipedia.org/wiki/List_of_municipalities_of_Norway"
  municipalities <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
    html_table()
  
  municipalities <- municipalities[[1]]
  
  municipalities$`Number[1](ISO 3166-2:NO)`<-str_pad(municipalities$`Number[1](ISO 3166-2:NO)`, 4, pad = "0")
  
  
  shape <<- readOGR(dsn = "data-raw/spatial", layer = "gadm36_NOR_2")
  require(rgeos)
  centers <- data.frame(gCentroid(shape, byid = TRUE))
  reglist<-shape@data$NAME_2
  centers$region <- reglist
  
  Dat_DL<-readRDS("C:/Users/matthew.grainger/Documents/Projects_in_development/HarvestGolem/data-raw/Lynx_reproduction.RDS")
  Dat_DL<-Dat_DL %>% 
    mutate("SiteCode"= str_sub(Kommunenr,0,2))
  
  #x %>% regex_inner_join(y, by = c(string = "seed"))
  centers<-centers %>% regex_inner_join(Dat_DL, by =c("region"="Name"))
  
  plotdat<<-centers %>% 
    group_by(Name, x,y,Aar) %>% 
    #filter(Aar==2017) %>% 
    tally()
  # 
  # 
  #leaflet() %>% 
  # addTiles() %>% 
  #addPolygons(data=shape, weight = 2, color = "black") %>%
  #addCircleMarkers(data = plotdat,
  #       lng = ~x, lat = ~y ,radius=~n,label = ~Name) 
  #addWebGLHeatmap(data=plotdat,lng=~x, lat=~y,intensity =~n)
  
  # load the required packages
  
  ###########################
  options(encoding="UTF-8")
  
  ##Get data from the stats agency
  ###Lynx Registered mortality 
  #Table 1 all mortality
  # url for POST
  url <- "https://data.ssb.no/api/v0/en/table/03984/"
  # Query, copied from API Console
  # Run by highlighting all of this function and ctrl enter/r
  data <- '{  "query": [    {      "code": "Region",      "selection": {        "filter": "agg_single:Fylker1972",        "values": [          "01",          "02",          "03",          "04",          "05",          "06",          "07",          "08",          "09",          "10",          "11",          "12",          "14",          "15",          "50",          "16",          "17",          "18",          "19",          "20",          "21"          ]      }    },    {      "code": "Aarsak2",      "selection": {        "filter": "item",        "values": [          "00",          "01",          "02",          "03",          "04",          "05","06",          "07",          "08"          ]      }    },    {      "code": "Rovdyr",      "selection": {        "filter": "item",        "values": [          "4"          ]      }    }    ],  "response": {   "format": "json-stat2"}}'
  # post query
  d.tmp <- POST(url , body = data, encode = "json", verbose())
  # Get content from d.tmp as text, using fromJSONstat
  dattable <<- fromJSONstat(content(d.tmp, "text"))
  head(dattable)
  
  #Table 2 Licenses issued and lynx felled
  
  url <- "https://data.ssb.no/api/v0/en/table/06991/"
  data<-'{"query": [    {      "code": "Region",      "selection": {        "filter": "vs:Forvaltningsregion",        "values": [          "555501",          "555502",          "555503","555508",          "555505",          "555506",          "555507",          "555504"        ]      }    },    {      "code": "ContentsCode",      "selection": {"filter": "item",        "values": [          "GaupeKvTil",          "GaupeKvFelt"        ]      }    }  ],  "response": {    "format": "json-stat2"  }}'
  d.tmp <- POST(url , body = data, encode = "json", verbose())
  # Get content from d.tmp as text, using fromJSONstat
  dattable2 <<- fromJSONstat(content(d.tmp, "text"))
}

#' get www.ssb.no data 
#'
#' @return datasets to Global Environment
#' @noRd
get_full_quotas<-function(){
url <- "https://data.ssb.no/api/v0/en/table/06991/"
data<-'{"query": [    {      "code": "Region",      "selection": {        "filter": "vs:Forvaltningsregion",        "values": [          "555501",          "555502",          "555503","555508",          "555505",          "555506",          "555507",          "555504"        ]      }    },    {      "code": "ContentsCode",      "selection": {"filter": "item",        "values": [          "GaupeKvTil",          "GaupeKvFelt"        ]      }    }  ],  "response": {    "format": "json-stat2"  }}'
d.tmp <- httr::POST(url , body = data, encode = "json", httr::verbose())
# Get content from d.tmp as text, using fromJSONstat
dattable2 <- rjstat::fromJSONstat(httr::content(d.tmp, "text"))

National_licenses<<-dattable2 %>%
  dplyr::filter(contents=="Quota hunting, lynx, licenses issued") %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate("Year"=strsplit(`interval (year)`, "-")[[1]][2])

# The palette with black:
#cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#National_licenses_plot<-National_licenses %>% 
 # ggplot(aes(Year,value)) +
  #geom_bar(stat="identity",fill=cbPalette[3])+
  #labs(y="Number of licenses issued")+
  #ggpubr::theme_pubr()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))
return(National_licenses)
}

get_full_quotas()

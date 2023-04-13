#' R companion of La Societe Nouvelle macro_data API services
#'
#' @importFrom dplyr  %>%
#' @importFrom dplyr mutate
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#'
#' @param table Kind of data requested : eco-environmental footprints or structural forecasted economic data (required)
#' @param activity NACE Rev.2 economic activity requested : branch or division code (optional)
#' @param year Year or years requested (optional)
#' @param indic For footprint requests, eco-environnemental footprint dimension (optional). A list is available in the lsnr package [lsnr::get_indicator_list()]
#' @param area Geographic filter (optional)
#' @param type For footprint requests, type of data : observed, target, trend (required for footprints request)
#' @param classification Data NACE Rev.2 granularity : branch by 38 or division by 88 (required for footprints request)
#' @param flag Flag filter (optional). It informs on the status of each data observation.
#' @param path Linear or geometric path for target aims to be fulfilled (optional)
#' @param currency Currency filter (optional). Allows to narrow the request to current or constant prices.
#' @param product Only for structural economic data, filter for product (optional)
#' @param unit Unit filter (optional)
#'
#' @examples
#'
#' #Get CPEB forecasted data for division 10 between 2022 and 2030.
#'
#' lsnrstat(table = "structural",year = 2022:2030,type = "cpeb",activity = "10")
#'
#' #Get TEI forecasted data for branch JA use of product OZ between 2022 and 2030.
#'
#' lsnrstat(table = "structural",year = 2022:2030,type = "tei",activity = "JA",product = "OZ")
#'
#' #Get branch EZ 2018 footprints for NRG, GHG and ECO indicators.
#'
#' lsnrstat(table = "footprint",year = 2018,activity = "EZ",indic = c("NRG","GHG","ECO"))
#'
#' @return A [data.frame()].
#'
#' @export

lsnrstat = function(table = c("footprint","structural"),
                   activity = "",
                   year = "",
                   indic = c("","ECO","ART","GHG","SOC","GEQ","NRG","MAT","HAZ","KNW","WAT","WAS","IDR"),
                   area = "",
                   type = c("","tgt","trd","cpeb","ere","pat_nf","tei","tess"),
                   classification = c("A38","A88"),
                   flag = "",
                   path = c("","linear","geometric"),
                   currency = "",
                   product = "",
                   unit){

  #####################PARAMETERS FORMATTING####################

  if(all(grepl("footprint|fpt|empreinte",tolower(table)))){
    table = "macro_fpt"
  }else
  {
  if(all(tolower(table) == "na") || all(grepl("eco|str",tolower(table)))){
    table = "na"
  }else
  {
    stop("Wrong or missing 'table' input")
  }}


  #######################REQUESTS FORMATTING#######################

  if(table == "macro_fpt"){

    if(all(type == c("","tgt","trd","cpeb","ere","pat_nf","tei","tess")))
    {
      type = ""
    }

    if(length(type) != 1 || is.character(type) == F || type %in% c("","tgt","trd") == F){
      stop("Wrong or missing 'type' input")
    }

    if(all(indic %in% c("",lsnr:::get_indicator_list()[,"Indicator code"])) == F)
    {
      stop("Wrong 'indic' input")
    }


    if(length(classification) != 1 || is.character(classification) == F){

      wr = 1

      if(all(activity %in% unique(unlist(lsnr:::Divisions[,"DIVISION"]))))
      {
        classification = "a88"
        wr = 0
      }


      if(all(activity %in% unique(unlist(lsnr:::Divisions[,"BRANCH"]))))
      {
        classification = "a38"
        wr = 0
      }
      if(wr == 1)
        {
      stop("Wrong or missing 'classification' input")
        }
    }

    if(grepl("38",classification)){
      classification = "a38"
      branch = activity
    }

    if(grepl("88",classification)){
      classification = "a88"
      division = activity
    }

    parameters <<- c(as.list(environment()), list())

    parameters = parameters[names(parameters) %in% c("year","branch","division","aggregate","area","currency","target","path","flag","indic","path","trend")]

    for(i in 1:length(parameters)){
      parameters[[i]] = paste0(names(parameters)[[i]],"=",parameters[[i]])
    }

    filters = paste0(unlist(parameters),collapse = "&")

    type = ifelse(type == "","",paste0("_",tolower(type),"_"))

    link = paste0("https://api.lasocietenouvelle.org/macrodata/",table,type,"_",classification,"?",filters)

  }

  if(table == "na"){

    if(length(type) != 1 || is.character(type) == F || type %in% c("cpeb","ere","pat_nf","tei","tess") == F){
      stop("Wrong or missing 'type' input. For structural economic data, please use one of these types : 'cpeb','ere','pat_nf','tei','tess'")
    }

    if(all(grepl("38",classification))){
      classification = "A38"
    }

    if(all(grepl("88",classification))){
      classification = "A88"
    }

    if(sum(grepl("88",classification)) == 1 && sum(grepl("38",classification)) == 1){
      classification = ""
    }

    parameters <<- c(as.list(environment()), list())

    parameters = parameters[names(parameters) %in% c("year","activity","product","aggregate","unit","flag","classification","flag")]

    if(type == "cpeb")
    {
      parameters = parameters[names(parameters) != "product"]
    }

    for(i in 1:length(parameters)){
      parameters[[i]] = paste0(names(parameters)[[i]],"=",parameters[[i]])
    }

    filters = paste0(unlist(parameters),collapse = "&")

    link = paste0("https://api.lasocietenouvelle.org/macrodata/",table,"_",type,"?",filters)

  }

  ######################SUBMIT REQUEST AND FORMAT DATA#############
  tryCatch({

    raw_table = GET(link)

    formatted_table = fromJSON(rawToChar(raw_table$content))$data %>%
      mutate(lastupdate = as.Date(lastupdate),
             lastupload = as.Date(lastupload))

    if(table == "macro_fpt"){
    unit = lsnr:::get_indicator_list()[match(formatted_table$indic,lsnr:::get_indicator_list()[,"Indicator code"]),"Unit code"]
    formatted_table = formatted_table %>%
      mutate(unit = unit)
    }

  }, error = function(e) {
    print(e)
    stop(paste0("Données indisponibles : ",link))
  })


  return(formatted_table)

}

#' R companion of 'La Societe Nouvelle' macro_data API services
#'
#' @importFrom dplyr  %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr rename_with
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#'
#' @param dataset dataset requested : list available at <https://docs.lasocietenouvelle.org/series-donnees> (required)
#' @param filters filters to apply : list of dataset 'params' available through service [lsnstat_metadata] (optional)
#'
#' @examples
#'
#' #GET 'CPEB'  (Branch production and operations accounts) forecasted
#' # data for division "10" between 2023 and 2025.
#'
#' lsnstat_macrodata(dataset = "na_cpeb", filters="classification=A88&activity=10&year=2023+2024+2025")
#'
#' # GET 'TEI' (Intermediate consumption table) forecasted data
#' # for branch 'JA' use of product 'OZ' in 2022.
#'
#' lsnstat_macrodata(dataset = "na_tei", filters="activity=JA&product=OZ&year=2022")
#'
#' # GET branch 'EZ' 2018 production footprint for 'NRG' and 'GHG' indicators.
#'
#' lsnstat_macrodata(
#'   dataset = "macro_fpt_a38",
#'   filters="branch=EZ&year=2018&aggregate=PRD&indic=NRG+GHG"
#'   )
#'
#' @return A [data.frame()].
#'
#' @export

lsnstat_macrodata = function (dataset,filters)
{
  # check dataset param
  if (missing(dataset)) {
    stop("dataset is missing")
  }

  entrypoint = "https://api.lasocietenouvelle.org/macrodata/"

  if(!missing(filters)){
    endpoint = paste0(entrypoint,dataset, "?", filters)
  } else {
    endpoint = paste0(entrypoint,dataset)
  }

  tryCatch({

    print(endpoint)

    raw_data = GET(endpoint)
    res = fromJSON(rawToChar(raw_data$content))

    # RESPONSE NOT OK
    if (res$header$code!=200) {
      stop(res$header$message)
    }

    # NO DATA FOUND
    else if (length(res$data)==0) {
      stop("No data found")
    }

    # OK
    else {
      formatted_data = res$data %>%
        mutate(lastupdate = as.Date(lastupdate),
               lastupload = as.Date(lastupload)) %>%
        rename_with(toupper)
    }

  # API ERROR
  }, error = function(e) {
    print(e)
    stop(e)
  })

  return(formatted_data)
}

utils::globalVariables(c("lastupdate","lastupload"))

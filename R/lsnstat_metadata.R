#' Metadata query function for [lsnstat_macrodata] requests.
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#'
#' @param dataset dataset requested : list available at <https://docs.lasocietenouvelle.org/series-donnees> (required)
#' @param param list of dataset 'params' available for a dataset (optional)
#'
#' @examples
#'
#' # GET 'CPEB' (Branch production and operations accounts) table parameters and filters.
#'
#' lsnstat_metadata(dataset = "na_cpeb")
#'
#' # GET footprints filters for parameter 'indic'.
#'
#' lsnstat_metadata("macro_fpt_a38",param = "indic")
#'
#' # GET footprints filters for parameter 'unit'.
#'
#' lsnstat_metadata("macro_fpt_a38",param = "unit")
#'
#' # GET 'ERE' (resource-use equilibrium) table parameters and filters.
#'
#' lsnstat_metadata("na_pat_nf")
#'
#' @return A [data.frame()].
#'
#' @export


lsnstat_metadata = function (dataset,param)
{
  # check dataset param
  if (missing(dataset)) {
    stop("dataset is missing")
  }

  entrypoint = "https://api.lasocietenouvelle.org/macrodata/metadata/"

  if(!missing(param)){
    endpoint = paste0(entrypoint,dataset, "?", "param=", param)
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

    # OK
    else {
      formatted_data = res$metadata
    }

    # API ERROR
  }, error = function(e) {
    print(e)
    stop(e)
  })

  return(formatted_data)
}

#' R generic companion of 'La Societe Nouvelle' for all API services
#'
#'
#' @importFrom dplyr  %>%
#' @importFrom dplyr across
#' @importFrom dplyr mutate
#' @importFrom dplyr rename_with
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom tidyselect matches
#'
#' @param dataset dataset requested : list available at <https://docs.lasocietenouvelle.org/series-donnees> (required)
#' @param service service requested : list available at <https://api.lasocietenouvelle.org/> (required)
#' @param filters filters to apply : list of dataset 'params' available through service [lsnstat_metadata] (optional)
#'
#' @examples
#' # See [lsnstat_macrodata] and [lsnstat_statsdata] for precise examples.
#'
#' @return A [data.frame()].
#'
#' @export

lsnstat_getdata = function(dataset,filters,service = 'macrodata')
{
# check dataset param
if (missing(dataset)) {
  stop("dataset is missing")
}

entrypoint = paste0("https://api.lasocietenouvelle.org/",service,"/")

if(!missing(filters)){
  endpoint = paste0(entrypoint,dataset, "?", filters)
} else {
  endpoint = paste0(entrypoint,dataset)
}

tryCatch({

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
      mutate(across(matches('lastupdate'),as.Date),
             across(matches('lastupload'),as.Date)) %>%
      rename_with(toupper)
  }

  # API ERROR
}, error = function(e) {
  print(e)
  stop(e)
})

return(formatted_data)

}

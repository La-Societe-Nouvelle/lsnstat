#' R companion of 'La Societe Nouvelle' macrodata API services
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
#' # GET industry 'A01' 2018 production footprint for 'NRG' and 'GHG' indicators.
#'
#' lsnstat_macrodata(
#'   dataset = "macro_fpt",
#'   filters="industry=A01&year=2018&aggregate=PRD&indic=NRG+GHG"
#'   )
#'
#' @return A [data.frame()].
#'
#' @export

lsnstat_macrodata = function (dataset,filters = NULL)
{

  # check dataset param
  if (missing(dataset)) {
    stop("dataset is missing")
  }

  res = try(lsnstat_endpoint(endpoint = "macrodata", dataset = dataset, filters = filters),silent = T)

  if(inherits(res,"try-error"))
  {
  stop(res)
  }

  formatted_data = res %>%
        mutate(lastupdate = as.Date(lastupdate),
               lastupload = as.Date(lastupload)) %>%
        rename_with(toupper)

  return(formatted_data)
}

utils::globalVariables(c("lastupdate","lastupload"))

#' R companion of 'La Societe Nouvelle' statdata API services
#'
#' @param dataset dataset requested (required)
#' @param filters filters to apply (optional)
#'
#' @examples
#'
#' # GET industry 'K66' 2018 direct impacts over production value for 'NRG' and 'GHG' indicators.
#'
#' lsnstat_statdata(
#'   dataset = "direct_impacts_production",
#'   filters="id=FR_K66&year=2018&&indic=NRG+GHG"
#'   )
#'
#' @return A [data.frame()].
#'
#' @export

lsnstat_statdata = function (dataset,filters)
{
  # check dataset param
  if (missing(dataset)) {
    stop("dataset is missing")
  }

  res = try(lsnstat_endpoint(endpoint = "statdata", dataset = dataset, filters = filters),silent = T)

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

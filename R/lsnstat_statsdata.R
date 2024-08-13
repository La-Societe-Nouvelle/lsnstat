#' R companion of 'La Societe Nouvelle' stats_data API service
#'
#'
#' @param dataset dataset requested : list available at ... (required)
#' @param filters filters to apply : list of dataset 'params' available at ... (optional)
#'
#' @examples
#'
#' # GET the value added of sector C19 of France in 2018.
#'
#' lsnstat_statsdata(
#'   dataset = "figaro_main_aggregates",
#'   filters="year=2018&aggregate=VA&country=FR&industry=C19"
#'   )
#'
#' @return A [data.frame()].
#'
#' @export

lsnstat_statsdata = function (dataset,filters)
{
  formatted_data = lsnstat_getdata(dataset,filters,service = 'statsdata')

  return(formatted_data)
}



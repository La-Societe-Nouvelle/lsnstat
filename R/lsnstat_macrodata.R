#' R companion of 'La Societe Nouvelle' macro_data API service
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
  formatted_data = lsnstat_getdata(dataset,filters,service = 'macrodata')

  return(formatted_data)
}

utils::globalVariables(c("lastupdate","lastupload"))

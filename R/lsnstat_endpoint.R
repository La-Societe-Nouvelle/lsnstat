#' Link La Societe Nouvelle API services and R for flexible endpoints : return raw table
#'
#' @param endpoint endpoint requested : list available at <https://api.lasocietenouvelle.org/> (required)
#' @param dataset dataset requested (optional)
#' @param filters filters to apply (optional)
#'
#' @examples
#'
#' #macrodata macro_fpt request
#'
#' lsnstat_endpoint(endpoint = "macrodata", dataset = "macro_fpt", filters = "industry=C20&indic=GHG+NRG&year=2020")
#'

lsnstat_endpoint = function(endpoint,dataset = NULL,filters = NULL)
{
  # check endpoint param

  if (missing(endpoint)) {
    stop("endpoint is missing")
  }

  entrypoint = paste0("https://api.lasocietenouvelle.org/",endpoint)

  query = paste0(entrypoint,
                 ifelse(is.null(dataset),"","/"),dataset,
                 ifelse(is.null(filters),"","?"), filters)

  tryCatch({

    raw_data = GET(query)

    if(raw_data$status_code == 404)
    {
      stop("Wrong request specifying (404)")
    }

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
      formatted_data = res$data
    }

    # API ERROR
  }, error = function(e) {
    print(e)
    stop(e)
  })

  return(formatted_data)



}

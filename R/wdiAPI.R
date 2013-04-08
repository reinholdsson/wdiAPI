#' Get attributes
#' 
#' Function to get attributes (used in get_all_attrs)
#' 
#' @param xml XML character string
#' 
get_attrs <- function(xml) {
    if (!is.null(xml)) {
        attrs <- data.frame(matrix(xml, nrow = 1, byrow = TRUE))
        colnames(attrs) <- names(xml)
        return(attrs)
    } else {
        return(NULL)
    }
}

#' Get all attributes
#' 
#' Function to get all attributes as a list
#' 
#' @param xml XML character string
#' 
get_all_attrs <- function(xml) {
    attrs <- xmlApply(xmlRoot(xml), function(x) {
        
        # Root attributes
        root <- get_attrs(xmlAttrs(x))
        nodes <- get_attrs(unlist(xmlApply(x, xmlAttrs)))
        
        # Return all attributes (TODO: improve!)
        if (is.null(root)) res <- nodes
        else if (is.null(nodes)) res <- root
        else res <- cbind(root, nodes)
        
        return(res)
    })
    names(attrs) <- NULL
    return(attrs)
}

#' Get data from url
#' 
#' ...
#' 
#' @param url Url
#' @param stringsAsFactors Strings as factors
#' 
get_data_from_url <- function(url, stringsAsFactors = TRUE) {
    # Get XML
    f <- xmlParse(
        paste(readLines(url, warn = FALSE), collapse = "")
    )
    
    # Data
    data <- xmlToDataFrame(f, stringsAsFactors = stringsAsFactors)  # slow!

    # Get all attributes (e.g. <node attribute.id = "attribute"></node>
    attrs <- do.call("rbind", get_all_attrs(f))

    # Add attributes to data
    data <- cbind(data, attrs)
    
    return(data)
}

#' Get metadata
#' 
#' Query the WDI API for metadata given a specific subject.
#' 
#' @param subject Subject (e.g. "topics", "countries", "indicators")
#' @param limit Limit the number of return observations
#' 
#' @export
#'
#' @examples \dontrun{
#'  x <- get_metadata("topics")
#'  x <- get_metadata("countries")
#'  x <- get_metadata("indicators")
#' }
get_metadata <- function(subject, limit = 25000) {
    get_data_from_url(
        sprintf("http://api.worldbank.org/%s?per_page=%s", subject, limit)
    )
}

#' Get data
#' 
#' Query the WDI API for indicator data
#' 
#' @param indicator Character vector of wdi indicators
#' @param date Integer vector of years
#' @param country Character vector of countries (ISO 3 or ISO 2 letter codes)
#' @param limit Limit the number of return observations
#' 
#' @export
#' 
#' @examples \dontrun{
#'  x <- get_data("SP.POP.TOTL")
#' }
#' 
get_data <- function(indicator, date = 2012, country = NULL, limit = 25000) {
    
    if (is.null(country)) country <- "all"
    
    # For each indicator
    do.call("rbind", 
        lapply(indicator, function(ind) {
            
            # For each country
            do.call("rbind", 
                lapply(country, function(ctr) {
                    get_data_from_url(
                        sprintf("http://api.worldbank.org/countries/%s/indicators/%s?date=%s:%s&per_page=%s", ctr, ind, min(date), max(date), limit)
                    )
                })
            )
        })
    )
}


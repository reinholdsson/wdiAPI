library(XML)

# Get data from url
get_data_from_url <- function(url) {
    
    # Get XML
    f <- paste(readLines(url, warn = FALSE), collapse="")
    f <- xmlParse(f)
    
    # Data
    data <- xmlToDataFrame(f, stringsAsFactors = FALSE)
    
    # Add ids to data
    ids <- unlist(xmlApply(xmlRoot(f), xmlAttrs))
    names(ids) <- NULL
    data$id <- ids
    
    return(data)
}

# Get data given a specific subject (e.g. "topics", "countries", "indicators")
get_all <- function(x) {
    url <- sprintf("http://api.worldbank.org/%s?per_page=99999", x)
    get_data_from_url(url)
}

# Examples
# get_all("topics")
# get_all("countries")
# get_all("indicators")

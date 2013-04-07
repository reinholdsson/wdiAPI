library(XML)

# Fetch WDI xml file
get_wdi_xml <- function(url) {
    f <- paste(readLines(url, warn = FALSE), collapse="")
    f <- xmlParse(f)
}

# Get ids from WDI
get_ids <- function(url) {
    x <- get_wdi_xml(url)
    x <- xmlApply(xmlRoot(x), xmlAttrs)
    x <- unlist(x)
    names(x) <- NULL
    return(x)
}

# Get data given a specific subject (e.g. "topics", "countries", "indicators")
get_all <- function(x) {
    url <- sprintf("http://api.worldbank.org/%s?per_page=99999", x)
    res <- get_wdi(url)
    res <- xmlToDataFrame(res, stringsAsFactors = FALSE)
    res$id <- get_ids(url)
    return(res)
}

# Examples
# get_all("topics")
# get_all("countries")
# get_all("indicators")

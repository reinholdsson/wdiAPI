# Function to add attributes (used in get_all_attrs)
get_attrs <- function(x) {
    if (!is.null(x)) {
        attrs <- data.frame(matrix(x, nrow = 1, byrow = TRUE))
        colnames(attrs) <- names(x)
        return(attrs)
    } else {
        return(NULL)
    }
}

# Function to get all attributes as a list
get_all_attrs <- function(x) {
    attrs <- xmlApply(xmlRoot(x), function(x) {
        
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

# Get data from url
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

# Get data given a specific subject (e.g. "topics", "countries", "indicators")
get_all <- function(x) {
    get_data_from_url(
        sprintf("http://api.worldbank.org/%s?per_page=99999", x)
    )
}

# Examples
# get_all("topics")
# get_all("countries")
# get_all("indicators")

get_variable <- function(x) {
    get_data_from_url(
        sprintf("http://api.worldbank.org/countries/all/indicators/%s?per_page=9999", x)
    )
}

# Examples
# get_variable("SP.POP.TOTL")

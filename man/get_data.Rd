\name{get_data}
\alias{get_data}
\title{Get data}
\usage{
  get_data(indicator, date = 2012, country = NULL,
    limit = 25000)
}
\arguments{
  \item{indicator}{Character vector of wdi indicators}

  \item{date}{Integer vector of years}

  \item{country}{Character vector of countries (ISO 3 or
  ISO 2 letter codes)}

  \item{limit}{Limit the number of return observations}
}
\description{
  Query the WDI API for indicator data
}
\examples{
\dontrun{
 x <- get_data("SP.POP.TOTL")
}
}


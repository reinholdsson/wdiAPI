\name{get_metadata}
\alias{get_metadata}
\title{Get metadata}
\usage{
  get_metadata(subject, limit = 25000)
}
\arguments{
  \item{subject}{Subject (e.g. "topics", "countries",
  "indicators")}

  \item{limit}{Limit the number of return observations}
}
\description{
  Query the WDI API for metadata given a specific subject.
}
\examples{
\dontrun{
 x <- get_metadata("topics")
 x <- get_metadata("countries")
 x <- get_metadata("indicators")
}
}


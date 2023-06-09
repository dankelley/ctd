% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{dbToList}
\alias{dbToList}
\title{Read a SQLite database as a list}
\usage{
dbToList(dbname)
}
\arguments{
\item{dbname}{character value specifying the database file.}
}
\value{
a \link{list} containing items named after the tables.
}
\description{
This may be used, for example, to study the contents of a database
created with \code{\link[=ctdTag]{ctdTag()}}.  This can be handy at the commandline, e.g.

\if{html}{\out{<div class="sourceCode">}}\preformatted{Rscript -e 'ctd::dbToList("~/ctdTag_kelley.db")'
}\if{html}{\out{</div>}}
}
\examples{
\dontrun{
dbToList("~/ctdTag_kelley.db")
}
}
\author{
Dan Kelley
}

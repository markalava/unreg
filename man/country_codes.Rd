% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/translator_functions.R
\name{country_codes}
\alias{country_codes}
\title{Country codes associated with a region}
\usage{
country_codes(x, family = c("M49", "SDG", "WB_inc", "Dev"))
}
\arguments{
\item{x}{\emph{Region} identifier. Interpreted as \dQuote{code}
if \code{is.numeric(x)} and \dQuote{name} if
\code{is.character(x)}.}

\item{family}{Family to which the region referenced by \code{code}
belongs.}
}
\value{
Country codes as numeric.
}
\description{
Returns codes of all countries in the region
}
\seealso{
Other translator functions: 
\code{\link{code}()},
\code{\link{country_names}()},
\code{\link{name}},
\code{\link{reg_code}()},
\code{\link{reg_name}()}
}
\author{
Mark Wheldon
}
\concept{translator functions}

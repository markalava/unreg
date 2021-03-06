% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/translator_functions.R
\name{reg_code}
\alias{reg_code}
\title{Region code associated with a country}
\usage{
reg_code(
  x,
  level = c("1", "2", "other"),
  family = c("M49", "SDG", "WB_inc", "Dev"),
  verbose = FALSE
)
}
\arguments{
\item{x}{\emph{Country} identifier. Interpreted as \dQuote{code}
if \code{is.numeric(x)} and \dQuote{name} if
\code{is.character(x)}.}

\item{level}{Level of region. Higher levels are nested in lower
levels. E.g., \dQuote{Africa} is level \dQuote{1},
\dQuote{Eastern Africa} is level \dQuote{2}. Converted to
character if supplied as numeric. Level \dQuote{other} is for
regions that are not part of an official classification, such
as \dQuote{Europe} in the \dQuote{SDG} family and
\dQuote{middle income} in the \dQuote{WB_inc} family.}

\item{family}{Family to which the region referenced by \code{code}
belongs.}
}
\value{
A vector the same length as \code{x} containing the
    code(s) as numeric.
}
\description{
Returns the region code associated with a given country code. An error is thrown if a region
code is supplied.
}
\examples{

## Default family is "M49"
reg_code(250)
reg_code(c("France", "FRANCE", "spAIn"))

reg_code(250, family = "SDG")

## Level can be supplied as numeric for convenience
reg_code(250, 2, family = "SDG")

}
\seealso{
Other translator functions: 
\code{\link{code}()},
\code{\link{country_codes}()},
\code{\link{country_names}()},
\code{\link{name}},
\code{\link{reg_name}()}
}
\author{
Mark Wheldon
}
\concept{translator functions}

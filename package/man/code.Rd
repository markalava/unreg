% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/translator_functions.R
\name{code}
\alias{code}
\title{Country or region code}
\usage{
code(name, family = c("M49", "SDG"))
}
\arguments{
\item{name}{Name(s) of country(ies) or region(s) for which codes
are desired (case insensitive). Abbreviations may be used.}

\item{family}{Family of the region given by \code{name}. See
\sQuote{Details}.}
}
\value{
A vector the same length as \code{code} containing the ISO
    code(s) as numeric.
}
\description{
Returns the ISO code(s) associated with \code{name} (a country or aggregate).
}
\details{
Names of regions are not unique, e.g., \dQuote{Latin America and
the Caribbean} is a region in the \dQuote{M49} and \dQuote{SDG}
families and, as a restult, has two codes. Ambiguities in such
cases are resolved with argument \code{family}. If \code{name} is
ambiguous and \code{family} is not supplied a warning is given and
\dQuote{M49} is assumed.

The argument \code{family} is needed only to do this
disambiguation. Since only the \dQuote{M49} and \dQuote{SDG}
families have overlaps in region names, no other family type
should be passed to \code{family} when calling this particular
function.
}
\examples{
## Countries
code(c("France", "france", "FRANCE", "spain"))

## Regions
code(c("World", "Africa"))

## Accidentally provide code as character
code(c("France", "250"))

## Regions with more than one code
\dontrun{
code("Latin America and the Caribbean")
}

## Without warning (using abbreviation)
code("lac", family = "M49")
code("lac", family = "SDG")

}
\seealso{
Other translator functions: 
\code{\link{country_codes}()},
\code{\link{country_names}()},
\code{\link{name}},
\code{\link{reg_code}()},
\code{\link{reg_name}()}
}
\author{
Mark Wheldon
}
\concept{translator functions}

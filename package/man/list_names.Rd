% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lists.R
\name{list_names}
\alias{list_names}
\title{List all names of countries, regions, etc}
\usage{
list_names(capitalize = TRUE)
}
\arguments{
\item{capitalize}{Logical. Should names be capitalized, i.e., set in title case?}
}
\value{
Character vector of unique names (countries, regions, etc).
}
\description{
Lists the unique, casefolded names in
\code{unloc_df$name}. \code{unloc_df} is from the
\pkg{wpp2019} package.
}
\details{
If \code{capitalize} is \code{TRUE}, the names are converted to
title case. Some region names are stored in \code{unloc_df} in
all caps (e.g., "AFRICA").
}
\seealso{
Other listing functions: 
\code{\link{list_codes}()},
\code{\link{list_country_codes}()},
\code{\link{list_country_names}()},
\code{\link{list_reg_codes}()},
\code{\link{list_reg_colours}()},
\code{\link{list_reg_names}()}
}
\author{
Mark Wheldon
}
\concept{listing functions}

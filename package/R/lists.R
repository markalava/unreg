###
### List countries, areas, etc
###

##' List all names of countries, regions, etc
##'
##' Lists the unique, casefolded names in
##' \code{unloc_df$name}. \code{unloc_df} is from the
##' \pkg{wpp2019} package.
##'
##' If \code{capitalize} is \code{TRUE}, the names are converted to
##' title case. Some region names are stored in \code{unloc_df} in
##' all caps (e.g., "AFRICA").
##'
##' @param capitalize Logical. Should names be capitalized, i.e., set in title case?
##' @return Character vector of unique names (countries, regions, etc).
##' @author Mark Wheldon
##' @family listing functions
##' @export
list_names <- function(capitalize = TRUE) {
    unique(as.character(unloc_df$name))
    }


##' List all codes of countries, regions, etc
##'
##' Lists the unique codes in \code{unloc_df$code}. \code{unloc_df} is
##' from the \pkg{wpp2019} package.
##'
##' @return Numeric vector of unique codes (countries, regions, etc)
##' @author Mark Wheldon
##' @family listing functions
##' @export
list_codes <- function() {
    unique(as.numeric(unloc_df$country_code))
    }


##' List all country names
##'
##' All country names are listed.
##'
##' @return Character vector of country names.
##' @author Mark Wheldon
##' @family listing functions
##' @export
list_country_names <- function() {
    x <- unloc_df[unloc_df$location_type == 4, "name"]
    as.character(x)
    }


##' List country codes
##'
##' All country codes are listed.
##'
##' @return Numeric vector of country codes.
##' @author Mark Wheldon
##' @family listing functions
##' @export
list_country_codes <- function() {
    x <- unloc_df[unloc_df$location_type == 4, "country_code"]
    as.numeric(x)
}


##' List region codes
##'
##' List all region codes for the given family and level.
##'
##' @inheritParams reg_code
##' @return Character vector of region codes
##' @author Mark Wheldon
##' @family listing functions
##' @export
list_reg_codes <- function(level = c("1", "2"),
                           family = c("M49", "SDG", "WB", "Dev")) {

    ## TODO: Allow level to be 1:2?

    family <- match.arg(family)

    level <- as.character(level)
    level <- match.arg(level)

    if(identical(family, "M49")) {
        if(identical(level, "1")) {
            return(unique(as.numeric(unloc_df[unloc_df$location_type == 2,
                                              "country_code"])))
        } else if(identical(level, "2")) {
            return(unique(as.numeric(unloc_df[unloc_df$location_type == 3,
                                              "country_code"])))
        }
    } else if(identical(family, "SDG")) {
        if(identical(level, "1")) {
            return(as.numeric(internal_sdg_reg_L1_country_codes))
        } else if(identical(level, "2")) {
            return(as.numeric(internal_sdg_reg_L2_country_codes))
        }
    } else stop("family = '", family, "' not implemented.")
}


##' List region names
##'
##' List all region names for the given family and level.
##'
##' @inheritParams reg_code
##' @return Character vector of region names
##' @author Mark Wheldon
##' @family listing functions
##' @export
list_reg_names <- function(level = c("1", "2"),
                           family = c("M49", "SDG", "WB", "Dev")) {

    family <- match.arg(family)

    level <- as.character(level)
    level <- match.arg(level)

    if(identical(family, "M49")) {
        if(identical(level, "1")) {
            return(unique(as.character(unloc_df[unloc_df$location_type == 2,
                                              "name"])))
        } else if(identical(level, "2")) {
            return(unique(as.character(unloc_df[unloc_df$location_type == 3,
                                              "name"])))
        }
    } else if(identical(family, "SDG")) {
        if(identical(level, "1")) {
            return(as.character(name(internal_sdg_reg_L1_country_codes)))
        } else if(identical(level, "2")) {
            return(as.character(name(internal_sdg_reg_L2_country_codes)))
        }
    } else stop("family = '", family, "' not implemented.")
}

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
list_reg_codes <- function(level = c("1", "2", "other"),
                           family = c("M49", "SDG", "WB_inc", "Dev")) {

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
        } else(stop("level = '", level, "' not valid."))

    } else {
        internal_reg_country_codes <-
            make_agcode_var_name(family = family, level = level)
        if(!exists(internal_reg_country_codes)) {
            stop("The combination family = '", family, "' and level = '",
                 level, "' does not exist.")
        } else {
            internal_reg_country_codes <- get(internal_reg_country_codes)
        }
    }
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
list_reg_names <- function(level = c("1", "2", "other"),
                           family = c("M49", "SDG", "WB_inc", "Dev")) {

    family <- match.arg(family)

    level <- as.character(level)
    level <- match.arg(level)

    name(list_reg_codes(level = level, family = family))
}


##' List region colours
##'
##' List hex or RGB codes of the official colours to be used for
##' plotting various level 1 regions (there are no colours for level 2
##' regions).
##'
##' @param family Region family for which to return colour codes.
##' @param model Colour model (only \code{"hex"} and \code{"rgb"} are
##'     supported).
##' @param use_reg_names Logial. Should the names of the output object
##'     be the names of the region? Otherwise they are the codes.
##' @param add_world Logical. Should \dQuote{World} be added to the
##'     output?
##' @return Named character vector or list of region colours with
##'     region codes as names (or region names if \code{use_reg_names}
##'     is \code{TRUE}).
##' @author Mark Wheldon
##' @examples
##' list_reg_colours("SDG", "hex")
##' @family listing functions
##' @export
list_reg_colours <- function(family = c("M49", "SDG", "WB_Inc", "Dev"),
                             model = c("hex", "rgb"),
                             use_reg_names = FALSE,
                             add_world = FALSE) {
    family <- match.arg(family)
    model <- match.arg(model)
    col_var_name <-
        make_colour_code_var_name(family = family, model = model)
    col_var_name_world <-
        paste0("internal_world_colour_", model)
    if(add_world) out <- c(get(col_var_name), get(col_var_name_world))
    else out <- get(col_var_name)
    if(use_reg_names) names(out) <- name(names(out))
    return(out)
}

##' @rdname list_reg_colours
##' @examples list_reg_colors("SDG", "hex")
##' @export
list_reg_colors <- list_reg_colours

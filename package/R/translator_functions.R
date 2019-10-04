###
### Access ISO codes, names, etc
###


##' Country or region code
##'
##' Returns the ISO code(s) associated with \code{name}. If a region
##' is given, all codes associated will be returned in a vector.
##'
##' Names of regions are not unique, e.g., \dQuote{Latin America and
##' the Caribbean} is a region in the \dQuote{M49} and \dQuote{SDG}
##' families and, as a restult, has two codes. Ambiguities in such
##' cases are resolved with \code{family}. If \code{family} is not
##' supplied a warning is given and \dQuote{M49} is assumed.
##'
##' @param name Name(s) of country(ies) or region(s) for which codes
##'     are desired (case insensitive). Abbreviations may be used.
##' @param family Family of the region given by \code{name}. See
##'     \sQuote{Details}.
##' @return A vector the same length as \code{code} containing the ISO
##'     code(s) as numeric.
##' @author Mark Wheldon
##' @family translator functions
##' @examples
##' ## Countries
##' code(c("France", "france", "FRANCE", "spain"))
##'
##' ## Regions
##' code(c("World", "Africa"))
##'
##' ## Accidentally provide code as character
##' code(c("France", "250"))
##'
##' ## Regions with more than one code
##' \dontrun{
##' code("Latin America and the Caribbean")
##' }
##'
##' ## Without warning (using abbreviation)
##' code("lac", family = "M49")
##' code("lac", family = "SDG")
##'
##' @export
code <- function(name, family = c("M49", "SDG")) {

    name <- tolower(name_subs(name))
    family <- match.arg(family)

    ## 'NA' and invalid names mapped to 'NA'
    na.name <- is.na(name)
    na.name <- na.name | invalid_names(name, clean = FALSE)
    if(any(na.name)) {
        out <- numeric(length(name))
        out[na.name] <- NA
        if(length(out[!na.name]) > 0) {
            out[!na.name] <- code(name[!na.name], family = family)
        }
        return(out)
    }

    num_as_char <- suppressWarnings(!is.na(as.numeric(name)))
    if(any(num_as_char)) {
        warning("It looks like you supplied some codes as character; treating them as numeric codes.")
        name[num_as_char] <- name(as.numeric(name[num_as_char]))
    }

    miss_fam <- missing(family) || is.null(family)

    rows <- lapply(name, function(z) {
        which(z == tolower(unloc_df$name), useNames = FALSE)})
    dup_codes <- lapply(rows, "length") > 1

    if(any(dup_codes)) {
        if(miss_fam) warning("A 'name' has multiple 'codes' and 'family' was not specified. Returning the code from the '", family, "' family (the name was probably 'Europe', 'Latin America and the Caribbean', or 'Nothern America').")
        for(i in which(dup_codes)) {
            if(identical(family, "M49")) {
                rows[[i]] <- rows[[i]][which(unloc_df[rows[[i]],]$location_type == 2)]
            } else if(identical(family, "SDG")) {
                rows[[i]] <- rows[[i]][which(unloc_df[rows[[i]],]$location_type != 2)]
            }
        }
    }
    rows <- unlist(rows)
    return(as.numeric(unloc_df[rows, "country_code"]))
}


##' Country or region name
##'
##' Returns the name associated with \code{code}. If a region
##' is given, all names associated will be returned in a vector.
##'
##' @param code ISO code for the country or UN code for region.
##' @return A vector the same length as \code{code} containing the
##'     name(s) as character.
##' @author Mark Wheldon
##' @family translator functions
##' @examples
##'
##' ## Countries
##' name(c(250, 4))
##'
##' ## Regions
##' name(900)
##'
##' @export
name <- function(code) {

    ## 'NA' and non-country codes mapped to 'NA'
    na.code <- is.na(code)
    na.code <- na.code | invalid_codes(code)
    if(any(na.code)) {
        out <- numeric(length(code))
        out[na.code] <- NA
        if(length(out[!na.code]) > 0) {
            out[!na.code] <- name(code[!na.code])
        }
        return(out)
    }

    code <- as.numeric(code)
    vapply(code, function(z) {
        idx <- which(z == unloc_df$country_code,
                         useNames = FALSE)
            return(unloc_df[idx, "name"])
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}


##' Region code associated with a country
##'
##' Returns the region code associated with a given country code. An error is thrown if a region
##' code is supplied.
##'
##' @param x \emph{Country} identifier. Interpreted as \dQuote{code}
##'     if \code{is.numeric(x)} and \Quote{name} if
##'     \code{is.character(x)}.
##' @param family Family to which the region referenced by \code{code}
##'     belongs.
##' @param level Level of region. Higher levels are nested in
##'     lower levels. E.g., \dQuote{Africa} is level \dQuote{1},
##'     \dQuote{Eastern Africa} is level \dQuote{2}. Converted to
##'     character if supplied as numeric.
##' @return A vector the same length as \code{x} containing the
##'     code(s) as numeric.
##' @author Mark Wheldon
##' @family translator functions
##'
##' @examples
##'
##' ## Default family is "M49"
##' reg_code(250)
##' reg_code(c("France", "FRANCE", "spAIn"))
##'
##' reg_code(250, family = "SDG")
##'
##' ## Level can be supplied as numeric for convenience
##' reg_code(250, 2, family = "SDG")
##'
##' @export
reg_code <- function(x, level = c("1","2"),
                     family = c("M49", "SDG", "WB", "Dev")) {

    level <- as.character(level)
    level <- match.arg(level)

    family <- match.arg(family)

    if(is.numeric(x)) {
        code <- x
    } else if(is.character(x)) {
        code <- code(x, family = family)
    } else code <- NA

    ## 'NA' and non-country codes mapped to 'NA'
    na.code <- is.na(code)
    na.code <- na.code | invalid_country_codes(code)
    if(any(na.code)) {
        out <- numeric(length(code))
        out[na.code] <- NA
        if(length(out[!na.code]) > 0) {
            out[!na.code] <- reg_code(code[!na.code], level = level, family = family)
        }
        return(out)
    }

    if(identical(family, "M49")) {
        if(identical(level, "1")) {
            out <- unloc_df[unlist(sapply(code, function(z) {
                which(z == unloc_df$country_code, useNames = FALSE)
                }, USE.NAMES = FALSE)), "area_code"]
            return(as.numeric(out))
        } else if(identical(level, "2")) {
            out <- unloc_df[unlist(sapply(code, function(z) {
                which(z == unloc_df$country_code, useNames = FALSE)
                }, USE.NAMES = FALSE)), "reg_code"]
            return(as.numeric(out))
        }
    } else if(family %in% c("SDG", "WB")) {

        internal_reg_country_codes <-
            get(paste0("internal_", tolower(family),
                       "_reg_L", level, "_country_codes"))

        vapply(code, function(z) {
            unloc_df_z <- unloc_df[unloc_df$z == z,]
            for(codej in internal_reg_country_codes) {
                colnamej <- make_agcode_col_names(codej)
                if(identical(unloc_df_z[, colnamej], codej)) break()
            }
            return(as.numeric(codej))
        },
        FUN.VALUE = numeric(1), USE.NAMES = FALSE)
    } else stop("family = '", family, "' not implemented.")
}


##' Region name associated with a country or region
##'
##' Returns the region name associated with a given country code.
##'
##' If a region name is supplied it will just be returned.
##'
##' @inheritParams reg_code
##' @return Region name as character
##' @author Mark Wheldon
##' @family translator functions
##'
##' @examples
##'
##' ## Default family is "M49"
##' reg_name(250)
##'
##' reg_name(250, family = "SDG")
##'
##' ## Level can be supplied as numeric for convenience
##' reg_name("France", 2, family = "SDG")
##'
##' @export
reg_name <- function(x, level = c("1","2"),
                   family = c("M49", "SDG", "WB", "Dev")) {
    name(reg_code(x, family = family, level = level))
}


##' Country codes associated with a region
##'
##' Returns codes of all countries in the region
##'
##' @inheritParams reg_code
##' @return Country codes as numeric.
##' @author Mark Wheldon
##' @family translator functions
##'
##' @export
country_codes <- function(x, family = c("M49", "SDG", "WB", "Dev")) {

    if(length(x) > 1) {
        names(x) <- x
        out <- lapply(x, "country_codes", family = family)
        return(out)
    }

    family <- match.arg(family)

    if(is.numeric(x)) {
        code <- x
    } else if(is.character(x)) {
        code <- code(x, family = family)
    } else code <- NA

    ## TO DO: VALIDATE 'x' is a region from family 'family'
    ## 'NA' and non-country codes mapped to 'NA'
    na.code <- is.na(code)
    na.code <- na.code | invalid_reg_codes(code, family = family)
    if(any(na.code)) {
        out <- numeric(length(code))
        out[na.code] <- NA
        if(length(out[!na.code]) > 0) {
            out[!na.code] <- country_codes(code[!na.code], level = level, family = family)
            return(out)
        }
    }

    if(identical(family, "M49")) {
        unloc_df_reg <- unloc_df[unloc_df$location_type %in% 2:3,]
        unloc_df_country <- unloc_df[unloc_df$location_type == 4,]
        if(!(code %in% unloc_df_reg$country_code)) stop("'x' is not an M49 region code.")
        level <-
            unloc_df_reg[unloc_df_reg$country_code == code, "location_type"]
        if(identical(as.character(level), "2")) {
            return(unloc_df_country[unloc_df_country$area_code %in% code,
                                "country_code"])
        } else if(identical(as.character(level), "3")) {
            return(unloc_df_country[unloc_df_country$reg_code %in% code,
                                "region_code"])
        }
    } else stop("family = '", family, "' not implemented.")
}


##' Country names associated with a region
##'
##' Returns names of all countries in the region
##'
##' @inheritParams reg_code
##' @return Country names as character.
##' @author Mark Wheldon
##' @family translator functions
##' @export
country_names <- function(x, family = c("M49", "SDG", "WB", "Dev")) {
    out <- country_codes(x = x, family = family)
    name(out)
}

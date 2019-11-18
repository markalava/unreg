###
### Make data frames with tables giving country codes, names, and
### region codes and names.
###

### Subset unloc_df to get just countries
make_countries_df <- function(df = unloc_df) df[df$location_type == 4,]
subset_countries_df <- function(country_code, df = make_countries_df()) {
    df[df$country_code == country_code, ,drop = FALSE]
    }


##' Create table of countries and regions
##'
##' Creates a data frame with columns for country names and codes (if
##' requested) and corresponding regions (as requested).
##'
##' Unlike \code{\link{reg_code}} and \code{\link{reg_name}}, if
##' \code{level} is not specified both are returned.
##'
##' @param level Level of region. Higher levels are nested in lower
##'     levels. E.g., \dQuote{Africa} is level \dQuote{1},
##'     \dQuote{Eastern Africa} is level \dQuote{2}. Converted to
##'     character if supplied as numeric. The \sQuote{""}, or
##'     \code{NULL}, can be given to request all levels.
##' @param include_codes Logial. Include country and region codes in the
##'     result? You must set at least one of \code{include_codes} and
##'     \code{include_names} to \code{TRUE}.
##' @param include_names Logical. Include country and region names in the
##'     result?
##' @param stringsAsFactors Passed to \code{\link{data.frame}}. Note
##'     the default is \code{FALSE}.
##' @inheritParams reg_code
##' @return
##' @author Mark Wheldon
##' @family Table functions
##'
##' @examples
##' reg_table(level = 1, family = M49)
##'
##' @export
reg_table <- function(level = c("", "1", "2"),
                      family = c("M49", "SDG", "WB_inc", "Dev"),
                      include_codes = TRUE, include_names = TRUE,
                      stringsAsFactors = FALSE) {

    if(sum(include_codes, include_names) < 1) stop("Nothing to return. You must set at least one of 'include_codes' and 'include_names' to 'TRUE'.")

    if(is.null(level) || identical(level, "")) level <- 1:2
    level <- as.character(level)
    level <- match.arg(level, several.ok = TRUE)

    family <- match.arg(family, several.ok = TRUE)

    ## Using lists instead of data frames avoids copying (although
    ## with tables this small it hardly matters...)

    ## number of elements
    n_el <- (1 + (length(family) * length(level))) * (1 + isTRUE(include_names))

    ## Which are codes, which are names?
    if(include_names) {
        code_el <- seq(from = 1, to = n_el, by = 2)
        name_el <- seq(from = 2, to = n_el, by = 2)
    } else code_el <- 1:n_el

    lis <- vector("list", n_el)

    lcc <- list_country_codes()
    lis[[1]] <- lcc
    names(lis)[[1]] <- "country"

    el_idx <- 1
    code_el_fam <- code_el[-1]
    el_keep <- rep(TRUE, n_el)
    for(this_fam in family) {
        for(this_lev in level) {
            k <- code_el_fam[el_idx]
            fam_lev_codes <-
                make_agcode_var_name(family = this_fam, level = this_lev)
            if(identical(this_lev, "1") || identical(this_fam, "M49") || exists(fam_lev_codes)) {
                lis[[k]] <- reg_code(lcc, level = this_lev, family = this_fam)
                names(lis)[[k]] <- paste(this_fam, this_lev, sep = "_L")
            } else {
                el_keep[k] <- FALSE
                if(include_names) el_keep[k + 1] <- FALSE
            }
            el_idx <- el_idx + 1
        }
    }

    w_el_keep <- which(el_keep)
    if(identical(w_el_keep, c(1L,2L))) stop("No regions for this 'level' and 'family' combination.")

    code_el <- code_el[code_el %in% w_el_keep]
    name_el <- name_el[name_el %in% w_el_keep]

    if(include_codes) {
        names(lis)[code_el] <- paste(names(lis)[code_el], "code", sep = "_")
    }
    if(include_names) {
        for(i in name_el) {
            lis[[i]] <- name(lis[[i-1]])
            names(lis)[i] <- gsub("code", "name", names(lis)[i-1])
        }
    }

    if(!include_codes) {
        out_el <- name_el
    } else if(!include_names) {
        out_el <- code_el
    } else {
        out_el <- sort(unique(c(code_el, name_el)))
    }

    return(as.data.frame(lis[out_el]))
}


##' Create table with just countries and their codes
##'
##' Creates a table with country codes and their names. Optionally,
##' just a table with names or codes. This function returns a data
##' frame. If only one of \code{include_codes} and \code{include_names} is \code{TRUE}
##' it will be a data frame with one column. If you want a vector of
##' names or codes see \code{\link{code}} and \code{\link{name}}.
##'
##' @param include_codes
##' @param include_names
##' @param stringsAsFactors
##' @return A data frame with country codes, names, or both. Always a
##'     data frame, even if one of \code{include_codes} or \code{include_names} is
##'     \code{FALSE}.
##' @author Mark Wheldon
##' @family Table functions
##' @export
country_table <- function(include_codes = TRUE, include_names = TRUE,
                          stringsAsFactors = FALSE) {

    if(sum(include_codes, include_names) < 1) stop("Nothing to return. You must set at least one of 'include_codes' and 'include_names' to 'TRUE'.")

    if(!include_codes) {
        df <- unloc_df[unloc_df$location_type == 4, "name", drop = FALSE]
        if(stringsAsFactors) df$name <- factor(df$name)
        return(df)
    } else if(!include_names) {
          df <- unloc_df[unloc_df$location_type == 4, "country_code", drop = FALSE]
          colnames(df) <- "code"
          return(df)
      } else {
          df <- unloc_df[unloc_df$location_type == 4, c("name", "country_code")]
          colnames(df) <- c("name", "code")
          return(df)
      }
    }


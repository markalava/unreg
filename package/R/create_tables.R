###
### Make data frames with tables giving country codes, names, and
### region codes and names.
###


### Used by several functions.
### Subsets 'UNlocations'  to just countries and columns for SDG regions.
make_loc4_sdg_df <- function(df = unloc_df) {
    out <- df[df$location_type == 4,
              c("name", "country_code", "reg_code", "reg_name",
                internal_sdg_reg_L1_ag_cols)]
    out[, internal_sdg_reg_L1_ag_cols] <-
        out[, internal_sdg_reg_L1_ag_cols] != -1
    return(out)
}


### M49 table backend
M49_table <- function(codes = TRUE, names = TRUE,
                      level = c("1", "2"),
                      stringsAsFactors = FALSE) {

    if(sum(codes, names) < 1) stop("Nothing to return.")

    if(is.null(level) || identical(level, "")) level <- 1:2
    level <- as.character(level)
    level <- match.arg(level, several.ok = TRUE)
    l1 <- "1" %in% level
    l2 <- "2" %in% level

    loc4 <- unloc_df[unloc_df$location_type == 4,]

    df <- data.frame(code = loc4$country_code,
                     name = loc4$name,
                     stringsAsFactors = stringsAsFactors)

    if(l1) {
    df <- data.frame(df,
                     M49_reg_1_code = loc4$area_code,
                     M49_reg_1_name = loc4$area_name,
                     stringsAsFactors = stringsAsFactors)
    }
    if(l2) {
        df <- data.frame(df,
                     M49_reg_2_code = loc4$reg_code,
                     M49_reg_2_name = loc4$reg_name,
                     stringsAsFactors = stringsAsFactors)
        }
    if(!codes) return(df[,c(2, 4, 6)])
    else if(!names) return(df[,c(1, 3, 5)])
    else return(df)
    }


### SDG table backend
SDG_table <- function(codes = TRUE, names = TRUE,
                      level = c("1", "2"),
                      stringsAsFactors = FALSE) {

    if(sum(codes, names) < 1) stop("Nothing to return.")

    if(is.null(level) || identical(level, "")) level <- 1:2
    level <- as.character(level)
    level <- match.arg(level, several.ok = TRUE)
    l1 <- "1" %in% level
    l2 <- "2" %in% level

    loc4_sdg <- make_loc4_sdg_df()

    df <- data.frame(code = loc4_sdg$country_code,
                     name = loc4_sdg$name,
                     stringsAsFactors = stringsAsFactors)

    if(l1) {
        df <- data.frame(df,
                            SDG_reg_1_code = NA,
                            stringsAsFactors = stringsAsFactors
                         )
        for(j in seq_along(internal_sdg_reg_L1_ag_cols)) {
            df$SDG_reg_1_code[loc4_sdg[, internal_sdg_reg_L1_ag_cols[j]]] <-
                internal_sdg_reg_L1_country_codes[j]
        }
        df$SDG_reg_1_name <- name(df$SDG_reg_1_code)
        }
    if(l2) {
        df <- data.frame(df,
                         SDG_reg_2_code = loc4_sdg$reg_code,
                         SDG_reg_2_name = loc4_sdg$reg_name,
                         stringsAsFactors = stringsAsFactors
                         )
    }

    if(!names) return(df[,seq(from = 1, to = ncol(df), by = 2)])
    else {
        if(stringsAsFactors) {
            for(j in seq(from = 2, to = ncol(df), by = 2)) {
                df[,j] <- factor(df[,j])
            }
        } else {
            for(j in seq(from = 2, to = ncol(df), by = 2)) {
                df[,j] <- as.character(df[,j])
            }
        }
    }

    if(!codes) return(df[,seq(from = 2, to = ncol(df), by = 2)])
    else return(df)
}


##' Create table of countries and regions
##'
##' Creates a data frame with columns for country names and codes (if
##' requested) and corresponding regions (as requested).
##'
##' @param level Level of region. Higher levels are nested in lower
##'     levels. E.g., \dQuote{Africa} is level \dQuote{1},
##'     \dQuote{Eastern Africa} is level \dQuote{2}. Converted to
##'     character if supplied as numeric. The \sQuote{""}, or
##'     \code{NULL}, can be given to request all levels.
##' @param codes Logial. Include country and region codes in the
##'     result? You must set at least one of \code{codes} and
##'     \code{names} to \code{TRUE}.
##' @param names Logical. Include country and region names in the
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
                      family = c("M49", "SDG", "WB", "Dev"),
                      codes = TRUE, names = TRUE, stringsAsFactors = FALSE) {

    if(sum(codes, names) < 1) stop("Nothing to return. You must set at least one of 'codes' and 'names' to 'TRUE'.")

    if(is.null(level) || identical(level, "")) level <- 1:2
    level <- as.character(level)
    level <- match.arg(level, several.ok = TRUE)

    family <- match.arg(family, several.ok = TRUE)

    df <- data.frame(code = list_country_codes(),
                     stringsAsFactors = stringsAsFactors)
    if(names) df$name <- name(df$code)

    if("M49" %in% family) {
        out <- M49_table(codes = TRUE, names = names, level = level,
                         stringsAsFactors = stringsAsFactors)
        out <- out[, !colnames(out)=="name", drop = FALSE]
        df <- merge(df, out,
                   by = "code", all.x = TRUE)
    }

    if("SDG" %in% family) {
        out <- SDG_table(codes = TRUE, names = names, level = level,
                         stringsAsFactors = stringsAsFactors)
        out <- out[, !colnames(out)=="name", drop = FALSE]
        df <- merge(df, out,
                    by = "code", all.x = TRUE)
    }

    if(any(family %in% c("WB", "Dev"))) message("family types 'WB' and 'Dev' not yet implemented.")

    return(df)
}


##' Create table with just countries and their codes
##'
##' Creates a table with country codes and their names. Optionally,
##' just a table with names or codes. This function returns a data
##' frame. If only one of \code{codes} and \code{names} is \code{TRUE}
##' it will be a data frame with one column. If you want a vector of
##' names or codes see \code{\link{code}} and \code{\link{name}}.
##'
##' @param codes
##' @param names
##' @param stringsAsFactors
##' @return A data frame with country codes, names, or both. Always a
##'     data frame, even if one of \code{codes} or \code{names} is
##'     \code{FALSE}.
##' @author Mark Wheldon
##' @family Table functions
##' @export
country_table <- function(codes = TRUE, names = TRUE, stringsAsFactors = FALSE) {

    if(sum(codes, names) < 1) stop("Nothing to return. You must set at least one of 'codes' and 'names' to 'TRUE'.")

    if(!codes) {
        df <- unloc_df[unloc_df$location_type == 4, "name", drop = FALSE]
        if(stringsAsFactors) df$name <- factor(df$name)
        return(df)
    } else if(!names) {
          df <- unloc_df[unloc_df$location_type == 4, "country_code", drop = FALSE]
          colnames(df) <- "code"
          return(df)
      } else {
          df <- unloc_df[unloc_df$location_type == 4, c("name", "country_code")]
          colnames(df) <- c("name", "code")
          return(df)
      }
    }


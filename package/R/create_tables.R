###
### Make data frames with tables giving country codes, names, and
### region codes and names.
###


## Used by several functions.
## Subsets 'UNlocations'  to just countries and columns for SDG regions.
make_loc4_sdg_df <- function(df = unloc_df) {
    out <- df[df$location_type == 4,
              c("name", "country_code", "reg_code", "reg_name",
                internal_sdg_reg_L1_ag_cols)]
    out[, internal_sdg_reg_L1_ag_cols] <-
        out[, internal_sdg_reg_L1_ag_cols] != -1
    return(out)
}


M49_table <- function(codes = TRUE, names = TRUE, stringsAsFactors = FALSE) {
    if(sum(codes, names) < 1) stop("Nothing to return.")
    loc4 <- unloc_df[unloc_df$location_type == 4,]
    df <- data.frame(code = loc4$country_code,
                     name = loc4$name,
                     M49_reg_1_code = loc4$area_code,
                     M49_reg_1_name = loc4$area_name,
                     M49_reg_2_code = loc4$reg_code,
                     M49_reg_2_name = loc4$reg_name,
                     stringsAsFactors = stringsAsFactors)
    if(!codes) return(df[,c(2, 4, 6)])
    else if(!names) return(df[,c(1, 3, 5)])
    else return(df)
    }


SDG_table <- function(codes = TRUE, names = TRUE, stringsAsFactors = FALSE) {
    if(sum(codes, names) < 1) stop("Nothing to return.")

    loc4_sdg <- make_loc4_sdg_df()

    df <- data.frame(code = loc4_sdg$country_code,
                     name = loc4_sdg$name,
                     SDG_reg_1_code = NA,
                     SDG_reg_1_name = NA,
                     SDG_reg_2_code = loc4_sdg$reg_code,
                     SDG_reg_2_name = loc4_sdg$reg_name,
                     stringsAsFactors = stringsAsFactors
                     )
    ## !!!!!!!!!!!!!!!!!!!!!! HERE HERE HERE HERE
    for(j in seq_along(internal_sdg_reg_L1_ag_cols)) {
        df$reg_1_code[loc4_sdg[, internal_sdg_reg_L1_ag_cols[j]]] <-
            internal_sdg_reg_L1_country_codes[j]
    }

    if(!names) return(df[,c(1, 3, 5)])
    else {
        browser()
        if(stringsAsFactors)
            df$reg_1_name <- factor(name(df$reg_1_code))
        else df$reg_1_name <- as.character(name(df$reg_1_code))
        }

    if(!codes) return(df[,c(2,4,6)])
    else return(df)
}


##' .. title (sentence case, not terminated with a full-stop) ..
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param level
##' @param family
##' @param codes
##' @param names
##' @param stringsAsFactors
##' @return
##' @author Mark Wheldon
##' @export
loc_table <- function(level = c("1", "2"),
                      family = c("M49", "SDG", "WB", "Dev"),
                      codes = TRUE, names = TRUE, stringsAsFactors = FALSE) {

    level <- as.character(level)
    level <- match.arg(level, several.ok = TRUE)
    l1 <- "1" %in% level
    l2 <- "2" %in% level

    family <- match.arg(family, several.ok = TRUE)

    df <- data.frame(code = list_country_codes(),
                     stringsAsFactors = stringsAsFactors)
    if(names) df$name <- name(df$code)

    if("M49" %in% family) {
        out <- M49_table(codes = TRUE, names = names,
                         stringsAsFactors = stringsAsFactors)
        df <- merge(df[, !colnames(df)=="names"],
                    out, by = "code", all.x = TRUE)
    }

    if("SDG" %in% family) {
        out <- SDG_table(codes = TRUE, names = names,
                         stringsAsFactors = stringsAsFactors)
        df <- merge(df[, !colnames(df)=="names"],
                    out, by = "code", all.x = TRUE)
    }

    if(any(family %in% c("WB", "Dev"))) message("'family' types 'WB' and 'Dev' not yet implemented.")

    return(df)
}

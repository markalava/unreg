### FUNCTIONS

## From 'example("source", package = "base")
sourceDir <- function(path, trace = TRUE, ...) {
    for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
        if(trace) cat(nm,":")
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
    }
}


### SOURCE THE PACKAGE

sourceDir("R")
load("R/sysdata.rda")


###-----------------------------------------------------------------------------

### TESTS

head(reg_table(level = "1", family = "WB"))


reg_name(175, family = "WB")

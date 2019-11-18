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

reg_table(level = "2", family = "WB")


reg_code(175, family = "WB")

################################################################################
###
###  DATE CREATED: 2019-09-30
###
###  AUTHOR: Mark Wheldon
###
###  PROJECT: Making Family Planning Count
###
###  DESCRIPTION:
###
###     Build 'unreg' package
###
################################################################################

###-----------------------------------------------------------------------------
### * Set Up

###-----------------------------------------------------------------------------
### ** Options

## Print warnings as they occur
options(warn = 1)

###-----------------------------------------------------------------------------
### ** File Paths

## Source code
git_dir <- file.path("..", "..")
pkg_dir <- file.path(git_dir, "package")
src_dir <- file.path(pkg_dir, "R")

###-----------------------------------------------------------------------------
### ** Functions

example(source, echo = FALSE)
source("build_utils.R")

###-----------------------------------------------------------------------------
### * UNREG Package

owd <- getwd()
setwd(pkg_dir)
sourceDir("data-raw")
setwd(owd)

build_install(src_dir = src_dir, git_dir = git_dir)
library(unreg)

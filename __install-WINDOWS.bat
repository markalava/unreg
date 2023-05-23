@echo off
ECHO.
ECHO.

FOR %%I in (.) DO SET CurrDirName=%%~nxI

ECHO. ********************************************************************************
ECHO.
ECHO. PACKAGE NAME:  %CurrDirName%
ECHO. 
ECHO. ********************************************************************************

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.

ECHO. ================================================================================
ECHO. DATA
ECHO. ================================================================================

Rscript -e "setwd('data-raw'); sink(tempfile()); example(source); sink(); sourceDir('.')"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. DOCUMENT
ECHO. ================================================================================

Rscript -e "devtools::document()"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. INSTALL
ECHO. ================================================================================


CHDIR .. && R CMD INSTALL --build --install-tests %CurrDirName% && CHDIR %CurrDirName%
if %ERRORLEVEL% GEQ 1 PAUSE


ECHO. ================================================================================
ECHO. TESTS
ECHO. ================================================================================

Rscript -e "testthat::test_package('%CurrDirName%')"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
PAUSE

#' roptuna: Hyperparameter Optimization Using the Optuna Framework
#'
#' An R implementation of the Optuna hyperparameter optimization framework.
#' Provides define-by-run search spaces, TPE sampling, trial pruning, and
#' persistent study storage via SQLite.
#'
#' @keywords internal
"_PACKAGE"

## Suppress R CMD check note about .data pronoun used in ggplot2 aes()
utils::globalVariables(".data")

## Ensure package namespaces are imported (used via :: in R6 class bodies)
#' @importFrom R6 R6Class
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom jsonlite toJSON fromJSON
NULL

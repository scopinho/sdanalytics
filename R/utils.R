# Libraries ----------------------------------------------------------
#' @importFrom forcats as_factor
#' @import ggplot2
#' @import dplyr
#' @import waiter
#' @import bslib
#' @import gridlayout
#' @import bsicons
#' @import arrow
#' @import shiny
#' @import glue
#' @import lubridate


df <- arrow::open_dataset("./inst/extdata", format="parquet")



# Functions ----------------------------------------------------------



get_unique_labels <- function(df, col) {
  df |> dplyr::distinct(.data[[col]]) |> dplyr::collect() |> dplyr::filter(!is.na(.data[[col]])) |> dplyr::pull()
}

get_min_value <- function(df, col) {
  df |> dplyr::distinct(.data[[col]]) |> dplyr::collect() |> dplyr::slice_min(.data[[col]], na_rm = TRUE) |> dplyr::pull()
}

get_max_value <- function(df, col) {
  df |> dplyr::distinct(.data[[col]]) |> dplyr::collect() |> dplyr::slice_max(.data[[col]], na_rm = TRUE) |> dplyr::pull()
}




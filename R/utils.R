# Libraries ----------------------------------------------------------
#' @importFrom forcats as_factor
#' @import ggplot2
#' @import dplyr
#' @import waiter
#' @import bslib
#' @import gridlayout
#' @import bsicons
#' @import shiny
#' @import glue
#' @import lubridate
#' @import arrow
#' @import rlang
#' @import purrr
#' @importFrom htmltools css


get_db <- function() {
  arrow::open_dataset(system.file("extdata", package = "sdanalytics"),
  format = "parquet")
}


# Functions ----------------------------------------------------------



get_unique_labels <- function(df, col) {
  df |>
    distinct(.data[[col]]) |>
    collect() |>
    filter(!is.na(.data[[col]])) |>
    pull()
}

get_min_value <- function(df, col) {
  df |>
    distinct(.data[[col]]) |>
    collect() |>
    slice_min(.data[[col]], na_rm = TRUE) |>
    pull()
}

get_max_value <- function(df, col) {
  df |>
    distinct(.data[[col]]) |>
    collect() |>
    slice_max(.data[[col]], na_rm = TRUE) |>
    pull()
}

get_total_rows <- function(df) {
  df |>
    count() |>
    collect() |>
    pull()
}

get_total_table <- function(df) {
  df |>
    collect()
}

get_sla_missed <- function(df) {
  df |> 
  group_by(made_sla)|> 
  summarise(n = n()) |> 
  collect()|> mutate(perc = n/sum(n)*100) |> 
  filter (made_sla == TRUE) |> 
  select(perc) |> 
  pull()|> 
  round(1)

}
# return (as.character("99999"))



# parse_filter_from_inputs <- function (params_list){
#
#   map(params_list, ~glue("{.}"))
#
#   reduce('&', imap(list, ~expr(!!as.name(.y) == !!.x)))
#
#   #glue("{names(list[1])} == \"{list[1]}\"")
#
# }

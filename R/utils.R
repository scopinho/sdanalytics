# Libraries ----------------------------------------------------------
#' @importFrom forcats as_factor
#' @import dplyr
#' @import bslib
#' @import shiny
#' @importFrom htmltools css
#' @importFrom lubridate as_datetime as_date days wday
#' @importFrom glue glue
#' @importFrom waiter autoWaiter
#' @importFrom arrow open_dataset to_duckdb
#' @importFrom bsicons bs_icon
#' @importFrom echarts4r renderEcharts4r echarts4rOutput e_toolbox_feature e_area e_step e_gauge e_density e_visual_map e_title e_river e_y_axis e_effect_scatter e_flip_coords e_scatter e_angle_axis e_radius_axis e_polar e_line e_tooltip e_datazoom e_bar e_charts e_pie e_legend e_grid e_theme_custom e_labels 
#' @importFrom rmarkdown pandoc_available



get_dataset <- function(path) {
  
  if (is.null(path)){
    arrow <- arrow::open_dataset(system.file("extdata/part-0-small.parquet", package = "sdanalytics"),format = "parquet")

  }else{
    arrow <- arrow::open_dataset(sources = path,format = "parquet")
    
  }
  return(arrow)
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
  filter (incident_state == "Closed")|>
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

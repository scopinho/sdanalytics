#' @export openUI
# Libraries ----------------------------------------------------------
library(dplyr)
library(shiny)
library(waiter)
library(bslib)
library(gridlayout)
library(ggplot2)
library(forcats)
library(bsicons)
library (arrow)

# Data Sources -------------------------------------------------------


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


# Main -------------------------------------------------------

openUI <- function(...)  {

  ## UI ------------------------------------------------------ 
  ui <- page_navbar(
    
    nav(
      title = "Home",
      mod_home_UI("home")
    ),
    nav(
      title = "Details"
    ),
    
    title = "SD Analytics",
    # title = div(
    #   div(
    #     id = "nav-logo",
    #     img(src = "logo.png", style = "height: 30px;
    #       position: absolute;
    #       right: -10px;
    #       top: 10px;")
    #   ),
    #   "Techforg Analytics"
    # ),
    
    sidebar = sidebar(
      
      dateRangeInput(
        inputId = "opened_at",
        label = "Opened Date",
        start = get_min_value(df, "opened_at") |> format("%Y-%m-%d %H:%M"),
        end = get_max_value(df, "opened_at") |> format("%Y-%m-%d %H:%M"),
        min = get_min_value(df, "opened_at") |> format("%Y-%m-%d %H:%M"),
        max = get_max_value(df, "opened_at") |> format("%Y-%m-%d %H:%M")
    ),
      dateRangeInput(
        inputId = "resolved_at",
        label = "Resolved Date",
        start = get_min_value(df, "resolved_at") |> format("%Y-%m-%d %H:%M"),
        end = get_max_value(df, "resolved_at") |> format("%Y-%m-%d %H:%M"),
        min = get_min_value(df, "resolved_at") |> format("%Y-%m-%d %H:%M"),
        max = get_max_value(df, "resolved_at") |> format("%Y-%m-%d %H:%M")
      ),

       selectizeInput(
         inputId = "assignment_group",
         label = strong("Assignment Group"),
         choices = get_unique_labels(df, "assignment_group"),
         selected = get_max_value(df, "assignment_group"),
         multiple = TRUE
       ),
       selectizeInput(
         inputId = "incident_state",
         label = strong("Incident State"),
         choices = get_unique_labels(df, "incident_state"),
         selected = get_max_value(df, "incident_state"),
         multiple = TRUE
       ),
       radioButtons(
         inputId = "made_sla",
         label = "Made SLA?",
         choices = c("TRUE", "FALSE")
       ),

      # sliderInput(
      #   inputId = "reopen_count",
      #   label = "Reopened",
      #   min = 0,
      #   max = get_max_value(df, "reopen_count"),
      #   value = 0,
      # ),
      title = "Global Filters"
    ),
    #fillable = TRUE,
    theme =  bs_theme(version = 5)
  )
  
  ## Server -------------------------------------------------------
  
  server <- function(input, output, session) {
    

    params <- reactiveValues()
    params$df <- df

    params$opened_at <- reactive({input$opened_at})
    params$resolved_at <- reactive({input$resolved_at})
    params$assignment_group <- reactive({input$assignment_group})
    params$incident_state <- reactive({input$incident_state})
    params$made_sla <- reactive({input$made_sla})
    params$reopen_count <- reactive({input$reopen_count})

    mod_home_Server("home", params)
    
  }
  
  shinyApp(ui, server, ...)
}
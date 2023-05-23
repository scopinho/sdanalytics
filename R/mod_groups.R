# UI --------------------------------------------------------------------------
mod_groups_UI <- function(id, df) {
  ns <- NS(id)

## card_group_summary ---------------------------------------------------------
  
  card_group_summary <- card(
    class = c("text-light"),
    style = "background-color: rgb(112,000,132);",
    card_body(
      style = "padding-bottom: 0px;",
      card_body(
        style = "padding-bottom: 0px;",
        uiOutput(outputId = ns("group_summary")),
        br(),
        downloadButton(ns("group_report"), "Download Group Report...")
      )
      
    )
    
  )
  
## card_sla --------------------------------------------------------------------
  
  card_sla <- card(card_header("Service Level"),
                card_body(echarts4rOutput((ns(
                  "group_sla"
                )))),
                full_screen = TRUE)
  
## card_daily_open -------------------------------------------------------------
  
  
  card_daily_open <- card(card_header ("Daily Open Tickets"),
                card_body(echarts4rOutput((
                  ns("plot_daily_opened_echarts")
                ))),
                full_screen = TRUE)
  
## card_hours_per_day ----------------------------------------------------------
  
  card_hours_per_day <- card(
    card_header ("Hours per Days of the Week"),
    card_body(echarts4rOutput((
      ns("plot_hours_echarts")
    ))),
    full_screen = TRUE
  )

## card_top_categories ---------------------------------------------------------
  
  card_top_categories <- card(
    card_header ("Top Categories (Max. 10)"),
    card_body(echarts4rOutput((
      ns("plot_top_categories_echarts")
    ))),
    full_screen = TRUE
  )
  
## card_top_resolvers ---------------------------------------------------------
  
  card_top_resolvers <- card(card_header ("Top Resolvers (Max. 10)"),
                card_body(echarts4rOutput((
                  ns("plot_top_resolvers_echarts")
                ))),
                full_screen = TRUE)
  
## card_daily_resolved ---------------------------------------------------------
  
  card_daily_resolved <- card(card_header ("Daily Resolved Tickets"),
                card_body(echarts4rOutput((
                  ns("plot_daily_resolved_echarts")
                ))),
                full_screen = TRUE)
  
## card_by_priority------------------------------------------------------------
  
  card_by_priority <- card(card_header ("Tickets by Priority"),
                              card_body(echarts4rOutput((
                                ns("plot_by_priority_echarts")
                              ))),
                              full_screen = TRUE)

## Main Page -------------------------------------------------------------------
  
  tagList(

    page_fluid(
      layout_column_wrap(
        width = NULL,
        style = css(grid_template_columns = "0.7fr 2.3fr 1.0fr"),
        height = 300,
        
        card_group_summary,
        card_daily_open,
        card_sla
      ),
      
      p(),
      
      layout_column_wrap(
        width = NULL,
        height = 300,
        style = css(grid_template_columns = "4fr 2fr 2fr"),
        card_hours_per_day,
        card_top_categories,
        card_top_resolvers
      ),
      
      p(),
      
      layout_column_wrap(
        width = NULL,
        height = 500,
        style = css(grid_template_columns = "1fr 1fr"),
        
        card_daily_resolved, 
        card_by_priority
      )
      
    )
    
  )
}

# SERVER -----------------------------------------------------------------------

mod_groups_Server <- function(id, df, params) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
## Reactive Filtered Data ------------------------------------------------------
                 
                 filtered_data_groups <- reactive({
                   
                   #req(params$assignment_group())
                   
                   data <-
                     select(
                       df,
                       number,
                       made_sla,
                       opened_at,
                       resolved_by,
                       resolved_at,
                       assignment_group,
                       incident_state,
                       category,
                       priority
                     )
                   
                   if (is.na(params$opened_at()[1])) {
                     data <- data
                   }
                   else{
                     data <- filter(data, opened_at >= params$opened_at()[1])
                   }
                   
                   if (is.na(params$opened_at()[2])) {
                     data <- data
                   }
                   else{
                     data <- filter(data, opened_at <= params$opened_at()[2])
                   }
                   
                   if (is.na(params$resolved_at()[1])) {
                     data <- data
                   }
                   else{
                     data <- filter(data, resolved_at >= params$resolved_at()[1])
                   }
                   
                   if (is.na(params$resolved_at()[2])) {
                     data <- data
                   }
                   else{
                     data <- filter(data, resolved_at <= params$resolved_at()[2])
                   }
                   
                   if (is.null(params$assignment_group())) {
                     data <- data
                   }
                   else{
                     data <-
                       filter(data,
                              assignment_group %in% params$assignment_group()[1])
                   }
                   
                   if (is.null(params$incident_state())) {
                     data <- data
                   }
                   else{
                     data <- filter(data, incident_state %in% params$incident_state())
                   }
                   
                   data <- data
                   
                 })
                 
## card_group_summary ----------------------------------------------------------
                 
                 output$group_summary <- renderUI({
                   
                   group_resolvers <- filtered_data_groups() |>
                     select (resolved_by) |>
                     distinct() |>
                     count() |>
                     collect() |>
                     pull()
                   
                   tickets_received <- filtered_data_groups() |>
                     select (number) |>
                     distinct() |>
                     count() |>
                     collect() |>
                     pull()
                   
                   tickets_closed <- filtered_data_groups() |>
                     select (number, incident_state) |>
                     filter (incident_state %in% c("Resolved", "Closed")) |>
                     distinct(number) |>
                     count() |>
                     collect() |>
                     pull()
                   
                   group_resolvers <- 
                     if (is.null(params$assignment_group()[1])){
                       "<i>Select an <b>Assignment Group</b> at the \"Global Filter\" sidebar on the left to see the information here...</i>"
                     } else {
                     
                     glue(
                       "<b>Group</b>: {params$assignment_group()[1]}<br>
                                 <b>Group Resolvers</b>: {group_resolvers}<br>
                        <b>Tickets Received</b>: {tickets_received}<br>
                        <b>Tickets Closed</b>: {tickets_closed}"
                     )
                     
                     }
                   markdown(group_resolvers)
                 })
                 
## card_sla --------------------------------------------------------------------
                 
                 output$group_sla <- renderEcharts4r({
                   
                   req(params$assignment_group())
                   
                   sla <- filtered_data_groups() |>
                     select (number, made_sla) |>
                     group_by(made_sla) |>
                     summarise(value = n()) |>
                     collect() |>
                     mutate (perc = round(100 * (value / sum(value)), 1)) |>
                     filter (made_sla == TRUE) |>
                     select (perc) |>
                     pull()
                   
                   e_charts() |>
                     e_gauge(sla, "SLA", radius = "100%")
                   
                 })
                 
## card_hours_per_day ----------------------------------------------------------
                 
                 output$plot_hours_echarts <- renderEcharts4r({
                   
                   req(params$assignment_group())
                   
                   data <- filtered_data_groups() |>
                     select (number, opened_at) |>
                     group_by(wday = wday(opened_at),
                              hour = hour(opened_at)) |>
                     summarise(value = n()) |>
                     arrange (wday) |>
                     collect() |>
                     mutate (wday = as.character(wday(
                       wday, label = TRUE, abbr = TRUE
                     )))
                   
                   e_charts(data |> group_by(wday), wday) |>
                     e_tooltip(trigger = "item") |>
                     e_effect_scatter(hour, value) |>
                     e_y_axis(max = "24", formatter = "{value}:00") |>
                     e_legend(FALSE)
                   
                 })
                 
## card_daily_open ------------------------------------------------------------
                 
                 output$plot_daily_opened_echarts <- renderEcharts4r({
                   
                   req(params$assignment_group())
                   
                   data <-  filtered_data_groups() |>
                     group_by(day = as_date(opened_at)) |>
                     summarise(n = n()) |>
                     collect() |> arrange(day)
                   
                   e_charts(data, x = day) |>
                     e_bar(serie = n,
                           name = "Tickets",
                           legend = FALSE) |>
                     e_tooltip(trigger = "axis") |>
                     e_grid(top = "8", containLabel = FALSE)  |>
                     e_datazoom(toolbox = FALSE) |>
                     e_toolbox_feature(feature = "dataView")
                 })
                 
                 
## card_top_categories --------------------------------------------------------
                 
                 output$plot_top_categories_echarts <- renderEcharts4r({
                   
                   req(params$assignment_group())
                   
                   data <- filtered_data_groups() |>
                     select (number, category) |>
                     group_by(category) |>
                     summarise(value = n()) |>
                     arrange (value) |>
                     head(10) |>
                     collect()
                   
                   e_charts(data, category) |>
                     e_pie(value, roseType = "radius", legend = FALSE) |>
                     
                     # e_polar() |>
                     # e_angle_axis(category) |>
                     # e_radius_axis() |>
                     # e_bar(value, coord_system = "polar") |>
                     e_tooltip(trigger = "item")
                 })
                 
## card_top_resolvers ----------------------------------------------------------
                 
                 output$plot_top_resolvers_echarts <- renderEcharts4r({
                   req(params$assignment_group())
                   
                   data <- filtered_data_groups() |>
                     select (number, resolved_by) |>
                     group_by(resolved_by) |>
                     summarise(value = n()) |>
                     arrange (value) |>
                     head(10) |>
                     collect()
                   
                   e_charts(data, resolved_by) |>
                     e_bar(value, legend = FALSE) |>
                     e_flip_coords() |>
                     e_grid(left = "12", containLabel = TRUE) |>
                     e_tooltip(trigger = "item")
                 })
                 
 ## card_daily_resolved --------------------------------------------------------
                 
                 output$plot_daily_resolved_echarts <- renderEcharts4r({
                   req(params$assignment_group())
                   
                   data <-  filtered_data_groups() |>
                     group_by(day = as_date(resolved_at)) |>
                     summarise(value = n()) |>
                     collect() |> arrange(day)
                   
                   e_charts(data, x = day) |>
                     e_bar(serie = value,
                            name = "Tickets",
                            legend = FALSE) |>
                     e_tooltip(trigger = "axis") |>
                     e_grid(top = "8", containLabel = FALSE)  |>
                     e_datazoom(toolbox = FALSE) |>
                     e_toolbox_feature(feature = "dataView")
                 })
                 
## Download Report Button --------------------------------------------------------
                 
                 output$group_report <- downloadHandler(
                   filename = function() {
                     paste0("grupo_report_", params$assignment_group()[1], ".pdf")
                   }
                   ,
                   content = function (file = paste0("grupo_report_", params$assignment_group()[1], ".pdf")) {
                    
                    #withProgress(message = "Creating Report file. Please wait...", {
                    showNotification("Creating Reporting File. Please wait...")
                       
                     group_name <- params$assignment_group()[1]
                     dataset <- system.file("extdata/part-0-small.parquet", package = "sdanalytics")
                     
                     future::future({
                       rmarkdown::render(
                         input = "www/template_groups_report.Rmd",
                         output_file = file,
                         params = list(group = group_name, dataset = dataset)
                       )
                     #})
                     }, seed=TRUE)
                     
                   }
                 )
                 
## card_by_priority -----------------------------------------------------------
                 
                 output$plot_by_priority_echarts <- renderEcharts4r({
                   
                   req(params$assignment_group())
                   
                   data <-  filtered_data_groups() |>
                  group_by (priority) |> 
                     count(name = "value") |> 
                     arrange(priority) |> 
                     collect()
                   
                   data |> group_by(priority) |> 
                     e_charts(priority) |> 
                     e_bar(value)  |>
                     e_tooltip() |>
                     e_toolbox_feature(feature = "dataView")
                 })
                 
                 
               })
}

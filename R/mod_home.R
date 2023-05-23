# UI --------------------------------------------------------------------------

mod_home_UI <- function(id) {
  ns <- NS(id)
  
  
  ## card_total -----------------------------------------------------------------
  
  card_total <- card(card_body(
    style = "padding: 0",
    value_box(
      class = "text-dark",
      style = "--bs-primary-rgb: 251,139,36;",
      title = "Total",
      value = uiOutput(ns("totalTickets")),
      showcase = bsicons::bs_icon("postcard-fill")
    )
  ))
  
  ## card_summary ----------------------------------------------------------------
  
  card_summary <-
    card(card_body(style = "vertical-align: \"middle\";",
                   {
                     uiOutput(ns("summaryCard"))
                   }))
  
  ## card_open -------------------------------------------------------------------
  
  card_open <- card(card_body(
    style = "padding: 0",
    value_box(
      class = c("text-light"),
      style = "--bs-primary-rgb: 112,000,132;",
      title = "Open",
      value = uiOutput(ns("openTickets")),
      showcase = bsicons::bs_icon("pip-fill")
    )
  ))
  
  ## card_sla -------------------------------------------------------------------
  
  card_sla <- card(card_body(
    style = "padding: 0",
    value_box(
      class = c("text-light"),
      style = "--bs-primary-rgb: 72,77,109;",
      title = "SLA",
      value = uiOutput(ns("slaMissed")),
      showcase = bsicons::bs_icon("pie-chart-fill")
    )
  ))
  
  ## card_detail_table ----------------------------------------------------------
  
  card_detail_table <-
    card(
      card_header ("Ticket Details (Max. 10k rows)"),
      card_body(
        DT::DTOutput((
        ns("detailTable")
      ))),
      full_screen = TRUE
    )
  
  ## card_top_categories ---------------------------------------------------------
  
  card_top_categories <- card(card_header("TOP 10 Categories"),
                              card_body(echarts4rOutput((
                                ns("plot_donut_top_cats_echarts")
                              ))),
                              full_screen = TRUE)
  
  ## card_daily_open_tkts --------------------------------------------------------
  
  card_daily_open_tkts <- card(card_header ("Daily Tickets"),
                               card_body(echarts4rOutput((
                                 ns("plot_col_daily_echarts")
                               ))),
                               full_screen = TRUE)
  
  ## card_montly_sla ------------------------------------------------------
  
  card_montly_sla <- card(card_header ("Monthly SLA"),
                          card_body(echarts4rOutput((
                            ns("plot_col_sla_month_echarts")
                          ))),
                          full_screen = TRUE)
  
  
  # Main Page --------------------------------------------------------------------
  
  tagList(
    
    waiter::autoWaiter(html = waiter::spin_wave(), color = "#C5CAC3"),
    
    page_fluid(
      layout_column_wrap(
        width = NULL,
        style = css(grid_template_columns = "1fr 1fr 1fr 2fr"),
        height = 130,
        
        card_total,
        card_open,
        card_sla,
        card_summary
      ),
      
      p(),
      
      layout_column_wrap(
        width = NULL,
        height = 300,
        style = css(grid_template_columns = "2fr 2fr 2fr"),
        card_daily_open_tkts,
        card_top_categories,
        card_montly_sla
      ),
      
      p(),
      
      layout_column_wrap(
        width = NULL,
        height = 500,
        style = css(grid_template_columns = "2fr"),
        card_detail_table
      )
      
    )
    
  )
}

# SERVER ----------------------------------------------------------------------
mod_home_Server <- function(id, df, filtered_data) {
  moduleServer(id,
               function(input, output, session) {
## card_detail_table ------------------------------------------------------------
                 output$detailTable <- DT::renderDT({
                   
                   
                   info <- getCurrentOutputInfo()
                   
                   if (info$height() < 600) {
                     DT::datatable(filtered_data() |>
                                     slice_head(n = 10000) |>
                                     collect(),
                                   fillContainer = TRUE)
                   } else {
                     DT::datatable(
                       filtered_data() |>
                         slice_head(n = 10000) |>
                         collect(),
                       extensions = 'Buttons',
                       options = list(
                         paging = TRUE,
                         searching = TRUE,
                         ordering = TRUE,
                         lengthMenu = list(
                           c(10, 25, 50, 100, 10000),
                           c('10', '25', '50', '100', '10000')
                         ),
                         dom = 'lfrtipB',
                         buttons = c('copy', 'csv', 'excel')
                       ),
                       fillContainer = TRUE
                     )
                   }
                   
                 }, server = TRUE, future = FALSE)
                 
                 
## card_top_categories----------------------------------------------------------
                 
                 output$plot_donut_top_cats_echarts <-
                   renderEcharts4r({
                     data <-
                       filtered_data() |>
                       select(category) |>
                       count(category) |>
                       slice_head(n = 10) |>
                       arrange(desc(n)) |>
                       collect()
                     
                     e_charts(data, x = category) |>
                       e_pie(serie = n, radius = c("60%", "80%")) |>
                       e_legend(show = FALSE) |>
                       e_tooltip(trigger = "item")
                   })
                 
## daily_open_tickets ----------------------------------------------------------
                 
                 output$plot_col_daily_echarts <- renderEcharts4r({
                   data <- filtered_data() |>
                     group_by(day = as_date(opened_at)) |>
                     summarise(n = n()) |>
                     collect() |>  arrange(day)
                   
                   info <- getCurrentOutputInfo()
                   
                   if (info$height() < 600) {
                     e_charts(data, x = day) |>
                       e_line(serie = n,
                              name = "Tickets",
                              legend = FALSE) |>
                       e_tooltip(trigger = "axis") |>
                       e_grid(top = "8", containLabel = FALSE)  |>
                       e_datazoom(toolbox = FALSE)
                   }
                   else {
                     e_charts(data, x = day) |>
                       e_line(serie = n,
                              name = "Tickets",
                              legend = FALSE) |>
                       e_tooltip(trigger = "axis") |>
                       e_grid(top = "8", containLabel = FALSE) |>
                       e_datazoom()
                   }
                   
                 })
                 
## card_monthly_sla -------------------------------------------------------------
                 
                 output$plot_col_sla_month_echarts <-
                   renderEcharts4r({
                     data <- filtered_data() |>
                       filter(incident_state == "Closed") |>
                       select(opened_at, made_sla) |>
                       group_by(opened_at_year_month = format(opened_at, "%Y-%m"), made_sla) |>
                       summarise(n = n()) |>
                       group_by(opened_at_year_month) |>
                       collect() |>
                       mutate(perc = round(100 * (n / sum(n)), 1)) |>
                       collect() |>  arrange(opened_at_year_month)
                     
                     info <- getCurrentOutputInfo()
                     
                     if (info$height() < 600) {
                       e_charts(data |> group_by(made_sla), x = opened_at_year_month) |>
                         e_bar(serie = perc) |>
                         e_legend(show = TRUE, bottom = 5) |>
                         e_tooltip(trigger = "item") |>
                         e_grid(top = "20") |>
                         e_theme_custom('{"color":["#d2222d","#007000"]}') |>
                         e_labels()
                       
                     } else {
                       e_charts(data |> group_by(made_sla), x = opened_at_year_month) |>
                         e_bar(serie = perc) |>
                         e_legend(show = TRUE, bottom = 5) |>
                         e_tooltip(trigger = "item") |>
                         e_grid(top = "20") |>
                         e_theme_custom('{"color":["#d2222d","#007000"]}') |>
                         e_labels()
                     }
                   })
                 
## card_total ------------------------------------------------------------------
                 
                 output$totalTickets <- renderUI({
                   value <- filtered_data() |>
                     distinct(number) |>
                     count() |>
                     collect() |>
                     pull()
                   
                   h3(HTML(glue("<div> {value} Tickets<div>")))
                 })
                 
## card_open -------------------------------------------------------------------
                 output$openTickets <- renderUI({
                   value <- filtered_data() |>
                     filter(incident_state %in% c("New", "Active")) |>
                     distinct(number) |>
                     count() |>
                     collect() |>
                     pull()
                   
                   h3(HTML(glue("<div> {value} Tickets<div>")))
                 })
                 
                 
## card_sla --------------------------------------------------------------------
                 output$slaMissed <- renderUI({
                   value <- filtered_data() |>
                     filter (incident_state == "Closed") |>
                     group_by(made_sla) |>
                     summarise(n = n()) |>
                     collect() |>
                     mutate(perc = n / sum(n) * 100) |>
                     filter (made_sla == TRUE) |>
                     select(perc) |>
                     pull() |>
                     round(1)
                   
                   h3(HTML(glue("<div> {value} % <div>")))
                 })
                 
## card_summary ----------------------------------------------------------------
                 
                 output$summaryCard <- renderUI({
                   (HTML(
                     glue(
                       "<b>Total Records</b>: {df |> count()|> pull()} <br>
            <b>Min Open Date</b>: {df |> get_min_value(\"opened_at\") |> format(\"%Y-%m-%d\")} <br>
            <b>Max Resolved Date</b>: {df |> get_max_value(\"opened_at\") |> format(\"%Y-%m-%d\")}<br>
            <b>Total Assignment Groups</b>: {df |> distinct(assignment_group) |> count()|> collect()|> pull()} <br>"
                     )
                   ))
                 })
                 
               })
}

# UI --------------------------------------------------------------------------
mod_home_UI <- function(id) {
  
  ns <- NS(id)
  
  card1 <- card(card_body(
    value_box(
      class = c("text-dark"),
      style = "--bs-primary-rgb: 251,139,36;",
      title = "Total",
      value = uiOutput(ns("totalTickets")),
      showcase = bsicons::bs_icon("postcard-fill")
    )
  ))
  
  card2 <- card(card_body(
    value_box(
      class = c("text-light"),
      style = "--bs-primary-rgb: 112,168,132;",
      title = "Open",
      value = uiOutput(ns("openTickets")),
      showcase = bsicons::bs_icon("pip-fill")
    )
  ))
  
  card3 <- card(card_body(
    value_box(
      class = c("text-light"),
      style = "--bs-primary-rgb: 72,77,109;",
      title = "SLA",
      value = uiOutput(ns("slaMissed")),
      showcase = bsicons::bs_icon("pie-chart-fill")
    )
  ))
  
  
  card4 <- card(plotOutput((ns("plot_donut_sla_ggplot"))),
                full_screen = TRUE)
  
  card5 <- card(plotly::plotlyOutput((ns("plot_col_inc_state_ggplot"))),
                full_screen = TRUE)
  
  card6 <- card(card_header ("Ticket Details"),
                card_body(DT::DTOutput((ns(
                  "detailTable"
                )))),
                full_screen = TRUE
                )
  
  card7 <- card(echarts4r::echarts4rOutput((ns("plot_donut_sla_echarts"))),
                full_screen = TRUE)
  
  card8 <- card(card_header ("Monthly SLA"),
                card_body(
                  echarts4r::echarts4rOutput((ns("plot_col_sla_month_echarts")))
                  ),
                full_screen = TRUE)
  
  
  card9 <- card(card_header ("Monthly SLA"),
                card_body(plotly::plotlyOutput((
                  ns("plot_col_sla_month_plotly")
                ))),
                full_screen = TRUE)
  
  tagList(

    waiter::autoWaiter(c(ns("plot"))),

    waiter::waiterShowOnLoad(html = spin_fading_circles(), color = "black"),

    page_fluid(
      
      layout_column_wrap(
         width = NULL,
         style = css(grid_template_columns = "1fr 1fr 1fr"),
         height = 150,
         
        card1, card2, card3
        
       ),
      
      p(),
      
      layout_column_wrap(
        width = NULL,
        height = 300,
        style = css(grid_template_columns = "2fr 2fr 2fr"),
        card8, card7, card9
      ),
      
      p(),
      
      layout_column_wrap(
        width = NULL,
        height = 400,
        style = css(grid_template_columns = "2fr"),
        card6
      )
      
     )

  )
}

# SERVER --------------------------------------------------------------------------
mod_home_Server <- function(id, df, params) {
  moduleServer(
    id,
    function(input, output, session) {
      
      filtered_data <- reactive({
      
        data <- select(df, made_sla, opened_at, resolved_at, assignment_group, incident_state)
        
        if (is.na(params$opened_at()[1])) {data <- data}
        else{data <- filter(data, opened_at >= params$opened_at()[1])}

        if (is.na(params$opened_at()[2])) {data <- data}
        else{data <- filter(data, opened_at <= params$opened_at()[2])}

        if (is.na(params$resolved_at()[1])) {data <- data}
        else{data <- filter(data, resolved_at >= params$resolved_at()[1])}

        if (is.na(params$resolved_at()[2])) {data <- data}
        else{data <- filter(data, resolved_at <= params$resolved_at()[2])}

        if (is.null(params$assignment_group())) {data <- data}
        else{data <- filter(data,assignment_group %in% params$assignment_group())}

        if (is.null(params$incident_state())) {data <- data}
        else{data <- filter(data,incident_state %in% params$incident_state())}
        
        data <- data |> collect()       
      })
      
      output$detailTable <- DT::renderDT({
        DT::datatable(filtered_data(), fillContainer = TRUE)
      })

      output$plot_donut_sla_ggplot <- renderPlot({
        data <-
          get_total_table(
            select(
              df,
              made_sla,
              opened_at,
              resolved_at,
              assignment_group,
              incident_state
            ) |>
              filter(
                opened_at >= params$opened_at()[1],
                opened_at <= params$opened_at()[2],
                resolved_at >= params$resolved_at()[1],
                resolved_at <= params$resolved_at()[2],
                assignment_group %in% params$assignment_group(),
                incident_state %in% params$incident_state()
              )
          ) |> get_sla_missed() |>
          as_tibble() |>
          mutate(missed = -1 * (as.numeric(value) - 100)) |>
          mutate (value = as.numeric(value)) |>
          tidyr::pivot_longer(everything(), names_to = "category", values_to =
                                "count")
        
        data$fraction <- data$count / sum(data$count)
        data$ymax <- cumsum(data$fraction)
        data$ymin <- c(0, head(data$ymax, n = -1))
        data$labelPosition <- (data$ymax + data$ymin) / 2
        data$label <- paste0(data$category, "\n", data$count, " %")
        
        ggplot(data,
               aes(
                 ymax = ymax,
                 ymin = ymin,
                 xmax = 4,
                 xmin = 3,
                 fill = category
               )) +
          geom_rect() +
          geom_label(x = 3.5,
                     aes(y = labelPosition, label = label),
                     size = 6) +
          scale_fill_brewer(palette = 4) +
          coord_polar(theta = "y") +
          xlim(c(2, 4)) +
          theme_void() +
          theme(legend.position = "none")
      })
      
      output$plot_donut_sla_echarts <- echarts4r::renderEcharts4r({
        data <-
          filtered_data() |> get_sla_missed() |>
          as_tibble() |>
          mutate(missed = -1 * (as.numeric(value) - 100)) |>
          mutate (value = as.numeric(value)) |>
          tidyr::pivot_longer(everything(), names_to = "category", values_to =
                                "count")
        
          echarts4r::e_charts(data, x = category) |>
          echarts4r::e_pie(serie = count, radius = c("60%", "80%")) |>
            echarts4r::e_legend(show = FALSE)
      })
      
      output$plot_col_inc_state_ggplot <- plotly::renderPlotly({
        data <-
          get_total_table(
            select(
              df,
              made_sla,
              opened_at,
              resolved_at,
              assignment_group,
              incident_state
            ) |>
              filter(
                opened_at >= params$opened_at()[1],
                opened_at <= params$opened_at()[2],
                resolved_at >= params$resolved_at()[1],
                resolved_at <= params$resolved_at()[2],
                assignment_group %in% params$assignment_group()
                #incident_state %in% params$incident_state()
              )
          ) 
        
        p <- ggplot(data,
               aes(incident_state, fill = incident_state)) +
          geom_bar() +
          theme_void() 
        
        plotly::ggplotly(p) |> plotly::config(displayModeBar = F)
          
      })
      
      output$plot_col_sla_month_echarts <- echarts4r::renderEcharts4r({
          data <-
            filtered_data() |>  mutate(
              opened_at_year_month = glue::glue(
                "{lubridate::month(opened_at)}-{lubridate::year(opened_at)}"
              )
            ) |> select(made_sla, opened_at_year_month) |> 
            group_by(opened_at_year_month, made_sla) |> 
            count() |>
            mutate (opened_at_year_month = my(opened_at_year_month))
          
        echarts4r::e_charts(data |> group_by(made_sla), x = opened_at_year_month) |>
        echarts4r::e_bar(serie = n, label = list(normal = list(show = TRUE)) ) 
         
      })
      
      output$plot_col_sla_month_plotly <- plotly::renderPlotly({
        
        data <- filtered_data()
          
    
          data <- data |> mutate(
            opened_at_year_month = glue::glue(
              "{lubridate::month(opened_at)}-{lubridate::year(opened_at)}"
            )
          ) |> select(made_sla, opened_at_year_month) |> 
          group_by(opened_at_year_month, made_sla) |> 
          count() |>
          mutate (opened_at_year_month = my(opened_at_year_month))
        
          p <- ggplot (data |> group_by(made_sla), aes(opened_at_year_month, n, fill=made_sla)) +
            geom_col() +
            theme_minimal() +
            labs(y="SLA", x="Month-Year", fill = "SLA")
          
          plotly::ggplotly(p) |> plotly::config(displayModeBar = F)
        
      })
      
      output$totalTickets <- renderUI({


        value <- get_total_rows(select(filtered_data() , everything()))

        h3(HTML(
          glue("<div> {value} Tickets<div>")
        ))
      })

      output$openTickets <- renderUI({
        value <- get_total_rows(filtered_data())

        h3(HTML(
          glue("<div> {value} Tickets<div>")
        ))
      })
      
      output$slaMissed <- renderUI({

      value <- filtered_data() |> get_sla_missed()

        h3(HTML(
          glue("<div> {value} % <div>")
        ))
      })

      waiter::waiter_hide()
    }
  )
}

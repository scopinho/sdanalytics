# UI --------------------------------------------------------------------------
mod_groups_UI <- function(id, df) {
  
  ns <- NS(id)
  
  card1 <- card(
    class = c("text-light"),
    style = "background-color: rgb(112,000,132);",
    card_body(
      style = "padding-bottom: 0px;",
    # selectInput(
    #   inputId = ns("assignment_group"),
    #   label = strong("Assignment Group"),
    #   choices = get_unique_labels(df, "assignment_group"),
    #   selected = "",
    #   multiple = FALSE
    # ),
    card_body(
      style = "padding-bottom: 0px;",
      uiOutput(outputId = ns("group_summary")),
      br(),
      downloadButton(ns("group_report"), "Download Group Report...")
    )
     
    )
    
    )
  
  card3 <- card(card_header("Service Level"),
                card_body(echarts4rOutput((ns(
                  "group_sla"
                )))),
                full_screen = TRUE)
  
  
  card2 <- card(card_header ("Daily Open Tickets"),
                card_body(echarts4rOutput((
                  ns("plot_daily_opened_echarts")
                ))),
                full_screen = TRUE)
  
  card4 <- card(card_header ("Hours per Days of the Week"),
                card_body(echarts4rOutput((
                  ns("plot_hours_echarts")
                ))),
                full_screen = TRUE)
  
  card5 <- card(card_header ("Top Categories (Max. 10)"),
                card_body(echarts4rOutput((
                  ns("plot_top_categories_echarts")
                ))),
                full_screen = TRUE)
  
  card6 <- card(card_header ("Top Resolvers (Max. 10)"),
                card_body(echarts4rOutput((
                  ns("plot_top_resolvers_echarts")
                ))),
                full_screen = TRUE)
  card7 <- card(card_header ("Daily Resolved Tickets"),
                card_body(echarts4rOutput((
                  ns("plot_daily_resolved_echarts")
                ))),
                full_screen = TRUE)
  
  tagList(
    
    waiter::autoWaiter(),

    page_fluid(
      
      layout_column_wrap(
        width = NULL,
        style = css(grid_template_columns = "0.7fr 2.3fr 1.0fr"),
        height = 300,
        
        card1, card2, card3
      ),
      
      p(),
      
      layout_column_wrap(
        width = NULL,
        height = 300,
        style = css(grid_template_columns = "4fr 2fr 2fr"),
        card4, card5, card6
      ),
      
      p(),
      
      layout_column_wrap(
        width = NULL,
        height = 500,
        style = css(grid_template_columns = "2fr"),
        card7
      )
      
    )
    
  )
}

# SERVER --------------------------------------------------------------------------
mod_groups_Server <- function(id, df, params) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      filtered_data_groups <- reactive({
        
        req(params$assignment_group())

        data <- select(df, number, made_sla, opened_at, resolved_by, resolved_at, assignment_group, incident_state, category)

        if (is.na(params$opened_at()[1])) {data <- data}
        else{data <- filter(data, opened_at >= params$opened_at()[1])}

        if (is.na(params$opened_at()[2])) {data <- data}
        else{data <- filter(data, opened_at <= params$opened_at()[2])}

        if (is.na(params$resolved_at()[1])) {data <- data}
        else{data <- filter(data, resolved_at >= params$resolved_at()[1])}

        if (is.na(params$resolved_at()[2])) {data <- data}
        else{data <- filter(data, resolved_at <= params$resolved_at()[2])}

        if (is.null(params$assignment_group())) {data <- data}
        else{data <- filter(data,assignment_group %in% params$assignment_group()[1])}

        if (is.null(params$incident_state())) {data <- data}
        else{data <- filter(data,incident_state %in% params$incident_state())}

        data <- data

      })
      

      # filtered_data <- reactive(label = "Groups",{
      #   
      #   data <- select(df, everything())
      # 
      #   #if (is.null(params$assignment_group())) {data <- data}
      #   #else{data <- filter(data,assignment_group %in% params$assignment_group())}
      #      
      #    if (is.null(input$assignment_group)) {data <- data}
      #    else{data <- filter(data,assignment_group %in% input$assignment_group)}
      #   
      #   if (is.na(params$opened_at()[1])) {data <- data}
      #   else{data <- filter(data, opened_at >= params$opened_at()[1])}
      #   
      #   if (is.na(params$opened_at()[2])) {data <- data}
      #   else{data <- filter(data, opened_at <= params$opened_at()[2])}
      #   
      #   
      #   data <- data
      #   
      # })
      
      output$group_summary <- renderUI({
        
        group_resolvers <- filtered_data_groups()|> 
          select (resolved_by) |>
          distinct()|>
          count()|>
          collect()|>
          pull()
        
        tickets_received <- filtered_data_groups()|> 
          select (number) |>
          distinct()|>
          count()|>
          collect()|>
          pull()
        
        tickets_closed <- filtered_data_groups()|>
          select (number, incident_state) |>
          filter (incident_state %in% c("Resolved", "Closed")) |>
          distinct(number)|>
          count()|>
          collect()|>
          pull()
        
        group_resolvers <- glue("<b>Group</b>: {params$assignment_group()[1]}<br>
                                 <b>Group Resolvers</b>: {group_resolvers}<br>
                        <b>Tickets Received</b>: {tickets_received}<br>
                        <b>Tickets Closed</b>: {tickets_closed}")
        
        markdown(group_resolvers)
        
        
        
      })
      
      output$group_sla <- renderEcharts4r({
        
        sla <- filtered_data_groups()|> 
          select (number, made_sla) |>
          group_by(made_sla)|>
          summarise(value = n()) |>
          collect()|>
          mutate (perc = round(100*(value / sum(value)),1))|>
          filter (made_sla == TRUE)|> 
          select (perc) |>
          pull()
        
        e_charts() |>
          e_gauge(sla, "SLA", radius = "100%" )
      
      })
      
      output$plot_hours_echarts <- renderEcharts4r({
        
        data <- filtered_data_groups() |>
          select (number, opened_at) |>
          group_by(wday = wday(opened_at), hour = hour(opened_at))|>
          summarise(value = n()) |> 
          arrange (wday) |> 
          collect()|>
          mutate (wday = as.character(wday(wday, label = TRUE, abbr = TRUE)))

          e_charts(data|> group_by(wday), wday) |> 
            e_tooltip(trigger = "item")|>
            e_effect_scatter(hour, value) |> 
            e_y_axis(max = "24", formatter = "{value}:00") |>
            #e_visual_map(value) |>
            e_legend(FALSE) 
        
      })
      
      output$plot_daily_opened_echarts<- renderEcharts4r({

        data <-  filtered_data_groups() |>
          group_by(day = as_date(opened_at))|>
          summarise(n=n())|>
          collect() |> arrange(day)

        e_charts(data, x=day) |>
            e_bar(serie = n, name="Tickets", legend = FALSE) |>
            e_tooltip(trigger = "axis") |>
            e_grid(top = "8", containLabel=FALSE)  |>
            e_datazoom(toolbox=FALSE) |>
            e_toolbox_feature(feature = "dataView")
      })

      output$plot_top_categories_echarts<- renderEcharts4r({
        
        data <- filtered_data_groups() |>
          select (number, category) |>
          group_by(category)|>
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
      
      output$plot_top_resolvers_echarts<- renderEcharts4r({
        
        data <- filtered_data_groups() |>
          select (number, resolved_by) |>
          group_by(resolved_by)|>
          summarise(value = n()) |> 
          arrange (value) |> 
          head(10) |>
          collect()
        
        e_charts(data, resolved_by) |> 
          e_bar(value, legend = FALSE) |>
          e_flip_coords()|>
          e_grid(left = "12", containLabel=TRUE) |>
          
          # e_polar() |> 
          # e_angle_axis(category) |>
          # e_radius_axis() |> 
          # e_bar(value, coord_system = "polar") |> 
          e_tooltip(trigger = "item")
      })

      output$plot_daily_resolved_echarts<- renderEcharts4r({
        
        data <-  filtered_data_groups() |>
          group_by(day = as_date(resolved_at))|>
          summarise(n=n())|>
          collect() |> arrange(day)
        
        e_charts(data, x=day) |>
          e_line(serie = n, name="Tickets", legend = FALSE) |>
          e_tooltip(trigger = "axis") |>
          e_grid(top = "8", containLabel=FALSE)  |>
          e_datazoom(toolbox=FALSE) |>
          e_toolbox_feature(feature = "dataView")
      })
      
      output$group_report <- downloadHandler(
        filename = function(){
          paste0("grupo_report_", params$assignment_group()[1], ".pdf")}
        ,
        content = function (file = paste0("grupo_report_", params$assignment_group()[1], ".pdf")) {
          rmarkdown::render(
            input = "www/template.Rmd",
            output_file = file,
            params= list(group = params$assignment_group()[1])
          )
      }
      )
 
    }
  )
}

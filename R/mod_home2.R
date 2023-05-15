# UI --------------------------------------------------------------------------
mod_home2_UI <- function(id) {
  
  
  
  
  ns <- NS(id)
  
  card1 <- card(
    card_header("Scrolling content"),
    lapply(
      lorem::ipsum(paragraphs = 3, sentences = c(5, 5, 5)),
      tags$p
    )
  )
  card2 <- card(
    card_header("Nothing much here"),
    card_body(
      plotOutput(ns("slaPlot"))
    )
  )
  card3 <- card(
    full_screen = TRUE,
    card_header("Filling content"),
    card_body(
      class = "p-0",
      shiny::plotOutput("p")
    )
  )
  
  tagList(
    
    # tags$head(includeCSS("./www/styles.css")),
    
    # waiter::autoWaiter(c(ns("plot"))),
    
    # waiter::waiterShowOnLoad(html = spin_fading_circles(), color = "black"),
    
    
    
    
    page_fluid(
      
            
      layout_column_wrap(
        width = 1/2, height = 300, 
        card1, card2, card3
      ) 
      
  )
  )
}

# SERVER --------------------------------------------------------------------------
mod_home2_Server <- function(id, df, params, params2) {
  moduleServer(
    id,
    function(input, output, session) {
      #
      #       get_SLA_missed <- reactive({
      #
      #         total <- df |>
      #           dplyr::filter (.data[["opened_at"]] >= params$opened_at()[1],
      #                          .data[["opened_at"]] <= params$opened_at()[2],
      #                          .data[["resolved_at"]] >= params$resolved_at()[1],
      #                          .data[["resolved_at"]] <= params$resolved_at()[2],
      #                          .data[["assignment_group"]] %in% params$assignment_group(),
      #                          .data[["incident_state"]] %in% params$incident_state()
      #           ) |>
      #           dplyr::collect() |>
      #           dplyr::group_by(.data[["made_sla"]]) |>
      #           dplyr::summarise(n=n()) |>
      #           dplyr::mutate(perc = n/sum(n)) |>
      #           dplyr::filter(.data[["made_sla"]] == TRUE) |> dplyr::select(.data[["perc"]]) |> dplyr::pull() |> round(digits=2)
      #
      #         return (total)
      #       })
      
      # output$plot <- renderPlot({
      #
      #
      #
      #   # data <- df |> dplyr::filter(.data[["opened_at"]] >= params$opened_at()[1],
      #   #                                    .data[["opened_at"]] <= params$opened_at()[2],
      #   #                                    .data[["resolved_at"]] >= params$resolved_at()[1],
      #   #                                    .data[["resolved_at"]] <= params$resolved_at()[2],
      #   #                                    .data[["assignment_group"]] %in% params$assignment_group(),
      #   #                                    .data[["incident_state"]] %in% params$incident_state())  |>
      #   #   dplyr::slice_head() |>
      #   #   dplyr::collect()
      #
      #
      #     ggplot(df |> collect(), aes(x = .data[["opened_at"]],
      #                        y = .data[["incident_state"]],
      #                        color = as_factor(.data[["incident_state"]]),
      #                        shape = as_factor(.data[["incident_state"]])))
      #   # +
      #   #
      #   #   geom_point(size=3, show.legend = FALSE) +
      #   #   geom_smooth(show.legend = FALSE, method="lm", aes(fill=as.factor(.data[["incident_state"]])))  +
      #   #   geom_point(size=3, show.legend = FALSE) +
      #   #   labs(color = "incident_state")
      #
      #
      #
      # })
      
      
      output$detailTable <- DT::renderDT({
        get_total_table(select(df, made_sla, opened_at, resolved_at, assignment_group, incident_state) |>
                          filter(
                            made_sla == params$made_sla(),
                            opened_at >= params$opened_at()[1],
                            opened_at <= params$opened_at()[2],
                            resolved_at >= params$resolved_at()[1],
                            resolved_at <= params$resolved_at()[2],
                            assignment_group %in% params$assignment_group(),
                            incident_state %in% params$incident_state()
                          ))
      })
      
      output$slaPlot <- renderPlot({
        # get_total_table(select(df, made_sla, opened_at, resolved_at, assignment_group, incident_state) |>
        #                   filter(
        #                     made_sla == params$made_sla(),
        #                     opened_at >= params$opened_at()[1],
        #                     opened_at <= params$opened_at()[2],
        #                     resolved_at >= params$resolved_at()[1],
        #                     resolved_at <= params$resolved_at()[2],
        #                     assignment_group %in% params$assignment_group(),
        #                     incident_state %in% params$incident_state()
        #                   ))
        
        ggplot(mtcars, (aes(cyl, hp))) + geom_col()
      })
      
      
      
      #         # data <- get_total_table(select(df, made_sla, opened_at, resolved_at, assignment_group, incident_state) |>
      #         #   filter(
      #         #     opened_at >= params$opened_at()[1],
      #         #     opened_at <= params$opened_at()[2],
      #         #     resolved_at >= params$resolved_at()[1],
      #         #     resolved_at <= params$resolved_at()[2],
      #         #     assignment_group %in% params$assignment_group(),
      #         #     incident_state %in% params$incident_state() 
      #         #   )) |> get_sla_missed() |>
      #         #          as_tibble()|>
      #         #          mutate(missed = -1*(as.numeric(value) - 100)) |> 
      #         #          mutate (value = as.numeric(value)) |>
      #         #          tidyr::pivot_longer(everything(), names_to = "category", values_to="count")
      
      
      
      # # data$fraction <- data$count / sum(data$count)
      
      # # # Compute the cumulative percentages (top of each rectangle)
      # # data$ymax <- cumsum(data$fraction)
      
      # # # Compute the bottom of each rectangle
      # # data$ymin <- c(0, head(data$ymax, n=-1))
      
      # # # Compute label position
      # # data$labelPosition <- (data$ymax + data$ymin) / 2
      
      # # # Compute a good label
      
      # # data$label <- paste0(data$category, "\n", data$count, " %")
      
      # # # Make the plot
      # # 
      # # # ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
      # # #   geom_rect() +
      # # #   geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 6) +
      # # #   scale_fill_brewer(palette = 4) +
      # # #   coord_polar(theta = "y") +
      # # #   xlim(c(2, 4)) +
      # # #   theme_void() +
      # # #   theme(legend.position = "none")
      
      
      
      
      output$totalTickets <- renderUI({
        # date <- as.Date(params$opened_at()[1])
        # filter <- glue("opened_at >= as.Date(\"2017-01-01\")")
        
        
        value <- get_total_rows(select(df, everything()) |>
                                  filter(
                                    made_sla == params$made_sla(),
                                    opened_at >= params$opened_at()[1],
                                    opened_at <= params$opened_at()[2],
                                    resolved_at >= params$resolved_at()[1],
                                    resolved_at <= params$resolved_at()[2],
                                    assignment_group %in% params$assignment_group(),
                                    incident_state %in% params$incident_state()
                                  ))
        
        h3(HTML(
          glue(" {value} Tickets")
        ))
      })
      
      output$openTickets <- renderUI({
        value <- get_total_rows(select(df, everything()) |>
                                  filter(
                                    made_sla == params$made_sla(),
                                    opened_at >= params$opened_at()[1],
                                    opened_at <= params$opened_at()[2],
                                    resolved_at >= params$resolved_at()[1],
                                    resolved_at <= params$resolved_at()[2],
                                    assignment_group %in% params$assignment_group(),
                                    incident_state %in% c("Active", "New")
                                  ))
        
        h3(HTML(
          glue(" {value} Tickets")
        ))
      })
      
      output$slaMissed <- renderUI({
        
        
        value <- get_total_table(select(df, made_sla, opened_at, resolved_at, assignment_group, incident_state) |>
                                   filter(
                                     opened_at >= params$opened_at()[1],
                                     opened_at <= params$opened_at()[2],
                                     resolved_at >= params$resolved_at()[1],
                                     resolved_at <= params$resolved_at()[2],
                                     assignment_group %in% params$assignment_group(),
                                     incident_state %in% params$incident_state()
                                   )
        )  |> get_sla_missed()
        
        h3(HTML(
          glue(" {value} %")
        ))
      })
      
      # waiter::waiter_hide()
    }
  )
}

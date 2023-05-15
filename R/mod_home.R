# UI --------------------------------------------------------------------------
mod_home_UI <- function(id) {
  
  

  
  ns <- NS(id)
  tagList(

    # tags$head(includeCSS("./www/styles.css")),

    # waiter::autoWaiter(c(ns("plot"))),

    # waiter::waiterShowOnLoad(html = spin_fading_circles(), color = "black"),
    
 
    
    page_fluid(

      gridlayout::grid_container(
        layout = c(
          "header header header",
          "main3 main3 main3",
          "plot plot plot"
        ),
        row_sizes = c(
          "200px",
          "300px",
          "400px"
        ),
        col_sizes = c(
          "250px",
          "0.59fr",
          "1.41fr"
        ),
        gap_size = "1rem",
        grid_card(
          area = "header",
          card_body(
            grid_container(
              layout = c(
                "val01 val02 val03"
              ),
              row_sizes = c(
                "4fr"
              ),
              col_sizes = c(
                "1fr",
                "1fr",
                "1fr"
              ),
              gap_size = "10px",
              grid_card(
                area = "val01",
                card_body(
                  style = "padding: 0;",
                  value_box(
                    class = c("text-dark"),
                    style = "--bs-primary-rgb: 251,139,36;",
                    title = "Total",
                    value = uiOutput(ns("totalTickets")),
                    showcase = bsicons::bs_icon("piggy-bank")
                  )
                )
              ),
              grid_card(
                area = "val02",
                card_body(
                  style = "padding: 0",
                  value_box(
                    class = c("text-light"),
                    style = "--bs-primary-rgb: 112,168,132;",
                    title = "Open",
                    value = uiOutput(ns("openTickets")),
                    showcase = bsicons::bs_icon("piggy-bank")
                  )
                )
              ),
              grid_card(
                area = "val03",
                card_body(
                  style = "padding: 0",
                  value_box(
                    class = c("text-light"),
                    style = "--bs-primary-rgb: 72,77,109;",
                    title = "SLA",
                    value = uiOutput(ns("slaMissed")),
                    showcase = bsicons::bs_icon("piggy-bank")
                  )
                )
              )
            )
          )
        ),
        
        
        grid_card(
          area = "main3",
          DT::DTOutput((ns("detailTable"))),
          full_screen = TRUE
        ),
        
        grid_card(
          area = "plot",
          plotOutput((ns("slaPlot"))),
          full_screen = TRUE
        )
      
      )
    )
  )
}

# SERVER --------------------------------------------------------------------------
mod_home_Server <- function(id, df, params, params2) {
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
        
                data <- get_total_table(select(df, made_sla, opened_at, resolved_at, assignment_group, incident_state) |>
                  filter(
                    opened_at >= params$opened_at()[1],
                    opened_at <= params$opened_at()[2],
                    resolved_at >= params$resolved_at()[1],
                    resolved_at <= params$resolved_at()[2],
                    assignment_group %in% params$assignment_group(),
                    incident_state %in% params$incident_state()
                  )) |> get_sla_missed() |>
                         as_tibble()|>
                         mutate(missed = -1*(as.numeric(value) - 100)) |>
                         mutate (value = as.numeric(value)) |>
                         tidyr::pivot_longer(everything(), names_to = "category", values_to="count")
        
        
        
        data$fraction <- data$count / sum(data$count)

       data$ymax <- cumsum(data$fraction)

      data$ymin <- c(0, head(data$ymax, n=-1))

        data$labelPosition <- (data$ymax + data$ymin) / 2


        data$label <- paste0(data$category, "\n", data$count, " %")


         ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
           geom_rect() +
           geom_label(x = 3.5, aes(y = labelPosition, label = label), size = 6) +
           scale_fill_brewer(palette = 4) +
           coord_polar(theta = "y") +
           xlim(c(2, 4)) +
           theme_void() +
           theme(legend.position = "none")
      })

 

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





# UI --------------------------------------------------------------------------
mod_home_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    #tags$head(includeCSS("./www/styles.css")),
    
    waiter::autoWaiter(c(ns("plot"))),
    
    waiter::waiterShowOnLoad(html = spin_fading_circles(), color = "black"),
    
  page_fluid(
  
  # tags$style(HTML(".bslib-sidebar-layout>.main {
  #   padding: 0;
  #   
  #   .container-fluid {
  #   padding-left: calc(var(--bs-gutter-x)*.1);
  #   }")),
  
grid_container(
  
  
  layout = c(
    "header header header",
    "main main main3"
  ),
  row_sizes = c(
    "200px",
    "300px"
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
                  value = uiOutput(ns("valueBox1")),
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
              title = "Resolvers group",
              value = uiOutput(ns("valueBox2")),
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
              title = "Open Tickets",
              value = uiOutput(ns("valueBox3")),
              showcase = bsicons::bs_icon("piggy-bank")
            )
          )
        )
      )
    )
  ),
  
  grid_card(
    area = "main",
    plotOutput(ns("plot")),
    full_screen = TRUE

    
  ),
  
  grid_card(
    area = "main3",
    DT::DTOutput((ns("table1"))),
    full_screen = TRUE
  )
)
)
)
}

# SERVER --------------------------------------------------------------------------
mod_home_Server <- function(id, params) {
  moduleServer(
    id,
    function(input, output, session) {
      
      get_total <- reactive({

        total <- df |>
          dplyr::filter (.data[["made_sla"]] == params$made_sla(),
                         .data[["opened_at"]] >= params$opened_at()[1],
                         .data[["opened_at"]] <= params$opened_at()[2],
                         .data[["resolved_at"]] >= params$resolved_at()[1],
                         .data[["resolved_at"]] <= params$resolved_at()[2],
                         .data[["assignment_group"]] %in% params$assignment_group(),
                         .data[["incident_state"]] %in% params$incident_state()
                         ) |>
          dplyr::count() |>
          dplyr::collect() |>
          dplyr::pull()

        return (total)
      })
      
      get_total_table <- reactive({
        
        total <- df |>
          dplyr::filter (.data[["made_sla"]] == params$made_sla(),
                         .data[["opened_at"]] >= params$opened_at()[1],
                         .data[["opened_at"]] <= params$opened_at()[2],
                         .data[["resolved_at"]] >= params$resolved_at()[1],
                         .data[["resolved_at"]] <= params$resolved_at()[2],
                         .data[["assignment_group"]] %in% params$assignment_group(),
                         .data[["incident_state"]] %in% params$incident_state()
          ) |>
          dplyr::collect() 
        
        return (total)
      })

      get_total_open <- reactive({

        total <- df |>
          dplyr::filter (.data[["made_sla"]] == params$made_sla(),
                         .data[["opened_at"]] >= params$opened_at()[1],
                         .data[["opened_at"]] <= params$opened_at()[2],
                         .data[["resolved_at"]] >= params$resolved_at()[1],
                         .data[["resolved_at"]] <= params$resolved_at()[2],
                         .data[["assignment_group"]] %in% params$assignment_group(),
                         .data[["incident_state"]] %in% c("New", "Active")

          ) |>
          dplyr::count() |>
          dplyr::collect() |>
          dplyr::pull()

        return (total)
      })

      get_SLA_missed <- reactive({

        total <- df |>
          dplyr::filter (.data[["opened_at"]] >= params$opened_at()[1],
                         .data[["opened_at"]] <= params$opened_at()[2],
                         .data[["resolved_at"]] >= params$resolved_at()[1],
                         .data[["resolved_at"]] <= params$resolved_at()[2],
                         .data[["assignment_group"]] %in% params$assignment_group(),
                         .data[["incident_state"]] %in% params$incident_state()
          ) |>
          dplyr::collect() |>
          dplyr::group_by(.data[["made_sla"]]) |>
          dplyr::summarise(n=n()) |>
          dplyr::mutate(perc = n/sum(n)) |>
          dplyr::filter(.data[["made_sla"]] == TRUE) |> dplyr::select(perc) |> dplyr::pull() |> round(digits=2)

        return (total)
      })

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
      
      
      output$table1 <- DT::renderDT({
        
        get_total_table()

      })
      
      output$valueBox1<- renderUI({
        
        h3(HTML(
          glue(" {get_total()} Tickets"))
          )
        })

      output$valueBox2 <- renderUI({

        h3(HTML(
          glue("SLA: {get_SLA_missed()}%"))
        )
      })

      output$valueBox3 <- renderUI({
        h3(HTML(
          glue("{get_total_open()}"))
        )
      })

      waiter::waiter_hide()  
    }
  )
}


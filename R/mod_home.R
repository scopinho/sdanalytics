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
    plotOutput(ns("plot2")),
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
        
        total <- params$df |> 
          dplyr::filter (.data[["made_sla"]] == params$made_sla(),
                         .data[["opened_at"]] >= params$opened_at()[1],
                         .data[["opened_at"]] <= params$opened_at()[2],
                         .data[["resolved_at"]] >= params$resolved_at()[1],
                         .data[["resolved_at"]] <= params$resolved_at()[2],
                         .data[["assignment_group"]] %in% params$assignment_group(),
                         .data[["incident_state"]] %in% params$incident_state()
                         ) |>
          count() |> 
          collect() |> 
          pull() 
        
        return (total)
      })
      
      get_total_open <- reactive({
        
        total <- params$df |> 
          dplyr::filter (.data[["made_sla"]] == params$made_sla(),
                         .data[["opened_at"]] >= params$opened_at()[1],
                         .data[["opened_at"]] <= params$opened_at()[2],
                         .data[["resolved_at"]] >= params$resolved_at()[1],
                         .data[["resolved_at"]] <= params$resolved_at()[2],
                         .data[["assignment_group"]] %in% params$assignment_group(),
                         .data[["incident_state"]] %in% c("New", "Active")
                         
          ) |>
          count() |> 
          collect() |> 
          pull() 
        
        return (total)
      })
      
      get_SLA_missed <- reactive({
        
        total <- params$df |> 
          dplyr::filter (.data[["opened_at"]] >= params$opened_at()[1],
                         .data[["opened_at"]] <= params$opened_at()[2],
                         .data[["resolved_at"]] >= params$resolved_at()[1],
                         .data[["resolved_at"]] <= params$resolved_at()[2],
                         .data[["assignment_group"]] %in% params$assignment_group(),
                         .data[["incident_state"]] %in% params$incident_state()
          ) |>
          collect() |> 
          group_by(.data[["made_sla"]]) |> 
          summarise(n=n()) |> 
          mutate(perc = n/sum(n)) |> 
          filter(.data[["made_sla"]] == TRUE) |> select(perc) |> pull() |> round(digits=2)
        
        return (total)
      })

      # output$plot <- renderPlot({
      #   
      # 
      #   
      #   data <- mtcars |> dplyr::filter(.data[["cyl"]] == params$cyl())
      #   
      #     
      #     ggplot(data, aes(x = .data[["hp"]], 
      #                        y = .data[["mpg"]], 
      #                        color = as_factor(.data[["cyl"]]), 
      #                        shape = as_factor(.data[["cyl"]]))) +
      # 
      #     geom_point(size=3, show.legend = FALSE) +
      #     geom_smooth(show.legend = FALSE, method="lm", aes(fill=as.factor(.data[["cyl"]])))  +
      #     geom_point(size=3, show.legend = FALSE) +
      #     labs(color = "cyl")
      #     
      #     
      # 
      # })
      
      
      # output$plot2 <- renderPlot({
      #   
      #   data <- mtcars |> dplyr::filter(.data[["cyl"]] == params$cyl())
      #   
      #   
      #   ggplot(data, aes(x = .data[["hp"]], 
      #                    y = .data[["mpg"]],
      #                    color = as_factor(.data[["cyl"]]), 
      #                    shape = as_factor(.data[["cyl"]]))) +
      #     geom_point(size=3, show.legend = FALSE) +
      #     scale_color_discrete(labels = params$cyl()) +
      #     labs(color = "cyl")
      #     
      # 
      # })
      
      output$valueBox1<- renderUI({
        h3(HTML(
          glue::glue("{get_total()} Tickets"))
          )
        })

      output$valueBox2 <- renderUI({

        h3(HTML(
          glue::glue("SLA: {get_SLA_missed()}%"))
        )
      })

      output$valueBox3 <- renderUI({
        h3(HTML(
          glue::glue("{get_total_open()}"))
        )
      })

      waiter::waiter_hide()  
    }
  )
}



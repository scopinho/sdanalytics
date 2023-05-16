#' @export openUI


# Main -------------------------------------------------------

openUI <- function(...) {
  
  df <- get_db()
  
  accordion_panel1 <- accordion_panel(
    " Dates",
    dateRangeInput(
      inputId = "opened_at",
      label = strong("Opened Date"),
      start = NULL, #get_min_value(df, "opened_at") |> format("%Y-%m-%d %H:%M"),
      end = NULL, #get_max_value(df, "opened_at") |> format("%Y-%m-%d %H:%M"),
      min = get_min_value(df, "opened_at") |> format("%Y-%m-%d %H:%M"),
      max = get_max_value(df, "opened_at") |> format("%Y-%m-%d %H:%M")
    ),
    dateRangeInput(
      inputId = "resolved_at",
      label = strong("Resolved Date"),
      start = NULL, #get_min_value(df, "resolved_at") |> format("%Y-%m-%d %H:%M"),
      end = NULL, #get_max_value(df, "resolved_at") |> format("%Y-%m-%d %H:%M"),
      min = get_min_value(df, "resolved_at") |> format("%Y-%m-%d %H:%M"),
      max = get_max_value(df, "resolved_at") |> format("%Y-%m-%d %H:%M")
    )
    ,icon = bsicons::bs_icon("calendar-week")
  )
  
  accordion_panel2 <- accordion_panel(
    " Values",
    icon = bsicons::bs_icon("blockquote-right"),
    selectizeInput(
      inputId = "assignment_group",
      label = strong("Assignment Group"),
      choices = get_unique_labels(df, "assignment_group"),
      selected = NULL, #get_max_value(df, "assignment_group"),
      multiple = TRUE
      
      #,options = list(placeholder = "All")
    ),
  
    selectizeInput(
      inputId = "incident_state",
      label = strong("Incident State"),
      choices = get_unique_labels(df, "incident_state"),
      selected = get_unique_labels(df, "incident_state"),
      multiple = TRUE
    )
  )
  
  accordion_panel3 <- accordion_panel(
    " Numbers",
    icon = bsicons::bs_icon("calculator"),
    radioButtons(
      inputId = "made_sla",
      label = "Made SLA?",
      choices = c("TRUE", "FALSE")
    )
  )
  

  ## UI ------------------------------------------------------
  ui <- page_navbar(
    
    tags$head(includeCSS("./www/styles.css")),

    nav(
      title = "Home",
      #actionButton("debug", "debug"),
      mod_home_UI("home")
    ),
    nav(
      title = "Details"
    ),
    
    title = "SD Analytics",

    sidebar = sidebar(
      accordion(
        
      accordion_panel1
      ,
      accordion_panel2
      ,
      accordion_panel3
    )
      ,

      title = "Global Filters"
    ),

    theme = bs_theme(version = 5)
  )

  ## Server -------------------------------------------------------

  # Data Sources --------------------------------------------------


  server <- function(input, output, session) {
    observeEvent(input$debug, {
      browser()
    })


    params <- reactiveValues()



    # filters <- reactive({
    #   reactiveValuesToList(input)
    #   })

    # params2 <- reactive({
    #
    #   list <- filters()[stringr::str_detect(names(filters()),"opened_at|resolved_at|assignment_group|incident_state|made_sla|reopen_count")]
    #   #print(glue::glue("inside params2!: {list[1]}"))
    #
    #   for (i in seq_along(list)) {
    #     print(list[i])
    #     #params[[names(list())[i]]] <- list()[[i]]
    #     }
    #
    # })
 
    # observe({
    #   params2()
    #   #print(paste0("params2 observed!", params2()))
    # })

    # for (i in length(params2())) { params[[names(params2())[i]]] <- params2()[[i]]}

    params$opened_at <- reactive({
      input$opened_at
    })
    params$resolved_at <- reactive({
      input$resolved_at
    })
    params$assignment_group <- reactive({
      input$assignment_group
    })
    params$incident_state <- reactive({
      input$incident_state
    })
    params$made_sla <- reactive({
      input$made_sla
    })
    params$reopen_count <- reactive({
      input$reopen_count
    })
    
    mod_home_Server("home", df, params)
  }

  shinyApp(ui, server, ...)
}

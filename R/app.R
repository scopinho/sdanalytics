#' @export openUI


# Main -------------------------------------------------------



openUI <- function(...)  {
  
  df <- get_db()

  ## UI ------------------------------------------------------ 
  ui <- page_navbar(
    

    
    
    nav(
      title = "Home",
      actionButton("debug", "debug"),
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
  
  # Data Sources --------------------------------------------------


  server <- function(input, output, session) {
    
    observeEvent(input$debug, {
      browser()
    })
    
    
    params <- reactiveValues()
    #params2 <- reactiveValues()

    
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
    
    #for (i in length(params2())) { params[[names(params2())[i]]] <- params2()[[i]]}
    
    params$opened_at <- reactive({
      req(input$opened_at)
      input$opened_at
      })
    params$resolved_at <- reactive({
      req(input$resolved_at)
      input$resolved_at
      })
    params$assignment_group <- reactive({
      req(input$assignment_group)
      input$assignment_group
      })
    params$incident_state <- reactive({
      req(input$incident_state)
      input$incident_state
      })
    params$made_sla <- reactive({
	    req(input$made_sla)
	    input$made_sla
    })
    params$reopen_count <- reactive({
      req(input$reopen_count)
      input$reopen_count
      })

    params2 <- reactive({
    	input$made_sla
	input$opened_at[1]
	input$opened_at[2]
    })


    mod_home_Server("home", df, params, params2)
    
  }
  
  shinyApp(ui, server, ...)
}

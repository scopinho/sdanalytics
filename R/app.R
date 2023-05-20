#' @export openUI


# Main -------------------------------------------------------

openUI <- function(...) {
  
  argsv <- list(...)
  
  dataset_incidents <- get_dataset(path = NULL)
  
  df <- semi_join(dataset_incidents, dataset_incidents |> 
                group_by(number) |> 
                summarise(sys_updated_at = max(sys_updated_at)))
  
  #df <- get_db(path = "/home/scopinho/github/sdanalytics/inst/extdata/part-0-big.parquet")
  
  accordion_panel1 <- accordion_panel(
    " Dates",
    dateRangeInput(
      inputId = "opened_at",
      label = strong("Opened Date"),
      start = as_datetime(as.Date(get_max_value(df, "opened_at")) - days(90)) |> format("%Y-%m-%d %H:%M"),
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
      selected = "Group 70", #get_max_value(df, "assignment_group"),
      multiple = TRUE
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
  ui <- page_fluid( 
    
    includeCSS(system.file("www/styles.css", package = "sdanalytics")),

    page_navbar(
      
    nav(
      title = "Home",
      #actionButton("debug", "debug"),
      mod_home_UI("home")
    ),
    
    nav_menu("Assingment Group",
             nav("Group Details",
                 mod_groups_UI("groups", df)
                 ),
             nav("Groups Analysis")
             ),
    
    nav(
      title = "Help"
    )
    
    ,
    
    title = "SD Analytics",

    sidebar = sidebar(id = "sidebar_main",
      accordion(
        multiple = TRUE,
        open = c(" Dates", " Values"),
        
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
  )

  ## Server -------------------------------------------------------

  # Data Sources --------------------------------------------------


  server <- function(input, output, session) {
    observeEvent(input$debug, {
      #sidebar_toggle(id = "sidebar_main")
      #browser()
    })
    
    #bs_themer()
    #bs_theme_update(theme, bootswatch = "default")

    
    params <- reactiveValues()

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
    
    
    
    filtered_data <- reactive(label = "Global_Filters",{
      
      #data <- select(df, number, made_sla, opened_at, resolved_at, assignment_group, incident_state, category)
      data <- select(df, everything())
      
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
      
      data <- data

    })
    
    mod_home_Server("home", df, filtered_data)
    mod_groups_Server("groups", df, params)
  }

  shinyApp(ui, server, ...)
}

# UI --------------------------------------------------------------------------
mod_groups_analysis_UI <- function(id, df) {
  
  ns <- NS(id)
  
  tagList(

    page_fluid(
      
      layout_column_wrap(
        width = NULL,
        style = css(grid_template_columns = "1fr 1fr 1fr 2fr"),
        height = 130,
        
        card("..."), card("..."), card("..."), card("...")
      ),
      
      p(),
      
      layout_column_wrap(
        width = NULL,
        height = 300,
        style = css(grid_template_columns = "2fr 2fr 2fr"),
        card("..."), card("..."), card("...")
      )
    )
    
  )
}

# SERVER --------------------------------------------------------------------------
mod_groups_analysis_Server <- function(id, df, filtered_data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      
    }
  )
}

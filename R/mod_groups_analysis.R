Sys.setenv(TOKENIZERS_PARALLELISM="false")

transformers <- reticulate::import("transformers")
# 
# # Instantiate a pipeline

classifier <- transformers$pipeline(task = "text-classification", model = "distilbert-base-uncased-finetuned-sst-2-english")

get_predict <- function(text){
  outputs <- classifier(text)
  outputs |>
    purrr::pluck(1) |> unlist()
  #|> 
  #    tibble::as_tibble()
}


# UI --------------------------------------------------------------------------
mod_groups_analysis_UI <- function(id, df) {
  
  
  
  ns <- NS(id)

  
  card1 <- card(card_header(
    "Text"),
    card_body(
      textAreaInput(ns("text_input"), label = NULL, height = 200),
      actionButton(ns("btn_submit"), label = "Submit")
    )
  )  

  
  card2 <- card(card_header(
    "Sentiment Analysis"),
    card_body(
      textOutput(ns("text_output"))
    )
  ) 
  
  
  tagList(

    page_fluid(
      
      layout_column_wrap(
        width = NULL,
        style = css(grid_template_columns = "1fr 1fr"),
        height = 400,
        
        card1, card2
      )
      # ,
      # 
      # p(),
      # 
      # layout_column_wrap(
      #   width = NULL,
      #   height = 300,
      #   style = css(grid_template_columns = "2fr 2fr 2fr"),
      #   card("..."), card("..."), card("...")
      # )
    )
    
  )
}

# SERVER --------------------------------------------------------------------------
mod_groups_analysis_Server <- function(id, df, filtered_data) {
  moduleServer(
    id,
    function(input, output, session) {
      
    btn_clicked <- eventReactive(input$btn_submit, {
      get_predict(input$text_input)
      
    })  
      output$text_output <- renderText({
        btn_clicked()
      })
     
      
    }
  )
}

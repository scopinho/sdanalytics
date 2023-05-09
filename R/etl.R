# loadDataset <- function(source_zip_file, ...){
#   
#   source_zip_file <- "/mnt/d/big_data/Service_Desk_Demo/incident_event_log.zip"
#   df_vroom <- vroom::vroom(source_zip_file)
#   
#   ### Cols adjustments
#   
#   df_vroom$opened_at <- lubridate::dmy_hm(df_vroom$opened_at)
#   df_vroom$sys_created_at <- lubridate::dmy_hm(df_vroom$sys_created_at)
#   df_vroom$sys_updated_at <- lubridate::dmy_hm(df_vroom$sys_updated_at)
#   df_vroom$resolved_at <- lubridate::dmy_hm(df_vroom$resolved_at)
#   df_vroom$closed_at <- lubridate::dmy_hm(df_vroom$closed_at)
#   
#   df_vroom$active <- as.logical(df_vroom$active)
#   df_vroom$made_sla <- as.logical(df_vroom$made_sla)
#   df_vroom$knowledge <- as.logical(df_vroom$knowledge)
#   df_vroom$u_priority_confirmation <- as.logical(df_vroom$u_priority_confirmation)
# 
#     system.file("extdata", ".", package = "sdanalytics")
#         arrow::write_dataset(df_vroom,
#           system.file("extdata", "", package = "sdanalytics"),
#           format = "parquet")
# }
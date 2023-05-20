# #
# #   ### Load source zip file
# 
#    source_zip_file <- "/mnt/d/big_data/Service_Desk_Demo/incident_event_log.zip"
#    df_vroom <- vroom::vroom(source_zip_file)
# #
# #   ### Cols adjustments
# #
#    df_vroom$opened_at <- lubridate::dmy_hm(df_vroom$opened_at)
#    df_vroom$sys_created_at <- lubridate::dmy_hm(df_vroom$sys_created_at)
#    df_vroom$sys_updated_at <- lubridate::dmy_hm(df_vroom$sys_updated_at)
#    df_vroom$resolved_at <- lubridate::dmy_hm(df_vroom$resolved_at)
#    df_vroom$closed_at <- lubridate::dmy_hm(df_vroom$closed_at)
# 
#    df_vroom$active <- as.logical(df_vroom$active)
#    df_vroom$made_sla <- as.logical(df_vroom$made_sla)
#    df_vroom$knowledge <- as.logical(df_vroom$knowledge)
#    df_vroom$u_priority_confirmation <- as.logical(df_vroom$u_priority_confirmation)
# 
# df_small <- df_vroom 
# vroom::vroom_write(df_small, "/mnt/d/big_data/Service_Desk_Demo/df_small.tsv.gz")
# 
# arrow::write_dataset(df_small,
#                      "/mnt/d/big_data/Service_Desk_Demo/parquet_small_arrow",
#                      format = "parquet")
# 
# 
# # df_big <- df_vroom
# 
# #    # Augmented with
# #    for (i in seq(1:60)) {
# #      df_big <- rbind(df_big, df_vroom)
# #      print(paste(i, object.size(df_big), unit="Mb"))
# #    }
# #
# #    rows <- dim(df_big)[1]
# #
# #    df_big$new_number <- as.character(glue::glue("INC00-{seq(1:as.numeric(rows))}"))
# #    df_big$number <- df_big$new_number
# #    df_big <- dplyr::glimpse(df_big[,1:36])
# #
# #    vroom::vroom_write(df_big, "/mnt/d/big_data/Service_Desk_Demo/df_big.tsv.gz")
# #    
# #
# #    df_big <- vroom::vroom("/mnt/d/big_data/Service_Desk_Demo/df_big.tsv.gz")
# #    df_small <- vroom::vroom("/mnt/d/big_data/Service_Desk_Demo/df_small.tsv.gz")
# #
# #    arrow::write_dataset(df_big,
# #                         "/mnt/d/big_data/Service_Desk_Demo/parquet_big_arrow",
# #                         format = "parquet")
# #
# # 
# 
# ##################################################################################
# #
# #   ### Summarise Data
# 
# library (arrow)
# library (dplyr)
# 
# ds <- open_dataset("./part-0-big.parquet",format = "parquet")
# 
# #cols <- names (ds)
# #exprs <- purrr::map(cols, ~glue("{.}=max({.})"))
# #exprs <- set_names(exprs, cols)
# 
# ds_inc <- ds |>group_by(number) |> summarise(incident_state=max(incident_state),
#                                               active=max(active),
#                                               reassignment_count=max(reassignment_count),
#                                               reopen_count=max(reopen_count),
#                                               sys_mod_count=max(sys_mod_count),
#                                               made_sla=max(made_sla),
#                                               caller_id=max(caller_id),
#                                               opened_by=max(opened_by),
#                                               opened_at=max(opened_at),
#                                               sys_created_by=max(sys_created_by),
#                                               sys_created_at=max(sys_created_at),
#                                               sys_updated_by=max(sys_updated_by),
#                                               sys_updated_at=max(sys_updated_at),
#                                               contact_type=max(contact_type) ,
#                                               location=max(location), 
#                                               category=max(category), 
#                                               subcategory=max(subcategory),
#                                               u_symptom=max(u_symptom),
#                                               cmdb_ci=max(cmdb_ci) ,
#                                               impact=max(impact) ,
#                                               urgency=max(urgency) ,
#                                               priority=max(priority) ,
#                                               assignment_group=max(assignment_group) ,
#                                               assigned_to=max(assigned_to), 
#                                               knowledge=max(knowledge) ,
#                                               u_priority_confirmation=max(u_priority_confirmation) ,
#                                               notify=max(notify) ,
#                                               problem_id=max(problem_id) ,
#                                               rfc=max(rfc) ,
#                                               vendor=max(vendor), 
#                                               caused_by=max(caused_by) ,
#                                               closed_code=max(closed_code) ,
#                                               resolved_by=max(resolved_by) ,
#                                               resolved_at=max(resolved_at), 
#                                               closed_at=max(closed_at))
# 
#   arrow::write_dataset(ds_inc,
#                      "./",
#                      basename_template = "inc-{i}",
#                      format = "parquet")
# 
# 
# 
# 

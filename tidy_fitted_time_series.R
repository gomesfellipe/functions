tidy_fitted_time_series <- function(model){
  model %>% 
    fitted() %>% 
    timetk::tk_tbl(rename_index = "date") %>%
    dplyr::mutate(date = zoo::as.Date(date, frac=1))
}

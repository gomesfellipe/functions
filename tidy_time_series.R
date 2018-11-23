tidy_ts_forecast <- function(fitted, filter = F){
  tidy <- 
    fitted %>%
    sweep::sw_sweep(timekit_idx = TRUE, rename_index = "date") %>%
    dplyr::mutate(date = zoo::as.Date(date, frac=1))
  
  if(filter == T){
    tidy <- tidy %>% filter(key == "forecast")
  }
  tidy
}

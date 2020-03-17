# Esse script é um rascunho , apenas um esboco da funcao

# library(h2o) # automl
# h2o.init() 


h2o_tidy_automl <- function(){

tbl_stocks_automl <- 
  tbl_stocks %>% 
  as_tibble()  %>%
  group_by(symbol) %>% 
  nest() %>% 
  mutate(data = map(data, ~pp_data(.x) )) %>% 
  bind_cols(tibble(future_h2o = list(as.h2o(tbl_future),
                                     as.h2o(tbl_future),
                                     as.h2o(tbl_future))) ) %>% 
  mutate(train_h2o = map(data, ~.x  %>% as.h2o())) %>% 
  mutate(valid_h2o = map(data, ~.x %>% as.h2o()) ) %>% 
  mutate(test_h2o  = map(data, ~.x %>% as.h2o())) %>% 
  mutate(automl_models_h2o = pmap(list(train_h2o = train_h2o,
                                       valid_h2o = valid_h2o,
                                       test_h2o = test_h2o),
                                  function(train_h2o, valid_h2o, test_h2o){
                                    automl_models_h2o <- h2o.automl(
                                      x = setdiff(names(train_h2o), "close"), 
                                      y = "close", 
                                      training_frame = train_h2o, 
                                      validation_frame = valid_h2o, 
                                      leaderboard_frame = test_h2o, 
                                      max_runtime_secs = 60*30, 
                                      verbosity = NULL,
                                      stopping_metric = "deviance")
                                  })) %>% 
  mutate(automl_leader = map(automl_models_h2o, ~ .x@leader)) %>% 
  mutate(pred_h2o = map2(automl_leader, test_h2o, ~h2o.predict(.x, newdata = .y))) %>% 
  mutate(error_tbl = map2(data, pred_h2o, 
                          ~.x %>% filter(date > data_split2) %>%
                            add_column(pred = .y %>% as_tibble() %>% pull(predict)) %>%
                            rename(actual = close) %>%
                            mutate(error     = actual - pred,
                                   error_pct = error / actual) ))

saveRDS(tbl_stocks_automl, "tbl_stocks_automl.rds")
}

flextable_custom <- function(x){
  flextable(x) %>% 
  theme_zebra(odd_header = "darkgrey", odd_body = "white", even_body = "lightgrey") %>%
  color(part = "header", color = "white") %>% 
  bold(part = "header") %>%
  align(part = "all", align = "center")
}

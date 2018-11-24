# Retorna o inicio e o final da base como string
# Acrecentando entre head e tail a string "..."
# Bom para relatorios!

inicio_e_fim_da_base <- function(x){
  bind_rows(head(x) %>% map_df(as.character),
            map2_dfc(rep("...", ncol(x)),colnames(x), ~tibble(.x) %>% `colnames<-`(.y)),
            tail(x)%>% map_df(as.character))
}

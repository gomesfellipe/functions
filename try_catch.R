tryCatch({
# Exemplo de funcao que retornava error e gostaria que continuasse
# ref: https://stackoverflow.com/questions/14748557/skipping-error-in-for-loop
map2(
  list(atacado_Ult_12M, praticado_Ult_12M, varejo_Ult_12M, ipa_Ult_12M, ipc_Ult_12M),
  c("atacado", "praticado", "varejo", "ipa", "ipc"),
  ~ write.csv2(.x,
    paste0(
      read.table("dep/path.txt")[1,1] %>% as.character(), "/", .y, "_ultimo_12meses.csv"
    ),row.names=F))
}, error=function(e){cat("Atencao para exportar bases auxiliares:",conditionMessage(e), "\n")})

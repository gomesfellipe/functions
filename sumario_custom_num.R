sumario_custom_num <- 
  function(x,na_rm=T){
  pubg_tpp1 %>% map2_df(colnames(pubg_tpp1),
                      ~ bind_cols(variavel = .y,
                                 min = min(.x, na.rm = na_rm),
                                 q25 = quantile(.x,probs = c(0,1,0.25),na.rm = na_rm)[1] ,
                                 mediana = median(.x,na.rm = na_rm),
                                 media = mean(.x,na.rm = na_rm),
                                 q75 = quantile(.x,probs = c(0,1,0.25),na.rm = na_rm)[3],
                                 max = max(.x, na.rm = na_rm),
                                 desv_pad = sd(.x,na.rm = na_rm),
                                 variancia = var(.x,na.rm = na_rm),
                                 assimetria = skewness(.x,na.rm = na_rm), # ref: -1 < -1/2 < 0 > 1/2 > 1 .
                                 curtose = kurtosis(.x,na.rm = na_rm), 
                                 shapiro = shapiro.test(.x)$p.value,
                                 ks = ks.test(.x,"pnorm",
                                              mean(.x,na.rm = na_rm), sd(.x,na.rm = na_rm))$p.value,
                                 p_na = sum(is.na(.x))/length(.x),
                                 p_zeros = sum(.x==0,na.rm = na_rm)/length(.x)
                                 ) 
                     )
# ref: assimetria, curtose: https://www.medcalc.org/manual/skewnesskurtosis.php
}

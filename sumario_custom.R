sumario_custom <- 
  function(x,na_rm=T){
  pubg_tpp1 %>% map2_df(colnames(pubg_tpp1),
                      ~ bind_cols(variavel = .y,
                                  media = mean(.x,na.rm = na_rm),
                                 desv.pad. = sd(.x,na.rm = na_rm),
                                 variância = var(.x,na.rm = na_rm),
                                 mediana = median(.x,na.rm = na_rm),
                                 q1 = quantile(.x,probs = c(0,1,0.25),na.rm = na_rm)[1] ,
                                 q3 = quantile(.x,probs = c(0,1,0.25),na.rm = na_rm)[3],
                                 assimetria = skewness(.x,na.rm = na_rm), # ref: -1 < -1/2 < 0 > 1/2 > 1 .
                                 curtose = kurtosis(.x,na.rm = na_rm), 
                                 p_na = sum(is.na(.x))/length(.x),
                                 p_zeros = sum(.x==0,na.rm = na_rm)/length(.x),
                                 ) 
                     )
# ref: assimetria, curtose: https://www.medcalc.org/manual/skewnesskurtosis.php
}

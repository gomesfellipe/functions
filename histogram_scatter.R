#Disperses plus histograms of marginal distributions on the sides
histogram_scatter=function(base, x, y,fator){
  hist_top <- ggplot(base, aes(x, fill=fator)) + geom_density()+ guides(fill=FALSE)+labs(x=" ", y=" ")
  hist_right <- ggplot(base, aes(y, fill=fator)) + geom_density()+coord_flip()+ guides(fill=FALSE)+labs(x=" ", y=" ")
  empty <- ggplot()+annotate("text", label=str_c("R²=",as.character(r2(y,x)[1]),"%"), x=1, y=1)+
    theme(axis.ticks=element_blank(), 
          panel.background=element_blank(), 
          axis.text.x=element_blank(), axis.text.y=element_blank(),           
          axis.title.x=element_blank(), axis.title.y=element_blank())
  scatter <- ggplot(base, aes(y=y, x=x )) + geom_point(data=base, aes(color=fator)) + 
    geom_smooth(method="lm") +labs(x=" ", y=" ")
  grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(2, 4))
}
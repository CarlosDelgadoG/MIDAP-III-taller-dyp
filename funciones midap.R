

# GRAFO PHQ ---------------------------------------------------------------
grafo_phq <- function(vars,etiq){
  to_long <- function(var){
    var_tidy <- as.name(var)
    base <- select(elsoc_2016, !!var_tidy)
    base <- mutate(base,
                   "valor"=rep(var,nrow(base)))
    colnames(base)<- c("valor","variable")
    base$valor <- factor(base$valor,
                         labels=c('Not at all','Several days','More than half','Nearly everyday'))
    return(base)
  }
  
  nombres.facet<- function(viejos, nuevos){
    var.labs <- nuevos
    names(var.labs) <-viejos
    return(var.labs)
  }
  
  
  base_grafo<- bind_rows(lapply(vars, to_long))
  
  ggp<- ggplot(base_grafo,aes(x=valor))+
    geom_bar(fill="#9932CC",color="white")+
    labs(y="Frequency",
         x=element_blank())+
    theme_classic()+
    scale_x_discrete(labels=stringr::str_wrap(levels(base_grafo$valor), width =7),na.translate=FALSE)+
    facet_wrap(~variable,
               labeller = labeller(variable=nombres.facet(viejos = vars,
                                                          nuevos = etiq)))
  return(ggp)
}
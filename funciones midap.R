

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

# PHQ BOXPLOT ---------------------------------------------------------------
phq_boxplot <- function(var,titulo=NULL,etiq=NULL){
  
  var_tidy <- as.name(var)
  
  ggp<- ggplot(elsoc_2016,aes(x=!!var_tidy,y=s11_phq9))+
    geom_boxplot(color="#0db783")+
    labs(y="PHQ-9 Score",
         x=element_blank(),
         title = titulo)+
    theme_classic()+
    scale_x_discrete(labels=stringr::str_wrap(levels(getElement(elsoc_2016,var)), width =7),
                     na.translate=FALSE)
  if(!is.null(etiq)){
    ggp <- ggp+
      scale_x_discrete(labels=stringr::str_wrap(etiq, width =7),
                       na.translate=FALSE)
  }
  
  
  return(ggp)
}


# GRAFO COEFICIENTES ------------------------------------------------------

grafo_eff <- function(var){
  var_tidy <- as.name(var)
  tabla_grafo<-   tabla_coef%>%
    mutate(cont_eff= !!var_tidy >1)%>%
    filter(Variable != "Intercept")
  
  ggplot(tabla_grafo,aes(x=factor(Variable),y=!!var_tidy,color=cont_eff))+
    geom_point()+
    geom_hline(yintercept = 1,lty=2,color="cornflowerblue")+
    theme_classic()+
    labs(x="Variables")+
    scale_x_discrete(labels=stringr::str_wrap(levels(factor(tabla_grafo$Variable)), width =7))+
    theme(legend.position = "none")
}

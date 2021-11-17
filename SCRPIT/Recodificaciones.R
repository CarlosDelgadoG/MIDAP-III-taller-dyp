
load("ELSOC_2016.RData")
elsoc_2016 <-sjmisc::set_na(elsoc_2016,na=c(-888,-999))
#ESCALA PHQ-9
elsoc_2016$s11_01_rec <- car::recode(elsoc_2016$s11_01-1,"4=3")
elsoc_2016$s11_02_rec <- car::recode(elsoc_2016$s11_02-1,"4=3")
elsoc_2016$s11_03_rec <- car::recode(elsoc_2016$s11_03-1,"4=3")
elsoc_2016$s11_04_rec <- car::recode(elsoc_2016$s11_04-1,"4=3")
elsoc_2016$s11_05_rec <- car::recode(elsoc_2016$s11_05-1,"4=3")
elsoc_2016$s11_06_rec <- car::recode(elsoc_2016$s11_06-1,"4=3")
elsoc_2016$s11_07_rec <- car::recode(elsoc_2016$s11_07-1,"4=3")
elsoc_2016$s11_08_rec <- car::recode(elsoc_2016$s11_08-1,"4=3")
elsoc_2016$s11_09_rec <- car::recode(elsoc_2016$s11_09-1,"4=3")

elsoc_2016$s11_phq9<- with(elsoc_2016,(s11_01_rec+s11_02_rec+s11_03_rec+s11_04_rec+s11_05_rec+s11_06_rec+s11_07_rec+s11_08_rec+s11_09_rec))
attr(elsoc_2016$s11_phq9,"label") <- "Escala sintomatología depresiva"

#Sexo del entrevistado
elsoc_2016$m0_sexo_fac <- factor(elsoc_2016$m0_sexo,
                                 labels = c("Hombre","Mujer"))

attr(elsoc_2016$m0_sexo_fac,"label")<- "Sexo entrevistado"

  #En tratamiento por depresión
elsoc_2016$s14_fac <- factor(elsoc_2016$s14,
                             labels = c("No","Sí"),
                             levels = c(2,1))

attr(elsoc_2016$s14_fac,"label") <- "Tratamiento por Depresión"
#Satisfacción con la vida
elsoc_2016$s01_fac <- factor(car::recode(elsoc_2016$s01,"c(1,2)=1;3=2;c(4,5)=3"),
                             levels = c(1,2,3),
                             labels = c("Insatisfecho",
                                        "Ni satisfecho ni insatisfecho",
                                        "Satisfecho"))

setattr(elsoc_2016$s01_fac,"label","Satisfacción con la vida")

#Satisfaccion con ingreso

elsoc_2016$m16_fac<- factor(car::recode(elsoc_2016$m16, "c(1,2)=1;3=2;c(4,5)=3"),
labels = c("Instatisfecho", "Ni instatisfecho ni satisfecho","Satisfecho"))

setattr(elsoc_2016$m16_fac,"label","Satisfacción con ingreso")

#Percepción subjetiva de la salud
elsoc_2016$s03_fac <- factor(car::recode(elsoc_2016$s03, "c(1,2)=1;c(3,4,5)=2"),
                             labels = c("Mala-Regular","Buena-Excelente"))
setattr(elsoc_2016$s03_fac, "label","Percepción subjetiva estado salud")
#Sobrecarga endeudamiento
elsoc_2016$m43_fac<- factor(car::recode(elsoc_2016$m43, "c(1,2)=1;c(3,4)=2"),
                            labels = c("No Sobrecargado","Sobrecargado"))
setattr(elsoc_2016$m43_fac,"label","Sobrecarga endeudamiento")
#Grado en que mi vida se acerca a mi ideal
elsoc_2016$s02_fac <- factor(car::recode(elsoc_2016$s02, "c(1,2)=1;3=2;c(4,5)=3"),
                             labels = c("Se aleja de mi ideal","No se acerca ni se aleja","Se acerca a mi ideal"))
setattr(elsoc_2016$s02_fac,"label","Grado en que vida se acerca a ideal")

#Actividad principal

elsoc_2016$m02_fac<-factor(car::recode(elsoc_2016$m02, "c(1,2,3)=1;7=2;5=3;6=4;c(4,8,9)=5"),
                           labels = c("Trabajo Remunerado","Trabajo Doméstico No Remunerado","Jubilado","Desempleado","Otras Categorías"))

setattr(elsoc_2016$m02_fac,"label","Actividad principal entrevistado")
#Nivel educacional
elsoc_2016$m01_fac<- factor(car::recode(elsoc_2016$m01, "c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4"),
                            labels = c("Básica","Media","Técnica","Universitaria"))
setattr(elsoc_2016$m01_fac, "label","Nivel educacional")
#Estado civil 
elsoc_2016$m36_fac <- factor(car::recode(elsoc_2016$m36,"c(1,2,3)=1;c(4,5,6,7,8)=2;else=NA"),
                             labels = c("En Pareja","Soltero"))
setattr(elsoc_2016$m36_fac,"label","Estado Civil")
#Comportamiento prosocial
elsoc_2016$c07_prosocial <- with(elsoc_2016,(c07_01+c07_02+c07_03+c07_04+c07_05+c07_06+c07_07+c07_08))

attr(elsoc_2016$c07_prosocial,"label") <- "Escala comportamiento pro-social"

# ESTRESORES
elsoc_2016$s13_01_rec <-car::recode(elsoc_2016$s13_01-1, "0=1;1=0")
elsoc_2016$s13_02_rec <-car::recode(elsoc_2016$s13_02-1, "0=1;1=0")
elsoc_2016$s13_03_rec <-car::recode(elsoc_2016$s13_03-1, "0=1;1=0")
elsoc_2016$s13_04_rec <-car::recode(elsoc_2016$s13_04-1, "0=1;1=0")
elsoc_2016$s13_05_rec <-car::recode(elsoc_2016$s13_05-1, "0=1;1=0")
elsoc_2016$s13_06_rec <-car::recode(elsoc_2016$s13_06-1, "0=1;1=0")
elsoc_2016$s13_07_rec <-car::recode(elsoc_2016$s13_07-1, "0=1;1=0")
elsoc_2016$s13_08_rec <-car::recode(elsoc_2016$s13_08-1, "0=1;1=0")

elsoc_2016$s13_estresores <- with(elsoc_2016,(s13_01_rec+s13_02_rec+s13_03_rec+s13_04_rec+s13_05_rec+s13_06_rec+s13_07_rec+
                                                s13_08_rec))
attr(elsoc_2016$s13_estresores,"label")<- "Cantidad de eventos estresores"


# IMC ---------------------------------------------------------------------

elsoc_2016$s_imc <- with(elsoc_2016, s06/(s05/100)^2)
# SACAR OBSERVACIONES SOSPECHOSAS
elsoc_2016$s_imc[c(675,654,1699,442)] <-NA 
#Attributos
setattr(elsoc_2016$s_imc,"label","Indice de masa corporal")
elsoc_2016$s_imc_tramos <- factor(with(elsoc_2016, case_when(
                            s_imc >18.4 & s_imc < 25 ~ 1,
                            s_imc >24.9 & s_imc < 30 ~ 2,
                            s_imc > 29.9 & s_imc <35 ~ 3,
                            s_imc>34.9 & s_imc < 40 ~ 4,
                            s_imc > 40~5)),
                            labels = c("Normal", "Sobre Peso", "Obesidad I", "Obesidad II", "Obesidad III"))
elsoc_2016$s_imc_fac <- factor(with(elsoc_2016, case_when(
  s_imc >18.4 & s_imc < 25 ~ 1,
  s_imc >24.9 & s_imc < 30 ~ 2,
  s_imc > 29.9  ~ 3)),
  labels = c("Normal", "Sobre Peso", "Obesidad"))

setattr(elsoc_2016$s_imc_fac,"label","Tramos IMC")



# Jornada laboral ---------------------------------------------------------

elsoc_2016$m12_fac <- factor(with(elsoc_2016,case_when(
                          m12 <25 ~1,
                          m12 > 25 & m12 <46 ~2,
                          m12 >46 ~3)),
                          labels = c("Menos de 24 horas", "25 a 45 horas","Más de 45 horas"))
setattr(elsoc_2016$m12_fac,"label","Jornada Laboral")


# Tramos Cigarros ---------------------------------------------------------

elsoc_2016$s08_fac<- factor(with(elsoc_2016, case_when(
                                            s08== 0 ~1,
                                            s08 >0  ~2)),
                    labels = c("No Fumador", "Fumador"))


# TRANSFORMACION VARIABLE RESPUESTA ---------------------------------------

elsoc_2016$s11_phq9_log <-ifelse(elsoc_2016$s11_phq9 == 0,log(elsoc_2016$s11_phq9+0.1), log(elsoc_2016$s11_phq9)) 
elsoc_2016$s11_phq9_est <- with(elsoc_2016,(s11_phq9-mean(s11_phq9,na.rm=TRUE))/sd(s11_phq9,na.rm=TRUE)) 


# BASE MODELOS ------------------------------------------------------------

base_modelo <- select(elsoc_2016,s11_phq9,m02_fac,m0_sexo_fac,s01_fac,m36_fac,m01_fac,s03_fac,m43_fac,s02_fac,s14_fac,m0_edad,m17,s13_estresores,c07_prosocial)

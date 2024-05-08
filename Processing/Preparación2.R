##Ajustes Iniciales
rm(list=ls())
options(scipen=999)

#Cargar paquetes
pacman::p_load(sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2,# Para la mayoría de los gráficos
               GGally)
              

#Cargar base
load("Input/ELSOC_W01_v4.01_R.RData")
View(elsoc_2016)
names(elsoc_2016)
dim(elsoc_2016)

proc_data <- elsoc_2016 %>% select(m0_sexo,
                                   m0_edad,
                                   m01,
                                   c08_01:c08_04,
                                   c12_01,
                                   c12_03,
                                   c12_08,
                                   c13,
                                   c15,
                                   c22,
                                   c33)
names(proc_data)
sjlabelled::get_label(proc_data)

#Procesamiento de Variables
proc_data <- proc_data %>% set_na(., na = c(-999, -888))
proc_data <- na.omit(proc_data)

proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")
save(proc_data,file = "Input/proc_data.RData")

##Nivel educacional
frq(proc_data$m01)
proc_data$m01 <- car::recode(proc_data$m01, "c(1,2,3,4,5)=1; c(6,7)=2; c(8,9,10)=3")
proc_data$m01 <- factor(proc_data$m01,
                             labels = c("Educacion Técnica Superior", "Educación Universitaria"),
                             levels = c(2, 3))

proc_data <- rename(proc_data,"educacion"=m01)
get_label(proc_data$educacion)
proc_data$educacion <- set_label(x = proc_data$educacion,label = "Educación")
frq(proc_data$educacion)

##Selección de muestra
proc_data <- proc_data %>%
  filter(educacion %in% c("Educación Universitaria", "Educacion Técnica Superior"))

##NSE
frq(proc_data$c33)
proc_data <- proc_data %>% rename("NSE"=c33)
frq(proc_data$NSE)

proc_data$NSE <- set_label(proc_data$NSE, label = "Nivel Socioeconómico")
get_label(proc_data$NSE)
proc_data$NSE <- ifelse(proc_data$NSE %in% c(1, 2), "NSE Bajo",
                        ifelse(proc_data$NSE %in% c(3, 4), "NSE Medio",
                               ifelse(proc_data$NSE == 5, "NSE Alto", NA)))

proc_data$NSE<- factor(proc_data$NSE, levels = c("NSE Bajo", "NSE Medio", "NSE Alto"))
frq(proc_data$NSE)

##Sexo
frq(proc_data$m0_sexo)
proc_data$m0_sexo <- car::recode(proc_data$m0_sexo, "1=0;2=1")
proc_data$m0_sexo <- factor(proc_data$m0_sexo,
                         labels=c( "Hombre",
                                   "Mujer"),
                         levels=c(0,1))
proc_data <- rename(proc_data,"sexo"=m0_sexo)
get_label(proc_data$sexo)
proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")
frq(proc_data$sexo)

##Edad
frq(proc_data$m0_edad)
proc_data <- rename(proc_data,"edad"=m0_edad)
get_label(proc_data$edad)
proc_data$edad <- set_label(x = proc_data$edad,label = "Edad")
frq(proc_data$edad)

##Participación Política
proc_data <- proc_data %>% rename("apoyo_causa"=c08_01,  
                                  "part_marcha"=c08_02, 
                                  "part_huelga"=c08_03, 
                                  "rrss_pol"=c08_04)
proc_data$apoyo_causa <- set_label(x = proc_data$apoyo_causa,label = "Participación: Apoya Causa")
get_label(proc_data$apoyo_causa)
proc_data$part_marcha <- set_label(x = proc_data$part_marcha, label = "Participación: Marcha")
get_label(proc_data$part_marcha)
proc_data$part_huelga <- set_label(x = proc_data$part_huelga, label = "Participación: Huelga")
get_label(proc_data$part_huelga)
proc_data$rrss_pol <- set_label(x = proc_data$rrss_pol, label = "Participación: RRSS")
get_label(proc_data$rrss_pol)

frq(proc_data$apoyo_causa)
frq(proc_data$part_marcha)
frq(proc_data$part_huelga)
frq(proc_data$rrss_pol)

proc_data$apoyo_causa <- factor(proc_data$apoyo_causa,
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("Poco", "Poco", "Algo", "Mucha", "Mucha"))

proc_data$part_marcha <- factor(proc_data$part_marcha,
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("Poco", "Poco", "Algo", "Mucha", "Mucha"))
proc_data$part_huelga <- factor(proc_data$part_huelga,
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("Poco", "Poco", "Algo", "Mucha", "Mucha"))
proc_data$rrss_pol <- factor(proc_data$rrss_pol,
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("Poco", "Poco", "Algo", "Mucha", "Mucha"))

##Membresías
proc_data <- proc_data %>% rename("pert_jdv"=c12_01,  
                                  "pert_pp"=c12_03, 
                                  "pert_cde"=c12_08)


proc_data$pert_jdv <- set_label(proc_data$pert_jdv, label = "Pertenece: Junta de vecinos")
get_label(proc_data$pert_jdv)
proc_data$pert_pp <- set_label(proc_data$pert_pp, label = "Pertenece: Partido Político")
get_label(proc_data$pert_pp)
proc_data$pert_cde <- set_label(proc_data$pert_cde, label = "Pertenece: Centro de estudiantes")
get_label(proc_data$pert_cde)

frq(proc_data$pert_jdv)
frq(proc_data$pert_pp)
frq(proc_data$pert_cde)

proc_data$pert_jdv <- factor(proc_data$pert_jdv,
                                levels = c(1, 2, 3),
                                labels = c("No", "Si", "Si"))
proc_data$pert_pp <- factor(proc_data$pert_pp,
                             levels = c(1, 2, 3),
                             labels = c("No", "Si", "Si"))
proc_data$pert_cde <- factor(proc_data$pert_cde,
                             levels = c(1, 2, 3),
                             labels = c("No", "Si", "Si"))

##Interés en la política
proc_data <- proc_data %>% rename("interes_pol"=c13)
frq(proc_data$interes_pol)

proc_data$interes_pol <- set_label(proc_data$interes_pol, label = "Interés Político")
get_label(proc_data$interes_pol)

proc_data$interes_pol <- factor(proc_data$interes_pol,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("Sin interés", "Poco interés", "Poco interés", "Alto interés", "Alto interés"))

##Autoubicación Política
proc_data <- proc_data %>% rename("autoubic_pol"=c15)
frq(proc_data$autoubic_pol)

proc_data$autoubic_pol <- set_label(proc_data$autoubic_pol, label = "Ubicación Política")
get_label(proc_data$autoubic_pol)
proc_data$autoubic_pol <- car::recode(proc_data$autoubic_pol, "c(0,1,2,3)=1; c(4,5,6)=2; c(7,8,9,10)=3; c(11,12)=4")
proc_data$autoubic_pol <- factor(proc_data$autoubic_pol,
                             labels = c("Izquierda", "Centro", "Educacion superior", "Ninguna"),
                             levels = c(1, 2, 3, 4))

##Participación en Movimientos Sociales
proc_data <- proc_data %>% rename("part_movsoc"=c22)
proc_data$part_movsoc <- set_label(x = proc_data$part_movsoc,label = "Participación: Movimientos Sociales")
get_label(proc_data$part_movsoc)

proc_data$part_movsoc <- car::recode(proc_data$part_movsoc,
                             "1:2 = 1;
                              3 = 2;
                              4:5 = 3")

proc_data$part_movsoc <- factor(proc_data$part_movsoc,
                        labels = c("Baja", "Alta"),
                        levels = c(1, 2))
frq(proc_data$part_movsoc)


#Descriptivos
stargazer(proc_data,type = "text")

sjmisc::descr(proc_data)
sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown")

summarytools::dfSummary(proc_data, plain.ascii = FALSE)
view(dfSummary(proc_data, headings=FALSE))

#Gráficos
graph1 <- proc_data %>% ggplot(aes(x = interes_pol, fill = educacion)) + 
  geom_bar() +
  xlab("Interés Político") +
  ylab("Educación") + 
  labs(fill="educacion")+
  scale_fill_discrete(labels = c('Universitario','Tecnico Superior'))

graph1
ggsave(graph1, file="Output/graph1.png")


graph2 <- ggplot(proc_data, aes(x = NSE, fill = educacion)) +
  geom_bar(position = "fill") +
  facet_wrap(~ interes_pol) +
  labs(x = "NSE", y = "Proporción", fill = "Nivel educacional") +
  theme_minimal()
graph2
ggsave(graph2, file="Output/graph2.png")

##Asociación de Variables
dim(proc_data) # Dimensiones

sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")


##Correlación
proc_data <- mutate_all(proc_data, as.numeric)

M <- cor(indicadores,
         use = "complete.obs")
M

sjPlot::tab_corr(proc_data, 
                 triangle = "lower")

corrplot.mixed(M)


indicadores = proc_data %>% 
  rowwise() %>%
  mutate(participacion = mean(c(apoyo_causa, part_marcha,part_huelga,rrss_pol,part_movsoc)),
         pertenencia = mean(c(pert_jdv, pert_pp, pert_cde))) %>% 
  ungroup()

indicadores = indicadores %>% 
  rowwise() %>%
  mutate(interes_politico = mean(c(participacion, pertenencia))) %>% 
  ungroup()

indicadores %>% select(interes_politico) %>% head(10) # Primeros 10 casos
summary(indicadores$interes_politico)

indicadores <- indicadores %>%
  mutate(interes_politico = case_when(
    participacion >= 0.5 & pertenencia >= 0.5 ~ "Alto",
    participacion >= 0.5 & pertenencia < 0.5 ~ "Participación Alta, Pertenencia Baja",
    participacion < 0.5 & pertenencia >= 0.5 ~ "Participación Baja, Pertenencia Alta",
    TRUE ~ "Bajo"
  ))
prop.table(table(indicadores$interes_politico))*100

psych::alpha(dplyr::select(indicadores, participacion, pertenencia))

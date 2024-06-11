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
load("Input/data original/ELSOC_W01_v4.01_R.RData")
View(elsoc_2016)
names(elsoc_2016)
dim(elsoc_2016)

proc_data <- elsoc_2016 %>% select(m0_sexo,
                                   m0_edad,
                                   m01,
                                   c08_01:c08_04,
                                   c12_03,
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
save(proc_data,file = "Input/data proc/proc_data.RData")

##Nivel educacional
frq(proc_data$m01)
proc_data <- rename(proc_data,"educacion"=m01)
get_label(proc_data$educacion)
proc_data$educacion <- set_label(x = proc_data$educacion,label = "Educación")
frq(proc_data$educacion)
proc_data$educacion <- car::recode(proc_data$educacion, "c(1,2)=0; c(3,4)=1; c(5,6,8)=2; c(7,9,10)=3")
proc_data$educacion <- factor(proc_data$educacion,
                     labels = c("Sin Estudios", "Básica", "Media", "Superior"),
                     levels = c(0, 1, 2, 3))

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
proc_data <- proc_data %>% rename("pert_pp"=c12_03)

proc_data$pert_pp <- set_label(proc_data$pert_pp, label = "Pertenece: Partido Político")
get_label(proc_data$pert_pp)

frq(proc_data$pert_pp)

proc_data$pert_pp <- factor(proc_data$pert_pp,
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

##Autoubicación Política
proc_data <- proc_data %>% rename("autoubicpol"=c15)
proc_data$autoubicpol <- set_label(x = proc_data$autoubicpol,label = "Autoubicación Política")


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
  labs(fill="educacion")

graph1
ggsave(graph1, file="Output/graph1.png")


graph2 <- ggplot(proc_data, aes(x = NSE, fill = educacion)) +
  geom_bar() +
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

M <- cor(proc_data,
         use = "complete.obs")
M

sjPlot::tab_corr(proc_data, 
                 triangle = "lower")

corrplot.mixed(M)

M2 <- cor(dplyr::select(proc_data, apoyo_causa, part_marcha, part_huelga, 
                        rrss_pol, pert_pp, interes_pol, part_movsoc, NSE), use = "complete.obs")
corrplot.mixed(M2)

psych::alpha(dplyr::select(proc_data, apoyo_causa, part_marcha, part_huelga, 
                        rrss_pol, pert_pp, interes_pol, part_movsoc, NSE))

psych::alpha(dplyr::select(proc_data, apoyo_causa, part_marcha, part_huelga, 
                           rrss_pol, pert_pp, part_movsoc))

escala = proc_data %>% 
  rowwise() %>%
  mutate(participacion = mean(c(proc_data, apoyo_causa, part_marcha, part_huelga, 
                                rrss_pol, pert_pp, part_movsoc)),
         interes_politico = mean(c(interes_pol)),
         nse= mean(c(NSE))) %>% 
  ungroup()


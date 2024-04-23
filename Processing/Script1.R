##Ajustes Iniciales
rm(list=ls())
options(scipen=999)

#Cargar paquetes
install.packages("pacman")
pacman::p_load(sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2) # Para la mayoría de los gráficos

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
                                   c31)
names(proc_data)
sjlabelled::get_label(proc_data)

#Procesamiento de Variables
proc_data <- proc_data %>% set_na(., na = c(-999, -888))
proc_data <- na.omit(proc_data)

proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

##Nivel educacional
frq(proc_data$m01)
proc_data$m01 <- car::recode(proc_data$m01, "c(1,2,3,4,5)=1; c(6,7)=2; c(8,9,10)=3")
proc_data$m01 <- factor(proc_data$m01,
                             labels = c("Otro", "Educacion Técnica Superior", "Educación Universitaria"),
                             levels = c(1, 2, 3))
proc_data <- rename(proc_data,"educacion"=m01)
get_label(proc_data$educacion)
proc_data$educacion <- set_label(x = proc_data$educacion,label = "Educación")

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

##Edad
frq(proc_data$m0_edad)
proc_data <- rename(proc_data,"edad"=m0_edad)
get_label(proc_data$edad)
proc_data$edad <- set_label(x = proc_data$edad,label = "Edad")

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

library(sjlabelled)

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

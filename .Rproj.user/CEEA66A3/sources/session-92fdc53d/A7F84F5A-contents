## Ajustes Iniciales
rm(list = ls())
options(scipen = 999)

# Cargar paquetes
pacman::p_load(sjlabelled,
               dplyr, # Manipulacion de datos
               stargazer, # Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, # Tablas
               sjPlot, # Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2, # Para la mayoría de los gráficos
               GGally,
               car,
               texreg,
               sjlabelled, 
               fastDummies, 
               ggeffects)




# Cargar base
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
                                   c33,
                                   c18_11)

names(proc_data)
sjlabelled::get_label(proc_data)

# Procesamiento de Variables
proc_data <- proc_data %>% set_na(., na = c(-999, -888))
proc_data <- na.omit(proc_data)

proc_data <- as.data.frame(proc_data)
stargazer(proc_data, type = "text")
save(proc_data, file = "Input/data proc/proc_data.RData")

## Nivel educacional
frq(proc_data$m01)
proc_data <- rename(proc_data, "educacion" = m01)
get_label(proc_data$educacion)
proc_data$educacion <- set_label(x = proc_data$educacion, label = "Educación")
frq(proc_data$educacion)
proc_data$educacion <- car::recode(proc_data$educacion, "c(1,2)=0; c(3,4)=1; c(5,6,8)=2; c(7,9,10)=3")
proc_data$educacion <- factor(proc_data$educacion,
                              labels = c("Sin Estudios", "Básica", "Media", "Superior"),
                              levels = c(0, 1, 2, 3))

## NSE
frq(proc_data$c33)
proc_data <- proc_data %>% rename("NSE" = c33)
frq(proc_data$NSE)

proc_data$NSE <- set_label(proc_data$NSE, label = "Nivel Socioeconómico")
get_label(proc_data$NSE)
proc_data$NSE <- ifelse(proc_data$NSE %in% c(1, 2), "NSE Bajo",
                        ifelse(proc_data$NSE %in% c(3, 4), "NSE Medio",
                               ifelse(proc_data$NSE == 5, "NSE Alto", NA)))

proc_data$NSE <- factor(proc_data$NSE, levels = c("NSE Bajo", "NSE Medio", "NSE Alto"))
frq(proc_data$NSE)

## Sexo
frq(proc_data$m0_sexo)
proc_data$m0_sexo <- car::recode(proc_data$m0_sexo, "1=0;2=1")
proc_data$m0_sexo <- factor(proc_data$m0_sexo,
                            labels = c("Hombre", "Mujer"),
                            levels = c(0, 1))
proc_data <- rename(proc_data, "sexo" = m0_sexo)
get_label(proc_data$sexo)
proc_data$sexo <- set_label(x = proc_data$sexo, label = "Sexo")
frq(proc_data$sexo)

## Edad
frq(proc_data$m0_edad)
proc_data <- rename(proc_data, "edad" = m0_edad)
get_label(proc_data$edad)
proc_data$edad <- set_label(x = proc_data$edad, label = "Edad")
frq(proc_data$edad)

## Participación Política
proc_data <- proc_data %>% rename("apoyo_causa" = c08_01,
                                  "part_marcha" = c08_02,
                                  "part_huelga" = c08_03,
                                  "rrss_pol" = c08_04)
proc_data$apoyo_causa <- set_label(x = proc_data$apoyo_causa, label = "Participación: Apoya Causa")
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

## Membresías
proc_data <- proc_data %>% rename("pert_pp" = c12_03)

proc_data$pert_pp <- set_label(proc_data$pert_pp, label = "Pertenece: Partido Político")
get_label(proc_data$pert_pp)

frq(proc_data$pert_pp)

proc_data$pert_pp <- factor(proc_data$pert_pp,
                            levels = c(1, 2, 3),
                            labels = c("No", "Si", "Si"))

## Interés en la política
proc_data <- proc_data %>% rename("interes_pol" = c13)
frq(proc_data$interes_pol)

proc_data$interes_pol <- set_label(proc_data$interes_pol, label = "Interés Político")
get_label(proc_data$interes_pol)

proc_data$interes_pol <- factor(proc_data$interes_pol,
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("Sin interés", "Poco interés", "Poco interés", "Alto interés", "Alto interés"))

## Participación en Movimientos Sociales
proc_data <- proc_data %>% rename("part_movsoc" = c22)
proc_data$part_movsoc <- set_label(x = proc_data$part_movsoc, label = "Participación: Movimientos Sociales")
get_label(proc_data$part_movsoc)
proc_data$part_movsoc <- factor(proc_data$part_movsoc,
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("Poco", "Poco", "Algo", "Mucha", "Mucha"))
frq(proc_data$part_movsoc)

## Autoubicación Política
proc_data <- proc_data %>% rename("autoubicpol" = c15)
proc_data$autoubicpol <- set_label(x = proc_data$autoubicpol, label = "Autoubicación Política")
proc_data$autoubicpol <- car::recode(proc_data$autoubicpol, "c(0,1,2,3,4)=1; c(5)=2; c(6,8,9,10,11)=3")
proc_data$autoubicpol <- factor(proc_data$autoubicpol,
                              labels = c("Izquierda", "Centro", "Derecha"),
                              levels = c(1, 2, 3))
proc_data <- proc_data %>% filter(!is.na(autoubicpol))
frq(proc_data$autoubicpol)

## Desigualdad de ingreso
proc_data <- proc_data %>% rename("percep_desigualdad" = c18_11)
proc_data$percep_desigualdad <- set_label(x = proc_data$percep_desigualdad, label = "Percepción Desigualdad")
proc_data$percep_desigualdad <- car::recode(proc_data$percep_desigualdad, "1:2=1; 3=2; 4:5=3")
proc_data$percep_desigualdad <- factor(proc_data$percep_desigualdad,
                                       levels = c(1, 2, 3),
                                       labels = c("No percibe desigualdad", "Indeciso", "Percibe desigualdad"))
frq(proc_data$percep_desigualdad)

# Descriptivos
stargazer(proc_data, type = "text")

sjmisc::descr(proc_data)
sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")

summarytools::dfSummary(proc_data, plain.ascii = FALSE)
view(dfSummary(proc_data, headings = FALSE))

# Gráficos
graph1 <- proc_data %>%
  select(apoyo_causa, part_marcha, part_huelga, rrss_pol, part_movsoc) %>%
  rename(
    "Participación en Causa" = apoyo_causa,
    "Participación en Marcha" = part_marcha,
    "Participación en Huelga" = part_huelga,
    "Participación en RRSS" = rrss_pol,
    "Participación en Movimientos Sociales" = part_movsoc
  ) %>%
  sjPlot::plot_stackfrq() +
  theme(legend.position = "bottom") +
  labs(
    title = "Participación en diferentes actividades sociales",
    x = "Actividades",
    y = "Frecuencia"
  )

graph1
ggsave(graph1, file = "Output/graph1.png")

graph2 <- proc_data %>%
  ggplot(aes(x = interes_pol, fill = percep_desigualdad)) +
  geom_bar() +
  xlab("Interés Político") +
  ylab("Cantidad") +
  labs(fill = "Percepción Desigualdad") +
  scale_fill_discrete(labels = c('No percibe desigualdad', 'Indeciso', 'Percibe desigualdad')) +
  labs(title = "Relación entre Interés Político y Desigualdad")

graph2
ggsave(graph2, file = "Output/graph2.png")

## Asociación de Variables
dim(proc_data)

sjmisc::descr(proc_data,
              show = c("label", "range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")


##Correlación
proc_data <- mutate_all(proc_data, as.numeric)

M <- cor(proc_data,
         use = "complete.obs")
M

sjPlot::tab_corr(proc_data, 
                 triangle = "lower")

corrplot.mixed(M)

M2 <- cor(dplyr::select(proc_data, percep_desigualdad, apoyo_causa, part_marcha, part_huelga, rrss_pol, pert_pp, interes_pol, part_movsoc, NSE), use = "complete.obs")
corrplot.mixed(M2)

psych::alpha(dplyr::select(proc_data, apoyo_causa, part_marcha, part_huelga, percep_desigualdad,
                           rrss_pol, pert_pp, interes_pol, part_movsoc, NSE, educacion, edad, sexo))


psych::alpha(dplyr::select(proc_data, percep_desigualdad, apoyo_causa, part_marcha, part_huelga, 
                           rrss_pol, pert_pp, part_movsoc))

##Construcción de escala
proc_data <- proc_data %>% 
  rowwise() %>% 
  mutate(participacion_politica = sum(proc_data, apoyo_causa, part_marcha, part_huelga, 
                                      rrss_pol, pert_pp, part_movsoc, interes_pol, autoubicpol))
summary(proc_data$participacion_politica)

ggplot(proc_data, aes(x = participacion_politica)) +
  geom_histogram(binwidth=0.6, colour="black", fill="yellow") +
  theme_bw() +
  xlab("Participación Política") +
  ylab("Cantidad")

## Regresión Lineal
proc_data_reg <- proc_data %>% select(participacion_politica, edad, NSE, sexo, educacion, percep_desigualdad)

M <- cor(proc_data_reg, use = "complete.obs") 
diag(M) = NA 
rownames(M) <- c("A. Participación Política",
                 "B. Edad",
                 "C. NSE",
                 "D. Sexo",
                 "E. Educación",
                 "F. Percepción Desigualdad")
colnames(M) <-c("(A)", "(B)","(C)", "(D)", "(E)","(F)")

corrplot::corrplot(M,
                   method = "color", 
                   addCoef.col = "#000390", 
                   type = "upper", 
                   tl.col = "black", 
                   na.label = "-")

#Variable dependiente: Participación Política
summary(proc_data$participacion_politica)

proc_data_reg <- proc_data_reg %>%
  mutate(sexo = case_when(sexo == 1 ~ 1, #Para Hombres
                          sexo == 2 ~ 0)) #Para Mujeres

#Varaibles independientes: Percepción desigualdad, sexo, NSE, educación, edad
fit01 <- lm(participacion_politica ~ percep_desigualdad + sexo + NSE + educacion + edad, data = proc_data_reg)
fit02 <- lm(participacion_politica ~ percep_desigualdad + sexo + NSE + educacion, data = proc_data_reg)
fit03 <- lm(participacion_politica ~ percep_desigualdad + sexo + NSE, data = proc_data_reg)
fit04 <- lm(participacion_politica ~ percep_desigualdad + sexo, data = proc_data_reg)
fit05 <- lm(participacion_politica ~ percep_desigualdad, data = proc_data_reg)

# Verificar el número de coeficientes en un modelo para asegurarse de que custom.coef.names tenga la longitud correcta
length(coef(fit01))

# Definir los nombres personalizados para los coeficientes
custom_coef_names <- c("Intercepto", "Percepción Desigualdad", "Sexo (mujer)", "NSE Medio/NSE Alto", "Educación", "Edad")

# Mostrar los resultados de los modelos con nombres personalizados para los modelos y coeficientes
screenreg(list(fit01, fit02, fit03, fit04, fit05),
          custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"),
          custom.coef.names = custom_coef_names)

# Predicciones


# Función auxiliar para obtener coeficientes adecuados
get_coef_names <- function(fit, labels) {
  coef_names <- names(coef(fit))
  return(labels[1:length(coef_names)])
}

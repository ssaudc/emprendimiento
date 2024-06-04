library(haven)
library(dplyr)
library(ggplot2)
library(readr)
library(forcats)
library(lubridate)
library(tidyverse)
library(here)
library(cowplot)
library(patchwork)
library(openxlsx)
library(foreign)
library(readxl)
directorio_companias <- read_excel("Bases/directorio_companias.xlsx")


# Seleción de variables

directorio_companias$RUC<-as_factor(directorio_companias$RUC)
directorio_companias$FECHA_CONSTITUCION<-as_factor(directorio_companias$FECHA_CONSTITUCION)
directorio_companias$`SITUACIÓN LEGAL`<-as_factor(directorio_companias$`SITUACIÓN LEGAL`)
directorio_companias$REGIÓN<-as_factor(directorio_companias$REGIÓN)
directorio_companias$PROVINCIA<-as_factor(directorio_companias$PROVINCIA)
directorio_companias$`ÚLTIMO BALANCE`<-as_factor(directorio_companias$`ÚLTIMO BALANCE`)
directorio_companias$TIPO<-as_factor(directorio_companias$TIPO)


dc<-directorio_companias%>%
  select(RUC="RUC",
         fc="FECHA_CONSTITUCION",
         actividad=`SITUACIÓN LEGAL`,
         Region="REGIÓN",
         Provincia="PROVINCIA",
         UB="ÚLTIMO BALANCE",
         tipo_empresa="TIPO")

# Volvemos dicotomica la variable de situación legal(Activa/Inactiva)

dc<-dc%>%
  mutate(actividad_b=as.integer(actividad %in% c("ACTIVA")))

# Trabajmos Con la variable fc "Fecha de Creación"

dc <- dc %>%
  mutate(
    fecha_limpio = dmy(fc),          # Convertir `fc` a fechas
    ano = year(fecha_limpio),        # Extraer el año
    mes = month(fecha_limpio),       # Extraer el mes
    dia = day(fecha_limpio))         # Extraer el dia

#Primer grafico de observación de la cantidad de empresas abiertas en el tiempo 2000-2023
#Filtrado por la condicion de actividad


dc_1<-dc%>%
  filter(ano%in%c("2000","2001","2002","2003","2005","2006"
                  ,"2007","2008","2009","2010","2011","2012","2013"
                  ,"2014","2015","2016","2017","2018","2019","2020"
                  ,"2021","2022","2023"))
  

dc_1_suma <- dc_1 %>%
  group_by(ano) %>%
  summarize(empresas_abiertas = sum(actividad_b))

#Lista de colores

colores_personalizados <- c(
  "#FF6347", "#4169E1", "#32CD32", "#FFD700", "#FF8C00", "#8A2BE2", 
  "#48D1CC", "#00FF00", "#00FFFF", "#FF00FF", "#FF1493", "#8B4513", 
  "#800080", "#4682B4", "#808000", "#FF69B4", "#DC143C", "#FFA07A", 
  "#FF7F50", "#00CED1", "#556B2F", "#D2B48C", "#4B0082")

# gráfico de barras

grafico_empresas_abiertas <- ggplot(dc_1_suma, aes(x = factor(ano), y = empresas_abiertas, fill = factor(ano))) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Cantidad de Empresas Abiertas por Año", x = "Año", y = "Cantidad de Empresas") +
  scale_fill_manual(values = colores_personalizados) +
  theme_minimal()

# Mostrar el gráfico
print(grafico_empresas_abiertas)


###########################################################################
#Tabulamos los datos del grafico anterior para saber cuantos datos se descartaron

# Tabular los resultados de actividad_b
tabla_actividad_b <- table(dc_1$actividad_b)

# Mostrar la tabla de manera ordenada
tabla_actividad_b_ordenada <- sort(tabla_actividad_b, decreasing = TRUE)

# Mostrar la tabla ordenada
print(tabla_actividad_b_ordenada)


#######################################################################
#Segundo gráfico de observación de la cantidad de empresas abiertas en el tiempo 2000-2023
#Sin filtrar por la condicion de actividad

dc_total_empresas <- dc %>%
  group_by(ano) %>%
  summarize(total_empresas = n())

dc_total_empresas_f<-dc_total_empresas%>%
     filter(ano%in%c("2000","2001","2002","2003","2005","2006"
                      ,"2007","2008","2009","2010","2011","2012","2013"
                      ,"2014","2015","2016","2017","2018","2019","2020"
                      ,"2021","2022","2023"))

colores_personalizados <- c(
  "#FF6347", "#4169E1", "#32CD32", "#FFD700", "#FF8C00", "#8A2BE2", 
  "#48D1CC", "#00FF00", "#00FFFF", "#FF00FF", "#FF1493", "#8B4513", 
  "#800080", "#4682B4", "#808000", "#FF69B4", "#DC143C", "#FFA07A", 
  "#FF7F50", "#00CED1", "#556B2F", "#D2B48C", "#4B0082")

# gráfico de barras

grafico_empresas_abiertas_total <- ggplot(dc_total_empresas_f, aes(x = factor(ano), y = total_empresas, fill = factor(ano))) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Cantidad de Empresas Abiertas por Año", x = "Año", y = "Cantidad de Empresas") +
  scale_fill_manual(values = colores_personalizados) +
  theme_minimal()

# Mostrar el gráfico
print(grafico_empresas_abiertas_total)

###############################################################################
#Tabulamos los datos del gráfico anterior para observar la diferencia de datos


# Tabular y ordenar los resultados del total de empresas por año
tabla_total_empresas_f_ordenada <- dc_total_empresas_f %>%
  arrange(desc(total_empresas))

# Mostrar la tabla ordenada
print(tabla_total_empresas_f_ordenada)

# Sumar el total de empresas de la tabla filtrada
total_empresas_sum <- dc_total_empresas_f %>%
  summarize(total_empresas_sum = sum(total_empresas))

# Mostrar la tabla ordenada
print(total_empresas_sum)

##################################################################################

#Estadistica descriptiva


# Convertir ano y UB a numéricos
dc_1 <- dc_1 %>%
  mutate(
    ano = as.numeric(as.character(ano)),
    UB = as.numeric(as.character(UB)))

# Calcular los años que una empresa se mantiene abierta
dc_1 <- dc_1 %>%
  mutate(años_abierta = UB - ano)

# Filtrar empresas con años_abierta positivos (las que tienen un último balance posterior a la fecha de creación)
dc_1 <- dc_1 %>%
  filter(años_abierta > 0)

# Calcular el promedio de años que una empresa se mantiene abierta
promedio_años_abierta <- dc_1 %>%
  summarize(promedio_años = mean(años_abierta, na.rm = TRUE))

#Visualizar los años que estuvo abierta cada empresa
print(dc_1 %>% select(RUC, ano, UB, años_abierta))


# Calcular la mediana de años que una empresa se mantiene abierta por año de creación
mediana_años_por_ano <- dc_1 %>%
  group_by(ano) %>%
  filter(UB != 2023) %>%
  summarize(mediana_años_abierta = median(años_abierta, na.rm = TRUE))


# gráfico de la evolución de la mediana de años
grafico_mediana_años <- ggplot(mediana_años_por_ano, aes(x = ano, y = mediana_años_abierta)) +
  geom_line(color = "lightblue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Evolución de la Mediana de Años que se Mantiene Abierta una Empresa",
       x = "Año de Creación",
       y = "Mediana de Años Abierta") +
  theme_minimal()

print(grafico_mediana_años)


# Agrupar por año de creación y calcular la mediana de los años que una empresa se mantiene abierta
estadisticas_empresas <- dc_1 %>%
  group_by(ano) %>%
  summarize(mediana_años_abierta = median(años_abierta, na.rm = TRUE),
            promedio_años_abierta = mean(años_abierta, na.rm = TRUE),
            sd_años_abierta = sd(años_abierta, na.rm = TRUE),
            n = n())


# Crear el gráfico de líneas
grafico_mediana_años_abierta <- ggplot(estadisticas_empresas, aes(x = ano)) +
  geom_line(aes(y = mediana_años_abierta, color = "Mediana"), size = 1) +
  geom_point(aes(y = mediana_años_abierta, color = "Mediana"), size = 2) +
  geom_line(aes(y = promedio_años_abierta, color = "Promedio"), size = 1, linetype = "dashed") +
  geom_point(aes(y = promedio_años_abierta, color = "Promedio"), size = 2, shape = 21, fill = "white") +
  labs(title = "Mediana y Promedio de Años que las Empresas se Mantienen Abiertas por Año de Creación",
       x = "Año de Creación",
       y = "Años que las Empresas se Mantienen Abiertas",
       color = "Estadística") +
  scale_color_manual(values = c("Mediana" = "blue", "Promedio" = "red")) +
  theme_minimal()
print(grafico_mediana_años_abierta)

###################################################################################



# Calcular los años que una empresa se mantiene abierta
dc_1 <- dc_1 %>%
  mutate(años_abierta = UB - ano)

# Filtrar empresas con años_abierta positivos (las que tienen un último balance posterior a la fecha de creación)
dc_1 <- dc_1 %>%
  filter(años_abierta > 0)

# Crear una columna con el año de cierre
dc_1 <- dc_1 %>%
  mutate(ano_cierre = ano + años_abierta)

# Agrupar por año de cierre y año de creación para contar las empresas que cerraron
empresas_cerradas_por_ano <- dc_1 %>%                                
  group_by(ano_cierre, ano) %>%
  summarize(empresas_cerradas = n())%>%
  ungroup()

#En este calculo podemos ver una agrupación de la cantidad de empresas que cerraron cada año,
#para cada cohort de su año de creación (ej. La cantidad de empresas que fueron creadas en el
#año 2000 y que cerraron en el año 2003 es 28)

# Crear el gráfico de la evolución de las empresas cerradas por año de creación
grafico_empresas_cerradas <- ggplot(empresas_cerradas_por_ano, aes(x = ano_cierre, y = empresas_cerradas, color = as.factor(ano))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Cantidad de Empresas Cerradas por Año y Año de Creación",
       x = "Año de Cierre",
       y = "Cantidad de Empresas",
       color = "Año de Creación") +
  theme_minimal() +
  theme(legend.position = "right")

# Mostrar el gráfico
print(grafico_empresas_cerradas)



############################################################################
# Filtrar y calcular años_abierta
dc_1 <- dc_1 %>%
  mutate(años_abierta = UB - ano)
# Filtrar las empresas que se abrieron en los años específicos

años_especificos <- c(2000, 2004, 2008, 2012, 2014, 2016, 2020, 2021)
dc_1_filtrado <- dc_1 %>%
  filter(ano %in% años_especificos)

# Agrupar y contar la cantidad de empresas cerradas por año de cierre y año de creación, dejamos fuera 2023
empresas_cerradas_2 <- dc_1_filtrado %>%
  filter(UB != 2023) %>%
  group_by(ano, UB) %>%
  summarize(cantidad = n()) %>%
  ungroup()

# Crear el gráfico de líneas
grafico_empresas_cerradas <- ggplot(empresas_cerradas, aes(x = UB, y = cantidad, color = factor(ano), group = factor(ano))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Cantidad de Empresas Cerradas por Año y Año de Creación",
       x = "Año de Cierre",
       y = "Cantidad de Empresas",
       color = "Año de Creación") +
  theme_minimal() +
  theme(legend.position = "right")

# Mostrar el gráfico
print(grafico_empresas_cerradas)

#############################################

#Histograma de la distribución de tiempo que se mantienen abiertas las empresas

histograma_empresas <- ggplot(dc_1, aes(x = años_abierta)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución del Tiempo que las Empresas se Mantienen Abiertas",
       x = "Años que las Empresas se Mantienen Abiertas",
       y = "Número de Empresas") +
  theme_minimal()

print(histograma_empresas)



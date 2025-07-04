################################################################################
################################ LICENCIAS MATERNAS ############################
################################################################################
# Autor: Juan Nicolás Rico Avendaño

## Cargar paquetes ----

rm(list=ls())

packageList<-c("tidyverse", "stringr", "RCurl", "glue","rgeos","readr","ggrepel",
               "haven","raster","rgdal", "readxl", "gridExtra","xlsx",
               "XLConnect", "scales","colorspace", "broom","janitor","rlang",
               "samplingbook", "sf", "openxlsx", "RSQLite", "DBI", "networkD3",
               "ggalluvial", "ggplot2", "skimr", "lubridate", "svglite", "httr",
               "jsonlite", "purrr","robotstxt", "XML", "ggdensity", "gganimate",
               "ggpubr", "forecast", "tsibble", "timetk", "broom", "zoo", "nnet",
               "mlogit", "margins", "texreg", "xtable", "pscl","srvyr")

lapply(packageList,require,character.only=TRUE)
options(scipen = 999)

## Área de trabajo ----

path <- "C:/Users/Usuario/OneDrive - ADRES/Bases de datos/Licencias maternas"

## Carga de datos ----

base <- readr::read_csv(glue("{path}/csv/BDUA_licencias_maternas_ok.csv"))

## Pivotear base ----

base_pivot <- base %>% 
  pivot_wider(
    names_from = TPS_RGM_ID,
    values_from = total_dias
    ) %>% 
  dplyr::rename(
    dias_contri = C,
    dias_subsi = S
  ) %>% 
  dplyr::mutate(
    dias_total = dias_contri + dias_subsi
  )

## Calcular porcentaje por tipo de régimen (base 3) ----

base_3 <- base_pivot %>% 
  dplyr::mutate(
    per_contri = case_when(
      periodo == "antes" ~ dias_contri / 300,
      periodo == "licencia" ~ dias_contri / 126,
      T ~ dias_contri / dias_total
    ),
    per_subsi = case_when(
      periodo == "antes" ~ dias_subsi / 300,
      periodo == "licencia" ~ dias_subsi / 126,
      T ~ dias_subsi / dias_total
    )
  ) %>% 
  dplyr::select(
    id_nuevo, periodo, per_contri, per_subsi
  )

## Generar tabla 4 ----

base_4 <- base_3 %>% 
  dplyr::group_by(
    periodo
  ) %>% 
  dplyr::summarise(
    Contributivo = round(sum(per_contri, na.rm = T),0),
    Subsidiado = round(sum(per_subsi, na.rm = T),0)
  )

## Cálculos de matrices ----

# Antes - periodo licencia

matriz_a <- base_4 %>% 
  dplyr::filter(
    periodo != "posterior"
  )

## Exportar tablas en un libro excel ----

libro <- createWorkbook()

addWorksheet(libro, "tabla_2")
addWorksheet(libro, "tabla_3")
addWorksheet(libro, "tabla_4")

# Añadir la información a las hojas del excel

writeData(libro, sheet = "tabla_2", base_pivot)
writeData(libro, sheet = "tabla_3", base_3)
writeData(libro, sheet = "tabla_4", base_4)

saveWorkbook(libro, 
             glue("{path}/xlsx/tabla_licencia_materna_ok_2.xlsx"),
             overwrite = TRUE)

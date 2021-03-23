####
# Visualizacion de mujeres en el trabajo
# Brechas Salariales y Ocupacionales
####


### Bibliotecas
library(readr)
library(tidyr)
library(dplyr)
library(plotly)

### Datasets
breIng.annioEdadFte <- read_csv("trabajo/bre_ing_perc_annio__g_edad__fte_ingr_limpio.csv", 
                                locale = locale(decimal_mark = ","))
breIng.annioCalifcup <- read_csv("trabajo/bre_ing_prom_annio__calif_ocup_limpio.csv")
breIng.annioRubro <- read_csv("trabajo/bre_ing_rama_rama__annio_limpio.csv", 
                              locale = locale(decimal_mark = ","))


breOcup.annio <- read_csv("trabajo/bre_ocup_ppal_annio_limpio.csv")
breOcup.annioSexCargo <- read_csv("trabajo/jer_ocup_epf_sexo__annio__jer_ocup_limpio.csv")
breOcup.annioPorcentCargo <- read_csv("trabajo/cargos_supemp_annio__cargo_limpio.csv")


### Preparacion
breIng.annioEdadFte <- rename(select(filter(pivot_wider(breIng.annioEdadFte,
                                                        names_from= fuente_ingresos,
                                                        values_from= brecha_ing),
                                            grupo_edad == "10 – 64"),
                                     c(anio, Laboral)),
                              year = anio, brechaGral = Laboral) 

breIng.annioCalifcup <- rename(pivot_wider(breIng.annioCalifcup,
                                           names_from= calificacion_ocupacional,
                                           values_from = brecha_ing), 
                               year = anio, profesional= Profesional, tecnico = 'Técnico', operativo = Operativo, noCalif = "No calificado")

breIng.annioRubro <- rename(pivot_wider(filter(breIng.annioRubro,
                                               rama != "TOTAL"),
                                        names_from= rama,
                                        values_from = brecha_ing), 
                            year = anio)


### Visualizaciones
fig <- plot_ly(
  type = 'scatter',
  x = breIng.annioEdadFte$year, 
  y = -breIng.annioEdadFte$brechaGral,
  mode = "lines"
) %>%
  layout(
    title = "Brecha Salarial en CABA",
    xaxis = list(title = 'Año'),
    yaxis = list (title = 'Porcentaje',
                  range=c(10,25))
  )
fig


fig2 <- plot_ly(breIng.annioCalifcup,
                type = 'scatter',
                x = ~year, 
                mode = "lines"
) %>%
  layout(
    title = "Brecha Salarial en CABA por Calif",
    xaxis = list(title = 'Año'),
    yaxis = list (title = 'Porcentaje',
                  range=c(10,35))
) %>% 
  add_lines(y = -breIng.annioCalifcup$profesional, name = "Profesional"
) %>% 
  add_lines(y = -breIng.annioCalifcup$tecnico, name = "Tecnico"
) %>% 
  add_lines(y = -breIng.annioCalifcup$operativo, name = "Operativo"
) %>% 
  add_lines(y = -breIng.annioCalifcup$noCalif, name = "No calificado")
fig2

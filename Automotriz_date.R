library(lubridate)
library(tidyr)
library(readr)
library(dplyr)

clientes <- read_csv("../Desktop/NEOLAND DAVID/5. R/5. Lubridate y Purrr/Ejercicio/clientes.csv")
ventas <- read_csv("../Desktop/NEOLAND DAVID/5. R/5. Lubridate y Purrr/Ejercicio/ventas.csv")
comisiones <- read_csv("../Desktop/NEOLAND DAVID/5. R/5. Lubridate y Purrr/Ejercicio/comisiones.csv")
modelos <- read_csv("../Desktop/NEOLAND DAVID/5. R/5. Lubridate y Purrr/Ejercicio/modelos.csv")


# Resolución --------------------------------------------------------------------

# ej 1 ----

ventas %>% 
  mutate(
    year_facturacion = year(ymd(FechaVenta))
  ) %>% 
  group_by(year_facturacion) %>% 
  summarise(
    Facturacion = mean(Precio)
  )

# ej 2 ----

ventas %>% 
  mutate(
    mes_facturacion = month(ymd(FechaVenta), label = TRUE, abbr = FALSE)
  ) %>% 
  group_by(mes_facturacion) %>% 
  summarise(
    Facturacion = mean(Precio)
  ) %>% 
  arrange(desc(Facturacion)) %>% 
  head(3)

# ej 3 ----

# opcion 1
ventas %>% 
  mutate(
    demora =  FechaEntregaCoche - FechaPrimerContacto
  ) %>% 
  arrange(desc(demora)) %>% 
  head(1)

# opcion 2
ventas %>% 
  mutate(
    demora =  FechaEntregaCoche - FechaPrimerContacto
  ) %>%
  filter(demora == max(demora)) %>% View()


ventas %>% 
  mutate(
    dias_diferencia = difftime(FechaEntregaCoche, FechaPrimerContacto, units = "days")
  ) %>% 
  arrange(desc(dias_diferencia)) %>% 
  head(1) %>% View()



# ej 4 ----

ventas %>% 
  mutate(
    dia_semana = wday(FechaVenta, label = TRUE, abbr = FALSE)
  ) %>% 
  group_by(dia_semana) %>% 
  summarise(Facturacion = mean(Precio))


# ej 5 ----
# limpio tabla comisiones como en ejercicio anterior
comisiones <- comisiones %>% 
  separate_wider_delim(
    cols  = Coche,
    names = c("Marca", "Modelo"),
    delim = " - "
  ) %>% 
  pivot_longer(
    cols = c(3:27),
    names_to = "Year",
    values_to = "Comision"
  ) %>% 
  separate_wider_delim(
    cols = Comision,
    names = c("ComisionVariable", "ComisionFija"),
    delim = "/"
  ) %>% 
  mutate(
    across(c("Year", "ComisionFija", "ComisionVariable"), as.double)
  )

# calculo ejercicio

ventas %>% 
  left_join(modelos, by = "IdModelo") %>% 
  left_join(comisiones, by = c("Modelo", "Marca", "Year")) %>% 
  mutate(
    dia_semana = wday(FechaVenta),
    dia_semana_texto = wday(FechaVenta, label = TRUE),
    fin_de_semana = case_when(
      dia_semana %in% c(1,7) ~ "Fin de semana",
      dia_semana %in% c(2,3,4,5,6) ~ "Dia de semana",
      TRUE ~ "No se"
    )
  ) %>% 
  group_by(fin_de_semana) %>% 
  summarise(
    Promedio = mean(ComisionFija),
    Cantidad = n()
  )


# ej 6 ----
ventas %>% 
  left_join(modelos, by = "IdModelo") %>% 
  group_by(Year = year(FechaVenta), Marca) %>% 
  summarise(
    Facturacion = sum(Precio)
  ) %>% 
  pivot_wider(
    names_from = Year,
    values_from = Facturacion
  ) %>% View()

# ej 7 ----

ventas %>% 
  mutate(
    lead_time = FechaEntregaCoche - FechaVenta,
    cuanto_tardo = case_when(
      lead_time <= 7 ~ "Cumple plazo",
      TRUE ~ "No cumple plazo"
    )
  ) %>% 
  group_by(cuanto_tardo) %>% 
  summarise(
    Cantidad = n(),
    Porcentaje = n()/nrow(ventas) * 100
  )

# ej 8 ----

ventas %>% 
  mutate(
    year_venta = year(FechaVenta),
    mes_venta  = month(FechaVenta, label = TRUE, abbr = FALSE),
    lead_time = FechaEntregaCoche - FechaVenta,
    cuanto_tardo = case_when(
      lead_time <= 7 ~ "Cumple plazo",
      TRUE ~ "No cumple plazo"
    )
  ) %>% 
  filter(cuanto_tardo == "Cumple plazo") %>% 
  group_by(year_venta, mes_venta) %>% 
  summarise(
    Cantidad = n()
  ) %>% 
  pivot_wider(
    values_from = Cantidad,
    names_from = mes_venta
  ) %>% 
  View()

# ej 9 ----

# Parte 1
ventas %>% 
  mutate(
    demora = FechaVenta - FechaCompraCoche
  ) %>% 
  summarise(
    Promedio = mean(demora)
  )

# Parte 2
ventas %>% 
  left_join(modelos, by = "IdModelo") %>% 
  mutate(
    demora = FechaVenta - FechaCompraCoche
  ) %>% 
  group_by(Marca) %>% 
  summarise(
    Promedio = mean(demora)
  ) %>% 
  arrange(desc(Promedio)) %>% 
  head(1)


# ej 10 ----
clientes %>% 
  mutate(
    Edad = (today() - FechaNacimiento)/dyears(1),
    Edad = as.integer(Edad) # lo uso para redondear para abajo y que sean enteros las edades
  )

# ej 11 ----

clientes %>% 
  mutate(
    Year_next_b = case_when(
      month(FechaNacimiento) > month(today()) ~ 2024,
      (month(FechaNacimiento) == month(today())) & (day(FechaNacimiento) > day(today())) ~ 2024,
      TRUE ~ 2025
    ),
    prox_cumpleanios = make_date(Year_next_b, month(FechaNacimiento), day(FechaNacimiento)),
    dias_para_cumple = prox_cumpleanios - today()
  ) %>% 
  View()

clientes %>% 
  mutate(
    prox_cumpleanios = make_date(2024, month(FechaNacimiento), day(FechaNacimiento)),
    dias_para_cumple = prox_cumpleanios - today(),
    dias_para_cumple = case_when(
      dias_para_cumple < 0 ~ dias_para_cumple + 365,
      TRUE ~ dias_para_cumple
    )
  )
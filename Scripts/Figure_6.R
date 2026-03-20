#Figure 6

base <- read_excel("distribucion.xls")

mapa <- base %>% 
  group_by(Año, Provincia) %>%
  summarise(Total_Tratamientos = sum(Tratamientos, na.rm = TRUE)) %>%
  ungroup()

cod_prov <- read_excel("cod_prov.xlsx")

mapa <- mapa %>% 
  left_join(cod_prov, by = "Provincia")

mapa <- mapa %>% 
  dplyr::select(Año, Provincia, Total_Tratamientos, provincia, Total_P, cob_pub) %>% 
  dplyr::mutate(tto_pop = Total_Tratamientos/Total_P) %>% 
  dplyr::mutate(tto_pop_pub = Total_Tratamientos/cob_pub)

# # ---- PARÁMETROS GENERALES ----
# anios_seleccionados <- c("2003", "2016", "2023")
# 
# paletas_tratamientos <- list(
#   "Tratamientos" = "PuRd"
# )
# etiquetas_tratamientos <- list(
#   "Tratamientos" = "Tratamientos"
# )
# 
# # ---- SHAPEFILE DE PROVINCIAS ----
# provincias_ar <- geoAr::get_geo("ARGENTINA", "provincia") %>%
#   dplyr::mutate(provincia = sprintf("%02d", as.numeric(codprov_censo)))
# 
# 
# datos_filtrados_unidos <- mapa %>%
#   filter(Año %in% anios_seleccionados) %>%
#   left_join(provincias_ar, by = c("provincia" = "provincia")) %>% # Asumiendo que 'nombre' es la columna de provincias en el shapefile
#   # Convertir a un objeto sf después del join es buena práctica si no lo era antes
#   sf::st_as_sf()
# 
# 
# max_tratamientos_global <- max(datos_filtrados_unidos$tto_pop_pub, na.rm = TRUE)
# min_tratamientos_global <- min(datos_filtrados_unidos$tto_pop_pub, na.rm = TRUE)
# 
# 
# mapas_tratamientos <- list()
# 
# for (year in anios_seleccionados) {
#   # Filtrar los datos para el año actual
#   data_for_year <- datos_filtrados_unidos %>%
#     filter(Año == year)
#   
#   # Crear el mapa
#   p <- ggplot(data = data_for_year) +
#     geom_sf(aes(fill = tto_pop_pub), color = "gray20", linewidth = 0.1) +
#     scale_fill_distiller(
#       palette = paletas_tratamientos[["Tratamientos"]],
#       direction = 1,
#       name = etiquetas_tratamientos[["Tratamientos"]],
#       limits = c(min_tratamientos_global, max_tratamientos_global),
#       labels = scales::label_number(big.mark = ".", decimal.mark = ",")
#     ) +
#     labs(
#       title = paste0("Total de Tratamientos \n por Provincia - Año ", year),
#       subtitle = "Argentina"
#     ) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(hjust = 0.5, face = "bold"),
#       plot.subtitle = element_text(hjust = 0.5),
#       legend.position = "right",
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.text = element_blank(),
#       axis.title = element_blank(),
#       axis.ticks = element_blank()
#     ) +
#     coord_sf(default_crs = NULL) # O puedes probar coord_sf(lims_method = "geometry_bbox")
#   
#   
#   mapas_tratamientos[[as.character(year)]] <- p
# }

# 
# # Para mostrar los mapas (puedes verlos uno por uno o usar patchwork/cowplot para combinarlos)
# # Si quieres ver el mapa del 2003:
# print(mapas_tratamientos[["2003"]])
# 
# # Si quieres ver el mapa del 2016:
# print(mapas_tratamientos[["2016"]])
# 
# # Si quieres ver el mapa del 2023:
# print(mapas_tratamientos[["2023"]])
# 
# # Para combinarlos en una sola vista (requiere instalar y cargar patchwork)
# mapas_tratamientos[["2003"]] + mapas_tratamientos[["2016"]] + mapas_tratamientos[["2023"]] +
#   plot_layout(ncol = 3) # O la disposición que prefieras







# ---- PARÁMETROS GENERALES ----
# Solo necesitamos el año 2023 para esta tarea
anio_seleccionado <- "2023"

# Definimos las variables que queremos mapear
variables_a_mapear <- c("Total_Tratamientos", "tto_pop", "tto_pop_pub")

# Paletas y etiquetas (puedes ajustar según sea necesario)
paletas_mapa <- list(
  "Total_Tratamientos" = "Greens",
  "tto_pop" = "Greens", # Cambié para diferenciarlos
  "tto_pop_pub" = "Greens"  # Cambié para diferenciarlos
)

etiquetas_mapa <- list(
  "Total_Tratamientos" = "Total Treatments",
  "tto_pop" = "Treatments per population",
  "tto_pop_pub" = "Treatments per population \n with public coverage"
)

# ---- SHAPEFILE DE PROVINCIAS ----
# Cargar el shapefile (asegúrate de que geoAr esté configurado correctamente)
provincias_ar <- geoAr::get_geo("ARGENTINA", "provincia") %>%
  dplyr::mutate(provincia = sprintf("%02d", as.numeric(codprov_censo)))

# ---- FILTRAR Y UNIR DATOS PARA EL AÑO 2023 ----
datos_2023_unidos <- mapa %>%
  filter(Año == anio_seleccionado) %>%
  left_join(provincias_ar, by = c("provincia" = "provincia")) %>%
  sf::st_as_sf()


# ---- GENERACIÓN DE MAPAS POR VARIABLE ----
mapas_generados <- list()

for (var in variables_a_mapear) {
  # Calcular los límites min y max para la variable actual
  min_val <- min(datos_2023_unidos[[var]], na.rm = TRUE)
  max_val <- max(datos_2023_unidos[[var]], na.rm = TRUE)
  
  # Crear el mapa para la variable actual
  p <- ggplot(data = datos_2023_unidos) +
    geom_sf(aes(fill = .data[[var]]), color = "gray20", linewidth = 0.1) + # Usar .data[[var]] para seleccionar la columna dinámicamente
    scale_fill_distiller(
      palette = paletas_mapa[[var]],
      direction = 1,
      name = etiquetas_mapa[[var]],
      limits = c(min_val, max_val), # Límites específicos para cada variable
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    labs(
      title = paste0(etiquetas_mapa[[var]], " \n by Province - Year ", anio_seleccionado),
      subtitle = "Argentina"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ) +
    coord_sf(default_crs = NULL)
  
  mapas_generados[[var]] <- p
}

# --- MOSTRAR LOS MAPAS ---
# Puedes ver cada mapa individualmente:
print(mapas_generados[["Total_Tratamientos"]])
print(mapas_generados[["tto_pop"]])
print(mapas_generados[["tto_pop_pub"]])

# Para combinarlos en una sola vista usando patchwork:
mapas <- mapas_generados[["Total_Tratamientos"]] +
  mapas_generados[["tto_pop"]] +
  mapas_generados[["tto_pop_pub"]] +
  plot_layout(ncol = 3) # O la disposición que prefieras (e.g., nrow = 1)

ggsave(
  filename = "Figure_6.tiff",
  plot = mapas,
  device = "tiff",
  width = 12,
  height = 5,
  units = "in",
  dpi = 300
)

#Figure 6

base <- read_excel("Data/distribucion.xls")

mapa <- base %>% 
  group_by(Año, Provincia) %>%
  summarise(Total_Tratamientos = sum(Tratamientos, na.rm = TRUE)) %>%
  ungroup()

cod_prov <- read_excel("Data/cod_prov.xlsx")

mapa <- mapa %>% 
  left_join(cod_prov, by = "Provincia")

mapa <- mapa %>% 
  dplyr::select(Año, Provincia, Total_Tratamientos, provincia, Total_P, cob_pub) %>% 
  dplyr::mutate(tto_pop = Total_Tratamientos/Total_P) %>% 
  dplyr::mutate(tto_pop_pub = Total_Tratamientos/cob_pub)


anio_seleccionado <- "2023"

# Definimos las variables 
variables_a_mapear <- c("Total_Tratamientos", "tto_pop", "tto_pop_pub")

# Paletas y etiquetas
paletas_mapa <- list(
  "Total_Tratamientos" = "Greens",
  "tto_pop" = "Greens", 
  "tto_pop_pub" = "Greens"  
)

etiquetas_mapa <- list(
  "Total_Tratamientos" = "Total Treatments",
  "tto_pop" = "Treatments per population",
  "tto_pop_pub" = "Treatments per population \n with public coverage"
)

# Cargar el shapefile 
provincias_ar <- geoAr::get_geo("ARGENTINA", "provincia") %>%
  dplyr::mutate(provincia = sprintf("%02d", as.numeric(codprov_censo)))

datos_2023_unidos <- mapa %>%
  filter(Año == anio_seleccionado) %>%
  left_join(provincias_ar, by = c("provincia" = "provincia")) %>%
  sf::st_as_sf()


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

fig6 <- read_excel("Data/Entregas y medicamentos.xlsx")

# 1. Preparación de los datos: Identificar los puntos donde el VDM cambia
datos_para_geom_text <- fig6 %>%
  arrange(`Entrega N°`) %>%
  mutate(
    VDM_anterior = lag(`Medicamentos en VDM`),
    etiqueta_VDM = if_else(
      is.na(VDM_anterior) | (`Medicamentos en VDM` != VDM_anterior),
      as.character(`Medicamentos en VDM`),
      NA_character_
    )
  )

# 2. Identificar los puntos de corte entre fases
# Necesitas encontrar el número de entrega correspondiente a cada cambio de fase
fases <- data.frame(
  año_corte = c(2007, 2015, 2019),  # Años donde termina cada fase
  fase_nombre = c("2002-2007", "2008-2015", "2016-2019")
)

# Encuentra el último número de entrega de cada año de corte
cortes_entrega <- fig6 %>%
  filter(AÑO %in% c(2007, 2015, 2019)) %>%
  group_by(AÑO) %>%
  summarise(ultima_entrega = max(`Entrega N°`)) %>%
  pull(ultima_entrega)

# 3. Crea el gráfico
mi_grafico_entrega <- datos_para_geom_text %>%
  ggplot(aes(x = `Entrega N°`)) +
  
  # NUEVO: Líneas verticales punteadas para separar fases
  geom_vline(xintercept = cortes_entrega + 0.5,  # +0.5 para ponerla entre entregas
             linetype = "dashed", 
             color = "gray50", 
             linewidth = 0.6,
             alpha = 0.7) +
  
  # A. Línea de pasos (azul)
  geom_step(aes(y = `Medicamentos en VDM`, color = "Total en VDM"),
            linewidth = 1.2) +
  
  # A'. Etiquetas para la Línea Azul (Solo cuando cambia)
  geom_text(aes(y = `Medicamentos en VDM`,
                label = etiqueta_VDM),
            vjust = -0.5,
            hjust = 0.5,
            color = "#7570b3",
            size = 2.5) +
  
  # B. Línea y puntos (marrón)
  geom_line(aes(y = `Medicamentos enviados`, color = "Medicamentos enviados"),
            linewidth = 0.8) +
  geom_point(aes(y = `Medicamentos enviados`, color = "Medicamentos enviados"),
             size = 1, shape = 21, fill = "white") +
  
  # B'. Etiquetas para la Línea Marrón
  geom_text(aes(y = `Medicamentos enviados`,
                label = `Medicamentos enviados`),
            vjust = 1.5,
            hjust = 0.5,
            color = "#d95f02",
            size = 2.5) +
  
  # D. Configuración de Ejes y Títulos
  scale_color_manual(
    name = "Medicines",
    values = c("Total en VDM" = "#7570b3", "Medicamentos enviados" = "#d95f02"),
    labels = c("Distributed per Delivery", "VDM targets")
  ) +
  
  scale_y_continuous(
    name = "Total medicines",
    limits = c(0, max(fig6$`Medicamentos en VDM`, fig6$`Medicamentos enviados`) + 5)
  ) +
  
  scale_x_continuous(
    name = "Delivery year",
    breaks = fig6 %>%
      distinct(AÑO, .keep_all = TRUE) %>%
      pull(`Entrega N°`),
    labels = fig6 %>%
      distinct(AÑO, .keep_all = TRUE) %>%
      pull(AÑO)
  ) +
  
  labs(
    title = "VDM targets and distributed medicines per year"
  ) +
  
  # E. Tema y apariencia
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )

print(mi_grafico_entrega)


ggsave(
  filename = "Figure_5.tiff",
  plot = mi_grafico_entrega,
  device = "tiff",
  width = 12,
  height = 5,
  units = "in",
  dpi = 300
)



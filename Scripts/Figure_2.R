library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)
library(gt)
library(cowplot)
library(ggrepel)
library(readxl)
library(patchwork)

#Load databases
#load("datosFull_presu.Rdata")
load("Data/datos_programa.Rdata")
load("Data/datos_remediar_med.Rdata")

#calculo presupuesto remediar por año
presu_remediar <- datos_programa %>%
  group_by(ano) %>%
  summarise(vigente_total_remediar = sum(credito_vigente, na.rm = TRUE))

#### Tabla 3. Distribución de FF por año del total de REMEDIAR (sin inciso 1)
datos_programa$fuente_financiamiento_id[datos_programa$fuente_financiamiento_id == "1.1"] <- "National Treasury Funds"
datos_programa$fuente_financiamiento_id[datos_programa$fuente_financiamiento_id == "2.2"] <- "International Funding"
datos_programa$fuente_financiamiento_id[datos_programa$fuente_financiamiento_id == "1.4" | datos_programa$fuente_financiamiento_id == "1.5"] <- "Other"

tabla3 <- datos_programa %>%
  group_by(ano, fuente_financiamiento_id) %>%
  summarise(vigente_total_remediar = sum(credito_vigente, na.rm = TRUE))

tabla3 <- datos_programa %>%
  group_by(ano, fuente_financiamiento_id) %>%
  summarise(vigente_total_remediar = sum(credito_vigente, na.rm = TRUE), .groups = 'drop_last') %>%
  group_by(ano) %>%
  mutate(
    porcentaje_fuente = vigente_total_remediar / (sum(vigente_total_remediar, na.rm = TRUE)) * 100
  ) %>%
  ungroup()


colores_personalizados <- c(
  "International Funding" = "#DB7EB3",
  "National Treasury Funds" = "#5EC3C1",
  "Other" = "#7570b3"
)

graf_tabla3 <- ggplot(tabla3, aes(x = ano, y = porcentaje_fuente, fill = factor(fuente_financiamiento_id))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = colores_personalizados,
    name = "Funding source" 
  ) +
  scale_x_continuous(breaks = unique(tabla3$ano)) +
  labs(
    title = "REMEDIAR Budget Distribution by Funding Source by Year",
    x = "Year",
    y = "REMEDIAR budget",
    fill = "Funding source"
  ) +
theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  )


### Tabla 4. Presupuesto REMEDIAR por año

load("Data/datos_infl.RData")
datos_infl <- datos_infl %>%
   dplyr::filter(
     ifelse(ano == 2019, jurisdiccion_id == 85, jurisdiccion_id == 80)
   ) %>%
   dplyr::filter(inciso_id != 1) %>%
   dplyr::filter(
     (ano >= 2002 & ano <= 2010 & programa_id == 30 & actividad_id == 3) |
       (ano >= 2011 & ano <= 2017 & programa_id == 29 & actividad_id == 5) |
       (ano >= 2018 & ano <= 2020 & programa_id == 29 & actividad_id == 45) |
       (ano >= 2021 & ano <= 2024 & programa_id == 29 & actividad_id %in% c(15, 45, 49, 50))
   )

 tabla4 <- datos_infl %>%
   group_by(ano) %>%
   summarise(vigente_total_remediar_usd = sum(credito_vigente_usd, na.rm = TRUE),
             vigente_total_remediar_precios = sum(credito_vigente_precios_2024, na.rm = TRUE)
   )

 graf_tabla4a <- ggplot(tabla4, aes(x = ano, y = vigente_total_remediar_usd)) +
   geom_line(color = "steelblue", size = 1) +
   geom_point(color = "steelblue", size = 2) +
   geom_text(
     aes(label = sprintf("%.2f", vigente_total_remediar_usd)),
     vjust = -0.8,
     hjust = 0.5,
     size = 3,
     color = "black"
   ) +
   labs(
     title = "Current REMEDIAR Credit by Year in USD",
     x = "Year",
     y = "Current credit in USD"
   ) +
   scale_x_continuous(breaks = unique(tabla4$ano)) +
   theme_minimal() +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1),
     plot.title = element_text(size = 14),
     axis.title = element_text(size = 10)
   )

final <- graf_tabla4a+
  graf_tabla3 +
  plot_layout(ncol = 2) 


ggsave(
  filename = "Figure_2.tiff",
  plot = final,
  device = "tiff",
  width = 12,
  height = 5,
  units = "in",
  dpi = 300
)



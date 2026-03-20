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

load("Data/datos_remediar_med.RData")
datos_remediar_med$fuente_financiamiento_id[datos_remediar_med$fuente_financiamiento_id == "1.1"] <- "National Treasury Funds"
datos_remediar_med$fuente_financiamiento_id[datos_remediar_med$fuente_financiamiento_id == "2.2"] <- "International Funding"
datos_remediar_med$fuente_financiamiento_id[datos_remediar_med$fuente_financiamiento_id == "1.4" | datos_remediar_med$fuente_financiamiento_id == "1.5"] <- "Other"


#Table 11. Executed credit/current credit (%). By year and funding source.


tabla11 <- datos_remediar_med %>%
  group_by(ano, fuente_financiamiento_id) %>%
  summarise(
    total_credito_vigente = sum(credito_vigente, na.rm = TRUE),
    total_credito_devengado = sum(credito_devengado, na.rm = TRUE)   
  ) %>%
  mutate(
    ejecucion_anual = case_when(
      total_credito_vigente == 0 ~ NA_real_,
      TRUE ~ (total_credito_devengado / total_credito_vigente) * 100 
    )
  )



colores_personalizados <- c(
  "International Funding" = "#DB7EB3",
  "National Treasury Funds" = "#5EC3C1",
  "Other" = "#7570b3"
)

graf_tabla11 <- ggplot(tabla11, aes(x = factor(ano), y = ejecucion_anual, fill = factor(fuente_financiamiento_id))) +
  geom_col(position = position_dodge(width = 0.8, preserve = "single"), 
           width = 0.7) + 
  geom_vline(xintercept = seq(1.5, length(unique(tabla11$ano)) - 0.5, 1), 
             linetype = "dashed", color = "gray50", alpha = 0.7, size = 0.5) +
  geom_text(aes(label = round(ejecucion_anual, 1)), 
            position = position_dodge(width = 0.8), 
            vjust = ifelse(tabla11$ejecucion_anual >= 0, 0.5, 0.5),
            hjust = ifelse(tabla11$ejecucion_anual >= 0, -0.1, 1.1),
            size = 2.5,
            angle = 90) +
  scale_fill_manual(
    values = colores_personalizados, 
    name = "Funding source" 
  ) +
  labs(
    title = " REMEDIAR budget execution in percentage, by year, by source of funding",
    x = "Year",
    y = "Executed credit/current credit (%)",
    fill = "Funding source"
  ) +
 
  scale_x_discrete(breaks = unique(tabla11$ano), 
                   drop = FALSE) + 
  scale_y_continuous(breaks = seq(0, 200, by = 50)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.spacing.x = unit(2, "lines")) 

print(graf_tabla11)


ggsave(
  filename = "Figure_3.tiff",
  plot = graf_tabla11,
  device = "tiff",
  width = 12,
  height = 5,
  units = "in",
  dpi = 300
)


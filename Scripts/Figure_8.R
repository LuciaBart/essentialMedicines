#Figure 8

path_xlsx  <- "PAPER/Data/total_pais_public_2004_2012_2017_usd_trat.xlsx"
sheet_name <- "total_pais_public_serie"

df <- read_excel(path_xlsx, sheet = sheet_name) %>%
  transmute(
    Year = as.character(periodo),
    `Average Monthly Spending (USD)`= as.numeric(ingreso_medio),
    `Out-of-pocket Spending (% of Income)`= as.numeric(porc_ingreso_medio),
    `Treatments Distributed (Millions)`= as.numeric(tratamientos)
  )

long <- df %>%
  pivot_longer(
    cols = -Year,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = factor(
      Metric,
      levels = c(
        "Average Monthly Spending (USD)",
        "Out-of-pocket Spending (% of Income)",
        "Treatments Distributed (Millions)"
      )
    )
  )


metric_cols <- c(
  "Average Monthly Spending (USD)" = "#71A1D1",
  "Out-of-pocket Spending (% of Income)" = "#F39C5A",
  "Treatments Distributed (Millions)" = "#5EC3C1"
)


fmt_val <- function(x) format(round(x, 1), nsmall = 1)

lab_data <- long %>%
  mutate(
    y_lab = Value / 2,
    label_txt = fmt_val(Value)
  )

p_2 <- ggplot(long, aes(x = Year, y = Value, fill = Metric)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    data = lab_data,
    aes(y = y_lab, label = label_txt),
    color = "white", fontface = "bold", size = 4
  ) +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = metric_cols) +
  labs(
    title = "Evolution of Pharmaceutical Spending and Access",
    x = "Period", y = NULL,
    caption = "Note: The treatment value corresponds to the average number of units distributed over the two years of each biennial period."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 8),  
    plot.margin = margin(10, 16, 10, 16)
  )
p_2

ggsave(
  filename = "Figure_8.tiff",
  plot = p_2,
  device = "tiff",
  width = 12,
  height = 5,
  units = "in",
  dpi = 300
)

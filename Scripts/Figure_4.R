library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(grid)


df_raw <- read_excel("Data/Info para cuadro.xlsx", sheet = "Hoja3")

df <- df_raw %>%
  rename(
    Funding          = Financiamiento,
    DAYS_PUBLICATION = DIAS_PUBLICACIÓN,
    DAYS_BID_OPENING = DIAS_APERTURA,
    DAYS_AWARD_PO    = DIAS_OC,   
    DAYS_TOTAL       = DIAS_TOTAL
  )


df_long <- df %>%
  select(
    Funding,
    Publication   = DAYS_PUBLICATION,
    `Bid opening` = DAYS_BID_OPENING,
    `Award–PO`    = DAYS_AWARD_PO,
    Total         = DAYS_TOTAL
  ) %>%
  pivot_longer(-Funding, names_to = "Stage", values_to = "DAYS")

df_long <- df_long %>%
  mutate(Funding = dplyr::recode(Funding,
                                 "NACIONAL" = "National Treasury Funds",
                                 "INTERNACIONAL" = "International Funding"))


df_all  <- df_long %>% mutate(Funding = "All")
df_comb <- bind_rows(df_long, df_all) %>%
  mutate(
    Stage   = factor(Stage, levels = c("Publication", "Bid opening", "Award–PO", "Total")),
    Funding = factor(Funding, levels = c("National Treasury Funds", "International Funding", "All"))
  )

colors <- c("National Treasury Funds" = "#5EC3C1", "International Funding" = "#DB7EB3", "All" = "#d95f02")

medians <- df_comb %>%
  group_by(Funding, Stage) %>%
  summarise(median_days = round(median(DAYS, na.rm = TRUE)), .groups = "drop")


range_y <- diff(range(df_comb$DAYS, na.rm = TRUE))
nudge_y <- 0.04 * range_y  # tweak if needed


stage_labels <- c(
  "Publication"  = "Publ.",
  "Bid opening"  = "Open.",
  "Award–PO"     = "Award–PO",
  "Total"        = "Total"
)

p <- ggplot(df_comb, aes(x = Stage, y = DAYS, fill = Funding)) +
  geom_boxplot(width = 0.6, show.legend = FALSE) +
  geom_text(
    data = medians,
    aes(label = median_days, y = median_days + nudge_y),
    size = 2.5, fontface = "bold", vjust = 0
  ) +
  facet_wrap(~Funding, nrow = 1) +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = stage_labels) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0.08, 0.10))
  ) +
  labs(
    title    = "Distribution of Procurement Timelines by Stage and Funding Source",
    subtitle = "Analysis of public procurement processes",
    y = "Duration (days)", x = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text         = element_text(face = "bold", size = 10),
    plot.title         = element_text(face = "bold", hjust = 0.5, size = 12),
    plot.subtitle      = element_text(hjust = 0.5, size = 9),
    axis.text.x        = element_text(face = "bold", size = 8, lineheight = 0.9),
    axis.text.y        = element_text(size = 8),
    axis.title.y       = element_text(size = 8),
    panel.grid.major.x = element_blank(),
    legend.position    = "none",
    panel.spacing.x    = unit(14, "pt"),
    plot.margin        = margin(8, 18, 24, 18)
  ) +
  coord_cartesian(clip = "off")


foot <- ggplot() +
  annotate(
    "text", x = 1, y = 1,
    label = "Stage abbreviations: Publ. = Publication; Open. = Bid opening; Award–PO = Awarding & Purchase Order; Total = End-to-end process",
    size = 2.4, hjust = 0.5
  ) +
  theme_void() +
  xlim(0, 2)

# --- Compose (plot + footnote) ---
final <- p + foot + plot_layout(ncol = 1, heights = c(10, 1))


ggsave(
  filename = "Figure_4.tiff",
  plot = final,
  device = "tiff",
  width = 12,
  height = 5,
  units = "in",
  dpi = 300
)

# Figure 7

install.packages("janitor")
library(janitor)
library(readr)
library(tidyverse)

df_raw <- read_excel("Data/gasto_bolsillo_quintiles.xlsx") %>%
  clean_names() 

df <- df_raw %>%
  mutate(
    quintile = str_squish(quintil),
    gasto = parse_number(as.character(gasto),
                         locale = locale(decimal_mark = ",")),
    year_num = parse_number(momento),
    year = factor(year_num, levels = c(1997, 2004, 2017),
                  labels = c("1997","2004","2017")),
    quintile = factor(quintile,
                      levels = c("I","II","III","IV","V"),
                      ordered = TRUE)
  ) %>%
  select(-year_num)


pal_quint <- c(
  "I"   = "#7F7F7F",  
  "II"  = "#71A1D1",  
  "III" = "#F39C5A",  
  "IV"  = "#5EC3C1",  
  "V"   = "#DB7EB3"  
)


p_line_total <- ggplot(df,
                       aes(x = year,
                           y = gasto,
                           color = quintile,
                           group = quintile)) +
  geom_line(linewidth = 1, alpha = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(values = pal_quint, name = "Quintile") +
  labs(
    title = "Out-of-pocket spending on medicines – Total",
    x = "Year",
    y = "Spending on medicines (% of income)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

p_line_total

ggsave(
  filename = "Figure_7.tiff",
  plot = p_line_total,
  device = "tiff",
  width = 12,
  height = 5,
  units = "in",
  dpi = 300
)


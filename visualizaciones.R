# ==============================================================================
# VISUALIZACIONES: EATON-KORTUM (2024) - Gains from Trade
# Requiere haber ejecutado script.R (genera outputs/eaton_kortum_resultados.csv)
# ==============================================================================

library(tidyverse)

theta <- 4
exponente <- 0.2 / (0.3 * theta)
fuente <- "serif"

res <- read_csv("outputs/eaton_kortum_resultados.csv", show_col_types = FALSE) %>%
  mutate(
    pais = recode(iso3,
      MEX = "México", POL = "Polonia", USA = "Estados Unidos",
      JPN = "Japón", KOR = "Corea del Sur", DEU = "Alemania"
    ),
    grupo = if_else(iso3 %in% c("MEX", "POL"),
                    "Hubs de Nearshoring",
                    "Economías Industriales Tradicionales")
  )

paleta <- c("Hubs de Nearshoring" = "#C8553D",
            "Economías Industriales Tradicionales" = "#2F4B7C")

pie_fuente <- "Fuente: BACI (CEPII) 2024, OECD STAN 2025; cálculos propios. Exponente α/(βθ) = 1/6."

# ---- 1. Gains from Trade (barras horizontales) -------------------------------
g1 <- res %>%
  mutate(pais = fct_reorder(pais, Gains_pct)) %>%
  ggplot(aes(x = Gains_pct, y = pais, fill = grupo)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", Gains_pct)),
            hjust = -0.15, family = fuente, size = 4) +
  scale_fill_manual(values = paleta, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12)),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "Ganancias del comercio en el modelo Eaton-Kortum",
    subtitle = "Gᵢ = 100·[πᵢᵢ^(−1/6) − 1]; ganancia en ingreso/salario real (W/P) respecto a la autarquía",
    x = "Ganancias del comercio (%)", y = NULL, caption = pie_fuente
  ) +
  theme_minimal(base_family = fuente, base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(size = 9, colour = "grey35", hjust = 0),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/gains_from_trade.png", g1, width = 9, height = 5.5, dpi = 300, bg = "white")

# ---- 2. Cuota doméstica vs ganancias ----------------------------------------
curva <- tibble(pi_ii = seq(0.2, 0.8, length.out = 300)) %>%
  mutate(Gains_pct = 100 * (pi_ii^(-exponente) - 1))

etiqueta <- if (requireNamespace("ggrepel", quietly = TRUE)) {
  ggrepel::geom_text_repel(aes(label = pais, colour = grupo), family = fuente,
                           size = 4.2, show.legend = FALSE, box.padding = 0.6,
                           seed = 1)
} else {
  geom_text(aes(label = pais, colour = grupo), family = fuente, size = 4.2,
            vjust = -1, show.legend = FALSE)
}

g2 <- ggplot(res, aes(x = pi_ii, y = Gains_pct)) +
  geom_line(data = curva, colour = "grey30", linetype = "dashed", linewidth = 0.8) +
  geom_point(aes(colour = grupo), size = 3.8) +
  etiqueta +
  scale_colour_manual(values = paleta, name = NULL) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(labels = function(y) paste0(y, "%")) +
  labs(
    title = "Cuota del mercado doméstico y ganancias del comercio",
    subtitle = "Curva teórica: Gᵢ = 100·[πᵢᵢ^(−1/6) − 1], con α/(βθ) = 0.2/(0.3×4) = 1/6",
    x = "Cuota del mercado doméstico (πᵢᵢ)",
    y = "Ganancias del comercio, Gᵢ (%)",
    caption = paste0(pie_fuente, " Línea punteada: relación teórica de Eaton y Kortum (2002).")
  ) +
  theme_minimal(base_family = fuente, base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.caption = element_text(size = 9, colour = "grey35", hjust = 0),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("outputs/home_share_vs_gains.png", g2, width = 9, height = 5.5, dpi = 300, bg = "white")

message("Figuras guardadas en outputs/")

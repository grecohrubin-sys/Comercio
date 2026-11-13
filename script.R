# ==============================================================================
# PROYECTO: EATON-KORTUM & NEARSHORING (2024)
# ==============================================================================

library(tidyverse)

# 1. Identificar rutas de archivos
file_baci   <- list.files("data", pattern = "Y2024.*\\.csv$", recursive = TRUE, full.names = TRUE)
file_codes  <- list.files("data", pattern = "country_codes.*\\.csv$", recursive = TRUE, full.names = TRUE)
file_oecd   <- list.files("data", pattern = "^OECD.*\\.csv$", recursive = TRUE, full.names = TRUE)

# 2. Definir muestra de países (ISO3)
paises_iso <- c("MEX", "USA", "DEU", "POL", "JPN", "KOR")

# 3. Mapeo de códigos de país de BACI
df_codes <- read_csv(file_codes, show_col_types = FALSE)
iso_col  <- names(df_codes)[grep("country_iso3|iso3|code_3", names(df_codes), ignore.case = TRUE)][1]
code_col <- names(df_codes)[grep("country_code|code", names(df_codes), ignore.case = TRUE)][1]

df_paises <- df_codes %>%
  filter(get(iso_col) %in% paises_iso) %>%
  select(country_code = all_of(code_col), iso3 = all_of(iso_col), country_name = matches("^country_name$")) %>%
  filter(!str_detect(country_name, "\\(")) %>%
  distinct(iso3, .keep_all = TRUE)

# 4. Procesar flujos comerciales BACI 2024 (convertir a USD)
message("--> Procesando flujos comerciales BACI 2024...")
baci <- read_csv(file_baci, show_col_types = FALSE)

exports <- baci %>%
  filter(i %in% df_paises$country_code) %>%
  group_by(i) %>%
  summarise(X_i = sum(v, na.rm = TRUE) * 1000)

imports <- baci %>%
  filter(j %in% df_paises$country_code) %>%
  group_by(j) %>%
  summarise(M_i = sum(v, na.rm = TRUE) * 1000)

trade_df <- df_paises %>%
  left_join(exports, by = c("country_code" = "i")) %>%
  left_join(imports, by = c("country_code" = "j")) %>%
  mutate(X_i = replace_na(X_i, 0), M_i = replace_na(M_i, 0))

# 5. Cargar Producción Manufacturera (OECD STAN)
message("--> Procesando producción manufacturera OECD STAN...")
oecd_raw <- read_csv(file_oecd, show_col_types = FALSE)

# Extracción flexible de columna de país y valor
country_var <- names(oecd_raw)[grep("REF_AREA|LOCATION|Country", names(oecd_raw), ignore.case = TRUE)][1]
value_var   <- names(oecd_raw)[grep("OBS_VALUE|Value", names(oecd_raw), ignore.case = TRUE)][1]

# Annual-average exchange rates in local currency per USD
# Source: World Bank indicator PA.NUS.FCRF
fx_annual <- tibble(
  iso3 = c("MEX", "USA", "DEU", "POL", "JPN", "KOR"),
  OECD_year = c(2024L, 2024L, 2024L, 2024L, 2024L, 2023L),
  local_currency_per_usd = c(18.3048583333333, 1, 0.923889546117607,
                             3.98078333333333, 151.366291302751, 1305.6625)
)

oecd_clean <- oecd_raw %>%
  filter(
    get(country_var) %in% paises_iso,
    ACTIVITY == "C",
    MEASURE == "P1",
    PRICE_BASE == "V",
    UNIT_MEASURE == "XDC",
    as.integer(TIME_PERIOD) <= 2024
  ) %>%
  arrange(desc(as.integer(TIME_PERIOD))) %>%
  group_by(iso3 = get(country_var)) %>%
  summarise(
    Y_local = first(get(value_var) * 10^UNIT_MULT),
    OECD_year = as.integer(first(TIME_PERIOD)),
    .groups = "drop"
  ) %>%
  left_join(fx_annual, by = c("iso3", "OECD_year")) %>%
  mutate(Y_i = Y_local / local_currency_per_usd) %>%
  select(iso3, Y_i, OECD_year)

# 6. Consolidación del Modelo Eaton-Kortum
# Exponente de la aplicación manufacturera de Eaton & Kortum (2002):
# alpha / (beta * theta) = 0.2 / (0.3 * 4) = 1/6
alpha <- 0.2
beta  <- 0.3
theta <- 4
exponente <- alpha / (beta * theta)

ek_results <- trade_df %>%
  left_join(oecd_clean, by = "iso3") %>%
  mutate(
    # Absorción Doméstica D_i = Y_i + M_i - X_i
    D_i = Y_i + M_i - X_i,
    # Consumo Interno de Producción Nacional X_ii = Y_i - X_i
    X_ii = Y_i - X_i,
    # Home Share pi_ii = X_ii / D_i
    pi_ii = X_ii / D_i,
    # Gains from Trade (ingreso/salario real W/P): G_i = 100 * [pi_ii^(-1/6) - 1]
    Gains_from_Trade = pi_ii^(-exponente),
    Gains_pct = 100 * (pi_ii^(-exponente) - 1)
  )

# Crear directorio de salidas si no existe
if (!dir.exists("outputs")) dir.create("outputs")

# Guardar resultados en CSV
write_csv(ek_results, "outputs/eaton_kortum_resultados.csv")

print("==================================================")
print(" RESULTADOS DEL MODELO EATON-KORTUM (2024) ")
print("==================================================")
print(ek_results %>% select(iso3, OECD_year, Gains_from_Trade, Gains_pct))
# R/01_data_cleaning.R
# =========================================
# Clean raw input series and merge into TVP dataset
# =========================================

source("R/00_setup.R")

# -----------------------------------------
# 1) Clean repo rate series
# -----------------------------------------
repo_raw <- read_delim(
  "data/raw/repo_rate/repo_rate_raw.csv",
  delim = ";",
  locale = locale(decimal_mark = ",")
)

repo_clean <- repo_raw %>%
  select(Period, repo_rate = Average) %>%
  mutate(
    repo_rate = as.numeric(gsub(",", ".", repo_rate)),
    Date      = parse_date_time(Period, orders = "ym"),
    YearMonth = format(Date, "%Y-%m")
  ) %>%
  filter(Date >= as.Date("2010-01-01") & Date <= as.Date("2024-12-31")) %>%
  select(YearMonth, repo_rate) %>%
  arrange(YearMonth)

write_csv(repo_clean, "data/clean/repo_rate_2010_2024.csv")

# -----------------------------------------
# 2) Clean CPIF year-on-year inflation
# -----------------------------------------
cpif_raw <- read_csv("data/raw/cpif/cpif_raw.csv")

cpif_clean <- cpif_raw %>%
  mutate(
    Date      = as.Date(date),
    YearMonth = format(Date, "%Y-%m"),
    cpif_yoy  = as.numeric(cpif_yoy)
  ) %>%
  filter(YearMonth >= "2010-01", YearMonth <= "2024-12") %>%
  select(YearMonth, cpif_yoy) %>%
  arrange(YearMonth)

write_csv(cpif_clean, "data/clean/cpif_yoy_2010_2024.csv")

# -----------------------------------------
# 3) Clean unemployment rate series
# -----------------------------------------
labor_force_raw  <- read_excel(
  "data/raw/unemployment/LABOR_FORCE_2010-2024_RAW_DATA.xlsx",
  col_names = FALSE
)
unemployment_raw <- read_excel(
  "data/raw/unemployment/UNEMPLOYMENT_2010-2024_RAW_DATA.xlsx",
  col_names = FALSE
)

dates             <- as.character(unlist(labor_force_raw[3, -1]))
labor_force_vals  <- as.numeric(unlist(labor_force_raw[4, -1]))
unemployment_vals <- as.numeric(unlist(unemployment_raw[4, -c(1:3)]))

df_lf    <- tibble(date = dates, labor_force_1000   = labor_force_vals)
df_unemp <- tibble(date = dates, unemployed_1000    = unemployment_vals)

unemployment_clean <- left_join(df_lf, df_unemp, by = "date") %>%
  mutate(
    YearMonth         = str_replace(date, "M", "-"),
    unemployment_rate = 100 * unemployed_1000 / labor_force_1000
  ) %>%
  filter(YearMonth >= "2010-01", YearMonth <= "2024-12") %>%
  select(YearMonth, unemployment_rate) %>%
  arrange(YearMonth)

write_csv(unemployment_clean, "data/clean/unemployment_rate_2010_2024.csv")

# -----------------------------------------
# 4) Clean inflation expectations series
# -----------------------------------------
infl_exp_raw <- read_excel("data/raw/expectations/expectations_raw.xlsx")

expectations_clean <- infl_exp_raw %>%
  rename(Date = 1, cpi_expectation_1y = 2) %>%
  mutate(
    Date               = as.Date(Date),
    YearMonth          = format(Date, "%Y-%m"),
    cpi_expectation_1y = as.numeric(cpi_expectation_1y)
  ) %>%
  filter(YearMonth >= "2010-01", YearMonth <= "2024-12") %>%
  select(YearMonth, cpi_expectation_1y) %>%
  arrange(YearMonth)

write_csv(expectations_clean, "data/clean/expectations_2010_2024.csv")

# -----------------------------------------
# 5) Merge all series into a single TVP dataset
# -----------------------------------------
repo   <- read_csv("data/clean/repo_rate_2010_2024.csv",          show_col_types = FALSE)
cpif   <- read_csv("data/clean/cpif_yoy_2010_2024.csv",           show_col_types = FALSE)
unemp  <- read_csv("data/clean/unemployment_rate_2010_2024.csv",  show_col_types = FALSE)
expect <- read_csv("data/clean/expectations_2010_2024.csv",       show_col_types = FALSE)

tvp_data <- repo %>%
  left_join(cpif,   by = "YearMonth") %>%
  left_join(unemp,  by = "YearMonth") %>%
  left_join(expect, by = "YearMonth") %>%
  mutate(
    Date = as.Date(paste0(YearMonth, "-01"))
  ) %>%
  arrange(Date) %>%
  select(Date, repo_rate, cpif_yoy, unemployment_rate, cpi_expectation_1y)

write_csv(tvp_data, "output/clean_data/TVP_Taylor_input_2010_2024.csv")

# ===========================================
# 6) Sanity checks on merged dataset
# ===========================================

# Reload the final merged dataset
tvp_data <- read_csv(
  "output/clean_data/TVP_Taylor_input_2010_2024.csv",
  show_col_types = FALSE
)

# Check number of rows (should be 180: 15 years × 12 months)
print(paste("Number of observations:", nrow(tvp_data)))
if (nrow(tvp_data) != 180) {
  warning("Unexpected number of observations! Check raw data alignment.")
}

# Check for missing values
na_count <- sum(is.na(tvp_data))
print(paste("Number of missing values:", na_count))
if (na_count > 0) {
  warning("Merged dataset contains missing values!")
}

# Print basic summary statistics for all variables
summary(tvp_data)

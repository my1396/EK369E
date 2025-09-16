library(tidyverse)
library(lubridate)
library(data.table)
library(quantmod)
library(stargazer)
library(latex2exp)

dir_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir_folder) # set as working dir
source("fun_script.R")
getwd()


asset_a <- c(10, 6, 14, 2, 18)
asset_b <- c(12, 16, 8, 4, 20)
ptf <- (asset_a + asset_b) / 2

# correlation
cor(asset_a, asset_b)

# mean
mean(asset_a)
mean(asset_b)
mean(ptf)

# sd
sd(asset_a)
sd(asset_b)
sd(ptf)

(sd(asset_a) + sd(asset_b)) / 2 - sd(ptf)


## Data cleaning ---------------------------------------------------------------
# Fama-French 3-factor data
#   1. Can have empty lines and multiple tables in the same file, preview with excel before loading
#   2. In percentages
#   3. monthly frequency, in YYYYMM format, convert to Date type

factor_data <- read_csv("../data/FF_Research_Data_Factors_raw.csv")
factor_data <- factor_data %>%
    mutate(Date = ymd(paste0(Date, "01")))
factor_data <- factor_data %>% filter(between(Date, ymd("2000-01-01"), ymd("2025-09-01")))
factor_data

f_name <- "../data/FF_3Factors_US_monthly_2000-2025.csv"
# write_csv(data, f_name)

f_name <- "https://raw.githubusercontent.com/my1396/course_dataset/refs/heads/main/FF_3Factors_US_monthly.csv"
factor_data <- read_csv(f_name)
factor_data %>% as.data.table()

# Prepare factor data (convert percentages to decimals)
factor_data <- factor_data %>%
    mutate_at(vars(-Date), ~ . / 100) %>%
    mutate(
        year = year(Date),
        mon = month(Date)
    )

# Descriptive statistics
apply(factor_data[, -1], 2, quick_summary) %>% bind_rows(.id = "Variable")

# Visualize RF
factor_data %>%
    ggplot(aes(x = Date, y = RF)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Risk-Free Rate Over Time", y = "Risk-Free Rate (%)", x = "Date")

## Case study: Function to process stock data
process_stock_data <- function(symbol, start_date = "2000-01-01") {
    # Download stock data
    stock_data <- getSymbols(symbol, src = "yahoo", from = start_date, auto.assign = FALSE)

    # Convert to monthly data and calculate returns
    monthly_data <- stock_data %>% apply.monthly(last)
    monthly_data$return <- monthlyReturn(monthly_data[, 6], type = "arithmetic") # Adjusted close in col 6

    # Convert to tibble with date
    result <- monthly_data %>%
        as_tibble() %>%
        add_column(Date = index(monthly_data), .before = 1) %>%
        mutate(
            year = year(Date),
            mon = month(Date),
            symbol = symbol
        ) %>%
        select(Date, year, mon, symbol, return)

    return(result)
}

# Download and process three stocks
aapl_data <- process_stock_data("AAPL") # Large cap tech (high beta)
slab_data <- process_stock_data("SLAB") # Small cap tech (high beta)
ko_data <- process_stock_data("KO") # Large cap consumer staples (low beta)

# Combine stock data
stock_data <- bind_rows(aapl_data, slab_data, ko_data)

# Merge stock returns with Fama-French factor data
reg_data <- stock_data %>%
    left_join(factor_data[, -1], by = c("year", "mon")) %>%
    mutate(eRi = return - RF) %>%
    rename(rmrf = `Mkt-RF`) %>%
    filter(!is.na(eRi) & !is.na(rmrf))
reg_data

data_dir <- "/Users/menghan/Library/CloudStorage/OneDrive-Norduniversitet/EK369E 2025Fall/Slides/data/"
f_name <- paste0(data_dir, "FF_3Factors_case_study.csv")
write_csv(reg_data, f_name)


# Split data for individual regressions
aapl_reg_data <- reg_data %>% filter(symbol == "AAPL")
slab_reg_data <- reg_data %>% filter(symbol == "SLAB")
ko_reg_data <- reg_data %>% filter(symbol == "KO")

## CAPM ------------------------------------------------------------------------
capm_aapl <- lm(eRi ~ rmrf, data = aapl_reg_data)
capm_slab <- lm(eRi ~ rmrf, data = slab_reg_data)
capm_ko <- lm(eRi ~ rmrf, data = ko_reg_data)

# Display individual summaries
summary(capm_aapl)
summary(capm_slab)
summary(capm_ko)

# Side-by-side comparison using stargazer
stargazer(capm_aapl, capm_slab, capm_ko,
    title = "CAPM Results: Risk Profile Comparison",
    column.labels = c("AAPL (High Beta)", "SLAB (High Beta)", "KO (Low Beta)"),
    covariate.labels = c("Market Risk Premium (Beta)", "Alpha"),
    dep.var.labels = "Excess Return",
    type = "text"
)


## FF 3-Factor Model -----------------------------------------------------------

# Run Fama-French 3-factor model regressions
ff3_aapl <- lm(eRi ~ rmrf + SMB + HML, data = aapl_reg_data)
ff3_slab <- lm(eRi ~ rmrf + SMB + HML, data = slab_reg_data)
ff3_ko <- lm(eRi ~ rmrf + SMB + HML, data = ko_reg_data)

# Display individual summaries - FF3
summary(ff3_aapl)
summary(ff3_slab)
summary(ff3_ko)

# Side-by-side comparison using stargazer - FF3
stargazer(ff3_aapl, ff3_slab, ff3_ko,
    title = "Fama-French 3-Factor Model Results: Risk Profile Comparison",
    column.labels = c("AAPL (High Beta)", "SLAB (High Beta)", "KO (Low Beta)"),
    covariate.labels = c("Market Risk Premium (Beta)", "SMB", "HML", "Alpha"),
    dep.var.labels = "Excess Return",
    type = "text"
)

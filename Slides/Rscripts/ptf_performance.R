## ========================================================================== ##
## ptf_performance.R -----------------------------------------------------------
## ========================================================================== ##
##
## Downloads an actively managed ETF and its benchmark, pairs them with the
## Fama-French risk-free rate, and computes the performance measures used in
## 5_ptf-performance-evaluation.Rmd.
##
## Running it writes a snapshot to Slides/data/etf_case_study_monthly.csv so the
## slides can read a fixed file instead of hitting the network at knit time --
## the numbers then stay put across a semester. Re-run to refresh.

library(quantmod)
library(zoo)
library(readr)
library(ggplot2)
source("/Users/menghan/Library/CloudStorage/OneDrive-Norduniversitet/_shared-resources/scripts/_fig_theme.R")
source("/Users/menghan/Library/CloudStorage/OneDrive-Norduniversitet/EK369E/Slides/Rscripts/fun_script.R")
theme_set(my_theme)

## ========================================================================== ##
## 1. Parameters ---------------------------------------------------------------
## ========================================================================== ##

PTF <- "ARKK" # actively managed fund under evaluation
PTF <- "IWM" # small-cap ETF, for practical exercise

BMK    <- "SPY"           # passive benchmark
START  <- "2014-12-01"    # one month early: the first return needs a prior price
END    <- "2024-12-31"
RF_CSV <- "data/FF_3Factors_US_monthly_2000-2025.csv"
OUT <- sprintf("data/etf_%s_case_study_monthly.csv", PTF)

## ========================================================================== ##
## 2. Monthly returns from daily adjusted prices -------------------------------
## ========================================================================== ##

# Adjusted prices, so dividends and splits are already in the return.
monthly_return <- function(symbol) {
    px <- getSymbols(symbol, src = "yahoo", from = START, to = END,
                     auto.assign = FALSE)
    m  <- to.monthly(Ad(px), indexAt = "lastof", OHLC = FALSE)
    r  <- na.omit(ROC(m, type = "discrete"))   # discrete = simple return
    colnames(r) <- symbol
    r
}

ret_xts <- merge(monthly_return(PTF), monthly_return(BMK), join = "inner")



## ========================================================================== ##
## 3. Attach the risk-free rate ------------------------------------------------
## ========================================================================== ##

# Fama-French publishes RF in percent per month, hence the /100.
ff <- read_csv(RF_CSV, show_col_types = FALSE)
ff$ym <- as.yearmon(ff$Date)

ret <- data.frame(
    ym      = as.yearmon(index(ret_xts)),
    ptf_ret = as.numeric(ret_xts[, 1]),
    bmk_ret = as.numeric(ret_xts[, 2]))
ret <- merge(ret, data.frame(ym = ff$ym, rf = ff$RF / 100), by = "ym")
ret <- ret[order(ret$ym), ]
ret$Date <- as.Date(ret$ym, frac = 1)
ret <- ret[, c("Date", "ptf_ret", "bmk_ret", "rf")]

dir.create("data", showWarnings = FALSE)
write_csv(ret, OUT)
message(sprintf("%d months (%s to %s) written to %s",
                nrow(ret), min(ret$Date), max(ret$Date), OUT))

## ========================================================================== ##
## 4. Cumulative performance plot ----------------------------------------------
## ========================================================================== ##
library(ggsci)
library(PerformanceAnalytics)

ret <- read_csv(OUT, show_col_types = FALSE)
ret
ret_xts <- xts(ret[, -c(1,4)], order.by = ret$Date)
colnames(ret_xts) <- c("ARK Innovation", "S&P 500")
charts.PerformanceSummary(ret_xts)
# using PerformanceAnalytics
charts.PerformanceSummary(ret_xts)
# save to local, sized for a 16:9 slide that also carries a line of text
f_name <- "images/etf_ret_summary.png"
plot_png_base(
    charts.PerformanceSummary(ret_xts, main = "", legend.loc = "topleft",
                              colorset = c("#b2182b", "#43418A")),
    f_name, 9, 5
)

# using ggplot2
cumu_ret <- ret %>%
    transmute(Date = Date,
        ptf_ret = cumprod(1 + ptf_ret)-1, 
        bmk_ret = cumprod(1 + bmk_ret)-1)
cumu_ret <- xts(cumu_ret[, -1], order.by = cumu_ret$Date)
p_cumu <- cumu_ret %>% 
    autoplot(facets = NULL) +
    scale_color_aaas(labels = c(PTF, BMK)) +
    theme(
        legend.position = c(0.2, 0.8),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
    )
p_cumu

ggsave("images/etf_cumu_retu.png", p_cumu,
       width = 8, height = 4, dpi = 300, bg = "white")

## ========================================================================== ##
## 5. Performance measures -----------------------------------------------------
## ========================================================================== ##
# summary statistics for monthly returns
stats <- table.Stats(ret_xts, ci = 0.95, digits = 4)
stats

# Sharpe ratio
rf_xts <- xts(ret$rf, order.by = ret$Date)
colnames(rf_xts) <- "rf"
SharpeRatio(R = ret_xts, Rf = rf_xts)

# Annualized Sharpe ratio
SharpeRatio.annualized(R = ret_xts, Rf = rf_xts, geometric = FALSE)

# Downside risk measures
rbind(
    stats["Stdev",],
    SemiDeviation(ret_xts),
    VaR(ret_xts, p = 0.05),
    ES(ret_xts, p = 0.05)
)


# excess return over risk-free rate
er_xts <- ret_xts - as.numeric(rf_xts)
er_xts %>% colMeans()
apply(er_xts, 2, sd)

# verify the Sharpe ratio calculation
(er_xts %>% colMeans())/(apply(er_xts, 2, sd))

# Rolling performance
chart.RollingPerformance(R = ret_xts, width = 12, FUN = "Return.annualized", legend.loc = "topleft")
chart.RollingPerformance(R = ret_xts, width = 12, FUN = "StdDev.annualized", legend.loc = "topleft")
chart.RollingPerformance(R = er_xts, width = 12, FUN = "SharpeRatio.annualized", legend.loc = "topleft")
# 3-in-1 plot: return, risk, and Sharpe ratio
charts.RollingPerformance(R = ret_xts, width = 12, Rf = rf_xts$rf, legend.loc = "topleft")

# CAPM related measures with table.SFM
table.CAPM(Ra = ret_xts$`ARK Innovation`, Rb = ret_xts$`S&P 500`, Rf = rf_xts$rf, scale = 12, digits = 4)

# risk decomposition
table.SpecificRisk(Ra = ret_xts$`ARK Innovation`, Rb = ret_xts$`S&P 500`, Rf = rf_xts$rf, digits = 4)

# Performance by year
table.AnnualizedReturns(ret_xts$`ARK Innovation`, Rf = rf_xts$rf)

table_by_year <- function(R, Rf) {
    yr <- mapply(function(r, rf) table.AnnualizedReturns(r, Rf = rf),
        split(R, f = "years"), # xts::split.xts
        split(Rf, f = "years"),
        SIMPLIFY = FALSE
    )
    out <- do.call(cbind, yr)
    colnames(out) <- unique(format(index(R), "%Y"))
    # the Rf in the Sharpe row label comes from the first year only -- drop it,
    # since each column was computed against its own year's risk-free rate
    rownames(out)[3] <- "Annualized Sharpe"
    t(out)
}

round(table_by_year(ret_xts$`ARK Innovation`, rf_xts$rf), 3)

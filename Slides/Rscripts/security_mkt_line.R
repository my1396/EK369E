## This script
## Demonstrates the Security Market Line (SML) and effects of changing parameters
##      1. Basic SML with example assets
##      2. Effect of increasing market risk premium (equity risk premium)
##              pivoting around risk-free rate
##      3. Effect of increasing risk-free rate while holding equity risk premium constant
##              parallel upward shift
##      4. Effect of increasing risk-free rate while holding market return constant
##              pivoting around market return, flattening the SML

dir_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir_folder) # set as working dir


## Basic SML -------------------------
## Parameter setup
rf <- 0.0015 # 0.15% monthly
mrp <- 0.0060 # 0.60% monthly expected excess market return

assets <- tibble(
    Ticker = c("KO", "DEF", "AAPL", "SLAB", "GROW"),
    Name = c("Coca-Cola Co.", "Defensive Fund", "Apple Inc.", "Silicon Labs", "Growth ETF"),
    Beta = c(0.40, 0.80, 1.30, 1.60, 1.00),
    Observed_Raw = c(0.0045, 0.0055, 0.0100, 0.0105, 0.0120) # observed average raw returns
)

assets <- assets %>%
    mutate(
        Expected_Raw = rf + Beta * mrp,
        Position = case_when(
            Observed_Raw > Expected_Raw + 1e-6 ~ "Above SML (Undervalued)",
            Observed_Raw < Expected_Raw - 1e-6 ~ "Below SML (Overvalued)",
            TRUE ~ "On SML (Fairly Valued)"
        )
    )
beta_range <- c(0, 1.7)
sml_df <- tibble(Beta = beta_range, Raw_Return = rf + Beta * mrp)

# Calculate alpha for GROW
grow_data <- assets %>% filter(Ticker == "GROW")
grow_alpha <- grow_data$Observed_Raw - grow_data$Expected_Raw

# Calculate market return at beta = 1
market_return <- rf + 1 * mrp

# SML demonstration
p_SML <- ggplot() +
    # Add SML line
    geom_line(data = sml_df, aes(Beta, Raw_Return * 100), color = "#43418A", linewidth = 1) +
    # Observed returns
    geom_point(data = assets, aes(Beta, Observed_Raw * 100, color = Position), size = 3.2) +
    # Expected returns
    geom_point(data = assets, aes(Beta, Expected_Raw * 100), shape = 4, color = "grey40", size = 3) +
    geom_text(
        data = assets, aes(Beta, Observed_Raw * 100, label = Ticker, color = Position),
        nudge_y = 0.18,
        size = 5.5,
        show.legend = FALSE
    ) +
    # Add dashed line at beta = 1 to show market return
    geom_segment(
        aes(x = 0, xend = 1, 
        y = market_return * 100, 
        yend = market_return * 100), 
        linetype = "dashed", 
        color = "grey60", 
        alpha = 0.8) +
    # Add annotations
    annotate("text", x = -0.15, y = rf * 100 + 0.05, 
             label = sprintf("r[f] == %.2f*'%%'", rf * 100),
             hjust = 0.5, vjust = 1.4, size = 5.5, color = "#43418A", parse = TRUE) +
    annotate("text", x = -0.15, y = market_return * 100 + 0.05, 
             label = sprintf("r[M] == %.2f*'%%'", market_return * 100),
             hjust = 0.5, vjust = 1.4, size = 5.5, color = "#43418A", parse = TRUE) +
    # Add point at market return (beta = 1)
    geom_point(aes(x = 1, y = market_return * 100), color = "#43418A", size = 2.5, shape = 16) +
    # Add point at risk free return (beta = 0)
    geom_point(aes(x = 0, y = rf * 100), color = "#43418A", size = 2.5, shape = 16) +
    # Add alpha annotation for GROW
    geom_segment(
        data = grow_data, 
        aes(x = Beta, xend = Beta, y = Expected_Raw * 100, yend = Observed_Raw * 100),
        color = "#1b7837", linetype = "dashed", linewidth = 0.8
    ) +
    annotate("text", 
           x = grow_data$Beta + 0.12, 
           y = (grow_data$Expected_Raw + grow_data$Observed_Raw) * 50, 
           label = paste("α =", sprintf("%.2f%%", grow_alpha * 100)), 
           color = "#1b7837", 
           size = 5.5, 
           hjust = 1.5,
           fontface = "italic") +
    scale_color_manual(
        values = c(
            "Above SML (Undervalued)" = "#1b7837", 
            "Below SML (Overvalued)" = "#b2182b", 
            "On SML (Fairly Valued)" = "grey40"
            )) +
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.02))) + # Add padding to prevent cropping
    labs(
        title = "Security Market Line (Monthly Raw Returns)",
        y = latex2exp::TeX("Expected Raw Return $\\,E[r_i]\\,$ (%)"), x = "Beta (Systematic Risk)",
        color = "Position"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
p_SML



## SML with increased market risk premium --------------------------------------

# Demonstrate effect of increasing market risk premium while keeping rf constant
mrp_high <- 0.0080  # Increase to 0.8% monthly

# Create SML data for both scenarios
beta_range <- c(0, 1.7)
sml_original <- tibble(Beta = beta_range, Raw_Return = rf + Beta * mrp, Scenario = "Original (MRP = 0.6%)")
sml_high_mrp <- tibble(Beta = beta_range, Raw_Return = rf + Beta * mrp_high, Scenario = "Higher MRP (0.8%)")
sml_mrp_comparison <- bind_rows(sml_original, sml_high_mrp)

# Calculate market returns for both scenarios
market_return_original <- rf + 1 * mrp
market_return_high <- rf + 1 * mrp_high

# Create comparison plot
g <- guide_legend(nrow = 1) # define legend aes
breaks <- c("Original (MRP = 0.6%)", "Higher MRP (0.8%)")
labels <- c("Original (ERP = 0.6%)", "Higher ERP (0.8%)") %>% setNames(breaks)

p_SML_mrp_comparison <- ggplot() +
    geom_line(data = sml_mrp_comparison, 
        aes(Beta, Raw_Return * 100, color = Scenario, linetype = Scenario), 
        linewidth = 1.2) +
    # Add points for original assets (only on original SML)
    geom_point(data = assets, aes(Beta, Observed_Raw * 100), color = "grey30", size = 2.5, alpha = 0.7) +
    geom_text(data = assets, 
        aes(Beta, Observed_Raw * 100, label = Ticker), 
        nudge_y = 0.15, size = 4, color = "grey30") +
    # Add market return points for both scenarios
    geom_point(aes(x = 1, y = market_return_original * 100), color = "#43418A", size = 3, shape = 16) +
    geom_point(aes(x = 1, y = market_return_high * 100), color = "#d73027", size = 3, shape = 16) +
    # Add risk-free rate point (same for both)
    geom_point(aes(x = 0, y = rf * 100), color = "black", size = 3, shape = 16) +
    annotate("text", x = 0.05, y = rf * 100 + 0.05, 
             label = sprintf("r[f] == %.2f*'%%'", rf * 100),
             hjust = 1, vjust = 0, size = 5.5, color = "black", parse = TRUE) +
    # Add dashed line at beta = 1 to show market returns
    annotate("text", x = 0.05, y = market_return_original * 100, 
             label = sprintf("r[M-Low] == %.2f*'%%'", market_return_original * 100),
             hjust = 0.7, vjust = 0, 
             size = 5.5, color = "#43418A", 
             parse = TRUE) +
    geom_segment(
        aes(x = 0, xend = 1, 
        y = market_return_original * 100, 
        yend = market_return_original * 100), 
        linetype = "dashed", 
        color = "#43418A", 
        alpha = 0.8) +
    annotate("text", x = 0.05, y = market_return_high * 100, 
             label = sprintf("r[M-High] == %.2f*'%%'", market_return_high * 100),
             hjust = 0.7, vjust = 0, 
             size = 5.5, color = "#d73027",
             parse = TRUE) +
    geom_segment(
        aes(x = 0, xend = 1, 
        y = market_return_high * 100, 
        yend = market_return_high * 100), 
        linetype = "dashed", 
        color = "#d73027", 
        alpha = 0.8) +
    scale_color_manual(
        values = c(
            "Original (MRP = 0.6%)" = "#43418A", 
            "Higher MRP (0.8%)" = "#d73027"), 
        breaks = breaks, 
        labels = labels) +
    scale_linetype_manual(
        values = c(
            "Original (MRP = 0.6%)" = "solid", 
            "Higher MRP (0.8%)" = "dashed"),
        breaks = breaks, 
        labels = labels) +
    guides(color = g, linetype = g) + 
    scale_x_continuous(expand = expansion(mult = c(0.15, 0.1))) +
    labs(
        title = "Effect of Increasing Equity Risk Premium on SML",
        subtitle = "Risk-free rate held constant at 0.15%",
        y = latex2exp::TeX("Expected Raw Return $\\,E[r_i]\\,$ (%)"), 
        x = "Beta (Systematic Risk)",
        color = "Scenario",
        linetype = "Scenario"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

p_SML_mrp_comparison

## SML with increased risk free rate -------------------------------------------
### keeping equity risk premium constant -------------------------------------
# Demonstrate effect of increasing risk-free rate while keeping equity risk premium constant
rf_high <- 0.0025  # Increase risk-free rate to 0.25%
# Keep market risk premium (equity risk premium) constant at 0.6%
mrp_constant <- 0.0060

# Create SML data for both scenarios
beta_range <- c(0, 1.7)
sml_original_rf <- tibble(Beta = beta_range, Raw_Return = rf + Beta * mrp_constant, Scenario = "Original (rf = 0.15%)")
sml_high_rf <- tibble(Beta = beta_range, Raw_Return = rf_high + Beta * mrp_constant, Scenario = "Higher rf (0.25%)")
sml_rf_comparison <- bind_rows(sml_original_rf, sml_high_rf)

# Calculate market returns for both scenarios
market_return_original_rf <- rf + 1 * mrp_constant
market_return_high_rf <- rf_high + 1 * mrp_constant

# Create comparison plot for risk-free rate change
p_SML_rf_comparison <- ggplot() +
    geom_line(data = sml_rf_comparison, aes(Beta, Raw_Return * 100, color = Scenario, linetype = Scenario), 
              linewidth = 1.2) +
    # Add points for original assets (only on original SML)
    geom_point(data = assets, aes(Beta, Observed_Raw * 100), color = "grey30", size = 2.5, alpha = 0.7) +
    geom_text(data = assets, aes(Beta, Observed_Raw * 100, label = Ticker), 
              nudge_y = 0.15, size = 4, color = "grey30") +
    # Add market return points for both scenarios
    geom_point(aes(x = 1, y = market_return_original_rf * 100), color = "#43418A", size = 3, shape = 16) +
    geom_point(aes(x = 1, y = market_return_high_rf * 100), color = "#2166ac", size = 3, shape = 16) +
    # Add risk-free rate points (different for each scenario)
    geom_point(aes(x = 0, y = rf * 100), color = "#43418A", size = 3, shape = 16) +
    geom_point(aes(x = 0, y = rf_high * 100), color = "#2166ac", size = 3, shape = 16) +
    # Add parallel shift indication with arrow
    annotate("segment", x = 0.85, xend = 0.85, 
             y = (rf + 0.85 * mrp_constant) * 100, 
             yend = (rf_high + 0.85 * mrp_constant) * 100,
             arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
             color = "grey50", size = 0.8) +
    annotate("text", x = 0.9, y = ((rf + rf_high)/2 + 0.85 * mrp_constant) * 100, 
             label = "Parallel\nshift", hjust = 0, size = 3.5, color = "grey50") +
    # Add annotations for risk-free rates
    annotate("text", x = 0.05, y = rf * 100 - 0.05, 
             label = sprintf("r[f] == %.2f*'%%'", rf * 100),
             hjust = 0, vjust = 1, size = 4, color = "#43418A", parse = TRUE) +
    annotate("text", x = 0.05, y = rf_high * 100 + 0.05, 
             label = sprintf("r[f] == %.2f*'%%'", rf_high * 100),
             hjust = 0, vjust = 0, size = 4, color = "#2166ac", parse = TRUE) +
    scale_color_manual(values = c("Original (rf = 0.15%)" = "#43418A", "Higher rf (0.25%)" = "#2166ac")) +
    scale_linetype_manual(values = c("Original (rf = 0.15%)" = "solid", "Higher rf (0.25%)" = "dashed")) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(
        title = "Effect of Increasing Risk-Free Rate on SML",
        subtitle = "Equity risk premium held constant at 0.6%",
        y = latex2exp::TeX("Expected Raw Return $\\,E[r_i]\\,$ (%)"), 
        x = "Beta (Systematic Risk)",
        color = "Scenario",
        linetype = "Scenario"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

p_SML_rf_comparison


## SML with increased rf but constant market return ---------------------------

# Demonstrate effect of increasing risk-free rate while keeping market return constant
# This means MRP decreases by the amount of rf increase
rf_high_const_mkt <- 0.0025  # Increase risk-free rate to 0.25%
market_return_constant <- rf + 1 * mrp  # Keep market return at original level (0.75%)
mrp_adjusted <- market_return_constant - rf_high_const_mkt  # Adjusted MRP = 0.75% - 0.25% = 0.50%

# Create SML data for both scenarios
beta_range <- c(0, 1.7)
sml_original_const <- tibble(
    Beta = beta_range, 
    Raw_Return = rf + Beta * mrp, 
    Scenario = "Original (rf=0.15%, MRP=0.6%)")
sml_const_market <- tibble(
    Beta = beta_range, 
    Raw_Return = rf_high_const_mkt + Beta * mrp_adjusted, Scenario = "Higher rf (rf=0.25%, MRP=0.5%)")
sml_const_comparison <- bind_rows(sml_original_const, sml_const_market)

# Create comparison plot for constant market return scenario
p_SML_const_market <- ggplot() +
    geom_line(data = sml_const_comparison, aes(Beta, Raw_Return * 100, color = Scenario, linetype = Scenario), 
              linewidth = 1.2) +
    # Add points for original assets (only on original SML)
    geom_point(data = assets, aes(Beta, Observed_Raw * 100), color = "grey30", size = 2.5, alpha = 0.7) +
    geom_text(data = assets, aes(Beta, Observed_Raw * 100, label = Ticker), 
              nudge_y = 0.15, size = 4, color = "grey30") +
    # Add market return points (same for both scenarios at beta = 1)
    geom_point(aes(x = 1, y = market_return_constant * 100), color = "black", size = 3.5, shape = 16) +
    # Add risk-free rate points (different for each scenario)
    geom_point(aes(x = 0, y = rf * 100), color = "#43418A", size = 3, shape = 16) +
    geom_point(aes(x = 0, y = rf_high_const_mkt * 100), color = "#762a83", size = 3, shape = 16) +
    # Add annotations for risk-free rates
    annotate("text", x = 0.05, y = rf * 100 - 0.05, 
             label = sprintf("r[f] == %.2f*'%%'", rf * 100),
             hjust = 0, vjust = 1, size = 4, color = "#43418A", parse = TRUE) +
    annotate("text", x = 0.05, y = rf_high_const_mkt * 100 + 0.05, 
             label = sprintf("r[f] == %.2f*'%%'", rf_high_const_mkt * 100),
             hjust = 0, vjust = 0, size = 4, color = "#762a83", parse = TRUE) +
    # Add annotation for constant market return
    annotate("text", x = 1.05, y = market_return_constant * 100, 
             label = sprintf("r[M] == %.2f*'%%' ~ (constant)", market_return_constant * 100),
             hjust = 0, vjust = -0.2, size = 4, color = "black", parse = TRUE) +
    # Add MRP annotations
    annotate("text", x = 0.5, y = (rf + 0.5 * mrp) * 100 + 0.15, 
             label = sprintf("MRP = %.1f%%", mrp * 100),
             hjust = 0.5, vjust = 0, size = 3.5, color = "#43418A") +
    annotate("text", x = 0.5, y = (rf_high_const_mkt + 0.5 * mrp_adjusted) * 100 - 0.15, 
             label = sprintf("MRP = %.1f%%", mrp_adjusted * 100),
             hjust = 0.5, vjust = 1, size = 3.5, color = "#762a83") +
    # Add flattening indication with curved arrow
    annotate("segment", x = 1.3, xend = 1.4, 
             y = (rf + 1.3 * mrp) * 100, 
             yend = (rf_high_const_mkt + 1.3 * mrp_adjusted) * 100,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
             color = "grey50", size = 0.6) +
    annotate("text", x = 1.45, y = ((rf + 1.3 * mrp) + (rf_high_const_mkt + 1.3 * mrp_adjusted))/2 * 100, 
             label = "SML\nflattens", hjust = 0, size = 3.2, color = "grey50") +
    scale_color_manual(values = c("Original (rf=0.15%, MRP=0.6%)" = "#43418A", "Higher rf (rf=0.25%, MRP=0.5%)" = "#762a83")) +
    scale_linetype_manual(values = c("Original (rf=0.15%, MRP=0.6%)" = "solid", "Higher rf (rf=0.25%, MRP=0.5%)" = "dashed")) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.2))) +
    labs(
        title = "Effect of Increasing Risk-Free Rate with Constant Market Return",
        subtitle = "Market return held constant → Market risk premium decreases",
        y = latex2exp::TeX("Expected Raw Return $\\,E[r_i]\\,$ (%)"), 
        x = "Beta (Systematic Risk)",
        color = "Scenario",
        linetype = "Scenario"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

p_SML_const_market


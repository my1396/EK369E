dir_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir_folder) # set as working dir
library(tidyverse)

# Generate driftless random walk
set.seed(132)
t <- seq(1, 100, by=1)
et <- rnorm(length(t), mean=0, sd=1) # IID N(0,1) increments
pt <- c(0, cumsum(et))  # driftless random walk
df <- tibble(
  t = c(0, t), # for plotting p_t with p0 included
  et = c(0, et),
  pt = pt
)
df

# Reshape to long format for faceting
df_long <- df %>%
  pivot_longer(cols = c(et, pt), names_to = "series", values_to = "value")

# Optional: make series labels nicer
df_long$series <- factor(df_long$series,
                         levels = c("et", "pt"),
                         labels = c(expression(e[t]), expression(p[t])))

# Plot with facets
p <- ggplot(df_long, aes(x = t, y = value)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  facet_wrap(~series, ncol = 1, scales = "free_y", labeller = label_parsed) +
  labs(x = "t", y = NULL) +
  theme_bw(base_size = 14) +
  theme(strip.text = element_text(size = 14))
# Save the plot as a PNG file
f_name <- "images/RW.png"
# ggsave(f_name, p, width = 9, height = 6)


# Compute indicators and test statistics
It <- ifelse(et > 0, 1, 0)
It_1 <- c(NA, head(It, -1))
data <- tibble(et, It, It_1) %>%
    mutate(yt = It * It_1 + (1 - It) * (1 - It_1))
data

# Calculate CJ test statistic
Ns <- sum(data$yt, na.rm = TRUE)
Nr <- length(t) - Ns -1
CJ_hat <- Ns / Nr
cat("Number of sequences (Ns):", Ns)
cat("Number of reversals (Nr):", Nr)
cat("CJ test statistic:", round(CJ_hat, 3), "
")

# Test for H0: CJ = 1
cat("Number of positive returns:", sum(It))
pi_hat <- sum(It) / length(t)
pis_hat <- Ns / length(t)
sigma2 <- (pis_hat * (1 - pis_hat) + 2 * (pi_hat^3 + (1 - pi_hat)^3 - pis_hat^2)) /
    (length(t) * (1 - pis_hat)^4)
t_stat <- (CJ_hat - 1) / sqrt(sigma2)
p_value <- 2 * pnorm(-abs(t_stat))

cat("Test statistic:", round(t_stat, 3), "
")
cat("P-value:", round(p_value, 3), "
")

## Autocorrelation test -------------------------------------------------------

set.seed(123)
t <- seq(1, 100, by = 1)
et <- rnorm(length(t), mean = 0, sd = 1) # IID N(0,1) increments
pt <- c(0, cumsum(et)) # driftless random walk
# Apply Box-Pierce tests to our simulated data
returns <- diff(pt) # First differences = returns

Box.test(returns, lag = 1, type = "Ljung-Box")


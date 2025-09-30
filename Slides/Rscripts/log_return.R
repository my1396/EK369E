library(ggplot2)
library(latex2exp)
# Data: simple return r_t and continuous return z_t
r_t <- seq(-1, 1, by = 0.001)
z_t <- log(1 + r_t)
df <- data.frame(r_t = r_t, z_t = z_t)

# Plot with ggplot2
ggplot(df, aes(x = r_t, y = z_t)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) +
  labs(
    x = TeX("Simple return $~r_t$"),
    y = TeX("Continuously compounded return $~z_t$")
  ) +
  theme_minimal(base_size = 14)

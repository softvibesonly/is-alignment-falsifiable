library(ggplot2)

dlaplace <- function(x, mu, b) {
  (1 / (2 * b)) * exp(-abs(x - mu) / b)
}

x <- seq(0, 100, length.out = 1000)

# Each pillar is active over a different 80-unit span
intervals <- list(
  c(0, 80),    # Outer Alignment covers 0-80
  c(10, 90),   # Middle Alignment covers 10-90
  c(20, 100)   # Inner Alignment covers 20-100
)

params <- data.frame(
  mu = c(40, 50, 60),         # Center the peaks in each interval
                              # Actually don't mess with the mu anymore because now with the windowing function it um it just breaks lol
  b = c(30, 30, 30),          # Change the visual spread kinda like the SD for a Gaussian
  pillar = c("Outer Alignment", "Middle Alignment", "Inner Alignment")
)

# Build the data with masking
plotdata <- do.call(rbind, lapply(1:3, function(i) {
  interval <- intervals[[i]]
  mask <- x >= interval[1] & x <= interval[2]
  x_sub <- x[mask]
  # Linear windowing: 0 at interval ends, 1 at the center
  w <- 1 - abs(x_sub - params$mu[i]) / ((interval[2] - interval[1]) / 2)
  w[w < 0] <- 0 # (shouldn't be necessary if mu centered)
  # Now scale so peak is at 1
  y <- dlaplace(x_sub, params$mu[i], params$b[i])
  y_scaled <- y / max(y) * w
  data.frame(
    x = x_sub,
    y = y_scaled,
    pillar = params$pillar[i]
  )
}))

ggplot(plotdata, aes(x = x, y = y, color = pillar)) +
  geom_line(size = 0.7) +
  scale_y_continuous(
    limits = c(0, 1.05),          # Go a little above 1.0
    breaks = seq(0, 1, by = 0.5), # Only label at 0, 0.25, ..., 1.0
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    # limits = c(0, 102),
    breaks = seq(0, 100, by = 10),
    expand = c(0, 0)
  ) +
  theme_minimal() +
  labs(
    title = "Risk Coverage From Different Pillars",
    x = "'Failure Space'",
    y = "Relative Attention / Amount of Focus",
    color = "Pillar"
  ) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.4),
    axis.text.x = element_text(size = 7),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold",
                              margin = margin(b=1, t=10)),
    axis.title = element_text(size = 16),
    legend.position = "top",
    legend.title = element_text(size = 14),       # Adjust title text size
    legend.text = element_text(size = 13),
    legend.key.size = unit(1.9, 'lines'),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  ) +
  scale_color_manual(
    name = "Pillar", 
    values = c(
      "Inner Alignment" = "#D55E00",  # burnt orange
      "Middle Alignment" = "#009E73", # green
      "Outer Alignment" = "#0072B2"   # dark-ish blue
    )
  )


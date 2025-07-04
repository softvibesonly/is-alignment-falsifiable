# Load necessary library
library(ggplot2)

# Laplace PDF function
dlaplace <- function(x, mu, b) {
  (1 / (2 * b)) * exp(-abs(x - mu) / b)
}

# Create x values (let's say from -2 to 2 for the illustration)
x <- seq(-2, 2, length.out = 400)

# Parameters for each 'pillar'
params <- data.frame(
  mu = c(0, 0.3, -0.3),
  b = c(0.5, 0.5, 0.5),
  pillar = c("Outer Alignment", "Middle Alignment", "Inner Alignment")
)

# Build the data for plotting
plotdata <- do.call(rbind, lapply(1:3, function(i) {
  data.frame(
    x = x,
    y = dlaplace(x, params$mu[i], params$b[i]),
    pillar = params$pillar[i]
  )
}))

# Plot
ggplot(plotdata, aes(x = x, y = y, color = pillar)) +
  geom_line(size = 0.7) +
  theme_minimal() +
  labs(
    title = "Conceptual Coverage by Three Pillars of Alignment",
    x = "Risk/Failure Space",
    y = "Relative Attention (not literal probability density)",
    color = "Pillar"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "top"
  )


library(ggplot2)

# Laplace PDF
dlaplace <- function(x, mu, b) (1 / (2 * b)) * exp(-abs(x - mu) / b)

# Want: 87.5% overlap between adjacent curves
d <- 10 # spacing between peaks
target_overlap <- 0.875
b_val <- -d / log(target_overlap)  # b ≈ 74.89

# Centers for each pillar
mus <- c(40, 50, 60)

params <- data.frame(
  pillar = c("Outer Alignment", "Middle Alignment", "Inner Alignment"),
  mu = mus,
  b = b_val
)

x <- seq(0, 100, length.out = 1000)

# Build y for each pillar, scale so peak is 1
plotdata <- do.call(rbind, lapply(1:3, function(i) {
  y <- dlaplace(x, params$mu[i], params$b[i])
  data.frame(
    x = x,
    y = y / max(y),  # normalize peak
    pillar = params$pillar[i]
  )
}))

# Plot - filled area + outline
ggplot(plotdata, aes(x = x, y = y, color = pillar)) +
  geom_area(aes(fill = pillar), alpha = 0.15, position = "identity") +
  geom_line(size = 1) +
  scale_color_manual(values = c(
    "Inner Alignment" = "#E69F00", # orange
    "Middle Alignment" = "#009E73", # green
    "Outer Alignment" = "#56B4E9"  # blue
  )) +
  scale_fill_manual(values = c(
    "Inner Alignment" = "#E69F00",
    "Middle Alignment" = "#009E73",
    "Outer Alignment" = "#56B4E9"
  )) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Risk Coverage by Three Pillars of Alignment",
    x = "Failure Space",
    y = "Relative Attention"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "top",
    legend.title = element_blank()
  )


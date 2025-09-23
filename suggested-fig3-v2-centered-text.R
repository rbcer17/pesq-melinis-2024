# Load packages
library(ggplot2)
library(patchwork)

# Create example data
df <- data.frame(
  Year = rep(c(2003, 2004, 2005), times = 2 * 4),
  Cover = c(95, 94, 93, 35, 30, 40,     # Panel A
            91, 90, 92, 50, 30, 35,     # Panel B
            98, 80, 97, 40, 10, 30,     # Panel C
            92, 70, 85, 50, 5, 25),     # Panel D
  SE = c(2, 2, 2, 5, 8, 10,
         2, 3, 2, 10, 7, 6,
         1, 3, 1, 9, 4, 5,
         1, 4, 3, 7, 3, 4),
  Group = rep(c("Black", "White"), each = 3, times = 4),
  Panel = rep(c("Low Coverage", "High Coverage", "Fire May", "Int Mgmt May"), each = 6)
)

# Function to create individual panel
make_panel_plot <- function(panel_id) {
  dat <- subset(df, Panel == panel_id)
  
  ggplot(dat, aes(x = factor(Year), y = Cover, fill = Group)) +
    geom_bar(stat = "identity", position = position_dodge(0.9), color = "black") +
    geom_errorbar(aes(ymin = Cover - SE, ymax = Cover + SE),
                  position = position_dodge(0.9), width = 0.2) +
    scale_fill_manual(values = c("Black" = "black", "White" = "white")) +
    labs(x = "Year", y = "% Cover") +
    ylim(0, 105) +
    ggtitle(panel_id) +  # Use ggtitle for top-centered panel labels
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # centered
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# Create each panel
pA <- make_panel_plot("Low Coverage")
pB <- make_panel_plot("High Coverage")
pC <- make_panel_plot("Fire May")
pD <- make_panel_plot("Int Mgmt May")

# Combine plots in 2x2 layout with patchwork
final_plot <- (pA + pB) / (pC + pD)

# Show the plot
print(final_plot)

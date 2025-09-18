#new code for colorblind boxplots use viridis fill
# If not installed yet:
# install.packages("viridis")

library(ggplot2)
library(viridis)
#option = "D" gives a nice default viridis scheme (colorblind-safe).

ggplot(consol1, aes(x = coletafactor, y = pcviaveis, fill = anofactor)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "D") +  # Discrete viridis palette
  labs(x = "Collection", y = "Percentage of full caryopses", fill = "Year") +
  ggtitle("Low Coverage")
# install.packages("RColorBrewer")
library(RColorBrewer)
#Palettes like "Set2", "Dark2" and "Paired" are generally safe.

ggplot(consol1, aes(x = coletafactor, y = pcviaveis, fill = anofactor)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2") +  # Or "Dark2", "Set3" (colorblind safe)
  labs(x = "Collection", y = "Percentage of full caryopses", fill = "Year") +
  ggtitle("A")

#Replace geom_boxplot() with geom_boxplot_pattern() and add a pattern aesthetic mapped to your grouping variable.
# install.packages("remotes")
remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
ggplot(consol12, aes(x = coletafactor, y = pcviaveis, fill = anofactor, pattern = anofactor)) +
  geom_boxplot_pattern(
    pattern_fill = "black",        # Color of the pattern lines
    pattern_angle = 45,            # Angle of hatching lines
    pattern_density = 0.1,         # Density of pattern lines (0-1)
    pattern_spacing = 0.02,        # Spacing between pattern lines
    pattern_key_scale_factor = 0.6 # Legend pattern size scaling
  ) +
  scale_fill_manual(values = rep("white", length(unique(consol12$anofactor)))) + # white fill so pattern stands out
  labs(x = "Collection", y = "Percentage of full caryopses", fill = "Year", pattern = "Year") +
  ggtitle("A")
#Notes:

#pattern = anofactor tells ggplot to draw different hatch patterns for each "Year" group.

#You can customize pattern_angle, pattern_density, pattern_spacing for look & feel.

#Fill is set to white here so the hatching pattern is visible.

#pattern_fill sets the color of the hatch lines (usually black or gray).

#geom_boxplot_pattern works like geom_boxplot but supports patterns.
#Alternative if you want only black/white hatching fills without colors:
ggplot(consol12, aes(x = coletafactor, y = pcviaveis, pattern = anofactor)) +
  geom_boxplot_pattern(
    fill = "white",
    pattern = "crosshatch",
    pattern_fill = "black",
    pattern_density = 0.15,
    pattern_spacing = 0.02,
    pattern_key_scale_factor = 0.6
  ) +
  labs(x = "Collection", y = "Percentage of full caryopses", pattern = "Year") +
  ggtitle("A")

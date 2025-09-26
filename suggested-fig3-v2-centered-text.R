#EXAMPLE SCRIPT USING theoretical data
#TO MAKE THE PLOT RUN THIS SCRIPT WITH THE REAL DATA AND CALL THE DATAFRAME df
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
df = summary_data2
# Function to create individual panel
make_panel_plot <- function(panel_id) {
  dat <- subset(df, Panel == panel_id)
  
  ggplot(dat, aes(x = factor(Year), y = Cover, fill = Group)) +
    geom_bar(stat = "identity", position = position_dodge(0.9), color = "grey") +
    geom_errorbar(aes(ymin = Cover - SE, ymax = Cover + SE),
                  position = position_dodge(0.9), width = 0.2) +
    scale_fill_manual(values = c("Black" = "grey", "White" = "white")) +
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
# END EXAMPLE SCRIPT

# HOW TO PREPARE THE REAL DATA

library(tidyverse)
#https://www.statology.org/pivot_longer-in-r/
#pivot the data frame into a long format
coberturapivot = as.data.frame(data_only_cobertura_2003_a_2005_sent_04may2020)
coberturapivot1 = coberturapivot %>% pivot_longer(cols=c('total03', 'total04','total05'),
                                                  names_to='ano',
                                                  values_to='cobtotal')
coberturapivot2 = coberturapivot %>% pivot_longer(cols=c('mm03', 'mm04','mm05'),
                                                  names_to='anomm',
                                                  values_to='cobmm')
#remove columns 1 to 5 of coberturapivot2
coberturapivot2 <- coberturapivot2[ -c(1:5) ]
#add columns dataframe coberturapivot2 to coberturapivot1
df2 <- cbind(coberturapivot1, coberturapivot2)
#recode ano to have just numbers
df2$anocal = substr(df2$ano,6,7)
#pivot to have cobertura single variable
df3 = df2 %>% pivot_longer(cols=c('cobtotal', 'cobmm'),
                           names_to='tipocob',
                           values_to='percob')
#remove cols 3 4 5 6 7 from df3
df4 <- df3 [ -c(3:7) ]

#agora vamos usar o dataframe df4 para replicar a figura 3 do paper com sigmaplot
df4$tipocob <- factor(df4$tipocob, levels = c("cobtotal", "cobmm"))
#mydat_tibble <- mydat_tibble %>% 
#  rename(Years_From_Diagnosis = Yrs_From_Dx)

con1 <- subset(df4, trat == 1)
con2 <- subset(df4, trat == 2)
con3 <- subset(df4, trat == 3)
con4 <- subset(df4, trat == 4)
require(gridExtra)
#BOXPLOTS
plot1=ggplot(con1, aes(x=anocal,y=percob, fill = tipocob))+ geom_boxplot()
plot2=ggplot(con2, aes(x=anocal,y=percob, fill = tipocob))+ geom_boxplot()
plot3=ggplot(con3, aes(x=anocal,y=percob, fill = tipocob))+ geom_boxplot()
plot4=ggplot(con4, aes(x=anocal,y=percob, fill = tipocob))+ geom_boxplot()

plot1
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)

#BARPLOT WITH ERROR BARS FIGURE 3 FINAL
#calculate mean values of y
library(dplyr)
library(ggplot2)
summary_data2 <- df4 %>%
  group_by(trat,anocal,tipocob) %>%
  summarize(
    percent_cover = mean(percob),
    sd_value = sd(percob)
  )
#Recode trat to factor
summary_data2$Panel = as.factor(summary_data2$Panel)
#Rename the variables according to the plot
summary_data2 <- summary_data2 %>% rename(Group = tipocob)
summary_data2 <- summary_data2 %>% rename(Year = anocal)
summary_data2 <- summary_data2 %>% rename(Cover = percent_cover)
summary_data2 <- summary_data2 %>% rename(SE = sd_value)
summary_data2 <- summary_data2 %>% rename(Panel = trat)



library(forcats)
summary_data2$Group <- fct_recode(summary_data2$Group,
                                     "Black" = "cobtotal",  # Renaming "cobtotal" to "total"
                                     "White" = "cobmm")  # Renaming "cobmm" to "melinis"
summary_data2$Year <- fct_recode(summary_data2$Year,
                                 "2003" = "03",  # Renaming "03" to "2003"
                                 "2004" = "04", # Renaming "04" to "2004"
                                 "2005" = "05")  # Renaming "05" to "2005"
summary_data2$Panel <- fct_recode(summary_data2$Panel,
                                 "Low Coverage" = "1",  # Renaming "1" to "Low Coverage"
                                 "High Coverage" = "2", # Renaming "2" to "High Coverage"
                                 "Fire May" = "3", # Renaming "3" to "Fire May"
                                 "Int Mgmt May" = "4")  # Renaming "4" to "Int Mgmt May"

# COPY summary_data dataframe to a dataframe called df and plot
library(ggplot2)
Heart <- read.csv(file = 'C:/Users/IT CITY/Downloads/heart_disease_uci.csv')

#a)
# Calculate 2.5% and 97.5% quantiles
q025 <- quantile(Heart$age, 0.025)
q975 <- quantile(Heart$age, 0.975)

# Plot histogram and density curve
ggplot(Heart, aes(x = age)) +
  geom_histogram(aes(y = ..density..), binwidth = 5 , fill = "pink", color = "black", alpha = 0.7) +
  geom_density(color = "deeppink", alpha = .3,fill = "pink", size = 1) +
  geom_vline(xintercept = q025, linetype = "dashed", color = "black", size = 1.2) +
  geom_vline(xintercept = q975, linetype = "dashed", color = "black", size = 1.2) +
  annotate("text", x = q025, y = 0, label = paste0("2.5th percentile: ", round(q025, 2)), vjust = -1, color = "black", size = 3) +
  annotate("text", x = q975, y = 0, label = paste0("97.5th percentile: ", round(q975, 2)), vjust = -1, color = "black", size = 3) +
  labs(title = "Age Distribution", x = "Age", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.title = element_text(size = 12),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor =element_blank())


#a_2) bins are choosen based on Freedman–Diaconis:

bw <- 2 * IQR(Heart$age) / length(Heart$age)^(1/3)

q025 <- quantile(Heart$age, 0.025)
q975 <- quantile(Heart$age, 0.975)

ggplot(Heart, aes(x = age)) +
  geom_histogram(aes(y = ..density..), binwidth =bw , fill = "pink", color = "black", alpha = 0.7) +
  geom_density(color = "deeppink", alpha = .3,fill = "pink", size = 1) +
  geom_vline(xintercept = q025, linetype = "dashed", color = "black", size = 1.2) +
  geom_vline(xintercept = q975, linetype = "dashed", color = "black", size = 1.2) +
  annotate("text", x = q025, y = 0, label = paste0("2.5th percentile: ", round(q025, 2)), vjust = -1, color = "black", size = 3) +
  annotate("text", x = q975, y = 0, label = paste0("97.5th percentile: ", round(q975, 2)), vjust = -1, color = "black", size = 3) +
  labs(title = "Age Distribution", x = "Age", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 12),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor =element_blank())


################################################################################



#b)
# Draw the QQ-plot with some modifications
ggplot(Heart, aes(sample = thalch, color = sex, shape = sex)) +
  stat_qq() +
  stat_qq_line(linetype = "dashed", size = 1) +
  scale_shape_manual(values = c(16, 17)) +
  scale_color_manual(values = c("deeppink", "blue")) +
  labs(title = "Thalch distribution",
       x = "Quantiles",
       y = "Thalch",
       color = "Gender",
       shape = "Gender") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = "gray"),
        panel.grid.minor = element_blank())



################################################################################



#c)
# Sort the categories in origin by their frequencies
dataset_freq <- sort(table(Heart$dataset), decreasing = TRUE)

# Create a data frame with sorted categories and frequencies
df <- data.frame(dataset = names(dataset_freq),
                 freq = as.vector(dataset_freq))

# Draw the horizontal bar plot with modifications
ggplot(df, aes(x = freq, y = reorder(dataset, freq), fill = dataset)) +
  geom_col(color = "black", size = 0.1) +
  coord_flip() +
  labs(title = "Place of study frequencies",
       x = "Count",
       y = "Country") +
  scale_fill_manual(values=c("deeppink","lightblue","orange", "green"), guide = "none") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.y = element_line(color = "gray"),
        panel.grid.minor = element_blank())



################################################################################



#d)
#Draw the boxplot with modifications
ggplot(Heart, aes(x = trestbps, y = factor(cp), fill = factor(cp))) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2) +
  coord_flip() +
  labs(title = "Resting Blood Pressure and Chest Pain box plot",
       x = "Resting Blood Pressure (mm Hg)",
       y = "Chest Pain Type") +
  scale_fill_manual(values=c("deeppink","lightblue","orange", "green"), guide = "none") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        panel.grid.major.y = element_line(color = "gray"),
        panel.grid.minor = element_blank())

  
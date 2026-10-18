#libraries
library(ggplot2)
library(pwr)


df <- read.delim("Student_Weather_AllColumns_Concise.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
head(df)
cat("Variables:", ncol(df), "\nObservations:", nrow(df), "\n")
names(df)

# calculating the medians of the ratings for mild, moderate and severe weather conditions for willingness to attend in-person classes.
median(df$Mild_Weather_Rating, na.rm = TRUE)
median(df$Moderate_Weather_Rating, na.rm = TRUE)
median(df$Severe_Weather_Rating, na.rm = TRUE)
# Calculating IQRs
IQR(df$Mild_Weather_Rating, na.rm = TRUE)
IQR(df$Moderate_Weather_Rating, na.rm = TRUE)
IQR(df$Severe_Weather_Rating, na.rm = TRUE)

# Contingency table ------------------------------------------------------
# Individual frequency tables for each weather condition
table_mild     <- table(factor(df$Mild_Weather_Rating,     levels = 1:5))
table_moderate <- table(factor(df$Moderate_Weather_Rating, levels = 1:5))
table_severe   <- table(factor(df$Severe_Weather_Rating,   levels = 1:5))


contingency_table <- rbind(
  Mild     = table_mild,
  Moderate = table_moderate,
  Severe   = table_severe
)

contingency_table

# Save contingency table as CSV
write.csv(contingency_table, "contingency_table.csv", row.names = TRUE)



# Bar chart --------------------------------------------------------------
ratings <- data.frame(
  Condition = c(rep("Mild",     length(df$Mild_Weather_Rating)),
                rep("Moderate", length(df$Moderate_Weather_Rating)),
                rep("Severe",   length(df$Severe_Weather_Rating))),
  Rating = c(df$Mild_Weather_Rating,
             df$Moderate_Weather_Rating,
             df$Severe_Weather_Rating)
)

p <- ggplot(ratings, aes(x = factor(Rating), fill = Condition)) +
  geom_bar(position = "dodge") +
  labs(x = "Willingness Rating (1–5)",
       y = "Number of Students",
       title = "Distribution of Willingness Ratings by Weather Condition") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

p

# Save bar chart as PNG
ggsave("bar_chart_weather_ratings.png", plot = p, width = 7, height = 5, dpi = 300)

# Chi-squared test of independence
#chisq.test(contingency_table)

chi <- chisq.test(contingency_table, correct = FALSE)
# Effect size (Cramer's V)
V <- sqrt(chi$statistic / (sum(contingency_table) * (min(nrow(contingency_table), ncol(contingency_table)) - 1)))
chi
V

# Power analysis
pwr.chisq.test(w = V, df = 8, N = 13, sig.level = 0.05)

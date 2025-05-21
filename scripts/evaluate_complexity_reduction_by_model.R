# --------------------------------------------------------------
# Script: evaluate_complexity_reduction_by_model.R
# Purpose: Analyze refactoring effectiveness across LLMs using PMD-guided (HYBRID) pipeline
# Author: Sara Moussa
# Thesis: Automated Hybrid Refactoring: Combining Static Analysis and LLMs to Reduce Method Level Code Complexity
# --------------------------------------------------------------

# Load required libraries
library(dplyr)
library(stringr)
library(tidyr)
library(pgirmess)
library(ggplot2)
library(ggdist)
library(gghalves)

# Load data
data = read.csv("Experiment Results.csv", stringsAsFactors = FALSE)

# Clean and normalize fields
data$Average.reduction = gsub("%", "", data$Average.reduction)
data$Average.reduction[data$Average.reduction == "#N/A"] = NA
data$Average.reduction = as.numeric(data$Average.reduction)

data$Model = str_trim(data$Model)
data$Method = str_trim(data$Method)
data$Effectiveness.level = str_to_title(str_trim(data$Effectiveness.level))

# Filter HYBRID data
hybrid_data = data[data$Approach == "HYBRID", ]

# Summary statistics by model
summary_stats_hybrid = aggregate(
  Average.reduction ~ Model,
  data = hybrid_data,
  FUN = function(x) c(
    mean   = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE)
  )
)
summary_stats_hybrid = data.frame(
  Model = summary_stats_hybrid$Model,
  Mean_Average_Reduction   = summary_stats_hybrid$Average.reduction[, "mean"],
  Median_Average_Reduction = summary_stats_hybrid$Average.reduction[, "median"],
  SD_Average_Reduction     = summary_stats_hybrid$Average.reduction[, "sd"]
)
print(summary_stats_hybrid)

# Distribution of effectiveness levels
cat("Distribution of Effectiveness Levels (HYBRID Only)\n")
eff = table(hybrid_data$Effectiveness.level)
print(as.data.frame(eff[order(-eff)]))

# Flag failed cases
hybrid_data$Failed = grepl("Ineffective", hybrid_data$Effectiveness.level, ignore.case = TRUE)

# Define color palettes
model_palette = c(
  "Gemini 2.0 Flash"    = "#BFD8B8",
  "Llama 4 Maverick"    = "#B7CCE2",
  "R1 Distill Qwen 32B" = "#E3B6B1"
)
fail_colour = "#D94B4B"

# Raincloud plot
ggplot(hybrid_data, aes(x = Model, y = Average.reduction)) +
  stat_halfeye(
    aes(fill = Model),
    adjust = 0.6, width = 0.5, .width = 0,
    justification = -0.25, point_colour = NA
  ) +
  geom_boxplot(
    aes(fill = Model),
    width = 0.15, outlier.shape = NA, alpha = 0.65,
    colour = "#3C3C3C"
  ) +
  stat_dots(
    data = hybrid_data[hybrid_data$Failed == FALSE, ],
    aes(fill = Model, colour = Model),
    side = "left", justification = 1.1, binwidth = 1.5,
    dotsize = 0.8, alpha = 0.5, stroke = 0.4
  ) +
  stat_dots(
    data = hybrid_data[hybrid_data$Failed == TRUE, ],
    fill = fail_colour, colour = fail_colour,
    side = "left", justification = 1.1, binwidth = 1.5,
    dotsize = 0.6, alpha = 0.7, stroke = 0.4
  ) +
  scale_fill_manual(values = model_palette, guide = "none") +
  scale_colour_manual(values = model_palette, guide = "none") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.title.x    = element_blank(),
    axis.title.y    = element_text(size = 14, face = "bold"),
    axis.text.x     = element_text(size = 13, hjust = 0.5),
    plot.title      = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  annotate(
    "text",
    x = -Inf, y = -Inf,
    label = "Red = Ineffective",
    colour = fail_colour,
    hjust = -0.1, vjust = -0.8,
    size = 4
  ) +
  labs(
    title = "Average Code-Complexity Reduction by Model (HYBRID)",
    y     = "Average Reduction (%)"
  )

# Method × Model matrix (all cases)
method_model_matrix = aggregate(
  Average.reduction ~ Method + Model,
  data = hybrid_data,
  FUN = mean, na.rm = TRUE
)
method_model_matrix = reshape(
  method_model_matrix,
  timevar = "Model",
  idvar = "Method",
  direction = "wide"
)
rownames(method_model_matrix) = NULL
cat("Per-Method Average Reduction Matrix by Model\n")
print(method_model_matrix)

# Friedman test (all cases)
reduction_matrix = as.matrix(method_model_matrix[, -1])
cat("\nFriedman Test Result (all cases)\n")
friedman_result = friedman.test(reduction_matrix)
print(friedman_result)

if (friedman_result$p.value < 0.05) {
  cat("Result: Statistically significant differences exist among the LLMs (p <", round(friedman_result$p.value, 4), ")\n")
} else {
  cat("Result: No statistically significant difference detected among the LLMs (p =", round(friedman_result$p.value, 4), ")\n")
}

# Post-hoc comparisons (all cases)
cat("\nPost Hoc Pairwise Comparisons (FriedmanMC)\n")
posthoc_result = friedmanmc(reduction_matrix)
print(posthoc_result)

# Effective-only matrix and Friedman test
hybrid_effective = hybrid_data[hybrid_data$Failed == FALSE, ]
method_model_matrix_eff = aggregate(
  Average.reduction ~ Method + Model,
  data = hybrid_effective,
  FUN = mean, na.rm = TRUE
)
method_model_matrix_eff = reshape(
  method_model_matrix_eff,
  timevar = "Model",
  idvar = "Method",
  direction = "wide"
)
reduction_matrix_eff = as.matrix(method_model_matrix_eff[, -1])

cat("\nFriedman Test Result (effective cases)\n")
friedman_result_eff = friedman.test(reduction_matrix_eff)
print(friedman_result_eff)

if (friedman_result_eff$p.value < 0.05) {
  cat("Result: Statistically significant differences exist among the LLMs (p <", round(friedman_result_eff$p.value, 4), ")\n")
} else {
  cat("Result: No statistically significant difference detected among the LLMs (p =", round(friedman_result_eff$p.value, 4), ")\n")
}

cat("\nPost Hoc Pairwise Comparisons (FriedmanMC, effective cases)\n")
posthoc_result_eff = friedmanmc(reduction_matrix_eff)
print(posthoc_result_eff)

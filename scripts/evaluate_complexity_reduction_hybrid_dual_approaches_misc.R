# --------------------------------------------------------------
# Script: evaluate_complexity_reduction_hybrid_dual_approaches_misc.R
# Purpose: Analyze refactoring effectiveness for hybrid approach and comparing approaches plus miscellaneous statistics
# Author: Micael Olsson
# Thesis: Integrating Static Analysis and LLMs to Refactor Complex Methods
# --------------------------------------------------------------


library(tidyr)
library(showtext)
library(ggplot2)
library(ggdist)
library(gghalves)
library(readxl)
library(grid)
library(dplyr)

# Load data
data <- read_excel("Benchmark.xlsx", sheet = "Experiment Results")
font_add(family = "cm", regular = "computer-modern/cmunsx.ttf")
showtext_auto()

# Compute effect size
compute_effect_size <- function(V, n) {
  mu_V <- n * (n + 1) / 4
  sd_V <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  Z <- (V - mu_V - 0.5) / sd_V
  r <- Z / sqrt(n)
  return(r)
}

----------------------------------------------------------------------------


# RPLOT COMPARISON OF AVERAGE CODE COMPLEXITY REDUCTION TWO APPROACHES - OVERLAPPING EFFECTIVE ONLY

data$`Average reduction` <- as.numeric(data$`Average reduction`)

# Filter for effective entries only
effective_data <- data[
  !(data$`Effectiveness level` %in% c("Ineffective(Test Fails)", "Ineffective(Error or Untestable)", "Ineffective(Increased complexity)", "Ineffective(no change)
")) &
    !is.na(data$`Average reduction`), ]

# Subset
hybrid_effective <- effective_data[effective_data$Approach == "HYBRID", ]
llm_only_effective <- effective_data[effective_data$Approach == "LLM_ONLY", ]

# Unique identifiers (combining method and model name)
hybrid_effective$MethodModel <- paste(hybrid_effective$Method, hybrid_effective$`Model`, sep = "::")
llm_only_effective$MethodModel <- paste(llm_only_effective$Method, llm_only_effective$`Model`, sep = "::")

# Shared method-model combinations
common_ids <- intersect(hybrid_effective$MethodModel, llm_only_effective$MethodModel)

# Include only shared Method+Model from both
hybrid_common <- hybrid_effective[hybrid_effective$MethodModel %in% common_ids, ]
llm_common <- llm_only_effective[llm_only_effective$MethodModel %in% common_ids, ]

# Combine both datasets for plotting
combined_common <- rbind(
  data.frame(Approach = "HYBRID", Reduction = hybrid_common$`Average reduction`),
  data.frame(Approach = "LLM_ONLY", Reduction = llm_common$`Average reduction`)
)

ggplot(combined_common, aes(x = Approach, y = Reduction)) +
  # Density + halfeye
  ggdist::stat_halfeye(
    adjust = 0.4, width = 0.6, .width = 0,
    aes(fill = Approach), color = NA, alpha = 0.7
  ) +
  # Boxplot
  geom_boxplot(
    aes(fill = Approach),
    width = 0.1, outlier.shape = NA, alpha = 0.7
  ) +
  # Half points
  gghalves::geom_half_point(
    side = "l", shape = 21, alpha = 0.6, size = 2,
    aes(fill = Approach, color = Approach),
    position = position_jitter(width = 0.05, height = 0)
  ) +
  # Color mapping
  scale_fill_manual(values = c("HYBRID" = "#7EA4D9", "LLM_ONLY" = "#C4A5D3")) +
  scale_color_manual(values = c("HYBRID" = "#2F4F8A", "LLM_ONLY" = "#5B2E6D")) +
  # Axis and labels
  scale_x_discrete(labels = c("HYBRID" = "PMD-LLM-Refactor", "LLM_ONLY" = "LLM Only")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(
    title = "Average Code Complexity Reduction\nOverlapping Effective Refactorings",
    x = NULL,
    y = "Average Reduction (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(family = "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    plot.margin = unit(c(1, 1, 0.3, 1), "cm"),
    legend.position = "none"  
  )

# SUMMARY STATISTICS BOTH APPROACHES (OVERLAPPING EFFECTIVE)

# Hybrid
cat("\nHybrid Approach Stats:\n")
cat("Mean: ", mean(hybrid_common$`Average reduction`, na.rm = TRUE), "\n")
cat("Median: ", median(hybrid_common$`Average reduction`, na.rm = TRUE), "\n")
cat("SD: ", sd(hybrid_common$`Average reduction`, na.rm = TRUE), "\n")
cat("IQR: ", IQR(hybrid_common$`Average reduction`, na.rm = TRUE), "\n")

# LLM Only
cat("\nLLM-Only Approach Stats:\n")
cat("Mean: ", mean(llm_common$`Average reduction`, na.rm = TRUE), "\n")
cat("Median: ", median(llm_common$`Average reduction`, na.rm = TRUE), "\n")
cat("SD: ", sd(llm_common$`Average reduction`, na.rm = TRUE), "\n")
cat("IQR: ", IQR(llm_common$`Average reduction`, na.rm = TRUE), "\n")


# MEAN MEDIAN SD BOTH APPROACHES OVERLAPPING EFFECTIVE

# Calculate stats function
calc_stats <- function(x) {
  c(
    mean = mean(x),
    median = median(x),
    sd = sd(x)
  )
}

# Get stats for Hybrid effective
hybrid_stats <- calc_stats(hybrid_effective$`Average reduction`)

# Get stats for LLM Only effective
llm_stats <- calc_stats(llm_only_effective$`Average reduction`)

# Print results
hybrid_stats
llm_stats

# WILCOXON/EFFECT SIZE BOTH APPROACHES COMPARISON

# Sorted by MethodModel for pairing
hybrid_common <- hybrid_common[order(hybrid_common$MethodModel), ]
llm_common <- llm_common[order(llm_common$MethodModel), ]

# Check alignment
stopifnot(identical(hybrid_common$MethodModel, llm_common$MethodModel))

# Paired Wilcoxon signed-rank test
wilcox_paired_result <- wilcox.test(
  hybrid_common$`Average reduction`,
  llm_common$`Average reduction`,
  paired = TRUE,
  alternative = "greater",
  exact = FALSE
)

# Extract statistic and sample size
V <- wilcox_paired_result$statistic
n <- length(hybrid_common$`Average reduction`)

# Expected mean and SD of V under H0
mu_V <- n * (n + 1) / 4
sd_V <- sqrt(n * (n + 1) * (2 * n + 1) / 24)

# Z-score with continuity correction
Z <- (V - mu_V - 0.5) / sd_V

# Effect size (rank-biserial correlation)
r <- Z / sqrt(n)

# Output
print(wilcox_paired_result)
cat(sprintf("Effect size (r) = %.4f\n", r))



----------------------------------------------------------------------------
  


# RAINCLOUD HYBRID ONLY (EFFECTIVE REFACTORINGS)

# Filter Hybrid ineffective
  hybrid_ineffective <- data[data$Approach == "HYBRID" &
                               data$`Effectiveness level` %in% c("Ineffective(Test Fails)",
                                                                 "Ineffective(Error or Untestable)",
                                                                 "Ineffective(Increased complexity)",
                                                                 "Ineffective(no change)") &
                               !is.na(data$`Average reduction`), ]

ggplot() +
  # half-eye density (effective only)
  ggdist::stat_halfeye(
    data = hybrid_effective,
    aes(x = "", y = `Average reduction`),
    adjust = 0.4, width = 0.6, .width = 0,
    fill = "#5B78A7", color = NA, alpha = 0.7
  ) +
  # boxplot (effective only)
  geom_boxplot(
    data = hybrid_effective,
    aes(x = "", y = `Average reduction`),
    width = 0.1, outlier.shape = NA, fill = "#7EA4D9", alpha = 0.7
  ) +
  # dots for effective (left side)
  gghalves::geom_half_point(
    data = hybrid_effective,
    aes(x = "", y = `Average reduction`),
    side = "l", shape = 21, color = "#2F4F8A", fill = "#5B78A7", alpha = 0.6, size = 2,
    position = position_jitter(width = 0.05, height = 0)
  ) +
  # dots for ineffective (right side)
  gghalves::geom_half_point(
    data = hybrid_ineffective,
    aes(x = "", y = `Average reduction`),
    side = "r", shape = 21, color = "#9A2F2F", fill = "#FF9999", alpha = 0.6, size = 2,
    position = position_jitter(width = 0.05, height = 0)
  ) +
  labs(
    title = "Average Code Complexity Metric Reduction\nPMD-LLM-Refactor (Effective Refactorings Density)",
    y = "Average Reduction (%)", x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(family = "cm"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
    axis.text.y = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  ) +
  annotation_custom(
    grob = grobTree(
      textGrob("Blue = Effective", x = unit(0.76, "npc"), y = unit(0.12, "npc"),
               just = "left", gp = gpar(col = "#5B78A7", fontsize = 9, family = "cm")),
      textGrob("Red = Ineffective", x = unit(0.76, "npc"), y = unit(0.08, "npc"),
               just = "left", gp = gpar(col = "#9A2F2F", fontsize = 9, family = "cm"))
    ),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )
IQR(hybrid_effective$`Average reduction`, na.rm = TRUE)



# WILCOXON AND EFFECT SIZE HYBRID ONLY EFFECTIVE

# Wilcoxon signed-rank test (median = 0)
wilcox_result <- wilcox.test(hybrid_effective$`Average reduction`, mu = 0, alternative = "greater", exact = FALSE)

# Extract V and n
V <- wilcox_result$statistic
n <- length(hybrid_effective$`Average reduction`)

# Compute expected mean and SD of V under H0
mu_V <- n * (n + 1) / 4
sd_V <- sqrt(n * (n + 1) * (2 * n + 1) / 24)

# Compute Z-score with continuity correction
Z <- (V - mu_V - 0.5) / sd_V

# Compute effect size r (rank-biserial correlation)
r <- Z / sqrt(n)

# Print test result
print(wilcox_result)

# Print effect size
cat(sprintf("Effect size (r) = %.4f\n", r))



# WILCOXON AND EFFECT SIZE HYBRID ONLY ALL REFACTORINGS

# Filter all Hybrid refactorings (effective + ineffective), exclude NA
hybrid_all <- data[data$Approach == "HYBRID" &
                     !is.na(data$`Average reduction`), ]

# Wilcoxon signed-rank test (median = 0)
wilcox_result <- wilcox.test(hybrid_all$`Average reduction`, mu = 0, alternative = "greater", exact = FALSE)

# Extract V and n
V <- wilcox_result$statistic
n <- length(hybrid_all$`Average reduction`)

# Compute expected mean and SD of V under H0
mu_V <- n * (n + 1) / 4
sd_V <- sqrt(n * (n + 1) * (2 * n + 1) / 24)

# Compute Z-score with continuity correction
Z <- (V - mu_V - 0.5) / sd_V

# Compute effect size r (rank-biserial correlation)
r <- Z / sqrt(n)
r

print(wilcox_result)



# HYBRID ONLY INEFFECTIVE STATISTICS

# Stats for Hybrid ineffective
hybrid_ineffective_stats <- calc_stats(hybrid_ineffective$`Average reduction`)

# Print results
hybrid_ineffective_stats



----------------------------------------------------------------------------


# VIOLATION STATISTICS

# All possible violation types
all_violations <- c("CognitiveComplexity", "CyclomaticComplexity", 
                    "ExcessiveLinesOfCode", "NPathComplexity")

# Parse and split violations
split_violations <- strsplit(data[["Original Violations"]], ",\\s*")

# Count individual violations
all_found <- unlist(split_violations)
individual_counts <- table(factor(all_found, levels = all_violations))

# Count sorted combinations of violations
sorted_combinations <- sapply(split_violations, function(x) paste(sort(x), collapse = ", "))
combo_counts <- table(sorted_combinations)
combo_counts <- sort(combo_counts, decreasing = TRUE)

# Convert to data frames
individual_df <- data.frame(
  Violation = names(individual_counts),
  Count = as.integer(individual_counts),
  row.names = NULL
)

combo_df <- data.frame(
  Combination = names(combo_counts),
  Count = as.integer(combo_counts),
  row.names = NULL
)

# Output
print(individual_df)
print(combo_df)




----------------------------------------------------------------------------



# AVG METRIC VALUES PER PROJECT

before_metrics <- c("Cyclomatic (Before)", "Cognitive (Before)", "NPath (Before)", "LOC (Before)")

avg_before_by_project <- data %>%
  group_by(Project) %>%
  summarise(across(all_of(before_metrics), ~ mean(.x, na.rm = TRUE)))

print(avg_before_by_project)



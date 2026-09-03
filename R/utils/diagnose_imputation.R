# Diagnostic script to understand why FMI is so high
# Run this after the mice chunk in index.qmd

library(tidyverse)
library(glue)

# 1. Check missingness patterns ----
message("=== MISSINGNESS DIAGNOSTICS ===\n")

# Get the long-form data before imputation
surveys_long <- surveys_wide |>
  pivot_longer(
    cols = -c(pid, age, gender, edu_level, employment, marital_status,
              life_sat_baseline, wemwbs, diaries_completed, self_reported_weekly_play),
    names_to = c("variable", "wave"),
    names_pattern = "(.+)_w(.+)"
  ) |>
  mutate(wave = as.numeric(wave))

# Calculate composite scores
game_ns_items <- surveys_long |>
  filter(variable %in% c("bangs_1", "bangs_3", "bangs_5")) |>
  pivot_wider(names_from = variable, values_from = value) |>
  mutate(game_ns = rowMeans(pick(bangs_1, bangs_3, bangs_5), na.rm = FALSE))

# Missingness by wave
miss_by_wave <- game_ns_items |>
  group_by(wave) |>
  summarise(
    n_total = n(),
    n_missing = sum(is.na(game_ns)),
    pct_missing = 100 * n_missing / n_total
  )

message("Missingness in game_ns by wave:")
print(miss_by_wave, n = 30)

# Missingness by person
miss_by_person <- game_ns_items |>
  group_by(pid) |>
  summarise(
    n_waves = n(),
    n_missing = sum(is.na(game_ns)),
    pct_missing = 100 * n_missing / n_waves,
    first_complete = min(wave[!is.na(game_ns)], na.rm = TRUE),
    last_complete = max(wave[!is.na(game_ns)], na.rm = TRUE)
  ) |>
  arrange(desc(pct_missing))

message("\nMissingness by person (top 20 worst):")
print(head(miss_by_person, 20))

message(glue("\nOverall: {round(mean(miss_by_person$pct_missing), 1)}% missing per person on average"))
message(glue("People with >50% missing: {sum(miss_by_person$pct_missing > 50)} ({round(100*mean(miss_by_person$pct_missing > 50), 1)}%)"))

# 2. Check if missingness is MCAR, MAR, or MNAR ----
message("\n=== MISSINGNESS MECHANISM ===\n")

# Join with complete data to check if missingness relates to observed values
miss_patterns <- game_ns_items |>
  mutate(missing_game_ns = is.na(game_ns)) |>
  left_join(
    surveys_wide |> select(pid, age, diaries_completed, self_reported_weekly_play, wemwbs),
    by = "pid"
  )

# Test if missingness relates to person-level characteristics
message("Logistic regression: P(missing game_ns) ~ person characteristics")
miss_model <- glm(
  missing_game_ns ~ age + diaries_completed + self_reported_weekly_play + wemwbs,
  data = miss_patterns,
  family = binomial()
)
print(summary(miss_model))

# 3. Check predictor matrix density for game_ns variables ----
message("\n=== PREDICTOR MATRIX DIAGNOSTICS ===\n")

# Get predictor matrix from imputation
pred <- get("pred", envir = .GlobalEnv)

# Check how many predictors each game_ns variable uses
game_ns_vars <- grep("^bangs_[135]_w", colnames(pred), value = TRUE)
pred_density <- sapply(game_ns_vars, function(v) {
  100 * mean(pred[v, ])
})

message(glue("Predictor density for game_ns items (bangs_1, bangs_3, bangs_5):"))
message(glue("  Mean: {round(mean(pred_density), 1)}%"))
message(glue("  Range: {round(min(pred_density), 1)}% - {round(max(pred_density), 1)}%"))
message(glue("  (100% = using all {ncol(pred)} variables as predictors)"))

# Check if within-person temporal info is being used
message("\n=== TEMPORAL STRUCTURE CHECK ===")
# For each game_ns variable, check if adjacent waves are predictors
example_var <- game_ns_vars[15]  # Wave 15 as example
example_wave <- str_extract(example_var, "\\d+$")
adjacent_waves <- as.numeric(example_wave) + c(-2, -1, 1, 2)
adjacent_vars <- paste0("bangs_1_w", adjacent_waves)
adjacent_used <- sum(pred[example_var, adjacent_vars[adjacent_vars %in% colnames(pred)]])

message(glue("Example: {example_var} uses {adjacent_used}/4 adjacent waves as predictors"))
message("(Low numbers suggest temporal structure not well captured)")

# 4. Recommendation ----
message("\n=== RECOMMENDATIONS ===\n")

avg_missing <- mean(miss_by_person$pct_missing)
high_missing_pct <- 100 * mean(miss_by_person$pct_missing > 50)

if (avg_missing > 40) {
  message("⚠️  HIGH MISSINGNESS: Average {round(avg_missing, 1)}% per person")
  message("   → Consider filtering to people with <50% missing")
  message("   → High FMI may reflect genuine data quality issues")
}

if (mean(pred_density) < 5) {
  message("⚠️  VERY SPARSE PREDICTORS: Only {round(mean(pred_density), 1)}% of variables used")
  message("   → Try increasing quickpred() mincor from 0.1 to 0.05")
  message("   → Or manually include temporal structure (use 2l.pmm method)")
}

if (adjacent_used < 2) {
  message("⚠️  WEAK TEMPORAL STRUCTURE: Adjacent waves not well connected")
  message("   → Consider 2l.pmm method designed for longitudinal data")
  message("   → Or reduce mincor to capture temporal autocorrelation")
}

message("\nBOTTOM LINE:")
message("High FMI may be unavoidable with this much missing data.")
message("Consider: sensitivity analysis comparing complete case vs imputed results.")
message("Report both and discuss differences as a limitation.")

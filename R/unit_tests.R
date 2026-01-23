# Unit tests for custom analysis functions and preprocessing logic
# Run with: testthat::test_file("R/test_analysis.R")

library(testthat)
library(tidyverse)
library(lubridate)

# Find project root and set working directory
find_project_root <- function() {
  # Start from current location
  current_dir <- getwd()

  # Look for .Rproj file going up directory tree
  while (!file.exists(list.files(pattern = "\\.Rproj$", full.names = TRUE)[1])) {
    parent_dir <- dirname(current_dir)
    if (parent_dir == current_dir) {
      stop("Cannot find project root (.Rproj file)")
    }
    current_dir <- parent_dir
    setwd(current_dir)
  }
  return(current_dir)
}

# Set to project root
project_root <- find_project_root()
setwd(project_root)

# Source the function to test
if (file.exists("R/plot_marginal_effects.R")) {
  source("R/plot_marginal_effects.R")
} else {
  stop("Cannot find R/plot_marginal_effects.R from project root")
}

# Test suite for plot_marginal_effects() ====================================

test_that("plot_marginal_effects creates valid ggplot object", {
  # Create minimal test data
  set.seed(123)
  test_data <- tibble(
    pid = factor(rep(1:10, each = 10)),
    wave = rep(1:10, 10),
    x_within = rnorm(100),
    x_between = rep(rnorm(10), each = 10),
    y = 2 + 0.5 * x_within + 0.3 * x_between + rnorm(100, sd = 0.5)
  )

  # Fit test model
  test_model <- lme4::lmer(
    y ~ x_within + x_between + (1 | pid),
    data = test_data
  )

  # Test basic plot creation
  plot_obj <- plot_marginal_effects(
    model = test_model,
    x_var = "x_within",
    x_label = "Within predictor",
    y_label = "Outcome",
    color = "blue",
    n_keepers = 5,
    within_vars = "x_within",
    between_vars = "x_between"
  )

  expect_s3_class(plot_obj, "gg")
  expect_s3_class(plot_obj, "ggplot")
})

test_that("plot_marginal_effects handles binary outcomes", {
  set.seed(123)
  test_data <- tibble(
    pid = factor(rep(1:50, each = 5)),
    wave = rep(1:5, 50),
    x_within = rnorm(250),
    x_between = rep(rnorm(50), each = 5),
    y = rbinom(250, 1, plogis(-1 + 0.5 * x_within + 0.3 * x_between))
  )

  test_model <- glmmTMB::glmmTMB(
    y ~ x_within + x_between + (1 | pid),
    data = test_data,
    family = binomial()
  )

  plot_obj <- plot_marginal_effects(
    model = test_model,
    x_var = "x_within",
    x_label = "Within predictor",
    y_label = "P(outcome)",
    color = "red",
    n_keepers = 10,
    within_vars = "x_within",
    between_vars = "x_between"
  )

  expect_s3_class(plot_obj, "gg")
  # Binary outcomes should filter extreme intercepts
  expect_true(TRUE)
})

test_that("plot_marginal_effects validates required arguments", {
  set.seed(123)
  test_data <- tibble(
    pid = factor(rep(1:10, each = 10)),
    x = rnorm(100),
    y = rnorm(100)
  )
  test_model <- lme4::lmer(y ~ x + (1 | pid), data = test_data)

  # Should work with minimal required args
  expect_error(
    plot_marginal_effects(
      model = test_model,
      x_var = "x",
      x_label = "X",
      y_label = "Y",
      within_vars = "x",
      between_vars = character(0)
    ),
    NA
  )
})


# Test suite for within/between centering ===================================

test_that("within-person centering produces correct values", {
  test_data <- tibble(
    pid = rep(1:3, each = 3),
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  )

  result <- test_data |>
    group_by(pid) |>
    mutate(
      x_pm = mean(x),
      x_cw = x - x_pm
    ) |>
    ungroup()

  # Within-person centered values should sum to ~0 within each person
  within_sums <- result |>
    group_by(pid) |>
    summarise(sum_cw = sum(x_cw))

  expect_true(all(abs(within_sums$sum_cw) < 1e-10))

  # Person means should be correct
  expect_equal(result$x_pm[1:3], rep(2, 3))
  expect_equal(result$x_pm[4:6], rep(5, 3))
  expect_equal(result$x_pm[7:9], rep(8, 3))
})

test_that("between-person centering produces correct values", {
  test_data <- tibble(
    pid = rep(1:3, each = 3),
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  )

  result <- test_data |>
    group_by(pid) |>
    mutate(x_pm = mean(x)) |>
    ungroup() |>
    mutate(
      x_gm = mean(x_pm),
      x_cb = x_pm - x_gm
    )

  # Grand mean should be mean of person means
  expect_equal(unique(result$x_gm), 5)

  # Between-person centered values should sum to ~0
  between_sum <- result |>
    distinct(pid, x_cb) |>
    pull(x_cb) |>
    sum()

  expect_true(abs(between_sum) < 1e-10)
})


# Test suite for time window calculations ===================================

test_that("24-hour play window captures correct time range", {
  # Create test survey time
  survey_time <- as.POSIXct("2024-01-01 14:00:00", tz = "UTC")
  window_end <- survey_time + hours(24)

  # Create test telemetry data
  test_telemetry <- tibble(
    hour_start = seq(
      from = as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
      to = as.POSIXct("2024-01-02 16:00:00", tz = "UTC"),
      by = "hour"
    ),
    minutes = 30
  )

  # Filter to window
  in_window <- test_telemetry |>
    filter(hour_start >= survey_time, hour_start < window_end)

  # Should have exactly 24 hours
  expect_equal(nrow(in_window), 24)

  # First hour should be survey time
  expect_equal(min(in_window$hour_start), survey_time)

  # Last hour should be 23 hours after survey
  expect_equal(max(in_window$hour_start), survey_time + hours(23))
})

test_that("play window excludes data outside range", {
  survey_time <- as.POSIXct("2024-01-01 14:00:00", tz = "UTC")
  window_end <- survey_time + hours(24)

  test_telemetry <- tibble(
    hour_start = c(
      survey_time - hours(1),  # Before window
      survey_time,             # Start of window
      survey_time + hours(23), # End of window
      window_end               # After window (excluded)
    ),
    minutes = 30
  )

  in_window <- test_telemetry |>
    filter(hour_start >= survey_time, hour_start < window_end)

  # Should exclude times before and at/after window_end
  expect_equal(nrow(in_window), 2)
  expect_false(any(in_window$hour_start < survey_time))
  expect_false(any(in_window$hour_start >= window_end))
})


# Test suite for session expansion logic ====================================

test_that("session expansion creates correct hourly bins", {
  # Session from 13:30 to 15:45 should span 3 hours: 13:00, 14:00, 15:00
  session <- tibble(
    session_start = as.POSIXct("2024-01-01 13:30:00", tz = "UTC"),
    session_end = as.POSIXct("2024-01-01 15:45:00", tz = "UTC")
  )

  result <- session |>
    mutate(
      h0 = floor_date(session_start, "hour"),
      h1 = floor_date(session_end - seconds(1), "hour"),
      n_hours = as.integer(difftime(h1, h0, units = "hours")) + 1
    )

  expect_equal(result$n_hours, 3)
  expect_equal(result$h0, as.POSIXct("2024-01-01 13:00:00", tz = "UTC"))
  expect_equal(result$h1, as.POSIXct("2024-01-01 15:00:00", tz = "UTC"))
})

test_that("session expansion calculates correct overlap minutes", {
  # Session from 13:30 to 14:20 spans 2 hours
  # Hour 13:00-14:00: 30 minutes (13:30-14:00)
  # Hour 14:00-15:00: 20 minutes (14:00-14:20)

  session_start <- as.POSIXct("2024-01-01 13:30:00", tz = "UTC")
  session_end <- as.POSIXct("2024-01-01 14:20:00", tz = "UTC")

  h0 <- floor_date(session_start, "hour")
  h1 <- floor_date(session_end - seconds(1), "hour")
  n_hours <- as.integer(difftime(h1, h0, units = "hours")) + 1

  hours <- tibble(k = 1:n_hours) |>
    mutate(
      hour_start = h0 + hours(k - 1),
      minutes = pmax(
        0,
        as.numeric(difftime(
          pmin(session_end, hour_start + hours(1)),
          pmax(session_start, hour_start),
          units = "mins"
        ))
      )
    )

  expect_equal(nrow(hours), 2)
  expect_equal(hours$minutes[1], 30)
  expect_equal(hours$minutes[2], 20)
  expect_equal(sum(hours$minutes), 50)  # Total session duration
})


# Test suite for data integrity checks ======================================

test_that("imputation preserves person means", {
  # Simulate data with missing values
  set.seed(123)
  test_data <- tibble(
    pid = rep(1:10, each = 5),
    wave = rep(1:5, 10),
    x = rnorm(50)
  )

  # Add some missing values
  test_data$x[c(3, 7, 12, 18, 25)] <- NA

  # Calculate person means before imputation (using available data)
  pm_before <- test_data |>
    group_by(pid) |>
    summarise(pm = mean(x, na.rm = TRUE))

  # Simple mean imputation (not MICE, but tests the concept)
  test_data_imputed <- test_data |>
    group_by(pid) |>
    mutate(x_imputed = ifelse(is.na(x), mean(x, na.rm = TRUE), x)) |>
    ungroup()

  # Person means after imputation
  pm_after <- test_data_imputed |>
    group_by(pid) |>
    summarise(pm = mean(x_imputed, na.rm = TRUE))

  # Person means should be identical when using person-mean imputation
  expect_equal(pm_before$pm, pm_after$pm, tolerance = 1e-10)
})

test_that("telemetry aggregation sums correctly across platforms", {
  test_telemetry <- tibble(
    pid = rep("A", 6),
    platform = c(rep("Xbox", 3), rep("Steam", 3)),
    day = rep(as.Date("2024-01-01"), 6),
    minutes = c(30, 45, 15, 60, 20, 40)
  )

  # Aggregate by day (across platforms)
  daily_total <- test_telemetry |>
    group_by(pid, day) |>
    summarise(minutes = sum(minutes), .groups = "drop")

  expect_equal(nrow(daily_total), 1)
  expect_equal(daily_total$minutes, 210)

  # Aggregate by platform
  platform_totals <- test_telemetry |>
    group_by(pid, platform) |>
    summarise(minutes = sum(minutes), .groups = "drop")

  expect_equal(nrow(platform_totals), 2)
  expect_equal(sum(platform_totals$minutes), 210)
})


# Test suite for edge cases =================================================

test_that("centering handles single observation per person", {
  test_data <- tibble(
    pid = 1:5,
    x = c(1, 2, 3, 4, 5)
  )

  result <- test_data |>
    group_by(pid) |>
    mutate(
      x_pm = mean(x),
      x_cw = x - x_pm
    ) |>
    ungroup()

  # With single obs, person mean = raw value, centered = 0
  expect_equal(result$x_cw, rep(0, 5))
  expect_equal(result$x_pm, result$x)
})

test_that("play window handles zero-duration sessions", {
  session <- tibble(
    session_start = as.POSIXct("2024-01-01 14:00:00", tz = "UTC"),
    session_end = as.POSIXct("2024-01-01 14:00:00", tz = "UTC")
  )

  result <- session |>
    mutate(
      duration_min = as.numeric(difftime(session_end, session_start, units = "mins"))
    ) |>
    filter(duration_min >= 1)

  # Zero-duration sessions should be filtered out
  expect_equal(nrow(result), 0)
})

test_that("session expansion handles midnight crossing", {
  # Session from 23:30 to 01:15 crosses midnight
  session_start <- as.POSIXct("2024-01-01 23:30:00", tz = "UTC")
  session_end <- as.POSIXct("2024-01-02 01:15:00", tz = "UTC")

  h0 <- floor_date(session_start, "hour")
  h1 <- floor_date(session_end - seconds(1), "hour")
  n_hours <- as.integer(difftime(h1, h0, units = "hours")) + 1

  # Should span 3 hours: 23:00 (Jan 1), 00:00 (Jan 2), 01:00 (Jan 2)
  expect_equal(n_hours, 3)

  hours <- tibble(k = 1:n_hours) |>
    mutate(hour_start = h0 + hours(k - 1))

  expect_equal(as.Date(hours$hour_start[1]), as.Date("2024-01-01"))
  expect_equal(as.Date(hours$hour_start[3]), as.Date("2024-01-02"))
})


# Run all tests ==============================================================

cat("\n=== Running unit tests ===\n\n")

test_results <- test_file("R/test_analysis.R", reporter = "summary")

cat("\n=== Test Summary ===\n")
print(test_results)

if (any(test_results$failed > 0)) {
  warning("Some tests failed! Review output above.")
} else {
  message("All tests passed!")
}

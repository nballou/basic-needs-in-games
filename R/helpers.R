# Shared presentation constants for manuscript.qmd / supplement.qmd (and the
# Study B / diagnostics figures). Colour palette and human-readable labels for
# the snake_case model terms.

colors <- list(
  game_ns = "#009988", # Teal for game need satisfaction
  global_ns = "#44BB99", # Light teal for global need satisfaction
  global_nf = "#EE6677", # Red for global need frustration
  nintendo = "#E60012", # Nintendo red
  xbox = "#107C10", # Xbox green
  steam = "#215e8a", # Steam dark blue
  within = "#228833", # Green for within-person variance
  between = "#BBBBBB" # Gray for between-person variance
)

labels <- c(
  # Base variables
  "game_ns" = "Game need satisfaction",
  "game_nf" = "Game need frustration",
  "global_ns" = "Global need satisfaction",
  "global_nf" = "Global need frustration",
  "session_length" = "Session length",
  "session_gap" = "Time to next session",
  # Within-person
  "game_ns_cw" = "Game need satisfaction (within)",
  "game_nf_cw" = "Game need frustration (within)",
  "global_ns_cw" = "Global need satisfaction (within)",
  "global_nf_cw" = "Global need frustration (within)",
  # Between-person
  "game_ns_cb" = "Game need satisfaction (between)",
  "game_nf_cb" = "Game need frustration (between)",
  "global_ns_cb" = "Global need satisfaction (between)",
  "global_nf_cb" = "Global need frustration (between)",
  # Within-person (alternate)
  "game_ns (within-person)" = "Game need satisfaction (within-person)",
  "global_ns (within-person)" = "Global need satisfaction (within-person)",
  "global_nf (within-person)" = "Global need frustration (within-person)",
  # Between-person (alternate)
  "game_ns (between-person)" = "Game need satisfaction (between-person)",
  "global_ns (between-person)" = "Global need satisfaction (between-person)",
  "global_nf (between-person)" = "Global need frustration (between-person)",
  # Interaction
  "game_ns_cw:global_nf_cw" = "Game need satisfaction × Global need frustration (within)",
  # Displacement
  "displaced_core_domain" = "Displaced core domain",
  "displaced_core_domainTRUE" = "Displaced core domain",
  # Variance components
  "Within-person" = "Within-person",
  "Between-person" = "Between-person",
  # Other
  "(Intercept)" = "Intercept"
)

report_lmer_term <- function(
  model,
  term,
  level = 0.95,
  ci_method = c("Wald", "profile", "boot"),
  digits_est = 3,
  digits_ci = 3,
  digits_p = 3,
  p_method = c("none", "wald_z"),
  label_p = "p"
) {
  ci_method <- match.arg(ci_method)
  p_method <- match.arg(p_method)

  sm <- summary(model)
  ct <- sm$coefficients

  if (!term %in% rownames(ct)) {
    stop(
      sprintf(
        "Term '%s' not found. Available terms: %s",
        term,
        paste(rownames(ct), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  est <- unname(ct[term, "Estimate"])
  se <- unname(ct[term, "Std. Error"])

  ci_mat <- suppressMessages(confint(
    model,
    parm = term,
    level = level,
    method = ci_method
  ))
  ci <- as.numeric(ci_mat[1, ])

  p_str <- ""
  if (p_method == "wald_z") {
    z <- est / se
    p <- 2 * stats::pnorm(abs(z), lower.tail = FALSE)
    p_str <- paste0(
      ", z = ",
      round(z, digits_est),
      ", ",
      label_p,
      " = ",
      round(p, digits_p)
    )
  }

  glue::glue(
    "{round(est, digits_est)}, SE = {round(se, digits_est)}, ",
    "{round(level*100)}% CI [{round(ci[1], digits_ci)}, {round(ci[2], digits_ci)}]{p_str}"
  )
}

# Extract and clean results from each model (within-person effects only).
# Set exponentiate = TRUE to report odds ratios rather than raw log-odds
# coefficients (for the binary H2 outcome); the point estimate is then shown
# to two decimal places on the odds-ratio scale.
clean_results <- function(pooled_obj, window_name, exponentiate = FALSE) {
  summary(pooled_obj) |>
    as_tibble() |>
    filter(
      # Only include the H2 within-person predictors
      term %in% c("game_ns_cw", "global_nf_cw")
    ) |>
    mutate(
      # summary.mipo() returns `term` as a factor; index labels by the
      # character value, not the factor's integer codes
      term = as.character(term),
      term = ifelse(term %in% names(labels), labels[term], term)
    ) |>
    select(term, estimate, p.value) |>
    mutate(
      point = if (exponentiate) {
        scales::number(exp(estimate), 0.01)
      } else {
        as.character(round(estimate, 3))
      },
      # Format as "Est (p)"
      result = glue(
        "{point} ({ifelse(p.value < 0.001, '<.001', round(p.value, 3))})"
      ),
      window = window_name
    ) |>
    select(term, window, result)
}

# Pull a single pooled coefficient for inline reporting, formatted as a number.
# Set exponentiate = TRUE for an odds ratio from a logistic model.
pooled_coef <- function(pooled_obj, term_name, exponentiate = FALSE, accuracy = 0.01) {
  est <- summary(pooled_obj) |>
    as_tibble() |>
    filter(term == term_name) |>
    pull(estimate)

  if (length(est) != 1) {
    stop(glue("Term '{term_name}' not found (or not unique) in pooled summary"))
  }

  scales::number(if (exponentiate) exp(est) else est, accuracy)
}

# Format the estimate and p-value for one coefficient from a pooled model,
# e.g. "B = 0.21, p < .001" or (exponentiate = TRUE) "OR = 0.91, p = .016".
report_estimate_p <- function(
  pooled_obj,
  term_name,
  exponentiate = FALSE,
  lbl = if (exponentiate) "OR" else "B"
) {
  row <- summary(pooled_obj) |>
    as_tibble() |>
    filter(term == term_name)
  est <- if (exponentiate) exp(row$estimate) else row$estimate
  p <- row$p.value
  p_str <- if (p < 0.001) "p < .001" else glue("p = {scales::number(p, .001)}")
  glue("{lbl} = {scales::number(est, 0.01)}, {p_str}")
}

# One sentence-fragment comparing a coefficient across the primary,
# >=15-survey subsample, and complete-case fits, for the sensitivity line
# reported alongside each hypothesis test.
report_sample_sensitivity <- function(
  primary,
  restricted,
  cc,
  term_name,
  exponentiate = FALSE
) {
  f <- function(x) report_estimate_p(x, term_name, exponentiate)
  glue(
    "{f(primary)} in the primary sample; ",
    "{f(restricted)} in the >=15-survey subsample; ",
    "{f(cc)} for complete cases"
  )
}
# Format helper functions
format_mean_sd <- function(x) {
  sprintf("%.1f (%.1f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

format_n_pct <- function(x, level) {
  n <- sum(x == level, na.rm = TRUE)
  pct <- 100 * n / sum(!is.na(x))
  sprintf("%d (%.1f%%)", n, pct)
}

# Build one categorical breakdown block for the participant table: a header
# row plus one row per level. The "Primary sample" column covers every row of
# `data`; the subsample column covers the rows flagged `data$in_subsample`
# (participants with >=15 completed surveys).
create_categorical_section <- function(data, var_name, header, levels) {
  header_row <- tibble(
    Characteristic = header,
    `Primary sample` = "",
    `≥15-survey subsample` = ""
  )

  level_rows <- tibble(level = levels) |>
    mutate(
      Characteristic = glue("    {level}"),
      `Primary sample` = map_chr(
        level,
        ~ format_n_pct(data[[var_name]], .x)
      ),
      `≥15-survey subsample` = map_chr(
        level,
        ~ format_n_pct(data[[var_name]][data$in_subsample], .x)
      )
    ) |>
    select(-level)

  bind_rows(header_row, level_rows)
}

# Report within-between estimates with consistent formatting.
#
# By default the estimate is reported on its native scale (a raw coefficient,
# labelled "B" in the manuscript text). Set exponentiate = TRUE for a logistic
# model to report an odds ratio instead: the point estimate and its 95% CI are
# exponentiated (the CI bounds are the exp() of the Wald interval on the
# log-odds scale) and the SE -- which is not meaningful on the odds-ratio
# scale -- is omitted from the formatted string.
report_wb_estimate <- function(
  pooled_summary,
  term_cw,
  term_cb = NULL,
  accuracy = 0.01,
  stat_label = "t",
  exponentiate = FALSE
) {
  # Helper to format p-values consistently

  format_p <- function(p) {
    if (p < 0.001) "<.001" else as.character(round(p, 3))
  }

  # Build the formatted result string for one coefficient row, on either the
  # raw (B, with SE) or the exponentiated (OR, no SE) scale.
  format_row <- function(row) {
    p_str <- format_p(row$p.value)
    ci_low <- row$estimate - 1.96 * row$std.error
    ci_high <- row$estimate + 1.96 * row$std.error

    if (exponentiate) {
      glue(
        "{number(exp(row$estimate), accuracy)} ",
        "[95% CI: {number(exp(ci_low), accuracy)}, {number(exp(ci_high), accuracy)}], ",
        "{stat_label} = {number(row$statistic, accuracy)}, p = {p_str}"
      )
    } else {
      glue(
        "{number(row$estimate, accuracy)}, SE = {number(row$std.error, accuracy)}, ",
        "[95% CI: {number(ci_low, accuracy)}, {number(ci_high, accuracy)}], ",
        "{stat_label} = {number(row$statistic, accuracy)}, p = {p_str}"
      )
    }
  }

  # Point estimate alone (odds ratio when exponentiate = TRUE), for interpretation
  format_coef <- function(row) {
    number(if (exponentiate) exp(row$estimate) else row$estimate, accuracy)
  }

  # Extract day-level estimate with CI and p-value
  day_level_row <- pooled_summary |>
    filter(term == term_cw)

  if (nrow(day_level_row) == 0) {
    stop(glue("Term '{term_cw}' not found in pooled summary"))
  }

  day_level <- format_row(day_level_row)

  # Optionally extract 30-day aggregate estimate
  if (!is.null(term_cb)) {
    aggregate_row <- pooled_summary |>
      filter(term == term_cb)

    if (nrow(aggregate_row) == 0) {
      stop(glue("Term '{term_cb}' not found in pooled summary"))
    }

    return(list(
      day_level = day_level,
      aggregate = format_row(aggregate_row),
      day_coef = format_coef(day_level_row),
      agg_coef = format_coef(aggregate_row)
    ))
  }

  return(list(
    day_level = day_level,
    day_coef = format_coef(day_level_row)
  ))
}


#' Plot marginal effects with individual trajectories
#'
#' Creates a plot showing individual-level predictions as faint background lines
#' and population-level marginal effects as a bold line with confidence ribbon.
#'
#' For binary outcome models, the function automatically filters out individuals
#' with extreme random intercepts (>1.5 SD from mean) to prevent trajectories
#' from saturating at 0 or 1, which obscures the meaningful variation.
#'
#' @param model A fitted model object (glmmTMB, lme4, etc.)
#' @param x_var Character string naming the focal within-person predictor to plot on x-axis
#' @param x_label Character string for x-axis label
#' @param y_label Character string for y-axis label
#' @param color Character string specifying the color for the population curve and ribbon
#' @param n_keepers Integer, number of individuals to show as background lines (default: 50)
#' @param n_points Integer, resolution of the x-axis grid (default: 101)
#' @param use_person_cb Logical, whether to use each pid's own between-person covariates (default: TRUE)
#' @param cb_value Numeric, value to fix between-person covariates when use_person_cb = FALSE (default: 0)
#' @param within_vars Character vector of all within-person centered predictor names in the model
#' @param between_vars Character vector of all between-person centered predictor names in the model
#'
#' @return A ggplot2 object
#'
#' @examples
#' plot_relationship(
#'   model = h1mod,
#'   x_var = "game_ns_cw",
#'   x_label = "Need satisfaction in games",
#'   y_label = "Need satisfaction in daily life",
#'   color = "blue",
#'   within_vars = c("game_ns_cw"),
#'   between_vars = c("game_ns_cb")
#' )
plot_relationship <- function(
  model,
  x_var,
  x_label,
  y_label,
  color = "blue",
  n_keepers = 50,
  n_points = 101,
  use_person_cb = TRUE,
  cb_value = 0,
  within_vars,
  between_vars
) {
  # Extract model frame and ensure pid is a factor
  fit_df <- model$frame |> mutate(pid = factor(pid))

  # Get range and representative wave value
  x_rng <- range(fit_df[[x_var]], na.rm = TRUE)
  # Convert wave to numeric for median calculation (handles factors, characters, etc.)
  wave0 <- median(as.numeric(as.character(fit_df$wave)), na.rm = TRUE)

  # Calculate between-person covariates per individual
  pid_cb <- fit_df |>
    group_by(pid) |>
    summarise(
      across(all_of(between_vars), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # Select individuals for background based on random slope extremes
  re_pid <- coef(model)$cond$pid |>
    as.data.frame() |>
    tibble::rownames_to_column("pid") |>
    rename(ri = `(Intercept)`)

  # For binary outcomes, filter to keep only individuals whose predicted
  # probabilities stay in a reasonable range (not saturated at 0 or 1)
  is_binary <- family(model)$family == "binomial"

  if (is_binary) {
    # More aggressive filtering: keep only individuals with intercepts
    # within 1 SD of mean to avoid saturation
    ri_mean <- mean(re_pid$ri, na.rm = TRUE)
    ri_sd <- sd(re_pid$ri, na.rm = TRUE)
    re_pid <- re_pid |>
      filter(abs(ri - ri_mean) < 1 * ri_sd)
  }

  # Select by random slope if it exists, otherwise by random intercept
  if (x_var %in% names(re_pid)) {
    re_pid <- re_pid |> arrange(.data[[x_var]])
  } else {
    # If no random slope for x_var, just arrange by intercept
    re_pid <- re_pid |> arrange(ri)
  }

  n_avail <- nrow(re_pid)
  n_keep <- min(n_keepers, n_avail)
  n_low <- floor(n_keep / 2)
  n_high <- ceiling(n_keep / 2)

  keepers <- bind_rows(
    re_pid |> slice(1:n_low),
    re_pid |> slice((n_avail - n_high + 1):n_avail)
  ) |>
    pull(pid)

  # Identify non-focal within-person predictors to hold at 0
  hold_zero <- setdiff(within_vars, x_var)

  # Create newdata for individual curves
  nd_ind <- expand_grid(
    pid = factor(keepers, levels = levels(fit_df$pid)),
    !!x_var := seq(x_rng[1], x_rng[2], length.out = n_points)
  ) |>
    left_join(pid_cb, by = "pid") |>
    mutate(wave = wave0)

  # Set between-person covariates based on use_person_cb
  if (!use_person_cb) {
    nd_ind <- nd_ind |>
      mutate(across(all_of(between_vars), ~cb_value))
  }

  # Set non-focal within-person predictors to 0
  if (length(hold_zero) > 0) {
    for (var in hold_zero) {
      nd_ind[[var]] <- 0
    }
  }

  # Get individual predictions
  indiv <- nd_ind |>
    mutate(
      estimate = predict(
        model,
        newdata = nd_ind,
        type = "response",
        re.form = NULL
      )
    )

  # Create newdata for population-level marginal curve
  nd_marginal <- tibble(
    !!x_var := seq(x_rng[1], x_rng[2], length.out = n_points),
    wave = wave0
  )

  # Add between-person covariates at cb_value
  for (var in between_vars) {
    nd_marginal[[var]] <- cb_value
  }

  # Add non-focal within-person predictors at 0
  if (length(hold_zero) > 0) {
    for (var in hold_zero) {
      nd_marginal[[var]] <- 0
    }
  }

  # Get marginal predictions with CI using marginaleffects
  mean_curve <- marginaleffects::predictions(
    model,
    newdata = nd_marginal,
    re.form = NA,
    vcov = TRUE
  )

  # Create plot
  # Use lower alpha for binary outcomes to reduce visual clutter
  indiv_alpha <- if (is_binary) 0.08 else 0.12

  ggplot() +
    geom_line(
      data = indiv,
      mapping = aes(x = .data[[x_var]], y = estimate, group = pid),
      alpha = indiv_alpha
    ) +
    geom_ribbon(
      data = mean_curve,
      mapping = aes(x = .data[[x_var]], ymin = conf.low, ymax = conf.high),
      fill = color,
      alpha = 0.25
    ) +
    geom_line(
      data = mean_curve,
      mapping = aes(x = .data[[x_var]], y = estimate),
      linewidth = 1,
      color = color
    ) +
    labs(x = x_label, y = y_label)
}

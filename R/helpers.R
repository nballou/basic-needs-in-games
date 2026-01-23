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
    p_str <- paste0(", ", label_p, " = ", round(p, digits_p))
  }

  glue::glue(
    "{round(est, digits_est)}, {round(level*100)}% CI [{round(ci[1], digits_ci)}, {round(ci[2], digits_ci)}]{p_str}"
  )
}

# Extract and clean results from each model (within-person effects only)
clean_results <- function(pooled_obj, window_name) {
  summary(pooled_obj) |>
    as_tibble() |>
    filter(
      # Only include the H2 within-person predictors
      term %in% c("game_ns_cw", "global_nf_cw")
    ) |>
    mutate(
      term = ifelse(term %in% names(labels), labels[term], term)
    ) |>
    select(term, estimate, p.value) |>
    mutate(
      # Format as "Est (p)"
      result = glue(
        "{round(estimate, 3)} ({ifelse(p.value < 0.001, '<.001', round(p.value, 3))})"
      ),
      window = window_name
    ) |>
    select(term, window, result)
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

# Function to create categorical breakdown
create_categorical_section <- function(data, var_name, header, levels) {
  # Header row
  header_row <- tibble(
    Characteristic = header,
    `Full sample` = "",
    `Analytic sample` = ""
  )

  # Level rows
  level_rows <- tibble(level = levels) |>
    mutate(
      Characteristic = glue("    {level}"),
      `Full sample` = map_chr(
        level,
        ~ format_n_pct(
          data[[var_name]][data$sample == "Full eligible sample"],
          .x
        )
      ),
      `Analytic sample` = map_chr(
        level,
        ~ format_n_pct(
          data[[var_name]][data$sample == "Analytic sample"],
          .x
        )
      )
    ) |>
    select(-level)

  bind_rows(header_row, level_rows)
}

# Report within-between estimates with consistent formatting
report_wb_estimate <- function(
  pooled_summary,
  term_cw,
  term_cb = NULL,
  accuracy = 0.01
) {
  # Helper to format p-values consistently

  format_p <- function(p) {
    if (p < 0.001) "<.001" else as.character(round(p, 3))
  }

  # Extract day-level estimate with CI and p-value
  day_level_row <- pooled_summary |>
    filter(term == term_cw)

  if (nrow(day_level_row) == 0) {
    stop(glue("Term '{term_cw}' not found in pooled summary"))
  }

  day_p <- format_p(day_level_row$p.value)
  day_level <- day_level_row |>
    mutate(
      ci_low = estimate - 1.96 * std.error,
      ci_high = estimate + 1.96 * std.error,
      result = glue(
        "{number(estimate, accuracy)} [95% CI: {number(ci_low, accuracy)}, {number(ci_high, accuracy)}], p = {day_p}"
      )
    ) |>
    pull(result)

  # Optionally extract 30-day aggregate estimate
  if (!is.null(term_cb)) {
    aggregate_row <- pooled_summary |>
      filter(term == term_cb)

    if (nrow(aggregate_row) == 0) {
      stop(glue("Term '{term_cb}' not found in pooled summary"))
    }

    agg_p <- format_p(aggregate_row$p.value)
    aggregate <- aggregate_row |>
      mutate(
        ci_low = estimate - 1.96 * std.error,
        ci_high = estimate + 1.96 * std.error,
        result = glue(
          "{number(estimate, accuracy)} [95% CI: {number(ci_low, accuracy)}, {number(ci_high, accuracy)}], p = {agg_p}"
        )
      ) |>
      pull(result)

    # Also return just the coefficient for interpretation
    day_coef <- number(day_level_row$estimate, accuracy)
    agg_coef <- number(aggregate_row$estimate, accuracy)

    return(list(
      day_level = day_level,
      aggregate = aggregate,
      day_coef = day_coef,
      agg_coef = agg_coef
    ))
  }

  day_coef <- number(day_level_row$estimate, accuracy)

  return(list(
    day_level = day_level,
    day_coef = day_coef
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

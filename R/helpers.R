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

pred_diff_fmt <- function(model, prev_var, prev_mean, accuracy = 0.1) {
  predictions(
    model,
    newdata = datagrid(
      model = model,
      game_ns_cw = c(0, 9),
      game_ns_cb = 0,
      !!prev_var := prev_mean
    ),
    re.form = NA
  ) |>
    as_tibble() |>
    arrange(game_ns_cw) |>
    mutate(
      estimate = exp(estimate),
      conf.low = exp(conf.low),
      conf.high = exp(conf.high)
    ) |>
    summarise(
      est = diff(estimate),
      lo = diff(conf.low),
      hi = diff(conf.high)
    ) |>
    transmute(
      formatted = glue::glue(
        "{number(est, accuracy = accuracy)} ",
        "[95% CI {number(lo, accuracy = accuracy)}, ",
        "{number(hi, accuracy = accuracy)}]"
      )
    ) |>
    pull(formatted)
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
report_wb_estimate <- function(pooled_summary, term_cw, term_cb = NULL, accuracy = 0.01) {
  # Extract day-level estimate with CI
  day_level_row <- pooled_summary |>
    filter(term == term_cw)

  if (nrow(day_level_row) == 0) {
    stop(glue("Term '{term_cw}' not found in pooled summary"))
  }

  day_level <- day_level_row |>
    mutate(
      ci_low = estimate - 1.96 * std.error,
      ci_high = estimate + 1.96 * std.error,
      result = glue("{number(estimate, accuracy)} [95% CI: {number(ci_low, accuracy)}, {number(ci_high, accuracy)}]")
    ) |>
    pull(result)

  # Optionally extract 30-day aggregate estimate
  if (!is.null(term_cb)) {
    aggregate_row <- pooled_summary |>
      filter(term == term_cb)

    if (nrow(aggregate_row) == 0) {
      stop(glue("Term '{term_cb}' not found in pooled summary"))
    }

    aggregate <- aggregate_row |>
      mutate(
        ci_low = estimate - 1.96 * std.error,
        ci_high = estimate + 1.96 * std.error,
        result = glue("{number(estimate, accuracy)} [95% CI: {number(ci_low, accuracy)}, {number(ci_high, accuracy)}]")
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

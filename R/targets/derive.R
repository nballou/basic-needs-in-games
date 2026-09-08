# Convert a completed mids object to long format, compute composite need
# scores from imputed items, and add within/between-person centered
# variables. Used for both the main analytical-sample imputation and each
# survey-completion-threshold sensitivity variant (S4) -- `telemetry_vars`
# must be filtered to the same sample as the mids object.
derive_analysis_dat <- function(imp, telemetry_vars) {
  complete(imp, action = "long", include = TRUE) |>
    pivot_longer(
      cols = -c(
        .imp,
        .id,
        pid,
        age,
        gender,
        edu_level,
        employment,
        marital_status,
        wemwbs,
        diaries_completed,
        self_reported_weekly_play
      ),
      names_to = c(".value", "wave"),
      names_pattern = "(.+)_w(.+)"
    ) |>
    mutate(wave = as.factor(wave)) |>
    mutate(
      global_ns = rowMeans(pick(bpnsfs_1, bpnsfs_3, bpnsfs_5), na.rm = TRUE),
      global_nf = rowMeans(pick(bpnsfs_2, bpnsfs_4, bpnsfs_6), na.rm = TRUE),
      game_ns = rowMeans(pick(bangs_1, bangs_3, bangs_5), na.rm = TRUE),
      game_nf = rowMeans(pick(bangs_2, bangs_4, bangs_6), na.rm = TRUE)
    ) |>
    left_join(telemetry_vars, by = c("pid", "wave")) |>
    group_by(.imp, pid) |>
    mutate(
      global_ns_pm = mean(global_ns, na.rm = TRUE),
      global_nf_pm = mean(global_nf, na.rm = TRUE),
      game_ns_pm = mean(game_ns, na.rm = TRUE),
      game_nf_pm = mean(game_nf, na.rm = TRUE)
    ) |>
    ungroup() |>
    group_by(.imp) |>
    mutate(
      global_ns_gm = mean(global_ns_pm, na.rm = TRUE),
      global_nf_gm = mean(global_nf_pm, na.rm = TRUE),
      game_ns_gm = mean(game_ns_pm, na.rm = TRUE),
      game_nf_gm = mean(game_nf_pm, na.rm = TRUE),

      # Within-person centered (deviation from person mean)
      global_ns_cw = global_ns - global_ns_pm,
      global_nf_cw = global_nf - global_nf_pm,
      game_ns_cw = game_ns - game_ns_pm,
      game_nf_cw = game_nf - game_nf_pm,

      # Between-person centered (person mean - grand mean)
      global_ns_cb = global_ns_pm - global_ns_gm,
      global_nf_cb = global_nf_pm - global_nf_gm,
      game_ns_cb = game_ns_pm - game_ns_gm,
      game_nf_cb = game_nf_pm - game_nf_gm
    ) |>
    ungroup()
}

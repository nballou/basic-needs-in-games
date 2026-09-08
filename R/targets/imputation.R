# Pivot analytical-sample survey data to wide format for MICE, filtered to
# participants with at least `min_waves` completed diaries. Used both for the
# main analytical sample (min_waves = 15) and each survey-completion-
# threshold sensitivity variant (S4).
build_wide_survey <- function(surveys, min_waves = 15) {
  surveys |>
    filter(diaries_completed >= min_waves) |>
    select(
      pid,
      wave,
      age,
      gender,
      edu_level,
      employment,
      marital_status,
      wemwbs,
      diaries_completed,
      self_reported_weekly_play,
      self_reported_played_24h,
      displaced_core_domain,
      bpnsfs_1,
      bpnsfs_2,
      bpnsfs_3,
      bpnsfs_4,
      bpnsfs_5,
      bpnsfs_6,
      bangs_1,
      bangs_2,
      bangs_3,
      bangs_4,
      bangs_5,
      bangs_6
    ) |>
    pivot_wider(
      id_cols = c(
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
      names_from = wave,
      values_from = c(
        self_reported_played_24h,
        displaced_core_domain,
        bpnsfs_1,
        bpnsfs_2,
        bpnsfs_3,
        bpnsfs_4,
        bpnsfs_5,
        bpnsfs_6,
        bangs_1,
        bangs_2,
        bangs_3,
        bangs_4,
        bangs_5,
        bangs_6
      ),
      names_sep = "_w"
    )
}

# Build the MICE method vector, predictor matrix, and conditional where-matrix
# for a wide-format survey data frame produced by build_wide_survey().
setup_mice_inputs <- function(surveys_wide) {
  played_vars <- names(surveys_wide)[grepl(
    "^self_reported_played_24h_w",
    names(surveys_wide)
  )]
  bpnsfs_vars <- names(surveys_wide)[grepl("^bpnsfs_[1-6]_w", names(surveys_wide))]
  bangs_vars <- names(surveys_wide)[grepl("^bangs_[1-6]_w", names(surveys_wide))]
  displaced_vars <- names(surveys_wide)[grepl(
    "^displaced_core_domain_w",
    names(surveys_wide)
  )]

  methods <- setNames(rep("", ncol(surveys_wide)), names(surveys_wide))
  methods[played_vars] <- "logreg"
  methods[c(bpnsfs_vars, bangs_vars, displaced_vars)] <- "pmm"

  person_level_vars <- c(
    "pid",
    "age",
    "gender",
    "edu_level",
    "employment",
    "marital_status",
    "wemwbs",
    "diaries_completed",
    "self_reported_weekly_play"
  )
  methods[person_level_vars] <- ""

  # Impute all missing cells by default, except: don't impute BANGS items
  # when the person reported not playing that day (self_reported_played_24h
  # == "No") and the BANGS item is missing -- those aren't missing data, the
  # question wasn't applicable.
  where_matrix <- is.na(surveys_wide)
  for (bangs_var in bangs_vars) {
    wave_num <- str_extract(bangs_var, "\\d+$")
    played_var <- paste0("self_reported_played_24h_w", wave_num)
    if (played_var %in% names(surveys_wide)) {
      no_play <- surveys_wide[[played_var]] == "No" &
        is.na(surveys_wide[[bangs_var]])
      where_matrix[no_play, bangs_var] <- FALSE
    }
  }

  pred <- quickpred(
    surveys_wide,
    mincor = 0.3,
    minpuc = 0.3,
    include = c("age", "wemwbs", "self_reported_weekly_play"),
    exclude = c(
      "pid",
      "gender",
      "edu_level",
      "employment",
      "marital_status",
      "diaries_completed"
    )
  )

  list(methods = methods, predictorMatrix = pred, where = where_matrix)
}

# Run MICE (parallelized via futuremice) on a wide-format survey data frame.
run_mice <- function(surveys_wide, m = 27, maxit = 5, seed = 8675309) {
  setup <- setup_mice_inputs(surveys_wide)
  futuremice(
    data = surveys_wide,
    m = m,
    method = setup$methods,
    predictorMatrix = setup$predictorMatrix,
    where = setup$where,
    maxit = maxit,
    n.core = parallel::detectCores() - 1,
    parallelseed = seed
  )
}

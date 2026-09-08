# Fit a glmmTMB model to each of `m` imputed datasets (dat filtered to
# .imp == i), in parallel via furrr with a reproducible seed (L'Ecuyer-CMRG
# RNG streams -- reproducible regardless of worker count or scheduling).
# Used by every H1/H2/H3 and S1-S9 multiple-imputation model-fitting step.
fit_mi_models <- function(
  dat,
  formula,
  m,
  family = gaussian(),
  seed = 8675309,
  workers = NULL,
  ...
) {
  workers <- workers %||% max(1, parallel::detectCores() - 1)
  future::plan(future::multisession, workers = workers)
  furrr::future_map(
    seq_len(m),
    function(i) {
      dat_i <- dplyr::filter(dat, .imp == i)
      glmmTMB::glmmTMB(formula, data = dat_i, family = family, ...)
    },
    .options = furrr::furrr_options(seed = seed)
  )
}

# Complete-case fit (no imputation): drop rows missing any `key_vars`, fit
# once, and wrap in mice::pool() so the result has the same pooled-summary
# structure as the MI fits. With no missing data the between-imputation
# variance is zero, so the pooled estimate/SE equal the single fit's.
fit_cc_model <- function(dat, formula, key_vars, family = gaussian(), ...) {
  cc <- tidyr::drop_na(dat, dplyr::all_of(key_vars))
  fit <- glmmTMB::glmmTMB(formula, data = cc, family = family, ...)
  mice::pool(rep(list(fit), 2))
}

# R Scripts and Utilities

This directory contains R functions and standalone analysis scripts.

## Files

### `plot_marginal_effects.R`
Reusable function for plotting marginal effects with individual trajectories. Automatically handles binary vs continuous outcomes.

**Usage in index.qmd:**
```r
source("R/plot_marginal_effects.R")

plot_marginal_effects(
  model = h1mod,
  x_var = "game_ns_cw",
  x_label = "Need satisfaction in games",
  y_label = "Need satisfaction in daily life",
  color = "blue",
  within_vars = c("game_ns_cw"),
  between_vars = c("game_ns_cb")
)
```

### `determine_m_imputations.qmd`
Standalone document for determining the required number of multiple imputations using von Hippel's (2020) protocol.

**Workflow (run once):**
1. In `index.qmd`, set `DETERMINE_M = TRUE`
2. Run `index.qmd` through the `post-imputation-centering` chunk
3. Render this document: `quarto render R/determine_m_imputations.qmd`
4. Follow the output instructions to update `M_FINAL` in `index.qmd`
5. Set `DETERMINE_M = FALSE` in `index.qmd`

**What it does:**
- Fits simplified pilot models to each imputation
- Calculates Fraction of Missing Information (FMI) using Rubin's rules
- Determines required M using von Hippel's quadratic rule
- Provides diagnostics for anomalously high FMI values
- Generates summary tables and visualizations

**Output:** HTML report with clear instructions for updating `index.qmd`

### `categorize_activities/`
Directory containing the activity classification script and its artifacts.

See `categorize_activities/README.md` for details.

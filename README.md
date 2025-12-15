# All BANG, little buck: Weak links between basic psychological needs and logged gaming behaviour

🔗 The manuscript and analysis output from this repo can be viewed at [https://nballou.github.io/basic-needs-in-games/](https://nballou.github.io/basic-needs-in-games/). 🔗

📄 The Stage 1 Registered Report for this project is available on OSF ([https://osf.io/pb5nu](https://osf.io/pb5nu)). 📄

This repository hosts the analysis code and manuscript for our Stage 2 Registered Report investigating the relationship between basic psychological needs and video game play behavior. Using intensive longitudinal data from the Open Play dataset, we test hypotheses derived from Self-Determination Theory about how need satisfaction and frustration relate to gaming behavior.

## Getting Started

The best starting point for understanding this analysis is to view the rendered manuscript at [https://nballou.github.io/basic-needs-in-games/](https://nballou.github.io/basic-needs-in-games/).

Key elements of the repo are:

- `index.qmd`, which generates the full manuscript including methods, results, and sensitivity analyses
- `R/plot_marginal_effects.R`, custom plotting function for visualizing model predictions
- `R/categorize_activities.qmd`, script for classifying displaced activities into core life domains
- `R/determine_m_imputations.qmd`, script for determining the number of imputations needed based on fraction of missing information
- `data/models/`, cached model fits to speed up rendering
- `references.bib`, bibliography for the manuscript
- `renv.lock`, package versions used in the analysis

## Data

This project uses data from the Open Play dataset, which is automatically downloaded from Zenodo when rendering the manuscript. The data includes:

- Multi-platform gaming telemetry (Nintendo Switch, Xbox, Steam)
- Daily diary surveys measuring need satisfaction/frustration and wellbeing
- Displaced activity classifications

Raw data is documented in detail in the [Open Play repository](https://github.com/digital-wellbeing/open-play) and its associated [codebook](https://github.com/digital-wellbeing/open-play/raw/refs/heads/main/codebook.xlsx).

## Reproducing

To reproduce the manuscript:

1. Ensure you have R and Quarto installed
2. Clone this repository
3. Install package dependencies: `renv::restore()` in R
4. Run `quarto render` from the project root

The first render will download the ~194MB dataset from Zenodo, which may take a few minutes. Subsequent renders will use the cached data.

**Note:** Model fits are cached in `data/models/` to speed up rendering. To refit all models from scratch, delete this directory before rendering.

## Contributing

Contributors should ensure they are up-to-date on the `main` branch and have installed all necessary packages from `renv` by running `renv::restore()` in R.

Contributors should then:

- create a new branch for their work
- make any desired changes (for readability, we recommend making frequent small commits focused on changing an individual feature, but these can be pushed less frequently)
- run `quarto render` locally to ensure their changes do not break the build
- submit a pull request when they are ready to merge their changes back into `main`

Rendering locally will update the `_freeze` folder, which in turn will update the GitHub Pages site when the PR is merged; make sure PRs commit changes to `_freeze` or they will fail the build check.

## R Code

In `R/`, we have helper functions and supplementary analyses:

- `plot_marginal_effects.R`: Custom plotting function for model predictions with individual trajectories
- `categorize_activities.qmd`: Uses LLM to classify free-text displaced activities into life domains
- `determine_m_imputations.qmd`: Calculates required number of imputations based on FMI
- `diagnose_imputation.R`: Diagnostic functions for checking imputation quality
- `test_analysis.R`: Unit tests for key analysis functions

## Project Structure

The analysis pipeline consists of:

1. **Data loading** (`index.qmd` lines 131-195): Downloads and reads data from Zenodo
2. **Data preparation** (`index.qmd` lines 196-314): Processes telemetry and survey data, calculates play windows
3. **Multiple imputation** (`index.qmd` lines 487-801): Handles missing data using MICE
4. **Main analyses** (`index.qmd` lines 1264-1966): Tests hypotheses H1-H3
5. **Sensitivity analyses** (`index.qmd` lines 2044-3275): Eight sensitivity analyses testing robustness
6. **Diagnostics** (`index.qmd` lines 3277-end): Imputation diagnostics and convergence checks

## License

Code is licensed under MIT. Data is licensed under CC0 (see [Open Play repository](https://github.com/digital-wellbeing/open-play) for details).

# Activity Classification Directory

This directory contains the Quarto document for classifying activity descriptions using Ollama, plus its associated artifacts.

## Files

- **categorize_activities.qmd** - Main classification script (in `R/utils/`)

## Generated Artifacts (gitignored)

- **categorize_activities_cache/** - Quarto cache for expensive LLM operations (sibling to this directory, in `R/utils/`)
- **categorize_activities_files/** - Rendered HTML output files (sibling to this directory, in `R/utils/`)
- **_log/** - Real-time progress logs

## Monitoring Progress

When rendering, monitor real-time progress with:
```bash
tail -f R/utils/categorize_activities_support/_log/classification_progress.log
```

## Output Data

Classification results are saved to the main data directory:
- `data/activity_categories.csv` - Production output
- `data/activity_categories_TEST.csv` - Test mode output

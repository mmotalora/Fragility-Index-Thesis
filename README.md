# Fragility Index — Perioperative Guidelines (Thesis Code)

This repository contains the R code supporting the thesis:

> **“The Fragility Index of the randomized clinical trials supporting the North American and European perioperative care guidelines for anesthesiologists between 2012 and 2022.”**

Author: **Margarita María Otálora**  
Supervisors: Martha B. Delgado, Fabián Gil, Lehana Thabane

---

## Contents

```
R/                      # Small helper functions (optional)
scripts/                # Ordered analysis scripts (run top-to-bottom)
data/
  raw/                  # Raw inputs (not in repo)
  processed/            # Intermediate, derived data
figures/                # Generated figures
outputs/
  tables/               # CSV/TSV tables exported by scripts
  models/               # Saved model objects (RDS)
.github/workflows/      # (Optional) CI for R CMD checks / linting
```

> ⚠️ No patient-level data is included. This project only uses published results and summary data extracted from RCTs cited in eligible guidelines.

---

## Reproducible Environment

- R (≥ 4.1.1)
- R packages: `fragilityindex`, `MASS`, `dplyr`, `tidyr`, `ggplot2`, `readr`, `purrr`, `stringr`, `broom`, `performance`, `car`, `sampler`
- (Optional) Use `renv` for project-local dependency management.

You can install all needed packages with:

```r
source("scripts/00_setup.R")
```

To capture the exact environment used in your run:

```r
source("scripts/07_reproducibility_checks.R")
# Produces outputs/sessionInfo.txt
```

---

## How to Run

1. Clone/download this repository.
2. Place your **extraction sheet / summary dataset** into `data/raw/` (see schema hints inside `scripts/01_data_ingest_and_clean.R`).
3. Run the scripts in order:

```bash
Rscript scripts/00_setup.R
Rscript scripts/01_data_cleaning.R
Rscript scripts/02_sample_size.R
Rscript scripts/03_summary_tables.R
Rscript scripts/04_Fragility_Index.R
Rscript scripts/05_analysis.R
Rscript scripts/06_figures.R
Rscript scripts/07_reproducibility_checks.R
```

Key outputs:
- Figures saved to `figures/` (PRISMA, FI/FQ plots, diagnostics, forest-like regression summary)
- Tables saved to `outputs/tables/`
- Model objects (negative binomial) saved to `outputs/models/`

---

## Data Notes

- `data/raw/` should contain the CSV/TSV you built from the guideline-linked RCTs.
- Variables expected include: `fi_single`, `pval_significance`, `sample_size`, `ltfu`, `moved`, `c_participants.factor`, `c_design.factor`, `rct_aim.factor`, `i_type.factor`, `rct_conceal_d.factor`, `rct_blind_d.factor`, `rct_centers.factor`, `funding.factor`, `pval_significance`, and the interaction `pval_significance*sample_size` (see scripts for mapping).
- If your column names differ, adapt the mapping section at the top of each script.

---

## Citation

If you use this code, please cite the thesis and (optionally) the Zenodo DOI you create for this repository.

---

## License

MIT — see `LICENSE`.

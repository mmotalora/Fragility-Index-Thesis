# Fragility Index --- Perioperative Guidelines (Thesis Code)

This repository contains the analytic dataset and R code supporting the
thesis:

> **"The Fragility Index of the randomized clinical trials supporting
> the North American and European perioperative care guidelines for
> anesthesiologists between 2012 and 2022."**

Author: **Margarita María Otálora Esteban**\
Supervisors: Martha B. Delgado, Fabián Gil, Lehana Thabane

------------------------------------------------------------------------

## Repository Structure

    00_utils.R
    01_data_cleaning.R
    02_sample_size.R
    03_summary_tabs.R
    04_compute_fragility.R
    04_tables.R
    05_analysis.R
    06_figures.R
    99_run_all.R

    data/
      fi_guideline_trials_master_2026-02.xlsx
      sample_list.xlsx
      PRISMA.csv

------------------------------------------------------------------------

## Data

This repository includes the full trial-level analytic dataset used for
all analyses:

-   `fi_guideline_trials_master_2026-02.xlsx` --- Master analytic
    dataset of included RCTs\
-   `sample_list.xlsx` --- Eligible trial sample list\
-   `PRISMA.csv` --- PRISMA flow information

⚠️ No patient-level or identifiable data are included.\
All data were extracted from published randomized controlled trials
cited in eligible perioperative clinical practice guidelines.

------------------------------------------------------------------------

## Reproducibility

All analyses, tables, and figures can be reproduced from a fresh R
session by running:

``` r
source("99_run_all.R")
```

The scripts are structured in ordered steps:

1.  Data cleaning\
2.  Sample size calculations\
3.  Summary tables\
4.  Fragility Index and Reverse FI computation\
5.  Regression modeling (negative binomial)\
6.  Figures and tables generation

------------------------------------------------------------------------

## R Version and Packages

Tested with:

-   R ≥ 4.1

Required packages include:

-   `readxl`
-   `dplyr`
-   `tidyr`
-   `ggplot2`
-   `MASS`
-   `broom`
-   `performance`
-   `car`
-   `fragilityindex`

Install any missing packages using:

``` r
install.packages(c("readxl","dplyr","tidyr","ggplot2","MASS","broom","performance","car","fragilityindex"))
```

------------------------------------------------------------------------

## Outputs

Running `99_run_all.R` will generate:

-   Summary tables\
-   Regression model outputs\
-   Fragility Index calculations\
-   Publication-ready figures

Outputs are written to the working directory unless otherwise specified
in the scripts.

------------------------------------------------------------------------

## Citation

If using this code or dataset, please cite the thesis:

Otálora MM. *The Fragility Index of the randomized clinical trials
supporting North American and European perioperative care guidelines for
anesthesiologists between 2012 and 2022.*

------------------------------------------------------------------------

## License

MIT License --- see `LICENSE`.

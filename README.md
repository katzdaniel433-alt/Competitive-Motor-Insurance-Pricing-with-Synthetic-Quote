# Competitive Motor Insurance Pricing with Synthetic Quote Markets

**Conversion Modelling and Price Optimisation**  
**Daniel F. Katz | Actuarial Science Honours | Curtin University | 2026**

## Overview

This repository contains the R Markdown code for an actuarial honours dissertation investigating whether competitive quotation information improves motor insurance conversion modelling and supports constrained pricing decisions when observed competitor quotations and unsuccessful conversion outcomes are unavailable.

The experiment generates **50,000 synthetic Australian motor insurance policy profiles** across the 2021–2025 cohorts, fits technical-pricing models, simulates a quote market with **six competing insurers**, generates customer purchase outcomes, compares conversion models and optimises the reference insurer's premiums. The 2025 quote cohort is split into **6,000 training**, **2,000 validation** and **2,000 test** profiles. Structural sensitivity, Monte Carlo regeneration and paired bootstrap analyses assess the stability of the findings.

In the dissertation's baseline independent test evaluation, targeted repricing increased expected reference-insurer conversion from **18.28% to 21.91%** (a **19.88% relative increase**) and increased **expected contribution after claims by 2.85%**, while keeping total quoted premiums close to the technical-pricing benchmark. These are **results of a simulated customer-choice process**, not estimates of outcomes achievable in the Australian insurance market.

## Repository layout

The `.Rmd` modules intentionally remain in the **repository root**: the code uses root-relative paths, and the sensitivity and Monte Carlo modules refer to earlier modules by their exact filenames.

```text
.
├── README.md
├── .gitignore
├── references.bib
├── 00_prepare_empirical_calibration_inputs.Rmd
├── 01_generate_synthetic_motor_portfolio.Rmd
├── 02_fit_technical_pricing_model.Rmd
├── 03_construct_synthetic_quote_market.Rmd
├── 04_generate_conversion_outcomes.Rmd
├── 05_fit_conversion_models.Rmd
├── 06_constrained_pricing_simulation.Rmd
├── 06b_paired_test_bootstrap_uncertainty.Rmd
├── 07_robustness_sensitivity_analysis.Rmd
├── 08_monte_carlo_seed_stability.Rmd
├── 09_generate_dissertation_figures_tables.Rmd
├── R/
│   └── helpers.R                        # Required shared functions
├── inputs/
│   ├── README.md                        # Input descriptions and sources
│   ├── raw/                             # Raw public calibration files
│   │   └── multiyear/
│   ├── calibrated_multiyear/            # Module 00 output
│   ├── vehicle_segment_lookup.csv       # Curated input, if shareable
│   ├── carexpert_vehicle_spec_lookup_95PCT.csv
│   └── racq_2025_vehicle_values.csv
├── outputs/                              # Generated; not tracked by default
└── dissertation/
    └── dissertation.pdf                 # Optional final thesis PDF
```

The input filenames shown above are examples of files expected by Module 00, **not files included automatically in the module archive**. Consult its configuration for the additional BITRE/ABS source filenames and supported input locations. Include third-party input data in a public repository only where redistribution is permitted.

## Computational workflow

| Module | Purpose |
|---|---|
| 00 | Prepare empirical calibration inputs from public statistics and curated vehicle data |
| 01 | Generate synthetic policies, claims and separately stored underlying expected claims costs |
| 02 | Fit paid-claim frequency and severity models and estimate technical premiums |
| 03 | Simulate reference and competitor quotations and calculate market-position variables |
| 04 | Generate customer purchasing decisions from the synthetic choice model |
| 05 | Fit and evaluate the GLM/GAM conversion models |
| 06 | Select and evaluate uniform and targeted premium adjustments |
| 06B | Bootstrap the independent test comparison using paired customer outcomes |
| 07 | Re-run relevant stages under alternative structural assumptions |
| 08 | Regenerate and re-estimate the experiment across independent random seeds |
| 09 | Produce dissertation figures and summary tables from saved outputs |

The main modelling sequence is **00 → 01 → 02 → 03 → 04 → 05 → 06**. Module **06B** follows Module 06. Modules **07** and **08** conduct additional analyses using the earlier modules; **09** is run once the required result files are present.

## Requirements

- R and RStudio (or an equivalent R Markdown environment).
- R Markdown, Pandoc and the R packages used by the modules.
- The shared file `R/helpers.R` and the empirical calibration inputs required by Module 00.

Install the principal packages in R:

```r
install.packages(c(
  "rmarkdown", "tidyverse", "readxl", "jsonlite", "broom",
  "mgcv", "lpSolve", "ggrepel", "knitr", "scales"
))
```

Run the modules **from the repository root**. For example:

```r
rmarkdown::render(
  "00_prepare_empirical_calibration_inputs.Rmd",
  knit_root_dir = normalizePath(".", mustWork = TRUE)
)
```

Repeat for subsequent modules in the order described above. The script `R/helpers.R` must be located in the `R/` directory, and the raw/curated input files must be placed at the paths expected by Module 00. Its CarExpert specification lookup is configured as `carexpert_vehicle_spec_lookup_95PCT.csv` and requires a weighted coverage check; the file cannot be reconstructed merely from the `.Rmd` modules.

## Outputs and evaluation

Generated files are written under `outputs/`, including:

```text
outputs/
├── synthetic_generator_multiyear/
├── technical_pricing/
├── quote_market/
├── conversion_generation/
├── conversion_models/
├── pricing_simulation/
├── pricing_bootstrap_uncertainty/
├── sensitivity_analysis/
├── monte_carlo_stability/
└── dissertation_exhibits/
    ├── figures/
    └── tables/
```

The evaluation distinguishes **reported claims** from **paid claims**, and **predicted conversion probabilities** from the underlying simulated customer-choice probabilities. Pricing decisions are selected using development data; the independent test sample is used to evaluate the selected strategy.

The analysis measures **expected contribution after claims** (conversion-weighted premium revenue less fitted expected claims costs). This is **not net insurer profit**: expenses, reinsurance, capital charges and other financial items are not separately modelled in the optimisation.

## Reproducibility and limitations

The code uses fixed seeds and saves intermediate files and validation checks. Reproduction requires the complete helper code, compatible package versions and the exact empirical/curated calibration inputs.

The portfolio is synthetic. Selected characteristics are informed by Australian public statistics, but the claims relationships, competitor pricing and customer demand include explicit simulation assumptions. Results should be interpreted as evidence about the performance of the proposed *methodology within that environment*, not empirical estimates of Australian customer behaviour.

# Depression awareness and self-perception among Syrian university students

Reproducible analysis pipeline for a cross-sectional survey of 967 medicine,
dentistry and pharmacy students at Damascus University and other Syrian
universities, measuring depressive symptoms with the Arabic Zung Self-Rating
Depression Scale (SDS) and knowledge of depression with a 17-item awareness
score.

One R script takes the raw survey workbook and writes **every table and figure
in the manuscript and its supplement** — 48 CSV tables and 19 PNG figures — with
no manual steps in between.

## Repository layout

```
.
├── R/analysis_SDS.R      the entire pipeline: cleaning -> tables -> figures
├── data/                 raw and cleaned workbooks (not distributed - see below)
├── docs/
│   ├── WORKFLOW.md       step-by-step rationale for every analytic decision
│   └── CODEBOOK.md       variable definitions, coding and derivations
└── outputs/
    ├── tables/           48 result tables as CSV
    └── figures/          19 figures as PNG (300 dpi)
```

## Requirements

R 4.3 or newer. The script installs anything missing from CRAN on first run:

`readxl`, `writexl`, `dplyr`, `tidyr`, `stringr`, `tibble`, `forcats`,
`ggplot2`, `scales`, `psych`, `GPArotation`, `broom`, `pROC`, `binom`,
`rcompanion`, `gridExtra`, `RColorBrewer`

## Running it

Put the raw survey workbook at `data/Depression Awareness.xlsx`, then from the
repository root:

```bash
Rscript R/analysis_SDS.R
```

Everything is written to `outputs/`. The run is deterministic — the bootstrap in
the mediation analysis is seeded — so re-running reproduces identical numbers.
To run from another working directory, point `SDS_BASE` at the repository root:

```bash
SDS_BASE=/path/to/depression-awareness-syria Rscript R/analysis_SDS.R
```

## Data availability

**The individual-level data are not included in this repository.** The survey
collected age, sex, faculty, socioeconomic status, chronic illness, prior
psychiatric diagnoses and item-level depression-scale responses; publishing that
at the row level is not covered by the consent participants gave. `data/` ships
empty apart from its own README, and `.gitignore` excludes `*.xlsx` so a local
copy is never committed by accident.

Everything in `outputs/` is aggregate and carries no individual-level
information, so the full set of published tables and figures is here and can be
checked against the manuscript without the source data.

Requests for the de-identified dataset go to the corresponding author, subject
to the approval of the Damascus University IRB (No. MD-281123-160).

## What the pipeline produces

| Prefix | Contents |
|---|---|
| `T1`–`T13b` | main-text tables: descriptives, reliability, item and factor analysis, bivariate tests, both regression models, subgroup and interaction analyses |
| `S1`–`S8` | supplementary: missingness, item distributions, post-hoc comparisons, full models, SDS ≥ 60 sensitivity |
| `S5`–`S10` | logistic-model assumption diagnostics: VIF and tolerance, Box–Tidwell, Cook's distance and leverage, Hosmer–Lemeshow, model adequacy, influence sensitivity |
| `S11`, `S11b` | linear-model diagnostics and the HC3 robust-standard-error refit |
| `T_*` | awareness items, information sources, reliability, mediation, moderation, outliers |

## Analysis notes

- **Non-parametric by default.** Shapiro–Wilk and Kolmogorov–Smirnov both reject
  normality of the SDS index, so group comparisons use Mann–Whitney U and
  Kruskal–Wallis with rank-biserial *r* and epsilon-squared effect sizes, and
  post-hoc pairwise Wilcoxon tests with Holm correction.
- **Odds ratios use Wald confidence intervals** (`exp(b ± 1.96 SE)`), matching
  the published tables and the sensitivity model.
- **Both regression models are diagnosed before interpretation.** The logistic
  model passes multicollinearity, linearity of the logit, influence, calibration
  and separation checks. The linear model fails Breusch–Pagan (LM = 35.01,
  *p* = 0.014), so it is also refitted with HC3 robust standard errors; only two
  academic-year contrasts change significance.
- **Implausible values are flagged, never dropped.** Seven GPA values below 20 on
  a 0–100 scale and a handful of impossible heights and BMIs are reported in
  `T_outliers.csv` and handled through sensitivity refits.

## Headline results

| | |
|---|---|
| Participants | 967 (0% missing on every variable) |
| Screen-positive for probable depression (SDS ≥ 50) | 61.1% (95% CI 58.0–64.1) |
| Zung SDS internal consistency | Cronbach's α = 0.881 |
| Awareness composite (17 items) | KR-20 = 0.548, median 11 of 17 |
| Linear model of the SDS index | R² = 0.185 (adjusted 0.169) |
| Logistic model discrimination | AUC = 0.687 |

## Citation

See `CITATION.cff`. Please cite the published paper rather than this repository
alone.

## License

Code is released under the MIT License (`LICENSE`). The manuscript text,
tables and figures remain under the copyright of their authors and publisher.

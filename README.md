# Replication of Ockenfels, Sliwka & Werner, "Multirater Performance Evaluations and Incentives"

Problem Set 3, Empirical Methods (University of Zurich, HS 2026)
Group: Nanami Fähndrich, Anina Kastner, Nicolas Santner, Stephania Skortidis

This repository contains our own R code for the replication of Table 3 and Figure 1 of
Ockenfels, A., Sliwka, D., & Werner, P., *Journal of Labor Economics* 43(4), 985–1004,
https://doi.org/10.1086/730321, and for our extension (fixed vs. random effects with Hausman test).

## Quick start

Set the working directory to this folder and run the master script:

```r
setwd("<path>/ps3-replication/PS3")
source("code/00_run_all.R")
```

This reproduces all tables and figures of the report from the raw data. No random numbers are used.

## Folder structure

| Folder / file | Content |
|---|---|
| `PS3/code/00_run_all.R` | Master script, runs scripts 04–07 in this order |
| `PS3/code/04_table3_submission.R` | Replication of Table 3 (report: Table 1) → `PS3/output/Table3_Replication.docx/.png` |
| `PS3/code/05_figure1_submission.R` | Replication of Figure 1 (report: Figure 1) → `PS3/output/Figure1_Replication.png` |
| `PS3/code/06_extension_fe_vs_re.R` | Extension: random vs. fixed effects with Hausman test (report: Table 2) → `PS3/output/Table_Extension_FE_vs_RE.docx/.png` |
| `PS3/code/07_critical_evaluation_checks.R` | Checks for Section 4.1 of the report (omitted variables, reverse causality, team ratings, learning, correlations); results are printed to the console |
| `PS3/data/Replication_Data_OSW_Multi-rater.dta` | Original replication data from the JOLE supplementary material (unmodified) |
| `PS3/output/` | Tables and figures as used in the report |

## Data source

The data file is taken unmodified from the authors' replication package (JOLE supplementary material):
https://www.journals.uchicago.edu/doi/10.1086/730321#supplementary-materials

The authors' original code (Stata do-file) and output are **not** part of this repository.

## Software

R 4.4.1 with the packages haven, plm, lmtest, car, flextable, ggplot2 and gridExtra.

```r
install.packages(c("haven", "plm", "lmtest", "car", "flextable", "ggplot2", "gridExtra"))
```

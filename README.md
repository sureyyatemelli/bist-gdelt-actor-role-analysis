# BIST–GDELT Actor-Role Analysis

Reproducible R analysis of BIST 100 responses to GDELT political events, actor roles, volatility, and quantile effects.

## Overview

This repository contains the R code and supporting files used to examine the relationship between political events captured by GDELT and Turkish stock market dynamics.

The empirical framework focuses on the role of political-event characteristics and actor positions in explaining BIST 100 market responses. The analysis includes return models, volatility models, quantile regressions, robustness checks, and measures of economic significance.

## Repository Structure

```text
bist-gdelt-actor-role-analysis/
│
├── README.md
├── LICENSE
├── .gitignore
│
├── code/
│   ├── run_all.R
│   ├── 03_tanimlayici_istatistikler.R
│   ├── 04_aktor_rolu_modelleri.R
│   ├── 04b_volatilite_modelleri.R
│   ├── 05_kantil_regresyon.R
│   ├── 06_saglamlik_ekonomik_anlamlilik.R
│   └── 07_session_info.R
│
├── data/
│
├── results/
│   ├── tables/
│   └── metrics/
│
└── figures/
```

## Empirical Framework

The analysis pipeline uses:

* Newey–West HAC inference for return models
* Newey–West HAC inference for volatility models
* Bootstrap inference for quantile regressions
* 500 bootstrap replications for quantile estimates
* Contemporaneous reaction models based on political-event information at time \(t\)
* Predictive specifications using lagged political-event information

The empirical strategy distinguishes contemporaneous market reactions from predictive relationships.

## Running the Analysis

The complete empirical analysis can be reproduced from the project root directory.

In R or RStudio, run:

```r
source("code/run_all.R")
```

The working directory must be the root directory of the repository rather than the `code` directory.

You can verify the working directory using:

```r
getwd()
```

and inspect the available files using:

```r
list.files()
```

## Analysis Pipeline

The master script executes the following files sequentially:

```text
03_tanimlayici_istatistikler.R
04_aktor_rolu_modelleri.R
04b_volatilite_modelleri.R
05_kantil_regresyon.R
06_saglamlik_ekonomik_anlamlilik.R
07_session_info.R
```

If all scripts are executed successfully, the R console will display:

```text
ALL ANALYSES COMPLETED SUCCESSFULLY
```

## Main Outputs

The analysis generates the following main outputs.

### Descriptive Statistics

```text
results/tables/tanimlayici_istatistikler.csv
```

### Actor-Role Models

```text
results/tables/aktor_rolu_modelleri.csv
results/metrics/aktor_rolu_esitlik_testi.json
```

### Volatility Models

```text
results/tables/volatilite_modelleri.csv
results/metrics/volatilite_aktor_rolu_esitlik_testi.json
```

### Quantile Regression

```text
results/tables/kantil_katsayilari.csv
results/tables/marjinal_etki_persentiller.csv
results/metrics/kantil_model_ozeti.json
```

### Figures

```text
figures/kantil_katsayi_yolu.png
figures/marjinal_etki.png
```

### Robustness and Economic Significance

```text
results/tables/saglamlik_modelleri.csv
results/tables/ekonomik_anlamlilik_var.csv
results/metrics/tani_kontrolleri.json
```

### Reproducibility Information

```text
results/metrics/session_info.txt
```

## Software

The analysis is implemented in R.

Exact package and software-version information generated during the analysis is reported in:

```text
results/metrics/session_info.txt
```

## Data

The analysis combines financial-market data for the BIST 100 index with political-event information derived from GDELT.

Raw data may not be included in the repository where redistribution restrictions, file-size limitations, or reproducibility considerations apply.

Additional information on data preparation and data sources should be provided in the associated manuscript.

## Reproducibility

For reproducible execution:

1. Clone or download the repository.
2. Open the project root directory in RStudio.
3. Install the required R packages.
4. Place the required input data in the appropriate `data/` directory.
5. Run:

```r
source("code/run_all.R")
```

6. Verify that the expected tables, metrics, and figures are created in the `results/` and `figures/` directories.

## Citation

If you use this repository, please cite the associated article.

Citation details will be added after publication.

## License

This repository is distributed under the MIT License.


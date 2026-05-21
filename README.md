# Volatility Spillovers in the Brazilian Tilapia Market

This repository contains an R workflow for analyzing price integration and dynamic connectedness across the Brazilian tilapia supply chain using a **Time-Varying Parameter Vector Autoregression (TVP-VAR)** connectedness framework proposed by Antonakakis et al. (2020).

The script integrates:

- Farm-gate prices
- Wholesale market prices
- Export prices
- Exchange rate dynamics

The analysis evaluates how shocks propagate across domestic and export markets through volatility spillovers and directional connectedness measures.

---

# Research Scope

The script investigates interactions among:

- Farm-gate tilapia markets in Brazil
- Wholesale prices
- Export prices for different product categories
- BRL/USD exchange rate

The methodology combines:

1. Data preprocessing and harmonization
2. Stationarity diagnostics
3. TVP-VAR connectedness estimation
4. Spillover decomposition
5. Regression analysis between export prices, exchange rate, and domestic prices

---

# Methodological Framework

The connectedness analysis follows:

> Antonakakis, N., Chatziantoniou, I., & Gabauer, D. (2020). Refined measures of dynamic connectedness based on TVP-VAR. *Journal of Risk and Financial Management*, 13(4), 84.

The script estimates:

- Total Connectedness Index (TCI)
- Directional spillovers ("TO" and "FROM")
- Net spillovers
- Dynamic connectedness networks

---

# Repository Structure

```text
.
├── ReturnSpillovers.R
├── CEPEA_20251209101009.xls
├── tilapia_ceagesp.xlsx
├── Export_GeralTilapiaBrasil.xlsx
├── output/
│   ├── figures/
│   ├── tables/
│   └── regression_results/
└── README.md
```

---

# Data Sources

## Farm-Gate Prices

Source: CEPEA weekly tilapia price series.

Markets included:

- Grandes Lagos
- Norte do Paraná
- Oeste do Paraná

## Wholesale Prices

Source: CEAGESP daily market data aggregated to weekly frequency.

Products:

- Whole tilapia
- Tilapia fillet

## Export Data

Source: Brazilian foreign trade statistics from Comex Stat.

Export categories:

- Fresh fillets
- Frozen fillets
- Frozen whole fish
- Fresh whole fish

## Exchange Rate

Source: Banco Central do Brasil series accessed through the `rbcb` package.

---

# Main Features

## Data Processing

- Weekly and monthly aggregation
- Missing-value imputation
- Linear interpolation
- Currency conversion (USD → BRL)

## Statistical Diagnostics

- Descriptive statistics
- Augmented Dickey-Fuller tests
- Ljung-Box tests
- ARCH-LM tests
- Jarque-Bera normality tests
- VAR stability diagnostics

## Connectedness Analysis

- TVP-VAR estimation
- Dynamic spillover decomposition
- Network connectedness plots
- Total Connectedness Index (TCI)

## Regression Models

OLS regressions estimating:

```text
Export Prices ~ Exchange Rate + Domestic Prices
```

---

# Required R Packages

The script automatically loads the following packages:

```r
packages <- c(
  "readxl", "writexl", "dplyr", "tidyr", "purrr", "stringr",
  "lubridate", "zoo", "xts",
  "ggplot2", "patchwork",
  "vars", "lmtest", "sandwich", "tseries", "urca",
  "moments", "nortest", "FinTS", "MTS",
  "ConnectednessApproach",
  "rbcb",
  "knitr", "kableExtra"
)
```

Install missing packages with:

```r
install.packages(c(
  "readxl", "writexl", "dplyr", "tidyr", "purrr", "stringr",
  "lubridate", "zoo", "xts",
  "ggplot2", "patchwork",
  "vars", "lmtest", "sandwich", "tseries", "urca",
  "moments", "nortest", "FinTS", "MTS",
  "ConnectednessApproach",
  "rbcb",
  "knitr", "kableExtra"
))
```

---

# How to Run

## 1. Clone the Repository

```bash
git clone https://github.com/your_username/your_repository.git
```

## 2. Open the Project in RStudio

Open:

```text
ReturnSpillovers.R
```

## 3. Place Input Files in the Working Directory

Required files:

```text
CEPEA_20251209101009.xls
tilapia_ceagesp.xlsx
Export_GeralTilapiaBrasil.xlsx
```

## 4. Run the Script

Execute the script sequentially.

---

# Outputs Generated

## Figures

The script automatically exports publication-quality figures:

- Export stack plot
- Price series panels
- Correlation heatmaps
- Cross-sectional volatility
- Connectedness network
- FROM connectedness
- TO connectedness
- NET connectedness
- TCI dynamics
- NPT dynamics

## Tables

Generated outputs include:

- Spillover tables
- Regression summaries
- Descriptive statistics
- ARCH diagnostics
- Normality tests

## Exported Files

```text
spillover_table.xlsx
spillover_from.csv
spillover_to.csv
spillover_net.csv
Monthly_TilapiaBrazil.xlsx
```

---

# Econometric Specification

The TVP-VAR connectedness model is estimated using:

- Hannan-Quinn lag selection criterion
- Rolling connectedness estimation
- Forecast horizon of 24 periods

The workflow uses log-returns computed as:

```math
r_t = \ln(P_t) - \ln(P_{t-1})
```

---

# Example Connectedness Measures

The analysis produces:

- Directional spillovers TO other markets
- Directional spillovers FROM other markets
- Net spillover transmitters and receivers
- Dynamic network structures

---

# Author

**Vinicius Fellype Cavalcanti de França**

Research areas:

- Aquaculture economics
- Fish market integration
- Econometrics
- Time-series modeling
- Connectedness analysis

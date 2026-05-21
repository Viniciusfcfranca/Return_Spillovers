# =============================================================================
#  VOLATILITY SPILLOVERS IN THE BRAZILIAN TILAPIA MARKET
#  Author : Vinicius Fellype Cavalcanti de França
#  Purpose: Farm-gate, wholesale and export price integration via TVP-VAR
#           connectedness (Antonakakis et al., 2020)
# =============================================================================


# -----------------------------------------------------------------------------
# 0. DEPENDENCIES
# -----------------------------------------------------------------------------

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

invisible(lapply(packages, library, character.only = TRUE))


# -----------------------------------------------------------------------------
# 1. HELPER FUNCTIONS
# -----------------------------------------------------------------------------

#' Compute descriptive statistics for a numeric matrix / xts object
descriptive_stats <- function(x) {
  data.frame(
    Mean = colMeans(x),
    SD   = apply(x, 2, sd),
    Min  = apply(x, 2, min),
    Max  = apply(x, 2, max),
    Skew = apply(x, 2, moments::skewness),
    Kurt = apply(x, 2, moments::kurtosis)
  )
}

#' Run ADF test (k = 1) on every column of a data frame / xts
run_adf <- function(x) {
  lapply(as.data.frame(x), function(col) tseries::adf.test(col, k = 1))
}

#' Run Ljung-Box test (lag = 10) on every column of a data frame / xts
run_ljung_box <- function(x) {
  lapply(as.data.frame(x), function(col) Box.test(col, lag = 10, type = "Ljung-Box"))
}

#' Run univariate ARCH-LM test on every column of a residual matrix
run_arch_tests <- function(residuals, lags) {
  lapply(colnames(residuals), function(v) {
    arch <- FinTS::ArchTest(residuals[, v], lags = lags)
    data.frame(
      Variable  = v,
      Chi_sq    = round(arch$statistic, 4),
      p_value   = round(arch$p.value,   4)
    )
  }) |> dplyr::bind_rows()
}

#' Run Anderson-Darling and Jarque-Bera normality tests on every residual column
run_normality_tests <- function(residuals) {
  lapply(colnames(residuals), function(v) {
    ad <- nortest::ad.test(residuals[, v])
    jb <- tseries::jarque.bera.test(residuals[, v])
    data.frame(
      Variable    = v,
      AD_p_value  = round(ad$p.value, 4),
      JB_p_value  = round(jb$p.value, 4)
    )
  }) |> dplyr::bind_rows()
}

#' Shared ggplot2 theme used across all publication figures
theme_publication <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.title     = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(size = 9),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Save a ggplot object as a high-resolution PNG
save_plot <- function(plot, filename, width = 3800, height = 2060, res = 400) {
  grDevices::png(filename, width = width, height = height, res = res)
  print(plot)
  grDevices::dev.off()
}


# -----------------------------------------------------------------------------
# 2. DATA INGESTION
# -----------------------------------------------------------------------------

## 2.1  Farm-gate prices (CEPEA weekly series) --------------------------------

farm_raw <- readxl::read_excel("CEPEA_20251209101009.xls")

farm_dates <- seq(
  from       = as.Date("2021-08-06"),
  by         = "week",
  length.out = nrow(farm_raw)
)

farm_prices <- data.frame(
  Time            = farm_dates,
  Big_Lakes       = farm_raw$`Grandes Lagos Valor R$/KG`,
  Northern_Parana = farm_raw$`Norte do Paraná Valor R$/KG`,
  Western_Parana  = farm_raw$`Oeste do Paraná Valor R$/KG`
)

## 2.2  Wholesale prices (CEAGESP daily → weekly) -----------------------------

wholesale_raw <- readxl::read_excel("tilapia_ceagesp.xlsx", sheet = "CEAGESP_daily")

weekly_wholesale <- wholesale_raw |>
  dplyr::mutate(
    Date = as.Date(Date),
    week = lubridate::floor_date(Date, unit = "week", week_start = 5)
  ) |>
  dplyr::filter(Product %in% c("TILAPIA", "FILE DE TILAPIA")) |>
  dplyr::group_by(week, Product) |>
  dplyr::summarise(AveragePrice = mean(AveragePrice, na.rm = TRUE), .groups = "drop")

tilapia_weekly <- weekly_wholesale |>
  dplyr::filter(Product == "TILAPIA") |>
  dplyr::select(week, Wholesale = AveragePrice)

fillet_weekly <- weekly_wholesale |>
  dplyr::filter(Product == "FILE DE TILAPIA") |>
  dplyr::select(week, Fillet = AveragePrice)

# Align to farm-gate date grid, then impute within-month NAs with the monthly mean
wholesale_series <- data.frame(Time = farm_dates) |>
  dplyr::left_join(tilapia_weekly, by = c("Time" = "week")) |>
  dplyr::left_join(fillet_weekly,  by = c("Time" = "week")) |>
  dplyr::mutate(
    year  = lubridate::year(Time),
    month = lubridate::month(Time)
  ) |>
  dplyr::group_by(year, month) |>
  dplyr::mutate(
    Wholesale = dplyr::if_else(is.na(Wholesale), mean(Wholesale, na.rm = TRUE), Wholesale),
    Fillet    = dplyr::if_else(is.na(Fillet),    mean(Fillet,    na.rm = TRUE), Fillet)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-year, -month)

# Merge farm-gate and wholesale; keep only 2022 onward; drop Fillet column
prices_weekly <- farm_prices |>
  dplyr::filter(Time >= as.Date("2022-01-01")) |>
  dplyr::left_join(wholesale_series, by = "Time") |>
  dplyr::select(-Fillet)

# Linear interpolation for remaining NAs; manual fix for trailing observation
prices_weekly <- prices_weekly |>
  dplyr::mutate(
    Wholesale = zoo::na.approx(Wholesale, na.rm = FALSE),
    Wholesale = dplyr::case_when(
      Time == as.Date("2025-12-05") & is.na(Wholesale) ~ 11.282222,
      TRUE ~ Wholesale
    )
  )

## 2.3  Export data (MDIC/Comex Stat) ----------------------------------------

export_raw <- readxl::read_excel("Export_GeralTilapiaBrasil.xlsx")
colnames(export_raw) <- trimws(colnames(export_raw))

export_raw <- export_raw |>
  dplyr::mutate(
    Category = dplyr::case_when(
      stringr::str_detect(`Descrição NCM`, stringr::regex("filé|file",    ignore_case = TRUE)) &
        stringr::str_detect(`Descrição NCM`, stringr::regex("congelad",   ignore_case = TRUE)) ~ "Frozen fillets",
      stringr::str_detect(`Descrição NCM`, stringr::regex("filé|file",    ignore_case = TRUE)) &
        stringr::str_detect(`Descrição NCM`, stringr::regex("fresc|refrigerad", ignore_case = TRUE)) ~ "Fresh fillets",
      stringr::str_detect(`Descrição NCM`, stringr::regex("fresc|refrigerad",   ignore_case = TRUE)) ~ "Fresh whole",
      stringr::str_detect(`Descrição NCM`, stringr::regex("congelad",     ignore_case = TRUE)) ~ "Frozen whole",
      TRUE ~ "Other"
    ),
    Month_num = as.numeric(stringr::str_extract(Mês, "^\\d+")),
    Date      = as.Date(sprintf("%d-%02d-01", Ano, Month_num))
  )

# Monthly FOB value and unit price by category
export_prices <- export_raw |>
  dplyr::filter(Ano >= 2020) |>
  dplyr::group_by(Date, Category) |>
  dplyr::summarise(
    FOB       = sum(`Valor US$ FOB`,       na.rm = TRUE),
    KG        = sum(`Quilograma Líquido`,  na.rm = TRUE),
    Price_USD = FOB / KG,
    .groups   = "drop"
  )

## 2.4  Exchange rate (BCB series 1 – BRL/USD) --------------------------------

fx_daily <- rbcb::get_series(1, start_date = "2020-01-01")

fx_monthly <- fx_daily |>
  dplyr::mutate(
    date      = as.Date(date),
    Data_mes  = lubridate::floor_date(date, "month")
  ) |>
  dplyr::group_by(Data_mes) |>
  dplyr::summarise(FX = mean(`1`, na.rm = TRUE), .groups = "drop")

# Convert export unit prices to BRL
export_prices <- export_prices |>
  dplyr::mutate(Data_mes = lubridate::floor_date(Date, "month")) |>
  dplyr::left_join(fx_monthly, by = "Data_mes") |>
  dplyr::mutate(Price_BRL = Price_USD * FX)

# Retain only the two categories used in the analysis
export_series <- export_prices |>
  dplyr::filter(Category %in% c("Fresh fillets", "Frozen whole")) |>
  dplyr::select(Data_mes, Category, Price_BRL, Price_USD, FX)


# -----------------------------------------------------------------------------
# 3. MONTHLY PANEL CONSTRUCTION
# -----------------------------------------------------------------------------

# Aggregate farm-gate and wholesale prices to monthly averages
prices_monthly <- prices_weekly |>
  dplyr::mutate(Data_mes = lubridate::floor_date(Time, "month")) |>
  dplyr::group_by(Data_mes) |>
  dplyr::summarise(
    Big_Lakes       = mean(Big_Lakes,       na.rm = TRUE),
    Northern_Parana = mean(Northern_Parana, na.rm = TRUE),
    Western_Parana  = mean(Western_Parana,  na.rm = TRUE),
    Wholesale       = mean(Wholesale,       na.rm = TRUE),
    .groups = "drop"
  )

# Reshape export prices to wide format (one column per category × currency)
export_wide <- export_series |>
  tidyr::pivot_wider(
    names_from  = Category,
    values_from = c(Price_BRL, Price_USD),
    names_glue  = "{Category}_{.value}"
  )

# Final balanced panel
panel_monthly <- prices_monthly |>
  dplyr::left_join(export_wide, by = "Data_mes")

writexl::write_xlsx(panel_monthly, "Monthly_TilapiaBrazil.xlsx")


# -----------------------------------------------------------------------------
# 4. EXPLORATORY PLOTS
# -----------------------------------------------------------------------------

## 4.1  Export stacked-area chart --------------------------------------------

export_stack <- export_prices |>
  dplyr::mutate(Year = lubridate::year(Date)) |>
  dplyr::group_by(Year, Category) |>
  dplyr::summarise(FOB = sum(FOB, na.rm = TRUE), .groups = "drop")

# Repeat last available year as a placeholder for the forthcoming year
last_year_data <- dplyr::filter(export_stack, Year == max(Year)) |>
  dplyr::mutate(Year = Year + 1L)

export_stack <- dplyr::bind_rows(export_stack, last_year_data) |>
  dplyr::mutate(
    Category = factor(
      Category,
      levels = c("Fresh fillets", "Frozen fillets", "Frozen whole", "Fresh whole")
    )
  )

plot_export_stack <- ggplot2::ggplot(
  export_stack,
  ggplot2::aes(x = Year, y = FOB / 1e6, fill = Category)
) +
  ggplot2::geom_area(color = "black", linewidth = 0.2, alpha = 0.95) +
  ggplot2::scale_fill_manual(values = c(
    "Fresh fillets"  = "black",
    "Frozen fillets" = "gray80",
    "Frozen whole"   = "gray65",
    "Fresh whole"    = "gray25"
  )) +
  ggplot2::scale_x_continuous(breaks = unique(export_stack$Year)) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, ceiling(max(export_stack$FOB / 1e6) / 10) * 10, by = 10),
    expand = c(0, 0)
  ) +
  ggplot2::labs(x = "Year", y = "FOB Value (Million USD)", fill = NULL) +
  ggplot2::theme_bw(base_size = 14) +
  ggplot2::theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    panel.grid.minor = ggplot2::element_blank(),
    legend.text      = ggplot2::element_text(size = 11),
    legend.background = ggplot2::element_blank(),
    legend.key       = ggplot2::element_blank()
  )

save_plot(plot_export_stack, "Export_stack.png", width = 3600, height = 1860)

## 4.2  Price series panels ---------------------------------------------------

# Rename columns to publication labels before reshaping
prices_labelled <- panel_monthly |>
  dplyr::rename(
    "Farmgate - Grandes Lagos"    = Big_Lakes,
    "Farmgate - Norte do Paraná"  = Northern_Parana,
    "Farmgate - Oeste do Paraná"  = Western_Parana,
    "Wholesale"                   = Wholesale,
    "Exchange rate"               = FX,
    "Export - Fresh fillets"      = `Fresh fillets_Price_BRL`,
    "Export - Frozen whole"       = `Frozen whole_Price_BRL`,
    "Export - Fresh fillets USD"  = `Fresh fillets_Price_USD`,
    "Export - Frozen whole USD"   = `Frozen whole_Price_USD`
  )

prices_long <- prices_labelled |>
  tidyr::pivot_longer(-Data_mes, names_to = "Market", values_to = "Price")

# Shared aesthetic mappings
color_scale <- c(
  "Farmgate - Grandes Lagos"   = "black",
  "Farmgate - Oeste do Paraná" = "black",
  "Farmgate - Norte do Paraná" = "black",
  "Wholesale"                  = "gray35",
  "Export - Fresh fillets"     = "gray65",
  "Export - Frozen whole"      = "gray65",
  "Export - Fresh fillets USD" = "gray65",
  "Export - Frozen whole USD"  = "gray65",
  "Exchange rate"              = "gray85"
)

linetype_scale <- c(
  "Farmgate - Grandes Lagos"   = "solid",
  "Farmgate - Oeste do Paraná" = "dotted",
  "Farmgate - Norte do Paraná" = "dashed",
  "Wholesale"                  = "dotdash",
  "Export - Fresh fillets"     = "solid",
  "Export - Frozen whole"      = "dashed",
  "Export - Fresh fillets USD" = "solid",
  "Export - Frozen whole USD"  = "dashed",
  "Exchange rate"              = "twodash"
)

#' Build one time-series panel with a panel label annotation
build_price_panel <- function(data, y_label, panel_letter,
                               hide_x_axis = TRUE) {
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = Data_mes, y = Price, color = Market, linetype = Market)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = color_scale) +
    ggplot2::scale_linetype_manual(values = linetype_scale) +
    ggplot2::labs(y = y_label, x = if (hide_x_axis) NULL else "Year") +
    ggplot2::annotate(
      "text", x = Inf, y = Inf, label = panel_letter,
      hjust = 1.2, vjust = 1.5, fontface = "bold", size = 5
    ) +
    theme_publication()

  if (hide_x_axis) {
    p <- p + ggplot2::theme(
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  }
  p
}

panel_a <- build_price_panel(
  dplyr::filter(prices_long, Market %in% c("Export - Fresh fillets", "Export - Frozen whole")),
  y_label = "Price (BRL/kg)", panel_letter = "A"
)

panel_b <- build_price_panel(
  dplyr::filter(prices_long, Market %in% c(
    "Farmgate - Grandes Lagos", "Farmgate - Oeste do Paraná",
    "Farmgate - Norte do Paraná", "Wholesale"
  )),
  y_label = "Price (BRL/kg)", panel_letter = "B"
)

panel_c <- build_price_panel(
  dplyr::filter(prices_long, Market == "Exchange rate"),
  y_label = "Exchange rate\n(BRL/USD)", panel_letter = "C",
  hide_x_axis = FALSE
)

panel_d <- build_price_panel(
  dplyr::filter(prices_long, Market %in% c("Export - Fresh fillets USD", "Export - Frozen whole USD")),
  y_label = "Price (USD/kg)", panel_letter = "D"
)

combined_plot <- (panel_a / panel_b / panel_c) +
  patchwork::plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom") &
  ggplot2::guides(
    color    = ggplot2::guide_legend(nrow = 2, byrow = TRUE),
    linetype = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
  )

ggplot2::ggsave("Price_Series.png", combined_plot,
                width = 3100 / 400, height = 2400 / 400, dpi = 400)

## 4.3  Correlation heatmap ---------------------------------------------------

#' Build a lower-triangular correlation heatmap
build_corr_heatmap <- function(wide_df) {
  corr_mat  <- wide_df |> dplyr::select(-Data_mes) |>
    cor(use = "pairwise.complete.obs")
  var_order <- colnames(corr_mat)

  corr_long <- as.data.frame(as.table(corr_mat)) |>
    dplyr::rename(Market1 = Var1, Market2 = Var2, Correlation = Freq) |>
    dplyr::mutate(
      Market1 = factor(Market1, levels = var_order),
      Market2 = factor(Market2, levels = var_order)
    ) |>
    dplyr::filter(as.numeric(Market1) >= as.numeric(Market2))

  ggplot2::ggplot(corr_long, ggplot2::aes(Market1, Market2, fill = Correlation)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Correlation)), size = 4) +
    ggplot2::scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0, limits = c(-1, 1), name = "Correlation"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::scale_y_discrete(limits = rev(var_order)) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid   = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
}

# BRL prices (excluding USD export columns)
prices_brl_wide <- prices_long |>
  dplyr::filter(!Market %in% c("Export - Frozen whole USD", "Export - Fresh fillets USD")) |>
  dplyr::select(Data_mes, Market, Price) |>
  tidyr::pivot_wider(names_from = Market, values_from = Price)

# USD prices (excluding BRL export columns)
prices_usd_wide <- prices_long |>
  dplyr::filter(!Market %in% c("Export - Frozen whole", "Export - Fresh fillets")) |>
  dplyr::select(Data_mes, Market, Price) |>
  tidyr::pivot_wider(names_from = Market, values_from = Price)

save_plot(build_corr_heatmap(prices_brl_wide), "Price_Correlation_BRL.png",
          width = 2600, height = 1860)
save_plot(build_corr_heatmap(prices_usd_wide), "Price_Correlation_USD.png",
          width = 2600, height = 1860)


# -----------------------------------------------------------------------------
# 5. LOG-RETURNS
# -----------------------------------------------------------------------------

prices_matrix <- prices_labelled |> dplyr::select(-Data_mes)

returns_xts <- xts::xts(
  diff(log(as.matrix(prices_matrix))),
  order.by = panel_monthly$Data_mes[-1]
) |> na.omit()

returns_df <- data.frame(
  Data_mes = zoo::index(returns_xts),
  zoo::coredata(returns_xts),
  check.names = FALSE
)


# -----------------------------------------------------------------------------
# 6. DESCRIPTIVE STATISTICS & STATIONARITY
# -----------------------------------------------------------------------------

## 6.1  Summary statistics
cat("\n=== Descriptive Statistics ===\n")
print(round(descriptive_stats(returns_xts), 4))

## 6.2  ADF tests
cat("\n=== ADF Tests (k = 1) ===\n")
print(run_adf(returns_xts))

## 6.3  Ljung-Box tests
cat("\n=== Ljung-Box Tests (lag = 10) ===\n")
print(run_ljung_box(returns_xts))

## 6.4  Cross-sectional volatility plot
cross_vol <- xts::xts(
  sqrt(rowSums((returns_xts - rowMeans(returns_xts))^2)),
  order.by = zoo::index(returns_xts)
)

cross_vol_df <- data.frame(Time = zoo::index(cross_vol), Volatility = zoo::coredata(cross_vol))

plot_cross_vol <- ggplot2::ggplot(cross_vol_df, ggplot2::aes(Time, Volatility)) +
  ggplot2::geom_line(linewidth = 1, color = "black") +
  ggplot2::labs(x = "Time", y = "Cross-sectional Volatility") +
  theme_publication()

save_plot(plot_cross_vol, "Cross_sectional_volatility.png", width = 3800, height = 2060)


# -----------------------------------------------------------------------------
# 7. TVP-VAR CONNECTEDNESS (Antonakakis et al., 2020)
# -----------------------------------------------------------------------------

# Use BRL-denominated series only
returns_brl <- returns_xts[, !colnames(returns_xts) %in%
                               c("Export - Frozen whole USD", "Export - Fresh fillets USD")]
returns_zoo <- as.zoo(returns_brl)

# Lag order selection via Hannan-Quinn criterion
lag_select <- vars::VARselect(returns_zoo, lag.max = 4, type = "const")
best_lag   <- as.integer(lag_select$selection["HQ(n)"])

cat("\nSelected lag order (HQ):", best_lag, "\n")

connectedness_model <- ConnectednessApproach::ConnectednessApproach(
  returns_zoo,
  nlag       = best_lag,
  nfore      = 24,
  window.size = 24,
  model      = "TVP-VAR",
  connectedness = "Time"
)


# -----------------------------------------------------------------------------
# 8. VAR DIAGNOSTICS
# -----------------------------------------------------------------------------

var_model <- vars::VAR(returns_zoo, p = best_lag, type = "const")
var_resid  <- residuals(var_model)
lags_test  <- max(10L, 2L * best_lag)

## 8.1  Serial correlation
cat("\n=== Portmanteau Test ===\n")
print(vars::serial.test(var_model, lags.pt = lags_test, type = "PT.asymptotic"))

cat("\n=== Breusch-Godfrey Test ===\n")
print(vars::serial.test(var_model, lags.bg = lags_test, type = "BG"))

## 8.2  Stability
var_roots  <- vars::roots(var_model)
cat("\n=== VAR Characteristic Roots ===\n")
print(round(var_roots, 4))
cat("Model stable?", ifelse(all(var_roots < 1), "YES\n", "NO\n"))

## 8.3  Normality
cat("\n=== Multivariate Normality (Jarque-Bera) ===\n")
print(vars::normality.test(var_model))

norm_table <- run_normality_tests(var_resid)
cat("\n=== Univariate Normality Tests ===\n")
print(knitr::kable(norm_table, format = "simple", caption = "Residual normality"))

## 8.4  ARCH effects
cat("\n=== Multivariate ARCH Test ===\n")
MTS::MarchTest(var_resid, lag = lags_test)

arch_table <- run_arch_tests(var_resid, lags_test)
cat("\n=== Univariate ARCH-LM Tests ===\n")
print(knitr::kable(arch_table, format = "simple", caption = "ARCH-LM (univariate)"))


# -----------------------------------------------------------------------------
# 9. CONNECTEDNESS RESULTS
# -----------------------------------------------------------------------------

## 9.1  Print summary tables
cat("\n=== Spillover Table ===\n")
print(connectedness_model$TABLE)

cat("\n=== Total Connectedness Index (TCI) ===\n")
print(connectedness_model$TCI)

cat("\n=== FROM / TO / NET Directional Connectedness ===\n")
print(connectedness_model$FROM)
print(connectedness_model$TO)
print(connectedness_model$NET)

## 9.2  Save results
writexl::write_xlsx(connectedness_model$TABLE, "spillover_table.xlsx")
utils::write.csv(connectedness_model$FROM, "spillover_from.csv")
utils::write.csv(connectedness_model$TO,   "spillover_to.csv")
utils::write.csv(connectedness_model$NET,  "spillover_net.csv")

## 9.3  Connectedness plots
plots_connectedness <- list(
  list(fn = "Network_plot.png",   w = 2600, h = 1860, call = function() ConnectednessApproach::PlotNetwork(connectedness_model)),
  list(fn = "FROM_plot.png",      w = 3800, h = 2060, call = function() ConnectednessApproach::PlotFROM(connectedness_model)),
  list(fn = "TO_plot.png",        w = 3800, h = 2060, call = function() ConnectednessApproach::PlotTO(connectedness_model)),
  list(fn = "NET_plot.png",       w = 3800, h = 2060, call = function() ConnectednessApproach::PlotNET(connectedness_model)),
  list(fn = "TCI_plot.png",       w = 2600, h = 1860, call = function() ConnectednessApproach::PlotTCI(connectedness_model)),
  list(fn = "NPT_plot.png",       w = 3800, h = 2060, call = function() ConnectednessApproach::PlotNPT(connectedness_model))
)

invisible(lapply(plots_connectedness, function(p) {
  grDevices::png(p$fn, width = p$w, height = p$h, res = 400)
  p$call()
  grDevices::dev.off()
}))


# -----------------------------------------------------------------------------
# 10. REGRESSION ANALYSIS: EXPORT PRICES ~ EXCHANGE RATE + DOMESTIC PRICE
# -----------------------------------------------------------------------------

export_vars   <- c("Export - Frozen whole", "Export - Fresh fillets")
domestic_vars <- c(
  "Farmgate - Grandes Lagos",
  "Farmgate - Norte do Paraná",
  "Farmgate - Oeste do Paraná",
  "Wholesale"
)
exchange_var  <- "Exchange rate"

#' Fit and summarise one OLS regression
fit_price_model <- function(data, dep_var, exch_var, dom_var) {
  temp <- data |>
    dplyr::select(dplyr::all_of(c(dep_var, exch_var, dom_var))) |>
    na.omit()

  formula_str <- paste0("`", dep_var, "` ~ `", exch_var, "` + `", dom_var, "`")
  fit <- lm(as.formula(formula_str), data = temp)

  coef_tbl <- coef(summary(fit))
  s        <- summary(fit)

  list(
    model      = fit,
    coef_table = data.frame(
      Variable  = rownames(coef_tbl),
      Beta      = round(coef_tbl[, "Estimate"],    5),
      Std_Error = round(coef_tbl[, "Std. Error"],  5),
      t_value   = round(coef_tbl[, "t value"],     4),
      p_value   = round(coef_tbl[, "Pr(>|t|)"],    4),
      check.names = FALSE
    ),
    R2     = round(s$r.squared,     4),
    R2_adj = round(s$adj.r.squared, 4)
  )
}

all_models   <- list()
summary_rows <- list()

for (dep in export_vars) {
  cat("\n====================================================\n")
  cat("Dependent variable:", dep, "\n")
  cat("====================================================\n\n")

  for (dom in domestic_vars) {
    result     <- fit_price_model(returns_df, dep, exchange_var, dom)
    model_name <- paste(dep, "~", exchange_var, "+", dom)

    all_models[[model_name]] <- result$model

    cat("Domestic price:", dom, "\n\n")
    print(knitr::kable(result$coef_table, format = "simple",
                       caption = paste("Model:", model_name)))
    cat("\nR² =", result$R2, "| Adjusted R² =", result$R2_adj, "\n\n")

    # Build summary row
    cm <- coef(summary(result$model))
    coef_names <- rownames(cm)
    fx_name  <- coef_names[grepl(exchange_var, coef_names, fixed = TRUE)]
    dom_name <- coef_names[grepl(dom, coef_names, fixed = TRUE)]
    summary_rows[[model_name]] <- data.frame(
      Export_var    = dep,
      Domestic_var  = dom,
      Beta_FX       = round(cm[fx_name,  "Estimate"], 5),
      p_FX          = round(cm[fx_name,  "Pr(>|t|)"], 4),
      Beta_domestic = round(cm[dom_name, "Estimate"], 5),
      p_domestic    = round(cm[dom_name, "Pr(>|t|)"], 4),
      R2            = result$R2,
      R2_adj        = result$R2_adj
    )
  }
}

summary_table <- dplyr::bind_rows(summary_rows)

cat("\n====================================================\n")
cat("REGRESSION SUMMARY TABLE\n")
cat("====================================================\n\n")
print(knitr::kable(summary_table, format = "simple", digits = 4,
                   caption = "OLS regression results"))

## Package citation
citation("ConnectednessApproach")

# ============================================================
# 06_saglamlik_ekonomik_anlamlilik.R
# Robustness + indicative economic significance + diagnostics
# ============================================================

required_packages <- c(
  "readr", "dplyr", "tidyr", "sandwich", "lmtest", "jsonlite"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop("Eksik R paketleri: ", paste(missing_packages, collapse = ", "))
}

source(file.path("code", "00_config.R"))
source(file.path("code", "01_utils.R"))

ROOT <- find_project_root()
ensure_dirs(ROOT)

panel_path <- file.path(ROOT, "data", "processed", "panel_gunluk.csv")
robust_path <- file.path(ROOT, "results", "tables", "saglamlik_modelleri.csv")
econ_path <- file.path(ROOT, "results", "tables", "ekonomik_anlamlilik_var.csv")
diag_path <- file.path(ROOT, "results", "metrics", "tani_kontrolleri.json")
quant_path <- file.path(ROOT, "results", "tables", "kantil_katsayilari.csv")

dat <- readr::read_csv(panel_path, show_col_types = FALSE) |>
  dplyr::arrange(date)

# ------------------------------------------------------------
# 1. Robustness grid
# VIX off/on, FX off/on, nominal/real, lag off/on; both roles
# ------------------------------------------------------------

spec_grid <- expand.grid(
  role = c("Actor1", "Actor2"),
  outcome = c("nominal", "real"),
  include_vix = c(FALSE, TRUE),
  include_fx = c(FALSE, TRUE),
  include_lag = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

fit_one_spec <- function(row) {
  suffix <- if (row$role == "Actor1") "a1" else "a2"
  y <- if (row$outcome == "real") "real_return_bist" else "return_bist"

  rhs <- c(
    paste0("goldstein_", suffix),
    paste0("event_count_", suffix, "_c")
  )
  if (row$include_vix) rhs <- c(rhs, "return_vix")
  if (row$include_fx) rhs <- c(rhs, "return_usdtry")
  if (row$include_lag) rhs <- c(rhs, "return_bist_lag1")

  f <- stats::as.formula(paste(y, "~", paste(rhs, collapse = " + ")))
  m <- stats::lm(f, data = dat, na.action = na.omit)
  tab <- hac_coeftable(m)

  tab |>
    dplyr::mutate(
      role = row$role,
      outcome = row$outcome,
      include_vix = row$include_vix,
      include_fx = row$include_fx,
      include_lag = row$include_lag,
      model_formula = paste(deparse(f), collapse = ""),
      N = stats::nobs(m),
      return_unit = return_unit_label
    )
}

robust_tabs <- lapply(seq_len(nrow(spec_grid)), function(i) {
  fit_one_spec(spec_grid[i, , drop = FALSE])
})
robust_all <- dplyr::bind_rows(robust_tabs)

# Return outcome coefficient scale is converted for reporting.
robust_all <- robust_all |>
  dplyr::mutate(
    estimate_reported = scale_return_for_output(estimate),
    std_error_reported = scale_return_for_output(std_error)
  )

readr::write_csv(robust_all, robust_path)

# ------------------------------------------------------------
# 2. Indicative economic significance:
# Historical-simulation VaR by Goldstein tercile.
#
# This is descriptive/indicative only. It does not include
# transaction costs, liquidity constraints or investability.
# ------------------------------------------------------------

var_by_tercile <- function(data, role) {
  g <- if (role == "Actor1") "goldstein_a1" else "goldstein_a2"

  d <- data |>
    dplyr::select(return_bist, dplyr::all_of(g)) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      goldstein_tercile = dplyr::ntile(.data[[g]], 3L),
      goldstein_tercile = factor(
        goldstein_tercile,
        levels = 1:3,
        labels = c("Low", "Middle", "High")
      )
    )

  d |>
    dplyr::group_by(goldstein_tercile) |>
    dplyr::summarise(
      N = dplyr::n(),
      mean_return = mean(return_bist),
      median_return = stats::median(return_bist),
      VaR_5pct = stats::quantile(return_bist, 0.05, names = FALSE, type = 7),
      VaR_1pct = stats::quantile(return_bist, 0.01, names = FALSE, type = 7),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      role = role,
      mean_return = scale_return_for_output(mean_return),
      median_return = scale_return_for_output(median_return),
      VaR_5pct = scale_return_for_output(VaR_5pct),
      VaR_1pct = scale_return_for_output(VaR_1pct),
      return_unit = return_unit_label,
      interpretation_note = paste(
        "Indicative historical-simulation exercise only;",
        "transaction costs and liquidity constraints are not included."
      )
    )
}

econ <- dplyr::bind_rows(
  var_by_tercile(dat, "Actor1"),
  var_by_tercile(dat, "Actor2")
)
readr::write_csv(econ, econ_path)

# ------------------------------------------------------------
# 3. Degenerate quantile diagnostics
# Exact estimate == 0 and p == 1 cases + near-constant checks
# ------------------------------------------------------------

diagnostic_vars <- c(
  "goldstein_a1", "goldstein_a2",
  "event_count_a1_c", "event_count_a2_c",
  "return_vix", "return_usdtry", "return_bist_lag1"
)

var_diag <- lapply(diagnostic_vars, function(v) {
  x <- dat[[v]]
  x <- x[!is.na(x)]
  tab <- table(x)
  list(
    variable = v,
    N = length(x),
    unique_values = length(unique(x)),
    sd = if (length(x) > 1) stats::sd(x) else NA_real_,
    range = if (length(x) > 0) as.numeric(diff(range(x))) else NA_real_,
    tied_observations = if (length(tab) > 0) sum(tab[tab > 1]) else 0L,
    max_tie_frequency = if (length(tab) > 0) max(tab) else 0L,
    near_constant = if (length(x) > 1) {
      stats::sd(x) < sqrt(.Machine$double.eps) ||
        length(unique(x)) <= 2
    } else TRUE
  )
})

degenerate_rows <- list()
if (file.exists(quant_path)) {
  qt <- readr::read_csv(quant_path, show_col_types = FALSE)
  hit <- qt |>
    dplyr::filter(
      !is.na(estimate_raw), !is.na(p_value),
      abs(estimate_raw) <= .Machine$double.eps,
      abs(p_value - 1) <= .Machine$double.eps
    )

  if (nrow(hit) > 0) {
    degenerate_rows <- split(hit, seq_len(nrow(hit)))
    degenerate_rows <- lapply(degenerate_rows, as.list)
  }
}

diag_json <- list(
  return_unit = return_unit_label,
  variable_diagnostics = var_diag,
  exact_zero_p1_quantile_cases = degenerate_rows,
  diagnostic_rule = paste(
    "Cases with coefficient exactly 0 and p=1 are flagged.",
    "Independent-variable uniqueness, standard deviation, range and tied observations are reported."
  )
)

json_write(diag_json, diag_path)

cat("Robustness table:", robust_path, "\n")
cat("Indicative VaR table:", econ_path, "\n")
cat("Diagnostics:", diag_path, "\n")
cat("RETURN_UNIT:", return_unit_label, "\n")

check_outputs(ROOT)
